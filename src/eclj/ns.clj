(ns eclj.ns
  (:refer-clojure :exclude [load])
  (:require #_[eclj.core] ; eclj.core/eval provided by eclj.boot
            [eclj.env :as env]
            [eclj.reader :as reader])
  (:import [clojure.lang Compiler RT]))

;;XXX This function (especially :eclj/alias) is a dirty hack.
(defn publish-vars [ns & {:keys [exclude]}]
  (doseq [[sym var] (ns-publics ns)
          :when (not (exclude sym))]
    (let [metadata (assoc (meta var) :eclj/alias var)]
      (intern *ns* (with-meta sym metadata) @var))))

(defn parse [[_ sym & body :as form]]
  (let [doc (when (string? (first body)) (first body))
        refs (if doc (next body) body)
        sym (if doc (vary-meta sym assoc :doc doc) sym)
        metadata (when (map? (first refs)) (first refs))
        refs (if metadata (next refs) refs)
        gencls (first (filter #(= :gen-class (first %)) refs))
        refclj (first (filter #(= :refer-clojure (first %)) refs))
        stmts (if refclj [] ['(eclj.core/refer-clojure)])
        stmts (into stmts
                    (for [[f & args] refs
                          :when (not (= :gen-class f))]
                      `(~(symbol "eclj.core" (name f))
                        ~@(map #(list 'quote %) args))))
        sym (if metadata (vary-meta sym merge metadata) sym)]
    {:name sym
     :meta (meta sym)
     :statements stmts
     :gen-class gencls}))


;;;; Adapted from clojure.core below here.

(defn throw-if
  "Throws a CompilerException with a message if pred is true"
  [pred fmt & args]
  (when pred
    (let [^String message (apply format fmt args)
          exception (Exception. message)
          raw-trace (.getStackTrace exception)
          boring? #(not= (.getMethodName ^StackTraceElement %) "doInvoke")
          trace (into-array (drop 2 (drop-while boring? raw-trace)))]
      (.setStackTrace exception trace)
      (throw (clojure.lang.Compiler$CompilerException.
              *file*
              (.deref clojure.lang.Compiler/LINE)
              (.deref clojure.lang.Compiler/COLUMN)
              exception)))))

(defn libspec?
  "Returns true if x is a libspec"
  [x]
  (or (symbol? x)
      (and (vector? x)
           (or
            (nil? (second x))
            (keyword? (second x))))))

(defn prependss
  "Prepends a symbol or a seq to coll"
  [x coll]
  (if (symbol? x)
    (cons x coll)
    (concat x coll)))

(defn root-resource
  "Returns the root directory path for a lib"
  {:tag String}
  [lib]
  (str \/
       (.. (name lib)
           (replace \- \_)
           (replace \. \/))))

(defn root-directory
  "Returns the root resource path for a lib"
  [lib]
  (let [d (root-resource lib)]
    (subs d 0 (.lastIndexOf d "/"))))

(defn ensure-core []
  (when-not (find-ns 'eclj.core)
    (clojure.core/require 'eclj.core)))

(defn load-eclj-stream [stream path]
  (ensure-core)
  (with-bindings {Compiler/LOADER (RT/makeClassLoader)}
    (with-open [stream stream]
      (last (map eclj.core/eval (reader/form-seq stream path))))))

(defn load-eclj [name]
  (if-let [stream (RT/resourceAsStream (RT/baseLoader) name)]
    (let [path (.getPath (RT/getResource (RT/baseLoader) name))]
      (load-eclj-stream stream path))
    (throw (Exception.
             (str "Could not locate EClj resource on classpath: " name)))))

(defn load* [scriptbase]
  (let [classfile (str scriptbase RT/LOADER_SUFFIX ".class")
        srcs (map #(str scriptbase "." %) ["clj" "eclj"])
        class-url (RT/getResource (RT/baseLoader) classfile)
        src-urls (remove nil? (map #(RT/getResource (RT/baseLoader) %) srcs))]
    (when (> (count src-urls) 1)
      (throw (Exception. (str scriptbase " is ambiguous: "
                              (mapv #(.getPath %) src-urls)))))
    (let [url (first src-urls)]
      (cond
        (and class-url (or (not url)
                           (> (RT/lastModified class-url classfile)
                              (RT/lastModified url (.getPath url)))))
          (RT/load scriptbase)
        (not url)
          (throw (ex-info "Could not locate class or source file on classpath."
                         {:files (into [classfile] srcs)}))
        (.endsWith (.getPath url) ".eclj")
          (load-eclj (str scriptbase ".eclj"))
        :else (RT/load scriptbase)))))

(defn check-cyclic-dependency
  "Detects and rejects non-trivial cyclic load dependencies. The
  exception message shows the dependency chain with the cycle
  highlighted. Ignores the trivial case of a file attempting to load
  itself because that can occur when a gen-class'd class loads its
  implementation."
  [path]
  (when (some #{path} (rest @#'clojure.core/*pending-paths*))
    (let [pending (map #(if (= % path) (str "[ " % " ]") %)
                       (cons path @#'clojure.core/*pending-paths*))
          chain (apply str (interpose "->" pending))]
      (throw-if true "Cyclic load dependency: %s" chain))))

(defn load [path]
  (let [^String path (if (.startsWith path "/")
                          path
                          (str (root-directory (ns-name *ns*)) \/ path))]
      (when @#'clojure.core/*loading-verbosely*
        (printf "(eclj.core/load \"%s\")\n" path)
        (flush))
      (check-cyclic-dependency path)
      (when-not (= path (first @#'clojure.core/*pending-paths*))
        (binding [*ns* *ns* ; Because in-ns will set! this.
                  clojure.core/*pending-paths*
                  (conj @#'clojure.core/*pending-paths* path)]
          (load* (.substring path 1))))))

(defn load-one
  "Loads a lib given its name. If need-ns, ensures that the associated
  namespace exists after loading. If require, records the load so any
  duplicate loads can be skipped."
  [lib need-ns require]
  (load (root-resource lib))
  (throw-if (and need-ns (not (find-ns lib)))
            "namespace '%s' not found after loading '%s'"
            lib (root-resource lib))
  (when require
    (dosync
     (commute @#'clojure.core/*loaded-libs* conj lib))))

(defn load-all
  "Loads a lib given its name and forces a load of any libs it directly or
  indirectly loads. If need-ns, ensures that the associated namespace
  exists after loading. If require, records the load so any duplicate loads
  can be skipped."
  [lib need-ns require]
  (dosync
   (commute @#'clojure.core/*loaded-libs* #(reduce conj %1 %2)
            (binding [clojure.core/*loaded-libs* (ref (sorted-set))]
              (load-one lib need-ns require)
              @#'clojure.core/*loaded-libs*))))

(defn load-lib
  "Loads a lib with options"
  [prefix lib & options]
  (throw-if (and prefix (pos? (.indexOf (name lib) (int \.))))
            "Found lib name '%s' containing period with prefix '%s'.  lib names inside prefix lists must not contain periods"
            (name lib) prefix)
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)
        {:keys [as reload reload-all require use verbose]} opts
        loaded (contains? @@#'clojure.core/*loaded-libs* lib)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        need-ns (or as use)
        filter-opts (select-keys opts '(:exclude :only :rename :refer))
        undefined-on-entry (not (find-ns lib))]
    (binding [clojure.core/*loading-verbosely* (or @#'clojure.core/*loading-verbosely* verbose)]
      (if load
        (try
          (load lib need-ns require)
          (catch Exception e
            (when undefined-on-entry
              (remove-ns lib))
            (throw e)))
        (throw-if (and need-ns (not (find-ns lib)))
                  "namespace '%s' not found" lib))
      (when (and need-ns @#'clojure.core/*loading-verbosely*)
        (printf "(eclj.core/in-ns '%s)\n" (ns-name *ns*)))
      (when as
        (when @#'clojure.core/*loading-verbosely*
          (printf "(eclj.core/alias '%s '%s)\n" as lib))
        (alias as lib))
      (when (or use (:refer filter-opts))
        (when @#'clojure.core/*loading-verbosely*
          (printf "(eclj.core/refer '%s" lib)
          (doseq [opt filter-opts]
            (printf " %s '%s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (apply refer lib (mapcat seq filter-opts))))))

(defn load-libs
  "Loads libs, interpreting libspecs, prefix lists, and flags for
  forwarding to load-lib"
  [& args]
  (let [flags (filter keyword? args)
        opts (interleave flags (repeat true))
        args (filter (complement keyword?) args)]
    ; check for unsupported options
    (let [supported #{:as :reload :reload-all :require :use :verbose :refer}
          unsupported (seq (remove supported flags))]
      (throw-if unsupported
                (apply str "Unsupported option(s) supplied: "
                     (interpose \, unsupported))))
    ; check a load target was specified
    (throw-if (not (seq args)) "Nothing specified to load")
    (doseq [arg args]
      (if (libspec? arg)
        (apply load-lib nil (prependss arg opts))
        (let [[prefix & args] arg]
          (throw-if (nil? prefix) "prefix cannot be nil")
          (doseq [arg args]
            (apply load-lib prefix (prependss arg opts))))))))
