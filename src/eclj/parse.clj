(ns eclj.parse
  (:require [eclj.common :refer (pure)]))

;TODO: Is "parse" the right word?
;TODO: file/line/column and other metadata
;TODO: namespaced heads
;TODO: validation conditions.
; ie raise error for (var inc inc), (quote x x), odd number bindings, etc

(defprotocol Expression
  (-parse [expr env]))

(defrecord Syntax [head form env]
  Expression
  (-parse [this env] this))

(defn parse [form env]
  (pure (map->Syntax (-parse form env))))

(defn parse-constant [x env]
  {:head :constant :form x :env env :value x})

(defn parse-collection [coll env]
  {:head :collection :form coll :env env})

(doseq [t [nil java.lang.Object]]
  (extend t Expression {:-parse parse-constant}))

(defmulti parse-seq (fn [xs env] (first xs)))

(defn parse-apply [[f & args :as form] env]
  {:head :apply :form form :env env :f f :args (vec args)})

(extend-protocol Expression

  clojure.lang.Symbol
  (-parse [sym env]
    {:head :name :form sym :env env})

  clojure.lang.ISeq
  (-parse [xs env]
    (cond
      (empty? xs) (parse-constant xs env)
      (symbol? (first xs)) (parse-seq xs env)
      :else (parse-apply xs env)))

  clojure.lang.AMapEntry
  (-parse [kvp env]
    (parse-collection (vec kvp) env))

)

(doseq [t [clojure.lang.PersistentArrayMap
           clojure.lang.PersistentHashMap
           clojure.lang.PersistentHashSet
           clojure.lang.PersistentQueue
           clojure.lang.PersistentTreeMap
           clojure.lang.PersistentTreeSet
           clojure.lang.PersistentVector]]
  (extend t Expression {:-parse parse-collection}))

(defmethod parse-seq :default
  [form env]
  ;TODO: expand-dot
  (parse-apply form env))

(defmethod parse-seq 'if
  [[_ test then else :as form] env]
  {:head :if :form form :env env
   :test test :then then :else else})

(defmethod parse-seq 'var
  [[_ sym :as form] env]
  {:head :var :form form :env env :sym sym})

(defmethod parse-seq 'do
  [[_ & body :as form] env]
  (let [v (vec body)]
    {:head :do :form form :env env
     :statements (pop v) :ret (peek v)}))

(defmethod parse-seq 'quote
  [[_ value :as form] env]
  {:head :quote :form form :env env :value value})

(defn implicit-do [body]
  (case (count (take 2 body))
    0 nil
    1 (first body)
    (list* 'do body)))

(defmethod parse-seq 'let*
  [[_ bindings & body] env]
  {:head :let
   :bindings (mapv (fn [[name init]] {:name name :init init})
                   (partition 2 bindings))
   :expr (implicit-do body)})

;TODO (defmethod parse-seq 'fn*
;TODO (defmethod parse-seq 'letfn*

(defmethod parse-seq 'try
  [[_ & body :as form] env]
  (let [catch? (every-pred seq? #(= (first %) 'catch))
        default? (every-pred catch? #(= (second %) :default))
        finally? (every-pred seq? #(= (first %) 'finally))]
    (loop [{:keys [state forms body] :as parser}
           {:state :start :forms body :body []
            :catches [] :default nil :finally nil}]
      (if-let [[form & forms*] forms]
        (let [parser* (assoc parser :forms forms*)]
          (case state
            :start
              (cond
                (catch? form) (recur (assoc parser :state :catches))
                (finally? form) (recur (assoc parser :state :finally))
                :else (recur (update-in parser* [:body] conj form)))
            :catches
              (cond
                (default? form)
                  (let [[_ _ name & dbody] form
                        default {:name name :expr (implicit-do dbody)}]
                    (recur (assoc parser* :default default :state :finally)))
                (catch? form)
                  (let [[_ type name & cbody] form
                        catch {:type type :name name
                               :expr (implicit-do cbody)}]
                    (recur (update-in parser* [:catches] conj catch)))
                (finally? form)
                  (recur (assoc parser :state :finally))
                :else
                  (throw (Exception. "Invalid try form")))
            :finally
              (let [[_ & fbody] form
                    finally (implicit-do fbody)]
                (recur (assoc parser* :finally finally :state :done)))
            :done
              (throw (Exception. "Unexpected form after finally"))))
        (-> parser
          (select-keys [:catches :default :finally])
          (assoc :head :try :form form :env env
                 :try (-> parser :body implicit-do)))))))

(defmethod parse-seq 'throw
  [[_ expr :as form] env]
  {:head :throw :form form :env env :expr expr})

(defmethod parse-seq 'def
  [[_ & body :as form] env]
  (let [[sym doc expr] (case (count body)
                         1 [(first body)]
                         2 [(first body) nil (second body)]
                         3 body)
        doc (or doc (-> sym meta :doc))]
    (merge
      {:sym sym :doc doc :form form :env env}
      (if (> (count body) 1)
        {:head :define :expr expr}
        {:head :declare}))))

(defmethod parse-seq 'new
  [[_ class & args :as form] env]
  {:head :new :form form :env env :class class :args (vec args)})

(defmethod parse-seq '.
  [[_ target & body :as form] env]
  (let [[member args] (if (and (= (count body) 1) (seq? (first body)))
                        [(ffirst body) (nfirst body)]
                        [(first body) (next body)])]
    {:head :interop :form form :env env
     :target target :member member :args (vec args)}))

;TODO (defmethod parse-seq 'set!
;TODO (defmethod parse-seq 'loop*
;TODO (defmethod parse-seq 'recur
;TODO (defmethod parse-seq 'clojure.core/import*
;TODO (defmethod parse-seq 'case
;TODO (defmethod parse-seq 'clojure.core/case
;TODO (defmethod parse-seq 'deftype* [form env]
;TODO (defmethod parse-seq 'reify* [form env]


(comment

  (require 'eclj.env)
  (defn ! [x]
    (fipp.edn/pprint (parse x (eclj.env/ns-env))))

  (! (parse 1 (eclj.env/ns-env)))

  (! nil)
  (! 5)
  (! ())
  (! '())
  (! '(f x))
  (! '(quote (f x)))
  (! '(let* []))
  (! '(let* [x 1 y 2] (+ x y)))
  (! '(let* [x 1 y 2] (println "!") (+ x y)))
  (! '(try))
  (! '(try 1))
  (! '(try 1 2))
  (! '(try 1 (catch Exception e 2)))
  (! '(try 1 (catch Exception e 2 3)))
  (! '(try 1 (catch RuntimeException e 2) (catch Exception e 3)))
  (! '(try 1 (catch :default e 2)))
  (! '(try 1 (catch :default e 2 3)))
  (! '(try 1 (finally 2 3)))
  (! '(throw x))
  (! '(def x))
  (! '(def ^{:doc "foo"} x))
  (! '(def x 1))
  (! '(def x "foo" 1))
  (! '(. x y z))

)
