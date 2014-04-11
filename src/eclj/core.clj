(ns eclj.core
  (:refer-clojure :only [])
  (:require [eclj.eval]
            [eclj.env :as env]
            [eclj.ns :as ns]
            [eclj.interpret.cps :refer (interpreter)]))

(ns/publish-vars 'clojure.core :exclude '#{
  eval case ns deftype defrecord defprotocol refer-clojure
})

;;XXX This ties the recursive knot for eclj.fn/fn-apply
(alter-var-root #'eclj.eval/*evaluator* (constantly interpreter))

(defn eval [x]
  (eclj.eval/eval x (env/ns-env)))

(defmacro case
  [e & clauses]
  (let [default? (odd? (count clauses))
        cases (partition 2 (if default? (butlast clauses) clauses))
        table (into {} (map vec cases))]
    `(let [x# ~e]
       (if-let [[_# e#] (find ~table x#)]
         (eclj.eval/eval e# ~&env)
         (if ~default?
           (eclj.eval/eval ~(last clauses) ~&env)
           (throw (ex-info (str "No clause matching " (pr-str x#))
                           {:error :no-matching-clause :value x#})))))))

;TODO: Implement deftype and friends with support for eclj functionality.
(defmacro deftype [& args] `(clojure.core/eval '~&form))
(defmacro defrecord [& args] `(clojure.core/eval '~&form))
(defmacro defprotocol [& args] `(clojure.core/eval '~&form))

(defmacro refer-clojure
  "Same as (refer 'clojure.eclj <filters>)"
  [& filters]
  `(eclj.core/refer '~'eclj.core ~@filters))

(defmacro ns
  "Sets *ns* to the namespace named by name (unevaluated), creating it
  if needed.  references can be zero or more of: (:refer-clojure ...)
  (:require ...) (:use ...) (:import ...) (:load ...) (:gen-class)
  with the syntax of refer-clojure/require/use/import/load/gen-class
  respectively, except the arguments are unevaluated and need not be
  quoted. (:gen-class ...), when supplied, defaults to :name
  corresponding to the ns name, :main true, :impl-ns same as ns, and
  :init-impl-ns true. All options of gen-class are
  supported. The :gen-class directive is ignored when not
  compiling. If :gen-class is not supplied, when compiled only an
  nsname__init.class will be generated. If :refer-clojure is not used, a
  default (refer 'clojure.core) is used.  Use of ns is preferred to
  individual calls to in-ns/require/use/import:

  (ns foo.bar
    (:refer-clojure :exclude [ancestors printf])
    (:require (clojure.contrib sql combinatorics))
    (:use (my.lib this that))
    (:import (java.util Date Timer Random)
             (java.sql Connection Statement)))"
  {:arglists '([name docstring? attr-map? references*])
   :added "1.0"}
  [name & references]
  ;;TODO: gen-class
  (let [{:keys [name statements]} (ns/parse &form)]
    `(do
       (clojure.core/in-ns '~name)
       ~@statements
       nil)))

;;XXX Why do I need this? Seems like the clojure.core/ns var is special cased.
(.setMacro #'eclj.core/ns)

(defn load
  "Loads Clojure code from resources in classpath. A path is interpreted as
  classpath-relative if it begins with a slash or relative to the root
  directory for the current namespace otherwise."
  {:added "1.0"}
  [& paths]
  (doseq [path paths]
    (ns/load path)))

(defn require
  "Loads libs, skipping any that are already loaded. Each argument is
  either a libspec that identifies a lib, a prefix list that identifies
  multiple libs whose names share a common prefix, or a flag that modifies
  how all the identified libs are loaded. Use :require in the ns macro
  in preference to calling this directly.

  Libs

  A 'lib' is a named set of resources in classpath whose contents define a
  library of Clojure code. Lib names are symbols and each lib is associated
  with a Clojure namespace and a Java package that share its name. A lib's
  name also locates its root directory within classpath using Java's
  package name to classpath-relative path mapping. All resources in a lib
  should be contained in the directory structure under its root directory.
  All definitions a lib makes should be in its associated namespace.

  'require loads a lib by loading its root resource. The root resource path
  is derived from the lib name in the following manner:
  Consider a lib named by the symbol 'x.y.z; it has the root directory
  <classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj. The root
  resource should contain code to create the lib's namespace (usually by using
  the ns macro) and load any additional lib resources.

  Libspecs

  A libspec is a lib name or a vector containing a lib name followed by
  options expressed as sequential keywords and arguments.

  Recognized options:
  :as takes a symbol as its argument and makes that symbol an alias to the
    lib's namespace in the current namespace.
  :refer takes a list of symbols to refer from the namespace or the :all
    keyword to bring in all public vars.

  Prefix Lists

  It's common for Clojure code to depend on several libs whose names have
  the same prefix. When specifying libs, prefix lists can be used to reduce
  repetition. A prefix list contains the shared prefix followed by libspecs
  with the shared prefix removed from the lib names. After removing the
  prefix, the names that remain must not contain any periods.

  Flags

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose
  :reload forces loading of all the identified libs even if they are
    already loaded
  :reload-all implies :reload and also forces loading of all libs that the
    identified libs directly or indirectly load via require or use
  :verbose triggers printing information about each load, alias, and refer

  Example:

  The following would load the libraries clojure.zip and clojure.set
  abbreviated as 's'.

  (require '(clojure zip [set :as s]))"
  {:added "1.0"}

  [& args]
  (apply ns/load-libs :require args))

(defn use
  "Like 'require, but also refers to each lib's namespace using
  clojure.core/refer. Use :use in the ns macro in preference to calling
  this directly.

  'use accepts additional options in libspecs: :exclude, :only, :rename.
  The arguments and semantics for :exclude, :only, and :rename are the same
  as those documented for clojure.core/refer."
  {:added "1.0"}
  [& args] (apply ns/load-libs :require :use args))
