(ns eclj.core
  (:refer-clojure :only [])
  (:require [eclj.eval]
            [eclj.env :as env]
            [eclj.ns :as ns]
            [eclj.interpret.cps :refer (interpreter)]))

(ns/copy-vars 'clojure.core :exclude '#{
  eval case deftype defrecord defprotocol refer-clojure
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

  (prn (ns/parse &form))
  (inc 5)
  nil)
