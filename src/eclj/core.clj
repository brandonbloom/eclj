(ns eclj.core
  (:refer-clojure :only [])
  (:require [eclj.eval]
            [eclj.env :as env]
            [eclj.ns :refer (copy-vars)]
            [eclj.interpret.cps :refer (interpreter)]))

(copy-vars 'clojure.core :exclude '#{
  eval case deftype defrecord defprotocol
})

;;XXX This ties the recursive knot for eclj.fn/fn-apply
(alter-var-root #'eclj.eval/*evaluator* (constantly interpreter))

(defn eval [x]
  (eclj.eval/eval x (env/ns-env)))

(defmacro case [e & clauses]
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
