(ns eclj.core
  (:refer-clojure :exclude [eval case])
  (:require [eclj.eval]))

(def eval eclj.eval/eval)

(defmacro case [e & clauses]
  (let [default? (odd? (count clauses))
        cases (partition 2 (if default? (butlast clauses) clauses))
        table (into {} (map vec cases))]
    `(let [x# ~e]
       (if-let [[_# e#] (find ~table x#)]
         (eval e# ~&env)
         (if ~default?
           (eval ~(last clauses) ~&env)
           (throw (ex-info {:error :no-matching-clause :value x#})))))))

;TODO deftype, defprotocol, etc
