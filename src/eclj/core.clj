(ns eclj.core
  (:refer-clojure :exclude [eval case deftype defrecord defprotocol])
  (:require [eclj.common :refer (map->Syntax)]
            [eclj.eval]
            [eclj.env :as env]
            [eclj.interpret :refer (interpreter)]))

(defn eval [x]
  (binding [eclj.eval/*evaluator* interpreter]
    (eclj.eval/eval x (env/ns-env))))

(defmacro case [e & clauses]
  (let [default? (odd? (count clauses))
        cases (partition 2 (if default? (butlast clauses) clauses))
        table (into {} (map vec cases))]
    `(let [x# ~e]
       (if-let [[_# e#] (find ~table x#)]
         (eclj.eval/eval e# ~&env)
         (if ~default?
           (eclj.eval/eval ~(last clauses) ~&env)
           (throw (ex-info {:error :no-matching-clause :value x#})))))))

;TODO: Implement deftype and friends with support for eclj functionality.
(defmacro deftype [& args] `(clojure.core/eval '~&form))
(defmacro defrecord [& args] `(clojure.core/eval '~&form))
(defmacro defprotocol [& args] `(clojure.core/eval '~&form))
