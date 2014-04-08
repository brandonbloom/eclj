(ns eclj.eval
  (:refer-clojure :exclude [eval])
  (:require [eclj.env :as env]))

(defprotocol Evaluator
  (eval-with* [this x env])
  (eval-with [this x env]))

(def evaluator-mixin
  {:eval-with (fn [evaluator x env]
                (let [y (eval-with* evaluator x env)]
                  (if (= (:op y) :answer)
                    (:value y)
                    (throw (ex-info (pr-str y) y)))))})

(def ^:dynamic *evaluator*)

(defn eval*
  ([x] (eval* x (env/ns-env)))
  ([x env] (eval-with* *evaluator* x env)))

(defn eval
  ([x] (eval x (env/ns-env)))
  ([x env] (eval-with *evaluator* x env)))
