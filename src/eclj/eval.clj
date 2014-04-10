(ns eclj.eval
  (:refer-clojure :exclude [eval]))

(defprotocol Evaluator
  (eval-with* [this x env])
  (eval-with [this x env]))

(defn ->result [effect]
  (case (:op effect)
    :answer (:value effect)
    :throw (throw (:error effect))
    (throw (ex-info (pr-str effect) effect))))

(def evaluator-mixin
  {:eval-with (fn [evaluator x env]
                (->result (eval-with* evaluator x env)))})

(def ^:dynamic *evaluator*)

(defn eval*
  ([syntax] (eval* syntax (:env syntax)))
  ([x env] (eval-with* *evaluator* x env)))

(defn eval
  ([syntax] (eval syntax (:env syntax)))
  ([x env] (eval-with *evaluator* x env)))
