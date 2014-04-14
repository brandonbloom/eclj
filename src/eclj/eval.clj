(ns eclj.eval)

(def ^:dynamic *evaluator*)

(defn effect [x env]
  ((:effect *evaluator*) x env))

(defn result [x env]
  ((:result *evaluator*) x env))
