(ns eclj.eval
  (:refer-clojure :exclude [eval]))

(def ^:dynamic *evaluator*)

(defn eval-cps [x env]
  ((:eval-cps *evaluator*) x env))

(defn eval [x env]
  ((:eval *evaluator*) x env))
