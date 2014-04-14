(ns eclj.ext-test
  (:refer-clojure :exclude [eval])
  (:require [eclj.core]))

(eclj.core/require 'eclj.ext)

(eclj.ext/eval 1)
(eclj.ext/eval '(+ 2 4))

(eclj.ext/eval '(eclj.ext/handle-with
                  (fn [effect]
                    (when (= (:op effect) :answer)
                      {:op :answer :value (inc (:value effect))}))
                  5))
