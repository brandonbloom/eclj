(ns eclj.ext-test
  (:refer-clojure :exclude [eval])
  (:require [eclj.core]))

;;TODO: Assertions

(eclj.core/require 'eclj.ext :reload)

(eclj.ext/eval 1)
(eclj.ext/eval '(+ 2 4))

(eclj.ext/eval '(eclj.ext/handle-with
                  (fn [effect]
                    (when (= (:op effect) :answer)
                      {:op :answer :value (inc (:value effect))}))
                  5))

(eclj.ext/eval '(eclj.ext/handle-with
                  (fn [effect]
                    (when (= (:op effect) :foo)
                      {:op :answer :value :bar}))
                  (eclj.ext/raise {:op :foo})))
