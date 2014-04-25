(ns eclj.boot
  (:refer-clojure :exclude [eval])
  (:require [eclj.interpret.cps :as cps]
            [eclj.env :refer (ns-env)]))

(defn eval
  ([form]
   (eval form (ns-env)))
  ([form env]
   (cps/interpret-result form env)))

(create-ns 'eclj.core)
(intern 'eclj.core 'eval eval)
(require '[eclj.ns])
(eclj.ns/load "/eclj/core")
