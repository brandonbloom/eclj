(ns eclj.boot
  (:require [eclj.interpret.cps :as cps]
            [eclj.env :refer (ns-env)]))

(defn boot-eval
  ([form]
   (boot-eval form (ns-env)))
  ([form env]
   (cps/interpret-result form env)))

(create-ns 'eclj.core)
(intern 'eclj.core 'eval boot-eval)
(require '[eclj.ns])
(eclj.ns/load "/eclj/core")
