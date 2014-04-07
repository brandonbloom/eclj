(ns eclj.eval
  (:refer-clojure :exclude [eval])
  (:require [eclj.env :as env]
            [eclj.interpret :refer (interpret)]))

(defn eval
  ([x]
   (interpret x (env/ns-env)))
  ([x env]
   (interpret x env)))
