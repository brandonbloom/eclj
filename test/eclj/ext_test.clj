(ns eclj.ext-test
  (:refer-clojure :exclude [eval])
  (:require [eclj.boot]))

(eclj.core/require 'eclj.ext :reload)

;;TODO: Assertions

(comment

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

  (eclj.ext/eval
    '(eclj.ext/handle-with
       (fn [{:keys [op k] :as effect}]
         (println "outer")
         (fipp.edn/pprint effect)
         (println)
         nil)
       (eclj.ext/handle-with
         (fn [{:keys [op k] :as effect}]
           (println "inner")
           (fipp.edn/pprint effect)
           (println)
           (when (= op :foo)
             {:op :answer :value (eclj.ext/continue k 123)}))
         (println "innermost")
         (eclj.ext/raise {:op :foo}))))

  (eclj.ext/eval
    '(eclj.ext/handle-with
       (fn [{:keys [op k] :as effect}]
         (println "outer")
         (fipp.edn/pprint effect)
         (println)
         (when (= op :foo)
           {:op :answer :value (eclj.ext/continue k 123)}))
       (eclj.ext/handle-with
         (fn [{:keys [op k] :as effect}]
           (println "inner")
           (fipp.edn/pprint effect)
           (println)
           nil)
         (println "innermost")
         (eclj.ext/raise {:op :foo}))))

)
