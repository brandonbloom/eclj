(ns eclj.fn
  (:refer-clojure :exclude [eval])
  (:require [eclj.common :refer (map->Syntax)]))

(def ^:dynamic *depth* 0)

(defn fn-apply [{:keys [env] :as f} arg]
  (let [eval (resolve 'eclj.core/eval)
        syntax (map->Syntax {:head :apply :f f :arg arg :env env})
        form (list 'eclj.core/eval (list 'quote syntax) env)]
    (binding [*depth* (inc *depth*)]
      ;(println "depth: " *depth*)
      (when (> *depth* 10)
        (throw (Exception. "fn-apply recursed too deeply")))
      (eval form))))

(defrecord Fn [name arities max-fixed-arity env]
  clojure.lang.Fn
  clojure.lang.IFn
  (applyTo [this args]
    (fn-apply this args))
  ;; *cringe*
  (invoke [this]
    (fn-apply this []))
  (invoke [this a]
    (fn-apply this [a]))
  (invoke [this a b]
    (fn-apply this [a b]))
  (invoke [this a b c]
    (fn-apply this [a b c]))
  (invoke [this a b c d]
    (fn-apply this [a b c d]))
  (invoke [this a b c d e]
    (fn-apply this [a b c d e]))
  (invoke [this a b c d e f]
    (fn-apply this [a b c d e f]))
  (invoke [this a b c d e f g]
    (fn-apply this [a b c d e f g]))
  (invoke [this a b c d e f g h]
    (fn-apply this [a b c d e f g h]))
  (invoke [this a b c d e f g h i]
    (fn-apply this [a b c d e f g h i]))
  (invoke [this a b c d e f g h i j]
    (fn-apply this [a b c d e f g h i j]))
  (invoke [this a b c d e f g h i j k]
    (fn-apply this [a b c d e f g h i j k]))
  (invoke [this a b c d e f g h i j k l]
    (fn-apply this [a b c d e f g h i j k l]))
  (invoke [this a b c d e f g h i j k l m]
    (fn-apply this [a b c d e f g h i j k l m]))
  (invoke [this a b c d e f g h i j k l m n]
    (fn-apply this [a b c d e f g h i j k l m n]))
  (invoke [this a b c d e f g h i j k l m n o]
    (fn-apply this [a b c d e f g h i j k l m n o]))
  (invoke [this a b c d e f g h i j k l m n o p]
    (fn-apply this [a b c d e f g h i j k l m n o p]))
  (invoke [this a b c d e f g h i j k l m n o p q]
    (fn-apply this [a b c d e f g h i j k l m n o p q]))
  (invoke [this a b c d e f g h i j k l m n o p q r]
    (fn-apply this [a b c d e f g h i j k l m n o p q r]))
  (invoke [this a b c d e f g h i j k l m n o p q r s]
    (fn-apply this [a b c d e f g h i j k l m n o p q r s]))
  (invoke [this a b c d e f g h i j k l m n o p q r s t]
    (fn-apply this [a b c d e f g h i j k l m n o p q r s t]))
  (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
    (fn-apply this (concat [a b c d e f g h i j k l m n o p q r s t] rest)))

  )
