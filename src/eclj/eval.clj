(ns eclj.eval
  (:refer-clojure :exclude [eval])
  (:require [eclj.env :as env]
            [eclj.interpret :refer (interpret)]))

(defn eval
  ([x]
   (interpret x (env/ns-env)))
  ([x env]
   (interpret x env)))

(comment

  (eval '(if xx 5))
  (eval 'foo)

  (eval '(fn []))
  (eval '(fn [x] x))
  (eval '(fn ([x] x)))
  (eval '(fn f [x] x))
  (eval '(fn f ([x] x)))
  (eval '(fn ([] 0) ([x] 1) ([x y] 2)))
  (eval '(fn ([] 0) ([x] 1) ([x y] 2) ([x y & zs] :n)))

  ((eval '(fn [x] x)) 5)

  (eval '(def declared))
  (eval '(def defined 1))
  (eval '(do (def redefined 2) (def redefined 3)))
  (eval '(def foo "bar" 4))
  [declared defined redefined foo (-> #'foo meta :doc)]

  (eval '(throw (ex-info "err" {})))
  (eval '(try (throw (ex-info "err" {}))))
  (eval '(try 1 (throw (ex-info "err" {})) 2
              (catch IllegalArgumentException e 2)))
  (eval '(try (throw (ex-info "err" {})) (catch :default e 3)))
  (eval '(try (throw (ex-info "err" {})) (catch :default e e)))
  (eval '(try (throw (ex-info "err" {}))
              (catch :default e e)
              (finally (prn 2))))

  (eval '(Apply. inc 5))
  (eval '(.valueOf String true))

  (time
    (dotimes [i 200]
      (eval '((fn factorial [x]
                (if (<= x 1)
                  1
                  (* x (factorial (- x 1)))))
              20))))

  (eval '(loop [] (inc (recur))))
  (eval '(import [java.util Date Currency]))

  (eval '(case "str"
           5 :number))

  (eval '(deftype Foo [bar]))
  (eval '(Foo. 1))
  (eval '(defrecord Point [x y]))

  (eval (list 'set! (list '.__methodImplCache (fn [])) ; explicitly a host fn.
                    (list 'clojure.lang.MethodImplCache. nil nil)))

  (eval '(defprotocol P))
  (eval '(defprotocol P (f [this])))

  (eval '(var Class))

  ;TODO defprotocol
  ;TODO monitor-enter and monitor-exit

)
