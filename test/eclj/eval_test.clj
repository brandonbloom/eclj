(ns eclj.eval-test
  (:refer-clojure :exclude [eval])
  (:require [eclj.eval]
            [eclj.interpret :refer (interpreter)]))

(defn interpret [x]
  (binding [eclj.eval/*evaluator* interpreter]
    (eclj.eval/eval x)))

(defn eval [x]
  (let [ret (interpret x)]
    (assert (= (clojure.core/eval x) ret)
            (str (pr-str x) " evaluated to " (pr-str ret)))
    (print ".") ;TODO: Better results reporting
    ret))

(eval 5)
(eval true)

(eval 'inc)
(eval #'inc)
(eval '#'inc)
(eval '(identity inc))
(eval 'Boolean)

(eval ())

(eval '(if true 5 10))
(eval '(if false 5 10))
(eval '(if true 5))
(eval '(if false 5))

(eval '(- 10 3))
(eval '((identity -) 10 3))
(eval '(+ (inc 5) (inc 10)))
(eval '(#'* (inc 4) 2))
(eval '(#'identity "hello"))

(eval '[1 "two" inc (+ 5 10)])
(eval '{1 "two" inc (+ 5 10)})
(eval '#{1"two" inc (+ 5 10)})

(eval '(do))
(eval '(do :x))
(eval '(do :x :y))
(eval ''(with-out-str (do (prn :x) (prn :y))))
(eval '(with-out-str (do (prn :x) (prn :y) (prn :z))))

(eval '(-> 8 inc (- 3)))

(eval '(let [] 1))
(eval '(let [x 2] x))
(eval '(let [x 2 y 4] (+ x y)))
(eval '(let [x 2 y 4 z 6] (+ x y z)))

(eval ''x)

(eval '((fn [] 1)))
(eval '((fn [x] x) 5))
(eval '(apply (fn [& args] (apply + args)) (range 1000)))

(eval '(try 1))
(eval '(try 1 (catch Throwable e 2)))
(eval '(with-out-str (try 1 (finally (prn 2)))))

(eval '(new String "abc"))
(eval '(String. "xyz"))

(eval '(. "abc" toUpperCase))
(eval '(. "abc" (toUpperCase)))
(eval '(.toUpperCase "abc"))
(eval '(. "abc" startsWith "x"))
(eval '(. "abc" (startsWith "x")))
(eval '(.startsWith "abc" "x"))

(eval 'Byte)
(eval '(. Byte TYPE))
(eval '(. String valueOf true))
(eval '(. String (valueOf true)))

(eval 'Byte/TYPE)
(eval '(identity Byte/TYPE))
(eval '(String/valueOf true))

(eval '(do (def ^:dynamic *foo* 1)
           (binding [*foo* 2]
             (set! *foo* 3)
             *foo*)))

(eval '((fn factorial [x]
          (if (<= x 1)
            1
            (* x (factorial (- x 1)))))
        5))

(eval '(letfn [(even? [x] (or (zero? x) (odd? (dec x))))
               (odd? [x] (and (not (zero? x)) (even? (dec x))))]
         ((juxt even? odd?) 11)))

(eval '((fn [acc n]
          (if (zero? n)
            acc
            (recur (+ acc n) (dec n))))
        0 10))

(eval '(loop [acc 0, n 10]
         (if (zero? n)
            acc
            (recur (+ acc n) (dec n)))))

;(eval '(case 5
;         5 :number))
;
;(eval '(case 5
;         5 :number
;         :default))
;
;(eval '(case "str"
;         5 :number
;         :default))
;
;(eval '(case [1 2 3]
;         5 :number
;         [1 2 3] :vector))

(eval '(import 'java.util.Date))


(comment

  ;;TODO: Create tests for these expected failures

  (def eval interpret)

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

  (eval 'foo.Bar)
  (eval '(import 'foo.Bar))

  ;TODO defprotocol
  ;TODO monitor-enter and monitor-exit

)
