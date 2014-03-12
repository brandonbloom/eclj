(ns eclj.core-test
  (:refer-clojure :exclude [eval])
  (:require [eclj.core :as eclj]))

(defn eval [expr]
  (let [ret (eclj.core/eval expr)]
    (assert (= (clojure.core/eval expr) ret)
            (str (pr-str expr) " evaluated to " (pr-str ret)))
    ret))

(eval 5)
(eval true)

(eval 'inc)
(eval #'inc)
(eval '(identity inc))
(eval 'Boolean)

(eval ())

(eval '(if true 5 10))
(eval '(if false 5 10))
(eval '(if true 5))
(eval '(if false 5))

(eval '(- 10 3))
(eval '(+ (inc 5) (inc 10)))
(eval '(#'* (inc 4) 2))

(eval '[inc 10])
(eval '#{(+ 5 10)})

(eval '(do))
(eval '(do :x))
(eval '(do (prn :x) :y))
(eval '(do (prn :x) (prn :y) :z))

(eval '(-> 8 inc (- 3)))

(eval '(let [] 1))
(eval '(let [x 2] x))
(eval '(let [x 2 y 4] (+ x y)))
(eval '(let [x 2 y 4 z 6] (+ x y z)))

(eval ''x)

(eval '((fn [x] x) 5))
(eval '(apply (fn [& args] (apply + args)) (range 1000)))

(eval '(try 1))
(eval '(try 1 (catch Throwable e 2)))
(eval '(try 1 (finally (prn 2))))

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
