(ns eclj.eval-test
  (:refer-clojure :exclude [eval])
  (:require [eclj.core]))

(alter-var-root #'eclj.eval/*evaluator*
                (constantly eclj.interpret.cps/interpreter))
;                (constantly eclj.interpret.meta/interpreter))

;;TODO: Better results reporting
(defn pass [] (print "."))
(defn fail [] (print "X"))
(defn pass-fail [x] (if x (pass) (fail)))

(defn =clj [x]
  (let [ret (eclj.core/eval x)]
    (assert (= (clojure.core/eval x) ret)
            (str (pr-str x) " evaluated to " (pr-str ret)))
    (pass)
    ret))

(defmacro expect [pred expr]
  `(let [x# (eclj.core/eval ~expr)]
     (pass-fail (~pred x#))
     x#))

(defmacro throws [pred expr]
  `(try
     (let [x# (eclj.core/eval ~expr)]
       (fail)
       x#)
     (catch Throwable e#
       (pass-fail (~pred (ex-data e#)))
       e#)))

(=clj 5)
(=clj true)
(=clj "str")

(=clj 'inc)
(=clj #'inc)
(=clj '#'inc)
(=clj '(identity inc))
(=clj 'Boolean)
(throws #(= (:error %) :undefined) 'something-undefined)

(=clj ())

(=clj '(if true 5 10))
(=clj '(if false 5 10))
(=clj '(if true 5))
(=clj '(if false 5))
(throws #(= (:error %) :undefined) '(if xx 5))

(=clj '(- 10 3))
(=clj '((identity -) 10 3))
(=clj '(+ (inc 5) (inc 10)))
(=clj '(#'* (inc 4) 2))
(=clj '(#'identity "hello"))

(=clj '[1 "two" inc (+ 5 10)])
(=clj '{1 "two" inc (+ 5 10)})
(=clj '#{1"two" inc (+ 5 10)})

(=clj '(do))
(=clj '(do :x))
(=clj '(do :x :y))
(=clj ''(with-out-str (do (prn :x) (prn :y))))
(=clj '(with-out-str (do (prn :x) (prn :y) (prn :z))))

(=clj '(-> 8 inc (- 3)))

(=clj '(let [] 1))
(=clj '(let [x 2] x))
(=clj '(let [x 2 y 4] (+ x y)))
(=clj '(let [x 2 y 4 z 6] (+ x y z)))

(=clj ''x)

(=clj '((fn [] 1)))
(=clj '((fn [x] x) 5))
(=clj '(apply (fn [& args] (apply + args)) (range 1000)))

(expect fn? '(fn []))
(expect fn? '(fn [x] x))
(expect fn? '(fn ([x] x)))
(expect fn? '(fn f [x] x))
(expect fn? '(fn f ([x] x)))
(expect fn? '(fn ([] 0) ([x] 1) ([x y] 2)))
(expect fn? '(fn ([] 0) ([x] 1) ([x y] 2) ([x y & zs] :n)))

(pass-fail (= 5 ((eclj.core/eval '(fn [x] x)) 5)))

(expect (complement bound?) '(def declared))
(expect bound? '(def defined 1))
(expect #(= @% 3) '(do (def redefined 2) (def redefined 3)))
(expect #(= @% 4) '(def foo "bar" 4))
(expect #(= (-> % meta :doc) "bar") '(def foo "bar" 4))

(=clj '(try 1))
(=clj '(try 1 (catch Throwable e 2)))
(=clj '(with-out-str (try 1 (finally (prn 2)))))
(throws (constantly true) '(throw (ex-info "err" {})))
(throws (constantly true) '(try (throw (ex-info "err" {}))))
(throws (constantly true)
        '(try 1 (throw (ex-info "err" {})) 2
              (catch IllegalArgumentException e 2)))
(pass-fail (= 3 (eclj.core/eval '(try (throw (ex-info "err" {}))
                                      (catch :default e 3)))))
(expect #(instance? Exception %)
        '(try (throw (ex-info "err" {}))
              (catch :default e e)))
(expect #(= % "2")
  (with-out-str
    (eclj.core/eval '(try (throw (ex-info "err" {}))
                          (catch :default e e)
                          (finally (print 2))))))
(throws #(= (:error %) :non-tail-position) '(loop [] (inc (recur))))

(=clj '(import [java.util Date Currency]))
(throws (constantly true) '(var Class))

(=clj '(new String "abc"))
(=clj '(String. "xyz"))

(=clj '(. "abc" toUpperCase))
(=clj '(. "abc" (toUpperCase)))
(=clj '(.toUpperCase "abc"))
(=clj '(. "abc" startsWith "x"))
(=clj '(. "abc" (startsWith "x")))
(=clj '(.startsWith "abc" "x"))

(=clj 'Byte)
(=clj '(. Byte TYPE))
(=clj '(. String valueOf true))
(=clj '(. String (valueOf true)))

(=clj 'Byte/TYPE)
(=clj '(identity Byte/TYPE))
(=clj '(String/valueOf true))

(=clj '(do (def ^:dynamic *foo* 1)
           (binding [*foo* 2]
             (set! *foo* 3)
             *foo*)))

(=clj '((fn factorial [x]
          (if (<= x 1)
            1
            (* x (factorial (- x 1)))))
        5))

(=clj '(letfn [(even? [x] (or (zero? x) (odd? (dec x))))
               (odd? [x] (and (not (zero? x)) (even? (dec x))))]
         ((juxt even? odd?) 11)))

(=clj '((fn [acc n]
          (if (zero? n)
            acc
            (recur (+ acc n) (dec n))))
        0 10))

(=clj '(loop [acc 0, n 10]
         (if (zero? n)
            acc
            (recur (+ acc n) (dec n)))))

(=clj '(import 'java.util.Date))

(=clj '(case 5
         5 :number))

(=clj '(case 5
         5 :number
         :default))

(=clj '(case "str"
         5 :number
         :default))

(=clj '(case [1 2 3]
         5 :number
         [1 2 3] :vector))

(throws #(= (:error %) :no-matching-clause)
        '(case "str" 5 :number))

(comment

  (def eval eclj.core/eval)

  (eval '(.valueOf String true)) ;XXX Not expected to work

  (time
    (dotimes [i 200]
      (eval '((fn factorial [x]
                (if (<= x 1)
                  1
                  (* x (factorial (- x 1)))))
              20))))

  (eval '(deftype Foo [bar]))
  (eval '(Foo. 1))
  (eval '(defrecord Point [x y]))
  (eval '(Point. 5 10))

  (eval (list 'set! (list '.__methodImplCache (fn [])) ; explicitly a host fn.
           (list 'clojure.lang.MethodImplCache. nil nil)))

  (eval '(defprotocol P))
  (eval '(defprotocol P (f [this])))
  (eval '(extend-protocol P Foo (f [this] :foo)))
  (eval '(f (Foo. 2)))
  (eval '(f (reify P (f [this] :reified))))

  (eval 'foo.Bar)
  (eval '(import 'foo.Bar))

)
