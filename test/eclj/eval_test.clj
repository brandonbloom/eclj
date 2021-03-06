(ns eclj.eval-test
  (:refer-clojure :exclude [eval])
  (:use [clojure.test])
  (:require [eclj.core]))

(defmethod assert-expr `=clj [msg [_ form]]
  `(let [expected# (clojure.core/eval ~form)
         actual# (eclj.core/eval ~form)]
     (do-report {:type (if (= expected# actual#) :pass :fail)
                 :message ~msg :expected expected# :actual actual#})
     actual#))

(defmacro =clj [expr]
  `(is (=clj ~expr)))

(defmethod assert-expr `throws [msg [_ pred expr]]
  (let [check `(~pred (ex-data ~'e))]
    `(try
       (eclj.core/eval ~expr)
       (do-report {:type :fail :message ~msg :expected '~check :actual nil})
       (catch Throwable ~'e
         (do-report {:type (if ~check :pass :fail)
                     :message ~msg :expected '~check :actual ~'e})
         ~'e))))

(defmacro throws [pred expr]
  `(is (throws ~pred ~expr)))

(defmacro expect [pred expr]
  `(is (~pred (eclj.core/eval ~expr))))

(deftest eval-test

(=clj 5)
(=clj true)
(=clj "str")

(=clj 'inc)
(=clj #'inc)
(=clj '#'inc)
(=clj '(identity inc))
(=clj '(#'identity #'inc))
(=clj 'Boolean)
(throws #(= (-> % :eclj/effect :error) :undefined) 'something-undefined)

(=clj ())

(=clj '(if true 5 10))
(=clj '(if false 5 10))
(=clj '(if true 5))
(=clj '(if false 5))
(throws #(= (-> % :eclj/effect :error) :undefined) '(if xx 5))

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
;XXX (=clj '(eclj.core/apply (fn [& args] (eclj.core/apply + args)) (range 1000)))
(=clj '(clojure.core/apply (fn [x] x) 'a []))
(=clj '(eclj.core/apply (fn [x] x) 'a []))

(expect fn? '(fn []))
(expect fn? '(fn [x] x))
(expect fn? '(fn ([x] x)))
(expect fn? '(fn f [x] x))
(expect fn? '(fn f ([x] x)))
(expect fn? '(fn ([] 0) ([x] 1) ([x y] 2)))
(expect fn? '(fn ([] 0) ([x] 1) ([x y] 2) ([x y & zs] :n)))

(is (= 5 ((eclj.core/eval '(fn [x] x)) 5)))

(expect (complement bound?) '(def declared))
(expect bound? '(def defined 1))
(expect #(= @% 3) '(do (def redefined 2) (def redefined 3)))
(expect #(= @% 4) '(def foo "bar" 4))
(expect #(= (-> % meta :doc) "bar") '(def foo "bar" 4))
(=clj '(-> (defn asdf [a b c]) meta :arglists))

(=clj '(try 1))
(=clj '(try 1 (catch Throwable e 2)))
(=clj '(with-out-str (try 1 (finally (prn 2)))))
(throws (constantly true) '(throw (ex-info "err" {})))
(throws (constantly true) '(try (throw (ex-info "err" {}))))
(throws (constantly true)
        '(try 1 (throw (ex-info "err" {})) 2
              (catch IllegalArgumentException e 2)))
(is (= 3 (eclj.core/eval '(try (throw (ex-info "err" {}))
                               (catch Exception e 3)))))
(is (= 3 (eclj.core/eval '(try (throw (ex-info "err" {}))
                               (catch :default e 3)))))
(expect #(instance? Exception %)
        '(try (throw (ex-info "err" {}))
              (catch :default e e)))
(expect #(= % "2")
  (with-out-str
    (eclj.core/eval '(try (throw (ex-info "err" {}))
                          (catch :default e e)
                          (finally (print 2))))))
(throws #(= (-> % :eclj/effect :error) :non-tail-position)
        '(loop [] (inc (recur))))

;;TODO: FIXME
#_(=clj '(try
         ;; The symbolic fn will be excuted by fnil's compiled fn.
         ((clojure.core/fnil #(throw %) (Exception. "!")) nil)
         (catch Exception e
           123)))

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

(=clj '(with-out-str (doseq [a [:x :y :z]] (prn a))))

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

)
