(ns eclj.parse-test
  (:require [eclj.env :refer (ns-env)]
            [eclj.parse :refer (parse)]))

(defn ! [x]
  (-> (parse x (ns-env))
      #_fipp.edn/pprint))

;;TODO: assertions

(! nil)
(! 5)
(! ())
(! '())
(! '(f x))
(! '(quote (f x)))
(! '(let* []))
(! '(let* [x 1] x))
(! '(let* [x 1 y 2] (+ x y)))
(! '(let* [x 1 y 2] (println "!") (+ x y)))
(! '(do))
(! '(try))
(! '(try 1))
(! '(try 1 2))
(! '(try 1 (catch Exception e 2)))
(! '(try 1 (catch Exception e 2 3)))
(! '(try 1 (catch RuntimeException e 2) (catch Exception e 3)))
(! '(try 1 (catch :default e 2)))
(! '(try 1 (catch :default e 2 3)))
(! '(try 1 (finally 2 3)))
(! '(throw x))
(! '(def x))
(! '(def ^{:doc "foo"} x))
(! '(def x 1))
(! '(def x "foo" 1))
(! '(. x y z))
(! '(.x y z))
(! '(fn* []))
(! '(fn* [] 1))
(! '(fn* foo [] 1))
(! '(fn* foo [x] x))
(! '(fn* foo [x y & z] z))
(! '(fn* foo [x y & z] x y z))
(! '(fn* ([x] x) ([x y] y)))
(! '(fn* foo ([x] x) ([x y] y)))
(! '(fn* foo ([x] x) ([x y] y) ([x y & z] z)))
(! '(set! x 1))
(! '(set! (.x y) 1))
(! '(letfn* [even? (clojure.core/fn even? [x]
                     (or (zero? x) (odd? (dec x))))
             odd? (clojure.core/fn odd? [x]
                    (and (not (zero? x)) (even? (dec x))))]
      ((juxt even? odd?) 11)))
(! '(eclj.core/case* :x {:x 1} 2))
