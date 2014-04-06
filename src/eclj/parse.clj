(ns eclj.parse
  (:require [eclj.common :refer (pure)]))

;TODO: Is "parse" the right word?
;TODO: file/line/column/etc
;TODO: namespaced heads
;TODO: validation conditions. ie raise error for (var inc inc) or (quote x x)

(defprotocol Expression
  (-parse [expr env]))

(defn parse-constant [x env]
  {:head :constant :form x :env env})

(defn parse-collection [coll env]
  {:head :collection :form coll :env env})

(doseq [t [nil java.lang.Object]]
  (extend t Expression {:-parse parse-constant}))

(defmulti parse-seq (fn [xs env] (first xs)))

(defn parse-apply [[f & args :as form] env]
  {:head :apply :form form :env env :f f :args args})

(extend-protocol Expression

  clojure.lang.Symbol
  (-parse [sym env]
    {:head :name :form sym :env env})

  clojure.lang.ISeq
  (-parse [xs env]
    (cond
      (empty? xs) (parse-constant xs env)
      (symbol? (first xs)) (parse-seq xs env)
      :else (parse-apply xs env)))

  clojure.lang.AMapEntry
  (-parse [kvp env]
    (parse-collection (vec kvp) env))

)

(doseq [t [clojure.lang.PersistentArrayMap
           clojure.lang.PersistentHashMap
           clojure.lang.PersistentHashSet
           clojure.lang.PersistentQueue
           clojure.lang.PersistentTreeMap
           clojure.lang.PersistentTreeSet
           clojure.lang.PersistentVector]]
  (extend t Expression {:-parse parse-collection}))

(defmethod parse-seq :default
  [form env]
  (parse-apply form env))

(defmethod parse-seq 'if
  [[_ test then else :as form] env]
  {:head :if :form form :env env
   :test test :then then :else else})

(defmethod parse-seq 'var
  [[_ sym :as form] env]
  {:head :var :form form :env env :sym sym})

(defmethod parse-seq 'do
  [[_ & body :as form] env]
  (let [v (vec body)]
    {:head :do :form form :env env
     :statements (pop v) :ret (peek v)}))

(defmethod parse-seq 'quote
  [[_ value :as form] env]
  {:head :quote :form form :env env :value value})

;;;;;;;;;;;; TODO
;(defmethod parse-seq 'let*
;(defmethod parse-seq 'eclj.core/fn**
;(defmethod parse-seq 'fn*
;(defmethod parse-seq 'letfn*
;(defmethod parse-seq 'try
;(defmethod parse-seq 'throw
;(defmethod parse-seq 'def
;(defmethod parse-seq 'new
;(defmethod parse-seq '.
;(defmethod parse-seq 'set!
;(defmethod parse-seq 'loop*
;(defmethod parse-seq 'recur
;(defmethod parse-seq 'clojure.core/import*
;(defmethod parse-seq 'case
;(defmethod parse-seq 'clojure.core/case
;(defmethod parse-seq 'deftype* [form env]
;(defmethod parse-seq 'reify* [form env]
;(defmethod parse-seq :default

(defrecord Syntax [head form env])

(defn parse [form env]
  (map->Syntax (-parse form env)))


(comment

  (require 'eclj.core)
  (defn ! [x]
    (fipp.edn/pprint (parse x (eclj.core/ns-env))))

  (! nil)
  (! 5)
  (! ())
  (! '())
  (! '(f x))
  (! ''(f x))

)
