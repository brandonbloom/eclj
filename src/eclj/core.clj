(ns eclj.core
  (:refer-clojure :exclude [eval]))


;;TODO: Eliminate gensym usage.


(defprotocol Expression
  (-eval [expr env]))

(defprotocol IEnvironment
  (-lookup [env sym])
  (-extend [env sym val]))

(defrecord Answer [value])
(defrecord Effect [action k])

;;; Actions
(defrecord Invoke [f args])
;; Conditions
;TODO: Revisit these to offer more useful restarts.
(defrecord Undefined [sym])
(defrecord NotCallable [f args])


(defn- thunk [expr env]
  #(-eval expr env))

(defn raise [action]
  (Effect. action ->Answer))

(defn answer? [x]
  (instance? Answer x))

(defn effect? [x]
  (instance? Effect x))

(defn unexpected [x]
  (throw (ex-info (str "Unexpected " (pr-str (class x)))
                  {:value x})))

(defn handle [x k]
  (cond
    (answer? x) #(k (:value x))
    (effect? x) (Effect. (:action x) #(handle ((:k x) %) k))
    (ifn? x) #(handle (x) k)
    :else (unexpected x)))


(defmulti eval-seq (fn [s env] (first s)))

(extend-protocol Expression

  nil
  (-eval [obj env]
    (Answer. nil))

  java.lang.Object
  (-eval [obj env]
    (Answer. obj))

  clojure.lang.Symbol
  (-eval [sym env]
    (-lookup env sym))

  clojure.lang.ISeq
  (-eval [list env]
    (if (empty? list)
      (Answer. list)
      (eval-seq list env)))

)

(defn eval-items [coll env]
  ((fn rec [dest src]
     (if (empty? src)
       (Answer. dest)
       (handle (thunk (first src) env)
               #(rec (conj dest %) (next src)))))
   (empty coll) coll))

(doseq [t [clojure.lang.PersistentArrayMap
           clojure.lang.PersistentHashMap
           clojure.lang.PersistentHashSet
           clojure.lang.PersistentQueue
           clojure.lang.PersistentTreeMap
           clojure.lang.PersistentTreeSet
           clojure.lang.PersistentVector]]
  (extend t Expression {:-eval eval-items}))

(defrecord Let [sym init expr]
  Expression
  (-eval [_ env]
    (handle (thunk init env)
            #(thunk expr (-extend env sym %)))))

(defrecord If [test then else]
  Expression
  (-eval [_ env]
    (if (symbol? test)
      (handle (-lookup env test)
              #(if % (thunk then env) (thunk else env)))
      (thunk (let [x (gensym)]
               (Let. x test
                 (If. x then else)))
             env))))

(defrecord Apply [f args]
  Expression
  (-eval [_ env]
    ;;TODO: Handle symbols, keywords, etc directly.
    (if (ifn? f)
      (raise (Invoke. f args))
      (raise (NotCallable. f args)))))

(defrecord Quote [expr]
  Expression
  (-eval [_ env]
    (Answer. expr)))

(defrecord Expand [macro form]
  Expression
  (-eval [_ env] ;TODO
    ))

(defmethod eval-seq 'if
  [[_ test then else] env]
  (thunk (If. test then else) env))

(defn macro? [x]
  (and (var? x)
       (-> x meta :macro)))

(defn apply-args [f args env]
  (handle (eval-items (reverse args) env)
          #(thunk (Apply. f %) env)))

(defmethod eval-seq :default
  [[head & tail :as form] env]
  (if (symbol? head)
    (handle (-lookup env head)
            #(if (macro? %)
               (thunk (Expand. % form) env)
               (apply-args % tail env)))
    (handle (thunk head env)
            #(apply-args % tail env))))

;TODO: Support pure symbolic fns, evaluate directly.

;; Ops from tools.analyzer
:binding
:catch
:const
:def
:do
:fn
:fn-method
:host-call
:host-field
:host-interop ;; either field access or no-args method call
:if
:invoke
:let
:letfn
:local
:loop
:map
:maybe-class ;; e.g. java.lang.Integer or Long
:maybe-host-form
:new
:quote
:recur
:set
:set!
:throw
:try
:var
:vector
:with-meta

(defrecord Environment [locals]

  IEnvironment

  (-lookup [_ sym]
    (if-let [var (resolve sym)]
      (Answer. var)
      (if-let [[_ val] (find locals sym)]
        (Answer. val)
        (raise (Undefined. sym)))))

  (-extend [this sym val]
    (assoc-in this [:locals sym] val))

  )


(def root-handlers
  {Invoke (fn [{:keys [f args]}]
            (apply f args))})

(defn interpret [expr]
  (loop [f #(-eval expr (Environment. {}))]
    (let [x (f)]
      (cond
        (fn? x) (recur x)
        (answer? x) x
        (effect? x) (let [{:keys [action k]} x]
                      (if-let [handler (root-handlers (class action))]
                        (recur #(k (handler action)))
                        x))
        :else (unexpected x)))))

(defn eval [expr]
  (let [x (interpret expr)]
    (if (answer? x)
      (:value x)
      (throw (ex-info (pr-str (:action x)) x)))))

(comment

  (eval 5)
  (eval true)

  (eval 'inc)
  (eval 'foo)

  (eval ())

  (eval (Let. 'x 1 2))
  (eval (Let. 'x 1 'x))
  (eval (Let. 'x 1 'y))
  (trampoline (:k (interpret (Let. 'x 1 'y))) 5)

  (eval '(if true 5 10))
  (eval '(if false 5 10))
  (eval '(if true 5))
  (eval '(if false 5))
  (eval '(if xx 5))

  (eval '(- 10 3))
  (eval '(+ (inc 5) (inc 10)))
  ;(eval '(#'* (inc 4)))

  (eval '[inc 10])
  (trampoline (:k (interpret '[x 10])) 5)
  (eval '#{(+ 5 10)})

)
