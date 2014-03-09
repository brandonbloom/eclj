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

;;; Special Forms
(defrecord Quote [expr])
(defrecord Let [sym init expr])
(defrecord If [test then else])
(defrecord Apply [f args])
(defrecord Expand [macro form])

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

(defn handle [x k]
  (cond
    (answer? x) #(k (:value x))
    (effect? x) (Effect. (:action x) #(handle ((:k x) %) k))
    (ifn? x) #(handle (x) k)
    :else (throw (Exception. (str "Unable to handle: " x)))))


(defmulti eval-seq (fn [s env] (first s)))

(defmethod eval-seq 'if
  [[_ test then else] env]
  (thunk (If. test then else) env))

(defn macro? [x]
  (and (var? x)
       (-> x meta :macro)))

(defmethod eval-seq :default
  [[head & tail :as form] env]
  (if (symbol? head)
    (handle (-lookup env head)
            #(thunk (if (macro? %)
                      (Expand. % form)
                      (Apply. % tail))
                    env))
    (thunk (Apply. (thunk head env) tail) env)))

;TODO: Treat external Fn invokes as an effect.
;TODO: Support pure symbolic fns, evaluate directly.

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

  Let
  (-eval [{:keys [sym init expr]} env]
    (handle (thunk init env)
            #(thunk expr (-extend env sym %))))

  If
  (-eval [{:keys [test then else] :as xx} env]
    (if (symbol? test)
      (handle (-lookup env test)
              #(if % (thunk then env) (thunk else env)))
      (thunk (let [x (gensym)]
               (Let. x test
                 (If. x then else)))
             env)))

  Apply
  (-eval [{:keys [f args]} env]
    ;;TODO: Handle symbols, keywords, etc directly.
    (if (ifn? f)
      (raise (Invoke. f args))
      (raise (NotCallable. f args))))

  )

;;TODO: Generalized collection-eval for vectors, maps, sets, etc


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

(extend-protocol Action

  java.lang.Object
  (-execute [action]
    action)

  )

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
        :else (throw (Exception. (str "Unable to interpret: " x)))))))

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
  ((:k (interpret (Let. 'x 1 'y))) 5)

  (eval '(if true 5 10))
  (eval '(if false 5 10))
  (eval '(if true 5))
  (eval '(if false 5))
  (eval '(if xx 5))

  (eval '(+ 5 10))

)
