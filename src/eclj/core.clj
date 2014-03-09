(ns eclj.core
  (:refer-clojure :exclude [eval]))

(defprotocol Expression
  (-eval [expr env]))

(defn- thunk [expr env]
  #(-eval expr env))

(defprotocol IEnvironment
  (-lookup [env sym])
  (-extend [env sym val]))

(defrecord Answer [value])
(defrecord Effect [action k])

(defrecord Environment [locals]

  IEnvironment

  (-lookup [_ sym]
    (if-let [var (resolve sym)]
      (Answer. var)
      (if-let [[_ val] (find locals sym)]
        (Answer. val)
        (Effect. (Undefined. sym) ->Answer))))

  (-extend [this sym val]
    (assoc-in this [:locals sym] val))

  )

;; Special Forms
(defrecord Quote [expr])
(defrecord Let [sym init expr])
(defrecord If [test then else])

;; Actions
(defrecord Undefined [sym])

(defn handle [x k]
  (cond
    (instance? Answer x) #(k (:value x))
    (instance? Effect x) (Effect. (:action x) #(handle ((:k x) %) k))
    (ifn? x) #(handle (x) k)
    :else (throw (Exception. (str "Unable to handle: " x)))))

(defmulti expand-seq first)

(defmethod expand-seq 'if
  [[_ test then else]]
  (If. test then else))

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
      (thunk (expand-seq list) env)))

  Let
  (-eval [{:keys [sym init expr]} env]
    (handle (thunk init env)
            #(thunk expr (-extend env sym %))))

  If
  (-eval [{:keys [test then else] :as xx} env]
    (cond
      (symbol? test)
        (handle (-lookup env test)
                #(if % (thunk then env) (thunk else env)))
      (instance? Boolean test)
        (if test
          (thunk then env)
          (thunk else env))
      :else
        (let [x (gensym)]
          (handle (thunk (Let. x test
                           (If. x then else))
                         env)
                  ->Answer))))

  )


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

(defn eval [expr]
  (trampoline #(-eval expr (Environment. '{}))))

(comment

  (eval 5)
  (eval true)

  (eval 'inc)
  (eval 'foo)

  (eval ())

  (eval (Let. 'x 1 2))
  (eval (Let. 'x 1 'x))
  (eval (Let. 'x 1 'y))
  ((:k (eval (Let. 'x 1 'y))) 5)

  (eval '(if true 5 10))
  (eval '(if false 5 10))
  (eval '(if true 5))
  (eval '(if false 5))

  ;(eval '(+ 5 10))

)
