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
(defrecord Resolve [sym])
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


(declare ->Fn symbolic-fn?)

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
  (-eval [_ _]
    (cond
      (symbolic-fn? f) (let [{:keys [expr env param]} f]
                         (thunk expr (-extend env param args)))
      ;TODO: Directly evaluate symbols & keywords?
      (ifn? f) (raise (Invoke. f args))
      :else (raise (NotCallable. f args)))))

(defrecord Quote [expr]
  Expression
  (-eval [_ env]
    (Answer. expr)))

(defrecord Expand [macro form]
  Expression
  (-eval [_ env]
    (handle (thunk (Apply. macro (list* form env (next form))) env)
            #(thunk % env))))

(defmethod eval-seq 'if
  [[_ test then else] env]
  (thunk (If. test then else) env))

(defmethod eval-seq 'var
  [[_ sym] env]
  (raise (Resolve. sym)))

(defmethod eval-seq 'do
  [[_ & body] env]
  (if (empty? body)
    (Answer. nil)
    (let [x (first body)
          f (thunk (first body) env)]
      (if-let [xs (next body)]
        (handle f (fn [_] (thunk (cons 'do xs) env)))
        f))))

(defmethod eval-seq 'let*
  [[_ bindings & body] env]
  (if (empty? bindings)
    (thunk (list* 'do body) env)
    (let [[sym init & bindings*] bindings]
      (thunk (Let. sym init
               (list* 'let* (vec bindings*) body))
             env))))

(defmethod eval-seq 'quote
  [[_ expr] env]
  (thunk (Quote. expr) env))

(defmethod eval-seq 'fn*
  [[_ & fn-tail] env]
  ;;TODO: Better parsing & validation.
  (let [[name impl] (if (symbol? (first fn-tail))
                      [(first fn-tail) (next fn-tail)]
                      [nil fn-tail])
        methods (for [[sig & body] (if (vector? (first impl))
                                     (list impl)
                                     impl)]
                  [sig body])
        max-fixed (apply max-key
                         (fn [[sig body]]
                           (count (take-while (complement #{'&}) sig)))
                         methods)
        arity-groups (group-by (fn [[sig body]]
                                 (if (some #{'&} sig)
                                   :variadic
                                   :fixed))
                               methods)
        arity-err (list 'assert false "Arity error")
        param (gensym "arg__")
        bind (fn [[sig body]]
               (list* 'let [sig param] body))
        cases (mapcat (fn [[sig body :as method]]
                        [(count sig) (bind method)])
                      (:fixed arity-groups))
        else (if-let [variadic (-> arity-groups :variadic first)]
               (bind variadic)
               arity-err)
        expr (list* 'condp '= (list 'count param)
                    (concat cases [else]))]
    (Answer. (->Fn name param expr env))))

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

(defrecord Environment [locals]

  IEnvironment

  (-lookup [_ sym]
    (if-let [[_ val] (find locals sym)]
      (Answer. val)
      (Effect. (Resolve. sym)
               #(if %
                  (Answer. %)
                  (raise (Undefined. sym))))))

  (-extend [this sym val]
    (assoc-in this [:locals sym] val))

  )


(def root-handlers
  {Invoke (fn [{:keys [f args]}]
            (apply f args))
   Resolve (fn [{:keys [sym]}]
             (resolve sym))})

(def empty-env (Environment. {}))

(defn interpret
  ([expr] (interpret empty-env))
  ([expr env]
   (loop [f #(-eval expr env)]
     (let [x (f)]
       (cond
         (fn? x) (recur x)
         (answer? x) x
         (effect? x) (let [{:keys [action k]} x]
                       (if-let [handler (root-handlers (class action))]
                         (recur #(k (handler action)))
                         x))
         :else (unexpected x))))))

(defn eval
  ([expr] (eval expr empty-env))
  ([expr env]
   (let [x (interpret expr env)]
     (if (answer? x)
       (:value x)
       (throw (ex-info (pr-str (:action x)) x))))))

(defrecord Fn [name param expr env]

  ;; Sketchy that I'm overriding defrecord's IFn implementation, but
  ;; it's necessary for Clojure interop. Also, IFn is always ugly.
  clojure.lang.IFn

  (invoke [this]
    (eval (Apply. this []) env))
  (invoke [this a]
    (eval (Apply. this [a]) env))
  (invoke [this a b]
    (eval (Apply. this [a b]) env))
  (invoke [this a b c]
    (eval (Apply. this [a b c]) env))
  (invoke [this a b c d]
    (eval (Apply. this [a b c d]) env))
  (invoke [this a b c d e]
    (eval (Apply. this [a b c d e]) env))
  (invoke [this a b c d e f]
    (eval (Apply. this [a b c d e f]) env))
  (invoke [this a b c d e f g]
    (eval (Apply. this [a b c d e f g]) env))
  (invoke [this a b c d e f g h]
    (eval (Apply. this [a b c d e f g h]) env))
  (invoke [this a b c d e f g h i]
    (eval (Apply. this [a b c d e f g h i]) env))
  (invoke [this a b c d e f g h i j]
    (eval (Apply. this [a b c d e f g h i j]) env))
  (invoke [this a b c d e f g h i j k]
    (eval (Apply. this [a b c d e f g h i j k]) env))
  (invoke [this a b c d e f g h i j k l]
    (eval (Apply. this [a b c d e f g h i j k l]) env))
  (invoke [this a b c d e f g h i j k l m]
    (eval (Apply. this [a b c d e f g h i j k l m]) env))
  (invoke [this a b c d e f g h i j k l m n]
    (eval (Apply. this [a b c d e f g h i j k l m n]) env))
  (invoke [this a b c d e f g h i j k l m n o]
    (eval (Apply. this [a b c d e f g h i j k l m n o]) env))
  (invoke [this a b c d e f g h i j k l m n o p]
    (eval (Apply. this [a b c d e f g h i j k l m n o p]) env))
  (invoke [this a b c d e f g h i j k l m n o p q]
    (eval (Apply. this [a b c d e f g h i j k l m n o p q]) env))
  (invoke [this a b c d e f g h i j k l m n o p q r]
    (eval (Apply. this [a b c d e f g h i j k l m n o p q r]) env))
  (invoke [this a b c d e f g h i j k l m n o p q r s]
    (eval (Apply. this [a b c d e f g h i j k l m n o p q r s]) env))
  (invoke [this a b c d e f g h i j k l m n o p q r s t]
    (eval (Apply. this [a b c d e f g h i j k l m n o p q r s t]) env))
  (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
    (eval (Apply. this
                  (concat [a b c d e f g h i j k l m n o p q r s t] rest))
          env))

  (applyTo [this args]
    (eval (Apply. this args) env))

  )

(defn symbolic-fn? [x]
  (instance? eclj.core.Fn x))

(comment

  (eval 5)
  (eval true)

  (eval 'inc)
  (eval 'foo)

  (eval #'inc)

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
  (eval '(#'* (inc 4) 2))

  (eval '[inc 10])
  (trampoline (:k (interpret '[x 10])) 5)
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

  (eval '(fn [x] x))
  (eval '(fn ([x] x)))
  (eval '(fn f [x] x))
  (eval '(fn f ([x] x)))
  (eval '(fn ([] 0) ([x] 1) ([x y] 2)))
  (eval '(fn ([] 0) ([x] 1) ([x y] 2) ([x y & zs] :n)))

  (eval '((fn [x] x) 5))
  (eval '(apply (fn [& args] (apply + args)) (range 1000)))
  ((eval '(fn [x] x)) 5)

  ;;TODO: Remaining ops from tools.analyzer
  :binding
  :catch
  :def
  :host-call
  :host-field
  :host-interop ;; either field access or no-args method call
  :invoke
  :letfn
  :local
  :loop
  :maybe-class ;; e.g. java.lang.Integer or Long
  :maybe-host-form
  :new
  :recur
  :set!
  :throw
  :try
  :with-meta

)
