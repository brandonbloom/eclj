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
(defrecord Deref [x])
(defrecord Invoke [f args])
(defrecord Resolve [sym])
(defrecord Throw [error])
(defrecord Declare [sym])
(defrecord Define [sym value])
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

(defn propegate [handler {:keys [action k]} nextk]
  (Effect. action #(handler (k %) nextk)))

(defn handle [x k]
  (cond
    (answer? x) #(k (:value x))
    (effect? x) (propegate handle x k)
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
    (handle (-lookup env sym)
            #(if (var? %)
               (raise (Deref. %))
               (Answer. %))))

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

(defrecord Bind [sym value expr]
  Expression
  (-eval [_ env]
    (thunk expr (-extend env sym value))))

(defrecord Let [sym init expr]
  Expression
  (-eval [_ env]
    (handle (thunk init env)
            #(thunk (Bind. sym % expr) env))))

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

(defrecord Catch [class sym expr])

(defn exception-handler [catches finally env]
  (fn handler [x k]
    (cond
      (answer? x) #(handle (thunk finally env)
                           (fn [_] (fn [] (k (:value x)))))
      (effect? x)
        (let [error (-> x :action :error)
              catch (some (fn [{:keys [class sym expr] :as catch}]
                            (when (instance? class error)
                              catch))
                            catches)]
          (if-let [{:keys [class sym expr]} catch]
            #(handle (thunk (Bind. sym error expr) env)
                     (fn [y]
                       (handle (thunk finally env)
                               (fn [_] (k y)))))
            (propegate handler x ->Answer)))
      (ifn? x) #(handler (x) k)
      :else (unexpected x))))

;;TODO: Replaces ordered-choice type-catches with a single catch block.
(defrecord Try [body catches finally]
  Expression
  (-eval [_ env]
    ((exception-handler catches finally env) (thunk body env) ->Answer)))

(defn parse-try [[_ & body]]
  (let [catch? (every-pred seq? #(= (first %) 'catch))
        default? (every-pred catch? #(= (second %) :default))
        finally? (every-pred seq? #(= (first %) 'finally))]
    (loop [parser {:state :start :forms body
                   :body [] :cblocks [] :dblock nil :fblock nil}]
      (if (seq? (:forms parser))
        (let [[form & forms*] (:forms parser)
              parser* (assoc parser :forms forms*)]
          (case (:state parser)
            :start
              (cond
                (catch? form) (recur (assoc parser :state :catches))
                (finally? form) (recur (assoc parser :state :finally))
                :else (recur (update-in parser* [:body] conj form)))
            :catches
              (cond
                (default? form)
                  (recur (assoc parser* :dblock form :state :finally))
                (catch? form)
                  (recur (update-in parser* [:cblocks] conj form))
                (finally? form)
                  (recur (assoc parser :state :finally))
                :else
                  (throw (Exception. "Invalid try form")))
            :finally
              (recur (assoc parser* :fblock form :state :done))
            :done
              (throw (Exception. "Unexpected form after finally"))))
        parser))))

(defmethod eval-seq 'try
  [expr env]
  (let [{:keys [body cblocks dblock fblock]} (parse-try expr)
        bexpr (list* 'do body)
        cblocks* (if dblock
                   (concat cblocks [(list* 'catch Throwable (nnext dblock))])
                   cblocks)
        fexpr (list* 'do (next fblock))]
    ;;TODO: Ensure items are exception classes.
    (handle (eval-items (mapv second cblocks*) env)
            (fn [classes]
              (let [catches (map (fn [class [_ _ sym & cbody]]
                                   (Catch. class sym (list* 'do cbody)))
                                 classes cblocks*)]
                (thunk (Try. bexpr catches fexpr) env))))))

(defmethod eval-seq 'throw
  [[_ expr] env]
  (handle (thunk expr env)
          #(raise (Throw. %))))

(defmethod eval-seq 'def
  [[_ & body] env]
  (let [[sym doc expr] (case (count body)
                         1 [(first body) nil clojure.lang.Var$Unbound]
                         2 [(first body) nil (second body)]
                         3 body)
        sym* (vary-meta sym assoc :doc doc)]
    (if (> (count body) 1)
      (handle (thunk expr env)
              #(raise (Define. sym* %)))
      (raise (Declare. sym*)))))

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
  {Deref (fn [{:keys [x]}]
           (deref x))
   Invoke (fn [{:keys [f args]}]
            (apply f args))
   Resolve (fn [{:keys [sym]}]
             (resolve sym))
   Declare (fn [{:keys [sym]}]
             (intern *ns* sym))
   Define (fn [{:keys [sym value]}]
               (intern *ns* sym value))})

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

  clojure.lang.IFn

  ;; *cringe*
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
  (eval '(identity inc))

  (eval ())

  (eval (Bind. 'x '(+ 1 2) 0))
  (eval (Bind. 'x '(+ 1 2) 'x))

  (eval (Let. 'x (+ 1 2) 0))
  (eval (Let. 'x (+ 1 2) 'x))
  (eval (Let. 'x (+ 1 2) 'y))
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

  (eval '(throw (ex-info "err" {})))
  (eval '(try 1))
  (eval '(try (throw (ex-info "err" {}))))
  (eval '(try 1 (catch Throwable e 2)))
  (eval '(try (throw (ex-info "err" {})) (catch :default e 3)))
  (eval '(try (throw (ex-info "err" {})) (catch :default e e)))
  (eval '(try 1 (throw (ex-info "err" {})) 2
              (catch IllegalArgumentException e 2)))
  (eval '(try 1 (finally (prn 2))))
  (eval '(try (throw (ex-info "err" {}))
              (catch :default e e)
              (finally (prn 2))))

  (eval '(def declared))
  (eval '(def defined 1))
  (eval '(do (def redefined 2) (def redefined 3)))
  (eval '(def foo "bar" 4))
  (list declared defined redefined foo (-> #'foo meta :doc))

  ;;TODO: Remaining ops from tools.analyzer
  :host-call
  :host-field
  :host-interop ;; either field access or no-args method call
  :letfn
  :loop
  :maybe-host-form
  :new
  :recur
  :set!
  :with-meta

)
