(ns eclj.core
  (:refer-clojure :exclude [eval])
  (:import [clojure.lang Reflector]))


;;TODO: Eliminate gensym usage.
;;TODO: Re-examine handling of metadata.


(defprotocol Expression
  (-eval [expr env]))

(defprotocol IEnvironment
  (-lookup [env sym])
  (-extend [env sym val]))

;; Wraps resolved values.
(defrecord Static [value])
(defrecord Dynamic [value])

;; Wraps all interpreter intermediate values.
(defrecord Answer [value])
(defrecord Effect [action k])

;;; Action Records
;; Top-level Actions
(defrecord Deref [x])
(defrecord Invoke [f args])
(defrecord Resolve [sym])
(defrecord Throw [error])
(defrecord Declare [sym])
(defrecord Define [sym value])
(defrecord New [class args])
(defrecord Interop [object member args])
(defrecord AssignVar [var value])
(defrecord AssignField [object field value])
(defrecord Import [sym])
;; Internal Actions
(defrecord Recur [args])

;; Conditions
; These actions are used as recoverable exceptions, not yet true conditions.
(defrecord Undefined [sym])
(defrecord NotCallable [f args]) ;TODO: NotApplicable?
(defrecord NonTailPosition [])
(defrecord NoMatchingClause [value])
(defrecord NotAssignable [location])


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
            (fn [{:keys [value] :as resolved}]
               (cond
                 (instance? Static resolved) (Answer. value)
                 (instance? Dynamic resolved)
                   (if (var? value)
                     (raise (Deref. value))
                     (Answer. value))
                 :else (unexpected resolved)))))

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
    (handle (thunk test env)
            #(thunk (if % then else) env))))

(defprotocol Applicable
  (-apply [this arg]))

(extend-protocol Applicable

  Object
  (-apply [this arg]
    (raise (NotCallable. this arg)))

  clojure.lang.IFn
  (-apply [this arg]
    (raise (Invoke. this arg)))

  clojure.lang.Var
  (-apply [this arg]
    (raise (Invoke. this arg)))

  ;TODO: Special case symbols & keywords ?

  )

(defrecord Apply [f arg]
  Expression
  (-eval [_ _]
    (-apply f arg)))

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
  (handle (raise (Resolve. sym))
          (fn [{:keys [value] :as resolved}]
            (assert (instance? Dynamic resolved))
            (Answer. value))))

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

(declare ->Fn)

(defmethod eval-seq 'eclj.core/fn**
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
        expr (list* 'clojure.core/case (list 'count param) ;TODO case
                    (concat cases [else]))]
    (Answer. (->Fn name param expr env))))

(defmethod eval-seq 'fn*
  [[_ & fn-tail :as form] env]
  (if (symbol? (first fn-tail))
    (let [name (first fn-tail)]
      (thunk (list 'eclj.core/ycombine
                   (list 'fn [name]
                         (list* 'eclj.core/fn** (next fn-tail))))
             env))
    (eval-seq (list* 'eclj.core/fn** fn-tail) env)))

(defmethod eval-seq 'letfn*
  [[_ [& forms] & body] env]
  (let [names (vec (take-nth 2 forms))
        decls (for [form (take-nth 2 (next forms))]
                (list 'fn names (list* 'fn (nnext form))))]
    (thunk (list* 'let [names (list* 'eclj.core/y*combine decls)]
                  body)
           env)))

(defrecord Catch [class sym expr])

(defn exception-handler [catches finally env]
  (fn handler [x k]
    (cond
      (answer? x) #(handle (thunk finally env)
                           (fn [_] (fn [] (k (:value x)))))
      (effect? x)
        (let [error (-> x :action :error)
              catch (some (fn [{:keys [class sym expr] :as catch}]
                            (when (and (instance? Throw (:action x))
                                       (instance? class error))
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
                         1 [(first body)]
                         2 [(first body) nil (second body)]
                         3 body)
        sym* (vary-meta sym assoc :doc doc)]
    (if (> (count body) 1)
      (handle (thunk expr env)
              #(raise (Define. sym* %)))
      (raise (Declare. sym*)))))

(defmethod eval-seq 'new
  [[_ sym & args] env]
  (handle (thunk sym env)
          (fn [class] ;TODO: Validate
            (fn []
              (handle (eval-items (vec args) env)
                      #(raise (New. class %)))))))

(defmethod eval-seq '.
  [[_ expr & body] env]
  (let [[member args] (if (and (= (count body) 1) (seq? (first body)))
                        [(ffirst body) (nfirst body)]
                        [(first body) (next body)])]
    (handle (thunk expr env)
            (fn [object]
              (handle (eval-items (vec args) env)
                      #(raise (Interop. object member %)))))))

(defmethod eval-seq 'set!
  [[_ location expr] env]
  (if (symbol? location)
    (handle (-lookup env location)
            (fn [{:keys [value] :as resolved}]
              (if (and (instance? Dynamic resolved) (var? value))
                (handle (thunk expr env)
                        #(raise (AssignVar. value %)))
                (raise (NotAssignable. location)))))
    (handle (thunk (first location) env)
            (fn [object]
              (handle (thunk expr env)
                      #(raise (AssignField. object (second location) %)))))))

(defmethod eval-seq 'loop*
  [[_ bindings & body] env]
  (let [syms (vec (take-nth 2 bindings))
        inits (take-nth 2 (next bindings))]
    (thunk (list* (list* 'eclj.core/fn** syms body) inits) env)))

(defmethod eval-seq 'recur
  [[_ & args] env]
  (handle (eval-items (reverse args) env)
          #(raise (Recur. %))))

(defmethod eval-seq 'clojure.core/import*
  [[_ sym] env]
  (raise (Import. sym)))

;;TODO: Don't capture unqualified 'case
(defmethod eval-seq 'case
  [[_ & tail] env]
  (thunk (list* 'clojure.core/case tail) env))

(defmethod eval-seq 'clojure.core/case
  [[_ expr & clauses] env]
  (let [default? (odd? (count clauses))
        cases (partition 2 (if default? (butlast clauses) clauses))
        table (into {} (map vec cases))]
    (handle (thunk expr env)
            (fn [value]
              (if-let [[_ e] (find table value)]
                (thunk e env)
                (if default?
                  (thunk (last clauses) env)
                  (raise (NoMatchingClause. value))))))))

(defn macro? [x]
  (and (var? x)
       (-> x meta :macro)))

(defn apply-args [f args env]
  (handle (eval-items (reverse args) env)
          #(thunk (Apply. f %) env)))

(defmethod eval-seq :default
  [[head & tail :as form] env]
  (if (symbol? head)
    (let [s (str head)]
      (cond
        (.endsWith s ".") (let [class (symbol (apply str (butlast s)))]
                            (thunk (list* 'new class tail) env))
        (.startsWith s ".") (let [member (symbol (apply str (next s)))
                                  [obj & args] tail]
                              (thunk (list* '. obj member args) env))
        :else (handle (-lookup env head)
                      #(let [{:keys [value]} %]
                         (if (and (instance? Dynamic %)
                                  (-> value meta :macro))
                           (thunk (Expand. value form) env)
                           (apply-args value tail env))))))
    (handle (thunk head env)
            #(apply-args % tail env))))

(defrecord Environment [locals]

  IEnvironment

  (-lookup [_ sym]
    (if-let [[_ value] (find locals sym)]
      (Answer. (Static. value))
      (Effect. (Resolve. sym)
               #(if %
                  (Answer. %)
                  (raise (Undefined. sym))))))

  (-extend [this sym value]
    (assoc-in this [:locals sym] value))

  )

(defn static-invoke [class member & args]
  (if (zero? (count args))
    (try
      (Reflector/getStaticField class member)
      (catch Exception e
        (Reflector/invokeStaticMethod
          class member clojure.lang.RT/EMPTY_ARRAY)))
    (Reflector/invokeStaticMethod class member (object-array args))))

(defn staticfn [class member]
  (fn [& args]
    (apply static-invoke class member args)))

(defn maybe-resolve [sym]
  (if-let [x (resolve sym)]
    (->Dynamic x)
    (try
      (->Dynamic (clojure.lang.RT/classForName (name sym)))
      (catch ClassNotFoundException _
        nil))))

(def root-handlers
  {

   Deref
   (fn [{:keys [x]}]
     (deref x))

   Invoke
   (fn [{:keys [f args]}]
     (apply f args))

   Resolve
   (fn [{:keys [sym]}]
     (or (maybe-resolve sym)
         (when-let [ns (namespace sym)]
           (let [{:keys [value]} (maybe-resolve (symbol ns))
                 n (name sym)]
             (when (instance? Class value)
               (->Dynamic
                 (try
                   (.get (.getField value n) value)
                   (catch NoSuchFieldException _
                     (staticfn value n)))))))))

   Declare
   (fn [{:keys [sym]}]
     (intern *ns* sym))

   Define
   (fn [{:keys [sym value]}]
     (let [var (intern *ns* sym value)]
       (when (-> sym meta :dynamic)
         (.setDynamic var))
       var))

   New
   (fn [{:keys [class args]}]
     (Reflector/invokeConstructor class (object-array args)))

   Interop
   (fn [{:keys [object member args]}]
     (let [s (str member)
           s (if (.startsWith s "-")
               (apply str (next s))
               s)]
       (if (instance? Class object)
         (apply static-invoke object s args)
         (if (zero? (count args))
           (Reflector/invokeNoArgInstanceMember object s)
           (Reflector/invokeInstanceMember s object (object-array args))))))

   AssignVar
   (fn [{:keys [var value]}]
     (var-set var value))

   AssignField ;TODO: Test this.
   (fn [{:keys [object field value]}]
     (let [field (name field)]
       (if (instance? Class object)
         (Reflector/setStaticField object field value)
         (Reflector/setInstanceField object field value))))

   Import
   (fn [{:keys [sym]}]
     (.importClass *ns* (clojure.lang.RT/classForName (name sym))))

   })

(def empty-env (Environment. {})) ;TODO This isn't empty, it's host-env.

(defn interpret
  ([expr] (interpret empty-env))
  ([expr env]
   (loop [f #(-eval expr env)]
     (let [x (f)]
       (cond
         (ifn? x) (recur x)
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


(defn recur-handler [f env]
  (fn handler [x k]
    (cond
      (answer? x) #(k (:value x))
      (effect? x) (let [{effectk :k :keys [action]} x]
                    (if (instance? Recur action)
                      (if (= effectk ->Answer)
                        (thunk (Apply. f (:args action)) env)
                        (raise (NonTailPosition.)))
                      (propegate handler x ->Answer)))
      (ifn? x) #(handler (x) k)
      :else (unexpected x))))

(defrecord Fn [name param expr env]

  Applicable
  (-apply [this args]
    (let [handler (recur-handler this env)]
      (handler (thunk expr (-extend env param args))
               ->Answer)))

  clojure.lang.IFn
  (applyTo [this args]
    (eval (Apply. this args)))
  ;; *cringe*
  (invoke [this]
    (eval (Apply. this [])))
  (invoke [this a]
    (eval (Apply. this [a])))
  (invoke [this a b]
    (eval (Apply. this [a b])))
  (invoke [this a b c]
    (eval (Apply. this [a b c])))
  (invoke [this a b c d]
    (eval (Apply. this [a b c d])))
  (invoke [this a b c d e]
    (eval (Apply. this [a b c d e])))
  (invoke [this a b c d e f]
    (eval (Apply. this [a b c d e f])))
  (invoke [this a b c d e f g]
    (eval (Apply. this [a b c d e f g])))
  (invoke [this a b c d e f g h]
    (eval (Apply. this [a b c d e f g h])))
  (invoke [this a b c d e f g h i]
    (eval (Apply. this [a b c d e f g h i])))
  (invoke [this a b c d e f g h i j]
    (eval (Apply. this [a b c d e f g h i j])))
  (invoke [this a b c d e f g h i j k]
    (eval (Apply. this [a b c d e f g h i j k])))
  (invoke [this a b c d e f g h i j k l]
    (eval (Apply. this [a b c d e f g h i j k l])))
  (invoke [this a b c d e f g h i j k l m]
    (eval (Apply. this [a b c d e f g h i j k l m])))
  (invoke [this a b c d e f g h i j k l m n]
    (eval (Apply. this [a b c d e f g h i j k l m n])))
  (invoke [this a b c d e f g h i j k l m n o]
    (eval (Apply. this [a b c d e f g h i j k l m n o])))
  (invoke [this a b c d e f g h i j k l m n o p]
    (eval (Apply. this [a b c d e f g h i j k l m n o p])))
  (invoke [this a b c d e f g h i j k l m n o p q]
    (eval (Apply. this [a b c d e f g h i j k l m n o p q])))
  (invoke [this a b c d e f g h i j k l m n o p q r]
    (eval (Apply. this [a b c d e f g h i j k l m n o p q r])))
  (invoke [this a b c d e f g h i j k l m n o p q r s]
    (eval (Apply. this [a b c d e f g h i j k l m n o p q r s])))
  (invoke [this a b c d e f g h i j k l m n o p q r s t]
    (eval (Apply. this [a b c d e f g h i j k l m n o p q r s t])))
  (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
    (eval (Apply. this
                  (concat [a b c d e f g h i j k l m n o p q r s t] rest))))

  )

(eval '(defn ycombine [f]
         ((fn [x] (x x))
            (fn [x]
              (f (fn [& args]
                   (apply (x x) args)))))))

(eval '(defn y*combine [& fs]
         (map (fn [f] (f))
              ((fn [x] (x x))
               (fn [p]
                 (map (fn [f]
                        (fn []
                          (apply f (map (fn [ff]
                                          (fn [& y]
                                            (apply (ff) y)))
                                        (p p)))))
                      fs))))))

(comment

  (eval (->Bind 'x '(+ 1 2) 0))
  (eval (->Bind 'x '(+ 1 2) 'x))

  (eval (->Let 'x (+ 1 2) 0))
  (eval (->Let 'x (+ 1 2) 'x))
  (eval (->Let 'x (+ 1 2) 'y))
  (trampoline (:k (interpret (->Let 'x 1 'y))) 5)

  (trampoline (:k (interpret '[x 10])) 5)

  (eval '(if xx 5))
  (eval 'foo)

  (eval '(fn [x] x))
  (eval '(fn ([x] x)))
  (eval '(fn f [x] x))
  (eval '(fn f ([x] x)))
  (eval '(fn ([] 0) ([x] 1) ([x y] 2)))
  (eval '(fn ([] 0) ([x] 1) ([x y] 2) ([x y & zs] :n)))

  ((eval '(fn [x] x)) 5)

  (eval '(def declared))
  (eval '(def defined 1))
  (eval '(do (def redefined 2) (def redefined 3)))
  (eval '(def foo "bar" 4))
  [declared defined redefined foo (-> #'foo meta :doc)]

  (eval '(throw (ex-info "err" {})))
  (eval '(try (throw (ex-info "err" {}))))
  (eval '(try 1 (throw (ex-info "err" {})) 2
              (catch IllegalArgumentException e 2)))
  (eval '(try (throw (ex-info "err" {})) (catch :default e 3)))
  (eval '(try (throw (ex-info "err" {})) (catch :default e e)))
  (eval '(try (throw (ex-info "err" {}))
              (catch :default e e)
              (finally (prn 2))))

  (eval '(Apply. inc 5))
  (eval '(.valueOf String true))

  (time
    (dotimes [i 200]
      (eval '((fn factorial [x]
                (if (<= x 1)
                  1
                  (* x (factorial (- x 1)))))
              20))))

  (eval '(loop [] (inc (recur))))
  (eval '(import [java.util Date Currency]))

  (eval '(case "str"
           5 :number))

  ;TODO deftype
  ;TODO defprotocol
  ;TODO reify
  ;TODO monitor-enter and monitor-exit


)
