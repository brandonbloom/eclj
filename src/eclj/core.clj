(ns eclj.core
  (:refer-clojure :exclude [eval])
  (:require [eclj.env :as env])
  (:import [clojure.lang Reflector]))


;;TODO: Eliminate gensym usage.
;;TODO: Re-examine handling of metadata.


(defprotocol Expression
  (-eval [expr env]))

;; Wraps all interpreter intermediate values.
(defrecord Answer [value])
(defrecord Effect [op k])


(defn- thunk [expr env]
  #(-eval expr env))

(defn raise [action]
  (map->Effect (merge {:k ->Answer} action)))

;;TODO: Implement restarts, etc.
(defn signal [condition]
  (raise (merge {:op :condition} condition)))

(defn answer? [x]
  (instance? Answer x))

(defn effect? [x]
  (instance? Effect x))

(defn unexpected [x]
  (throw (ex-info (str "Unexpected " (pr-str (class x)))
                  {:value x})))

(defn propegate [handler {:keys [k] :as effect} nextk]
  (assoc effect :k #(handler (k %) nextk)))

(defn handle [x k]
  (cond
    (answer? x) #(k (:value x))
    (effect? x) (propegate handle x k)
    (ifn? x) #(handle (x) k)
    :else (unexpected x)))

(defn lookup [{:keys [locals] :as env} sym]
  (if-let [[_ value] (find locals sym)]
    (Answer. {:origin :locals :value value})
    (raise {:op :resolve
            :env env
            :sym sym
            :k #(if %
                  (Answer. %)
                  (signal {:error :undefined :sym sym}))})))

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
    (handle (lookup env sym)
            (fn [{:keys [origin value]}]
               (case origin
                 :locals (Answer. value)
                 :host (Answer. value)
                 :namespace (raise {:op :deref :ref value})))))

  clojure.lang.ISeq
  (-eval [list env]
    (if (empty? list)
      (Answer. list)
      (eval-seq list env)))

  clojure.lang.AMapEntry
  (-eval [xs env]
    (thunk (vec xs) env))

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
    (thunk expr (assoc-in env [:locals sym] value))))

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
    (signal {:error :not-callable :f this :args arg})) ;TODO: NotApplicable?

  clojure.lang.IFn
  (-apply [this arg]
    (raise {:op :invoke :f this :args arg}))

  clojure.lang.Var
  (-apply [this arg]
    (raise {:op :invoke :f this :args arg}))

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
  (handle (raise {:op :resolve :env env :sym sym})
          (fn [{:keys [origin value]}]
            (assert (= origin :namespace))
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
                         (list* 'eclj.core/fn** fn-tail)))
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
        (let [error (:error x)
              catch (some (fn [{:keys [class sym expr] :as catch}]
                            (when (and (= (:op x) :throw)
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
          #(raise {:op :throw :error %})))

(defmethod eval-seq 'def
  [[_ & body] env]
  (let [[sym doc expr] (case (count body)
                         1 [(first body)]
                         2 [(first body) nil (second body)]
                         3 body)
        sym* (vary-meta sym assoc :doc doc)]
    (if (> (count body) 1)
      (handle (thunk expr env)
              #(raise {:op :define :sym sym* :value %}))
      (raise {:op :declare :sym sym*}))))

(defmethod eval-seq 'new
  [[_ sym & args] env]
  (handle (thunk sym env)
          (fn [class] ;TODO: Validate
            (fn []
              (handle (eval-items (vec args) env)
                      #(raise {:op :new :class class :args %}))))))

(defmethod eval-seq '.
  [[_ expr & body] env]
  (let [[member args] (if (and (= (count body) 1) (seq? (first body)))
                        [(ffirst body) (nfirst body)]
                        [(first body) (next body)])]
    (handle (thunk expr env)
            (fn [object]
              (handle (eval-items (vec args) env)
                      #(raise {:op :interop
                               :object object
                               :member member
                               :args %}))))))

(defn expand-dot [[head & tail]]
  (let [s (str head)]
    (cond
      (.endsWith s ".") (let [class (symbol (apply str (butlast s)))]
                          (list* 'new class tail))
      (.startsWith s ".") (let [member (symbol (apply str (next s)))
                                [obj & args] tail]
                            (list* '. obj member args)))))

(defmethod eval-seq 'set!
  [[_ location expr] env]
  (if (symbol? location)
    (handle (lookup env location)
            (fn [{:keys [origin value]}]
              (if (= origin :namespace)
                (handle (thunk expr env)
                        #(raise {:op :assign-var :var value :value %}))
                (signal {:error :not-assignable :location location}))))
    (let [[_ obj sym :as xx] (expand-dot location)] ;TODO: Validate.
      (handle (thunk obj env)
              (fn [instance]
                (handle (thunk expr env)
                        #(raise {:op :assign-field :object instance
                                 :field sym :value %})))))))

(defmethod eval-seq 'loop*
  [[_ bindings & body] env]
  (let [syms (vec (take-nth 2 bindings))
        inits (take-nth 2 (next bindings))]
    (thunk (list* (list* 'eclj.core/fn** syms body) inits) env)))

(defmethod eval-seq 'recur
  [[_ & args] env]
  (handle (eval-items (reverse args) env)
          #(raise {:op :recur :args %})))

(defmethod eval-seq 'clojure.core/import*
  [[_ sym] env]
  (raise {:op :import :sym sym}))

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
                  (signal {:error :no-matching-clause :value value})))))))

;;TODO: Proper effects for deftype and friends.
(defn host-eval [form env]
  (thunk (Apply. #'clojure.core/eval [form]) env))

(defmethod eval-seq 'deftype* [form env]
  (host-eval form env))

(defmethod eval-seq 'reify* [form env]
  (host-eval form env))

(defn apply-args [f args env]
  (handle (eval-items (reverse args) env)
          #(thunk (Apply. f %) env)))

(defmethod eval-seq :default
  [[head & tail :as form] env]
  (if (symbol? head)
    (if-let [expanded (expand-dot form)]
        (thunk expanded env)
        (handle (lookup env head)
                #(let [{:keys [origin value]} %]
                   (if (and (= origin :namespace)
                            (-> value meta :macro))
                     (thunk (Expand. value form) env)
                     (apply-args value tail env)))))
    (handle (thunk head env)
            #(apply-args % tail env))))

(defn interpret
  ([expr] (interpret expr (env/ns-env)))
  ([expr env]
   (loop [f #(-eval expr env)]
     (let [x (f)]
       (cond
         (ifn? x) (recur x)
         (answer? x) x
         (effect? x) (let [{:keys [op k]} x]
                       (if-let [handler (get-in env [:kernel op])]
                         (recur #(k (handler x)))
                         x))
         :else (unexpected x))))))

;;TODO: Overload that takes an env.
(defn eval [expr]
  (let [x (interpret expr (env/ns-env))]
    (if (answer? x)
      (:value x)
      (throw (ex-info (pr-str x) x)))))

(defn recur-handler [f env]
  (fn handler [x k]
    (cond
      (answer? x) #(k (:value x))
      (effect? x) (let [{effectk :k :keys [op]} x]
                    (if (= :recur op)
                      (if (= effectk ->Answer)
                        (thunk (Apply. f (:args x)) env)
                        (signal {:error :non-tail-position}))
                      (propegate handler x ->Answer)))
      (ifn? x) #(handler (x) k)
      :else (unexpected x))))

(defrecord Fn [name param expr env]

  Applicable
  (-apply [this args]
    (let [handler (recur-handler this env)]
      (handler (thunk expr (assoc-in env [:locals param] args))
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
  (trampoline (:k (interpret (->Let 'x 1 'y))) 5) ;XXX

  (trampoline (:k (interpret '[x 10])) 5) ;XXX

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

  (eval '(deftype Foo [bar]))
  (eval '(Foo. 1))
  (eval '(defrecord Point [x y]))

  (eval (list 'set! (list '.__methodImplCache (fn [])) ; explicitly a host fn.
                    (list 'clojure.lang.MethodImplCache. nil nil)))

  (eval '(defprotocol P))
  (eval '(defprotocol P (f [this])))

  (eval '(var Class))

  ;TODO defprotocol
  ;TODO monitor-enter and monitor-exit


)
