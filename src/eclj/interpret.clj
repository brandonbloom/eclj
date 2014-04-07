(ns eclj.interpret
  (:require [eclj.parse :refer (parse)]))

;; Wraps all interpreter intermediate values.
(defrecord Answer [value])
(defrecord Effect [op k])

(defn answer? [x]
  (instance? Answer x))

(defn effect? [x]
  (instance? Effect x))

(defn unexpected [x]
  (throw (ex-info (str "Unexpected " (pr-str (class x)))
                  {:value x})))

(defmulti interpret* :head)

(defn interpret
  ([{:keys [env] :as syntax}]
   (loop [f #(interpret* syntax)]
     (let [x (f)]
       (cond
         (ifn? x) (recur x)
         (answer? x) (:value x)
         (effect? x) (let [{:keys [op k]} x]
                       (if-let [handler (get-in env [:kernel op])]
                         (recur #(k (handler x)))
                         (throw (ex-info (pr-str x) x))))
         :else (unexpected x)))))
  ([expr env]
   (interpret (parse expr env))))

(defn thunk
  ([syntax] #(interpret* syntax))
  ([expr env] #(interpret* (parse expr env))))

(defn raise [action]
  (map->Effect (merge {:k ->Answer} action)))

;;TODO: Implement restarts, etc.
(defn signal [condition]
  (raise (merge {:op :condition} condition)))

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

(defmethod interpret* :constant
  [{:keys [value]}]
  (Answer. value))

(defn interpret-items [coll env]
  ((fn rec [dest src]
     (if (empty? src)
       (Answer. dest)
       (handle (thunk (first src) env)
               #(rec (conj dest %) (next src)))))
   (empty coll) coll))

(defmethod interpret* :collection
  [{:keys [coll env]}]
  (interpret-items coll env))

(defmethod interpret* :name
  [{:keys [sym env]}]
  (handle (lookup env sym)
          (fn [{:keys [origin value]}]
            (case origin
              :locals (Answer. value)
              :host (Answer. value)
              :namespace (raise {:op :deref :ref value})))))

(defmethod interpret* :if
  [{:keys [test then else env]}]
  (handle (thunk test env)
          #(thunk (if % then else) env)))

(defmethod interpret* :var
  [{:keys [sym env]}]
  (handle (raise {:op :resolve :env env :sym sym})
          (fn [{:keys [origin value]}]
            (assert (= origin :namespace))
            (Answer. value))))

(defmethod interpret* :do
  [{:keys [statements ret env]}]
  (if (seq statements)
    (handle (thunk {:head :do :env env
                    :statements (pop statements)
                    :ret (peek statements)})
            (fn [_] (thunk ret env)))
    (thunk ret env)))

(defmethod interpret* :bind
  [{:keys [name value expr env]}]
  (thunk expr (assoc-in env [:locals name] value)))

(defmethod interpret* :let
  [{:keys [bindings expr env]}]
  (if-let [[{:keys [name init]} & bindings*] (seq bindings)]
    (handle (thunk init env)
            #(thunk {:head :let :env (assoc-in env [:locals name] %)
                     :bindings (vec bindings*) :expr expr}))
    (thunk expr env)))

(defprotocol Applicable
  (-apply [this arg]))

(extend-protocol Applicable

  Object
  (-apply [this arg]
    (signal {:error :not-callable :f this :args arg}))

  clojure.lang.IFn
  (-apply [this arg]
    (raise {:op :invoke :f this :args arg}))

  clojure.lang.Var
  (-apply [this arg]
    (raise {:op :invoke :f this :args arg}))

  ;TODO: Special case symbols & keywords ?

  )

(defn recur-handler [f env]
  (fn handler [x k]
    (cond
      (answer? x) #(k (:value x))
      (effect? x) (let [{effectk :k :keys [op]} x]
                    (if (= :recur op)
                      (if (= effectk ->Answer)
                        (thunk {:head :apply :f f :arg (:args x) :env env})
                        (signal {:error :non-tail-position}))
                      (propegate handler x ->Answer)))
      (ifn? x) #(handler (x) k)
      :else (unexpected x))))

(defn fn-apply [{:keys [env] :as f} arg]
  (interpret {:head :apply :f f :arg arg :env env}))

(defrecord Fn [name arities max-fixed-arity env]

  Applicable
  (-apply [this args]
    (let [handler (recur-handler this env)
          argcount (count (if (counted? args)
                            args
                            (take max-fixed-arity args)))
          {:keys [params expr]} (arities (max argcount max-fixed-arity))
          env* (if name (assoc-in env [:locals name] this) env)
          ;;TODO: Don't generate form, destructure to env & use AST directly.
          form (list 'clojure.core/let [params (list 'quote args)] expr)]
      (handler (thunk form env*) ->Answer)))

  clojure.lang.IFn
  (applyTo [this args]
    (fn-apply this args))
  ;; *cringe*
  (invoke [this]
    (fn-apply this []))
  (invoke [this a]
    (fn-apply this [a]))
  (invoke [this a b]
    (fn-apply this [a b]))
  (invoke [this a b c]
    (fn-apply this [a b c]))
  (invoke [this a b c d]
    (fn-apply this [a b c d]))
  (invoke [this a b c d e]
    (fn-apply this [a b c d e]))
  (invoke [this a b c d e f]
    (fn-apply this [a b c d e f]))
  (invoke [this a b c d e f g]
    (fn-apply this [a b c d e f g]))
  (invoke [this a b c d e f g h]
    (fn-apply this [a b c d e f g h]))
  (invoke [this a b c d e f g h i]
    (fn-apply this [a b c d e f g h i]))
  (invoke [this a b c d e f g h i j]
    (fn-apply this [a b c d e f g h i j]))
  (invoke [this a b c d e f g h i j k]
    (fn-apply this [a b c d e f g h i j k]))
  (invoke [this a b c d e f g h i j k l]
    (fn-apply this [a b c d e f g h i j k l]))
  (invoke [this a b c d e f g h i j k l m]
    (fn-apply this [a b c d e f g h i j k l m]))
  (invoke [this a b c d e f g h i j k l m n]
    (fn-apply this [a b c d e f g h i j k l m n]))
  (invoke [this a b c d e f g h i j k l m n o]
    (fn-apply this [a b c d e f g h i j k l m n o]))
  (invoke [this a b c d e f g h i j k l m n o p]
    (fn-apply this [a b c d e f g h i j k l m n o p]))
  (invoke [this a b c d e f g h i j k l m n o p q]
    (fn-apply this [a b c d e f g h i j k l m n o p q]))
  (invoke [this a b c d e f g h i j k l m n o p q r]
    (fn-apply this [a b c d e f g h i j k l m n o p q r]))
  (invoke [this a b c d e f g h i j k l m n o p q r s]
    (fn-apply this [a b c d e f g h i j k l m n o p q r s]))
  (invoke [this a b c d e f g h i j k l m n o p q r s t]
    (fn-apply this [a b c d e f g h i j k l m n o p q r s t]))
  (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
    (fn-apply this (concat [a b c d e f g h i j k l m n o p q r s t] rest)))

  )

(defn syntax->fn [syntax]
  (map->Fn (select-keys syntax [:name :arities :max-fixed-arity :env])))

;;TODO: Can this just be a constant? parse would have to create the Fns.
;; That would mean letfn doesn't need to do the conversion either.
(defmethod interpret* :fn
  [syntax]
  (Answer. (syntax->fn syntax)))

(defmethod interpret* :letfn
  [{:keys [bindings expr env]}]
  (let [bindings* (into {} (for [[name f] bindings]
                             [name (syntax->fn (assoc f :env env))]))
        env* (update-in env [:locals] merge bindings*)]
    (thunk expr env*)))

(defn exception-handler [catches default finally env]
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
          (if-let [{:keys [name expr]} (or catch default)]
            #(handle (thunk {:head :bind :env env
                             :name name :value error :expr expr})
                     (fn [y]
                       (handle (thunk finally env)
                               (fn [_] (k y)))))
            (propegate handler x ->Answer)))
      (ifn? x) #(handler (x) k)
      :else (unexpected x))))

(defmethod interpret* :try
  [{:keys [try catches default finally env]}]
  (handle (interpret-items (mapv :type catches) env)
          (fn [classes] ;TODO: Ensure items are exception classes.
            (let [handler (exception-handler catches default finally env)]
              (handler (thunk try env) ->Answer)))))

(defmethod interpret* :throw
  [{:keys [expr env]}]
  (handle (thunk expr env)
          #(raise {:op :throw :error %})))

(defn apply-args [f args env]
  (handle (interpret-items (reverse args) env)
          #(thunk {:head :apply :f f :arg % :env env})))

(defmethod interpret* :apply
  [{:keys [f arg]}]
  (-apply f arg))

(defmethod interpret* :invoke
  [{:keys [f args env form]}]
  (if (symbol? f)
    (handle (lookup env f)
            #(let [{:keys [origin value]} %]
               (if (and (= origin :namespace)
                        (-> value meta :macro))
                 (thunk {:head :expand :macro value :form form :env env})
                 (apply-args value args env))))
    (handle (thunk f env)
            #(apply-args % args env))))

(defmethod interpret* :expand
  [{:keys [macro form env]}]
  (handle (thunk {:head :apply :env env :f macro
                  :arg (list* form env (next form))})
          #(thunk % env)))

(defmethod interpret* :new
  [{:keys [class args env]}]
  (handle (thunk class env)
          (fn [class*] ;TODO: Validate
            (fn []
              (handle (interpret-items args env)
                      #(raise {:op :new :class class* :args %}))))))

(defmethod interpret* :interop
  [{:keys [target member args env]}]
  (handle (thunk target env)
          (fn [object]
            (handle (interpret-items (vec args) env)
                    #(raise {:op :interop
                             :object object
                             :member member
                             :args %})))))

(defmethod interpret* :declare
  [{:keys [sym]}]
  (raise {:op :declare :sym sym}))

(defmethod interpret* :define
  [{:keys [sym expr env]}]
  (handle (thunk expr env)
          #(raise {:op :define :sym sym :value %})))

(defmethod interpret* :assign-var
  [{:keys [name expr env]}]
  (handle (lookup env name)
          (fn [{:keys [origin value]}]
            (if (= origin :namespace)
              (handle (thunk expr env)
                      #(raise {:op :assign-var :var value :value %}))
              (signal {:error :not-assignable :location value})))))

(defmethod interpret* :assign-field
  [{:keys [object field expr env]}]
  (handle (thunk object env)
          (fn [instance]
            (handle (thunk expr env)
                    #(raise {:op :assign-field :object instance
                             :field field :value %})))))

(defmethod interpret* :recur
  [{:keys [args env]}]
  (handle (interpret-items args env)
          #(raise {:op :recur :args %})))
