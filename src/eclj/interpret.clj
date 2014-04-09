(ns eclj.interpret
  (:refer-clojure :exclude [eval])
  (:require [eclj.common :refer (map->Syntax)]
            [eclj.parse :refer (parse)]
            [eclj.eval :refer (eval eval*)]
            [eclj.fn]))

(defn answer [x]
  {:op :answer :value x})

(defmulti interpret-syntax* :head)

(defn interpret* [x env]
  (let [{:keys [env] :as syntax} (parse x env)]
    (loop [f #(interpret-syntax* syntax)]
      (let [x (f)]
        (cond
          (fn? x) (recur x)
          (= (:op x) :answer) x
          :else (let [{:keys [op k]} x]
                  (if-let [handler (get-in env [:kernel op])]
                    (recur #(k (handler x)))
                    x)))))))

(defrecord Interpreter [])

(extend Interpreter
  eclj.eval/Evaluator
  (assoc eclj.eval/evaluator-mixin :eval-with*
         (fn [_ x env] (interpret* x env))))

(def interpreter (Interpreter.))

(defn thunk
  ([syntax] #(eval* (map->Syntax syntax)))
  ([expr env] #(eval* (parse expr env))))

(defn raise [action]
  (merge {:k answer} action))

;;TODO: Implement restarts, etc.
(defn signal [condition]
  (raise (merge {:op :condition} condition)))

(defn propegate [handler {:keys [k] :as effect} nextk]
  (assoc effect :k #(handler (k %) nextk)))

(defn handle [x k]
  (cond
    (fn? x) #(handle (x) k)
    (= (:op x) :answer) #(k (:value x))
    :else (propegate handle x k)))

(defn lookup [{:keys [locals] :as env} sym]
  (if-let [[_ value] (find locals sym)]
    (answer {:origin :locals :value value})
    (raise {:op :resolve
            :env env
            :sym sym
            :k #(if %
                  (answer %)
                  (signal {:error :undefined :sym sym}))})))

(defmethod interpret-syntax* :constant
  [{:keys [value]}]
  (answer value))

(defn interpret-items [coll env]
  ((fn rec [dest src]
     (if (empty? src)
       (answer dest)
       (handle (thunk (first src) env)
               #(rec (conj dest %) (next src)))))
   (empty coll) coll))

(defmethod interpret-syntax* :collection
  [{:keys [coll env]}]
  (interpret-items coll env))

(defmethod interpret-syntax* :name
  [{:keys [sym env]}]
  (handle (lookup env sym)
          (fn [{:keys [origin value]}]
            (case origin
              :locals (answer value)
              :host (answer value)
              :namespace (raise {:op :deref :ref value})))))

(defmethod interpret-syntax* :if
  [{:keys [test then else env]}]
  (handle (thunk test env)
          #(thunk (if % then else) env)))

(defmethod interpret-syntax* :var
  [{:keys [sym env]}]
  (handle (raise {:op :resolve :env env :sym sym})
          (fn [{:keys [origin value]}]
            (assert (= origin :namespace))
            (answer value))))

(defmethod interpret-syntax* :do
  [{:keys [statements ret env]}]
  (if (seq statements)
    (handle (thunk {:head :do :env env
                    :statements (pop statements)
                    :ret (peek statements)})
            (fn [_] (thunk ret env)))
    (thunk ret env)))

(defmethod interpret-syntax* :bind
  [{:keys [name value expr env]}]
  (thunk expr (assoc-in env [:locals name] value)))

(defmethod interpret-syntax* :let
  [{:keys [bindings expr env]}]
  (if-let [[{:keys [name init]} & bindings*] (seq bindings)]
    (handle (thunk init env)
            #(thunk {:head :let :env (assoc-in env [:locals name] %)
                     :bindings (vec bindings*) :expr expr}))
    (thunk expr env)))

(defprotocol Applicable
  (-apply [this arg]))

(defn recur-handler [f env]
  (fn handler [x k]
    (cond
      (fn? x) #(handler (x) k)
      (= (:op x) :answer) #(k (:value x))
      :else (let [{effectk :k :keys [op]} x]
              (if (= :recur op)
                (if (= effectk answer)
                  (thunk {:head :apply :f f :arg (:args x) :env env})
                  (signal {:error :non-tail-position}))
                (propegate handler x answer))))))

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

  eclj.fn.Fn
  (-apply [{:keys [name arities max-fixed-arity env] :as this} args]
    (let [handler (recur-handler this env)
          argcount (count (if (counted? args)
                            args
                            (take max-fixed-arity args)))
          {:keys [params expr]} (arities (max argcount max-fixed-arity))
          env* (if name (assoc-in env [:locals name] this) env)
          ;;TODO: Don't generate form, destructure to env & use AST directly.
          form `(let [~params '~args] ~expr)]
      (handler (thunk form env*) answer)))

  ;TODO: Special case symbols & keywords ?

  )

(defmethod interpret-syntax* :letfn
  [{:keys [bindings expr env]}]
  (thunk expr (update-in env [:locals] merge bindings)))

(defn exception-handler [catches default finally env]
  (fn handler [x k]
    (cond
      (fn? x) #(handler (x) k)
      (= (:op x) :answer) #(handle (thunk finally env)
                                   (fn [_] (fn [] (k (:value x)))))
      :else
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
            (propegate handler x answer))))))

(defmethod interpret-syntax* :try
  [{:keys [try catches default finally env]}]
  (handle (interpret-items (mapv :type catches) env)
          (fn [classes] ;TODO: Ensure items are exception classes.
            (let [handler (exception-handler catches default finally env)]
              (handler (thunk try env) answer)))))

(defmethod interpret-syntax* :throw
  [{:keys [expr env]}]
  (handle (thunk expr env)
          #(raise {:op :throw :error %})))

(defn apply-args [f args env]
  (handle (interpret-items (reverse args) env)
          #(thunk {:head :apply :f f :arg % :env env})))

(defmethod interpret-syntax* :apply
  [{:keys [f arg]}]
  (-apply f arg))

(defmethod interpret-syntax* :invoke
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

;;TODO: Replace expand special with Applicable Macro type?
(defmethod interpret-syntax* :expand
  [{:keys [macro form env]}]
  (handle (thunk {:head :apply :env env :f macro
                  :arg (list* form env (next form))})
          #(thunk % env)))

(defmethod interpret-syntax* :new
  [{:keys [class args env]}]
  (handle (thunk class env)
          (fn [class*] ;TODO: Validate
            (fn []
              (handle (interpret-items args env)
                      #(raise {:op :new :class class* :args %}))))))

(defmethod interpret-syntax* :interop
  [{:keys [target member args env]}]
  (handle (thunk target env)
          (fn [object]
            (handle (interpret-items (vec args) env)
                    #(raise {:op :interop
                             :object object
                             :member member
                             :args %})))))

(defmethod interpret-syntax* :declare
  [{:keys [sym]}]
  (raise {:op :declare :sym sym}))

(defmethod interpret-syntax* :define
  [{:keys [sym expr env]}]
  (handle (thunk expr env)
          #(raise {:op :define :sym sym :value %})))

(defmethod interpret-syntax* :assign-var
  [{:keys [name expr env]}]
  (handle (lookup env name)
          (fn [{:keys [origin value]}]
            (if (= origin :namespace)
              (handle (thunk expr env)
                      #(raise {:op :assign-var :var value :value %}))
              (signal {:error :not-assignable :location value})))))

(defmethod interpret-syntax* :assign-field
  [{:keys [object field expr env]}]
  (handle (thunk object env)
          (fn [instance]
            (handle (thunk expr env)
                    #(raise {:op :assign-field :object instance
                             :field field :value %})))))

(defmethod interpret-syntax* :loop
  [{:keys [bindings expr env]}]
  (let [syms (vec (take-nth 2 bindings))
        inits (vec (take-nth 2 (next bindings)))]
    ;;TODO: Generate AST directly instead of syntax forms.
    (handle (interpret-items inits env)
            #(thunk `((fn ~syms ~expr) ~@%) env))))

(defmethod interpret-syntax* :recur
  [{:keys [args env]}]
  (handle (interpret-items args env)
          #(raise {:op :recur :args %})))

(defmethod interpret-syntax* :import
  [{:keys [sym]}]
  (raise {:op :import :sym sym}))
