(ns eclj.interpret.cps
  (:refer-clojure :exclude [eval])
  (:require [eclj.common :refer (map->Syntax expansion?)]
            [eclj.parse :refer (parse)]
            [eclj.fn]))

(def answer ^:eclj/answer
  (fn [x] {:op :answer :value x}))

;; Necessary because eclj.interpret.meta redefines answer symbolically.
(defn tail-effect? [effect]
  (-> effect :k meta :eclj/answer))

(defmulti interpret-syntax :head)

(defn interpret [x env]
  (interpret-syntax (parse x env)))

(defn thunk [expr env]
  #(interpret expr env))

(defn thunk-syntax [syntax]
  (thunk (map->Syntax syntax) (:env syntax)))

(defmulti -apply (fn [f arg] (class f)))

(defmacro call [f & args]
  `(let [f# ~f]
     (if (instance? eclj.fn.Fn f#)
        (-apply f# [~@args])
        (f# ~@args))))

(defn interpret-effect [x env]
  (loop [f (thunk x env)]
    (let [x (call f)]
      (if (fn? x)
        (recur x)
        (let [{:keys [op k]} x]
          (if-let [handler (get-in env [:kernel op])]
            (recur #(call k (call handler x)))
            x))))))

(defn result [effect]
  (case (:op effect)
    :answer (:value effect)
    :throw (throw (:error effect))
    (throw (ex-info "Unhandled Effect" {:eclj/effect effect}))))

(defn interpret-result [x env]
  (result (interpret-effect x env)))

(defn raise [action]
  (merge {:k answer} action))

;;TODO: Implement restarts, etc.
(defn signal [condition]
  (raise (merge {:op :condition} condition)))

(defn handle-with [handler effect k]
  (let [rec #(handle-with handler % k)]
    (if (fn? effect)
      #(rec (effect))
      (or (handler effect k)
          (assoc effect :k #(rec (fn [] ((:k effect) %))))))))

(defn default-handler [effect k]
  (when (= (:op effect) :answer)
    #(k (:value effect))))

(defn handle [effect k]
  (handle-with default-handler effect k))

(defn lookup [{:keys [locals] :as env} sym]
  (if-let [[_ value] (find locals sym)]
    (answer {:origin :locals :value value})
    (raise {:op :resolve
            :env env
            :sym sym
            :k #(if %
                  (answer %)
                  (signal {:error :undefined :sym sym}))})))

(defmethod interpret-syntax :constant
  [{:keys [value]}]
  (answer value))

(defn interpret-items [coll env]
  ((fn rec [dest src]
     (if (empty? src)
       (answer dest)
       (handle (thunk (first src) env)
               #(rec (conj dest %) (next src)))))
   (empty coll) coll))

(defmethod interpret-syntax :collection
  [{:keys [coll env]}]
  (interpret-items coll env))

(defn macro? [{:keys [origin value]}]
  (and (= origin :namespace) (-> value meta :macro)))

(defmethod interpret-syntax :name
  [{:keys [sym env]}]
  (handle (lookup env sym)
          (fn [{:keys [origin value] :as resolved}]
            (cond
              (macro? resolved) (signal {:error :value-of-macro :name sym})
              (expansion? value) (thunk (:expr value) env)
              :else (case origin
                      :locals (answer value)
                      :host (answer value)
                      :namespace (raise {:op :deref :ref value}))))))

(defmethod interpret-syntax :if
  [{:keys [test then else env]}]
  (handle (thunk test env)
          #(thunk (if % then else) env)))

(defmethod interpret-syntax :var
  [{:keys [sym env]}]
  (handle (raise {:op :resolve :env env :sym sym})
          (fn [{:keys [origin value]}]
            (assert (= origin :namespace))
            (answer value))))

(defmethod interpret-syntax :do
  [{:keys [statements ret env]}]
  (if (seq statements)
    (handle (thunk-syntax {:head :do :env env
                           :statements (pop statements)
                           :ret (peek statements)})
            (fn [_] (thunk ret env)))
    (thunk ret env)))

(defmethod interpret-syntax :bind
  [{:keys [name value expr env]}]
  (thunk expr (assoc-in env [:locals name] value)))

(defmethod interpret-syntax :let
  [{:keys [bindings expr env]}]
  (if-let [[{:keys [name init]} & bindings*] (seq bindings)]
    (handle (thunk init env)
            #(thunk-syntax {:head :let :env (assoc-in env [:locals name] %)
                            :bindings (vec bindings*) :expr expr}))
    (thunk expr env)))

(defmethod -apply Object
  [f arg]
  (signal {:error :not-callable :f f :args arg}))

(defmethod -apply clojure.lang.IFn
  [f arg]
  (raise {:op :invoke :f f :args arg}))

(defmethod -apply clojure.lang.Var
  [f arg]
  (handle (raise {:op :deref :ref f})
          #(-apply % arg)))

(defn recur-handler [f env]
  (fn [effect k]
    (case (:op effect)
      :answer #(k (:value effect))
      :recur (if (tail-effect? effect)
               (thunk-syntax {:head :apply :f f :arg (:args effect) :env env})
               (signal {:error :non-tail-position}))
      nil)))

(defmethod -apply eclj.fn.Fn
  [{:keys [name arities max-fixed-arity env] :as f} args]
  (let [argcount (count (if (counted? args)
                          args
                          (take (inc max-fixed-arity) args)))
        {:keys [params expr] :as method} (or (arities argcount)
                                             (and (>= argcount max-fixed-arity)
                                                  (arities :more)))]
    (if method
      (let [env* (if name (assoc-in env [:locals name] f) env)
            ;;TODO: Don't generate form, destructure to env & use AST directly.
            form `(let [~params '~args] ~expr)]
        (handle-with (recur-handler f env)
                     (thunk form env*) answer))
      (signal {:error :arity, :f f :given argcount}))))

;TODO: defmethod -apply for symbols & keywords ?

(defmethod interpret-syntax :letfn
  [{:keys [bindings expr env]}]
  (thunk expr (update-in env [:locals] merge bindings)))

(defn exception-handler [catches default finally env]
  (fn handler [{:keys [op] :as effect} k]
    (case op
      :answer #(handle (thunk finally env)
                       (fn [_] (fn [] (k (:value effect)))))
      :throw (let [error (:error effect)
                   catch (some (fn [{:keys [class sym expr] :as catch}]
                                 (when (instance? class error) catch))
                                 catches)]
               (when-let [{:keys [name expr]} (or catch default)]
                 #(handle (thunk-syntax {:head :bind :env env
                                         :name name :value error :expr expr})
                          (fn [y]
                            (handle (thunk finally env)
                                    (fn [_] (k y)))))))
      nil)))

(defmethod interpret-syntax :try
  [{:keys [try catches default finally env]}]
  (handle (interpret-items (mapv :type catches) env)
          (fn [classes] ;TODO: Ensure items are exception classes.
            (let [catches* (map #(assoc %1 :class %2) catches classes)]
              (handle-with (exception-handler catches* default finally env)
                           (thunk try env) answer)))))

(defmethod interpret-syntax :raise
  [{:keys [expr env]}]
  (handle (thunk expr env) raise))

(defn apply-args [f args env]
  (handle (interpret-items (reverse args) env)
          #(thunk-syntax {:head :apply :f f :arg % :env env})))

(defmethod interpret-syntax :apply
  [{:keys [f arg]}]
  (-apply f arg))

(defmethod interpret-syntax :invoke
  [{:keys [f args env form] :as ast}]
  (if (symbol? f)
    (handle (lookup env f)
            #(let [{:keys [value] :as resolved} %]
               (cond
                 (macro? resolved) (thunk-syntax {:head :expand :macro value
                                                  :form form :env env})
                 (expansion? value) (thunk-syntax (assoc ast :f (:expr value)))
                 :else (apply-args value args env))))
    (handle (thunk f env)
            #(apply-args % args env))))

;;TODO: Replace expand special with -apply on a macro type.
(defmethod interpret-syntax :expand
  [{:keys [macro form env]}]
  (handle (thunk-syntax {:head :apply :env env :f macro
                         :arg (list* form env (next form))})
          #(thunk % env)))

(defmethod interpret-syntax :new
  [{:keys [class args env]}]
  (handle (thunk class env)
          (fn [class*] ;TODO: Validate
            (fn []
              (handle (interpret-items args env)
                      #(raise {:op :new :class class* :args %}))))))

(defmethod interpret-syntax :interop
  [{:keys [target member args env]}]
  (let [interop (fn [static? object]
                  (handle (interpret-items (vec args) env)
                          #(raise {:op :interop :static? static?
                                   :object object :member member :args %})))
        instance-invoke (fn []
                          (handle (thunk target env)
                                  #(interop false %)))]
    (if (symbol? target)
      (handle (lookup env target)
              (fn [{:keys [origin value] :as resolved}]
                (if (= origin :host)
                  (interop true value)
                  (instance-invoke))))
      (instance-invoke))))

(defn interpret-meta [x env]
  (handle (thunk (meta x) env)
          #(answer (with-meta x %))))

(defmethod interpret-syntax :declare
  [{:keys [sym env]}]
  (handle (interpret-meta sym env)
          #(raise {:op :declare :sym %})))

(defmethod interpret-syntax :define
  [{:keys [sym expr env]}]
  (handle (thunk expr env)
          (fn [value]
            (handle (interpret-meta sym env)
                    #(raise {:op :define :sym % :value value})))))

;;TODO just :assign, expand symbol macros for place
(defmethod interpret-syntax :assign-var
  [{:keys [name expr env]}]
  (handle (lookup env name)
          (fn [{:keys [origin value]}]
            (if (= origin :namespace)
              (handle (thunk expr env)
                      #(raise {:op :assign-var :var value :value %}))
              (signal {:error :not-assignable :location value})))))

(defmethod interpret-syntax :assign-field
  [{:keys [object field expr env]}]
  (handle (thunk object env)
          (fn [instance]
            (handle (thunk expr env)
                    #(raise {:op :assign-field :object instance
                             :field field :value %})))))

(defmethod interpret-syntax :loop
  [{:keys [bindings expr env]}]
  (let [syms (vec (take-nth 2 bindings))
        inits (vec (take-nth 2 (next bindings)))]
    ;;TODO: Generate AST directly instead of syntax forms.
    (handle (interpret-items inits env)
            (fn [values]
              (thunk `((fn ~syms ~expr)
                       ~@(map #(list 'quote %) values))
                     env)))))

(defmethod interpret-syntax :recur
  [{:keys [args env]}]
  (handle (interpret-items args env)
          #(raise {:op :recur :args %})))

(defmethod interpret-syntax :import
  [{:keys [sym]}]
  (raise {:op :import :sym sym}))

(defmethod interpret-syntax :case
  [{:keys [expr cases default env]}]
  (handle (thunk expr env)
          (fn [value]
            (if-let [[_ match] (find cases value)]
              (thunk match env)
              (thunk default env)))))

(defmethod interpret-syntax :meta
  [{:keys [expr meta env]}]
  (handle (thunk expr env)
          (fn [object]
            (handle (thunk meta env)
                    #(answer (with-meta object %))))))

(defmethod interpret-syntax :eval-effect
  [{:keys [expr env-expr env]}]
  (handle (thunk expr env)
          (fn [form]
            (handle (thunk env-expr env)
                    (fn [env*]
                      (handle-with (fn [effect k] (k (answer effect)))
                                   (thunk form env*)
                                   identity))))))

(defmethod interpret-syntax :reify
  [{:keys [env interfaces methods]}]
  (raise {:op :reify :env env :interfaces interfaces :methods methods}))

(defmethod interpret-syntax :deftype
  [{:keys [env tagname classname fields implements methods]}]
  (raise {:op :deftype :env env :tagname tagname :classname classname
          :fields fields :implements implements :methods methods}))
