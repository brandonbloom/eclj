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
