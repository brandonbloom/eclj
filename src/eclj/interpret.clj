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

(defn- thunk [expr env]
  #(interpret* expr env))

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
  [{:keys [form]}]
  (Answer. form))

(defmethod interpret* :name
  [{:keys [form env]}]
  (handle (lookup env form)
          (fn [{:keys [origin value]}]
            (case origin
              :locals (Answer. value)
              :host (Answer. value)
              :namespace (raise {:op :deref :ref value})))))

(defmethod interpret* :var
  [{:keys [sym env]}]
  (handle (raise {:op :resolve :env env :sym sym})
          (fn [{:keys [origin value]}]
            (assert (= origin :namespace))
            (Answer. value))))
