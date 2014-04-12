(ns eclj.eval
  (:refer-clojure :exclude [eval]))

(def ^:dynamic eval-cps)

(defn run [x env]
  (loop [f #(eval-cps x env)]
    (let [x (f)]
      (if (fn? x)
        (recur x)
        (let [{:keys [op k]} x]
          (if-let [handler (get-in env [:kernel op])]
            (recur #(k (handler x)))
            x))))))

(defn ->result [effect]
  (case (:op effect)
    :answer (:value effect)
    :throw (throw (:error effect))
    (throw (ex-info (pr-str effect) effect))))

(defn eval [x env]
   (->result (run x env)))
