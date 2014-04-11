(ns eclj.ns)

;;XXX This is likely all kinds of broken. Consider dynamic vars!
(defn copy-vars [ns & {:keys [exclude]}]
  (doseq [[sym var] (ns-publics ns)
          :when (not (exclude sym))]
    (intern *ns* (with-meta sym (meta var)) @var)))

(defn parse [[_ sym & body :as form]]
  (let [doc (when (string? (first body)) (first body))
        refs (if doc (next body) body)
        sym (if doc (vary-meta sym assoc :doc doc) sym)
        metadata (when (map? (first refs)) (first refs))
        refs (if metadata (next refs) refs)
        sym (if metadata (vary-meta sym merge metadata) sym)]
    {:name sym
     :meta (meta sym)
     :refs (vec (remove #(= :gen-class (first %)) refs))
     :gen-class (first (filter #(= :gen-class (first %)) refs))}))
