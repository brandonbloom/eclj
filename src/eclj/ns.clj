(ns eclj.ns)

(defn copy-vars [ns & {:keys [exclude]}]
  (doseq [[sym var] (ns-publics ns)
          :when (not (exclude sym))]
    (intern *ns* (with-meta sym (meta var)) @var)))
