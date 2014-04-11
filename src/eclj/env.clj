(ns eclj.env
  (:import [clojure.lang Reflector]))

(defrecord Env [namespace locals kernel])
;TODO: Should only the top env have kernel?

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

(def patches {#'clojure.core/case 'eclj.core/case
              #'clojure.core/ns 'eclj.core/ns
              #'clojure.core/deftype 'eclj.core/deftype
              #'clojure.core/defrecord 'eclj.core/defrecord
              #'clojure.core/defprotocol 'eclj.core/defprotocol})

(defn try-lookup [ns sym]
  (try
    (if-let [x (ns-resolve ns sym)]
      (if (var? x)
        (if-let [patch (patches x)]
          (try-lookup ns patch)
          {:origin :namespace
           :value (or (-> x meta :eclj/alias) x)})
        {:origin :host :value x})
      {:origin :host :value (clojure.lang.RT/classForName (name sym))})
    (catch ClassNotFoundException e
      nil)))

;;TODO: Namespaced keys?
(def kernel {

  :deref
  (fn [{:keys [ref]}]
    (deref ref))

  :invoke
  (fn [{:keys [f args]}]
    (apply f args))

  :resolve
  (fn [{:keys [env sym]}]
    (or (try-lookup (:namespace env) sym)
        (when-let [ns (namespace sym)]
          (let [{:keys [value]} (try-lookup (:namespace env) (symbol ns))
                n (name sym)]
            (when (instance? Class value)
              {:origin :host
               :value (try
                        (.get (.getField value n) value)
                        (catch NoSuchFieldException _
                          (staticfn value n)))})))))

  :declare
  (fn [{:keys [sym]}]
    (intern *ns* sym))

  :define
  (fn [{:keys [sym value]}]
    (let [var (intern *ns* sym value)]
      (when (-> sym meta :dynamic)
        (.setDynamic var))
      var))

  :new
  (fn [{:keys [class args]}]
    (Reflector/invokeConstructor class (object-array args)))

  :interop
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

  :assign-var
  (fn [{:keys [var value]}]
    (var-set var value))

  :assign-field ;TODO: Test this.
  (fn [{:keys [object field value]}]
    (let [field (name field)]
      (if (instance? Class object)
        (Reflector/setStaticField object field value)
        (Reflector/setInstanceField object field value))))

  :import
  (fn [{:keys [sym]}]
    (.importClass *ns* (clojure.lang.RT/classForName (name sym))))

})

(defn ns-env []
  (Env. *ns* {} kernel))
