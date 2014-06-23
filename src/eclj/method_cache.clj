(ns eclj.method-cache)

(defn super-chain [^Class c]
  (if (.isArray c) ;XXX EClj extension
    (let [^Class ctype (.getComponentType c)]
      (if (identical? ctype java.lang.Object) ;TODO consider (.isPrimitive ctype)
        [c java.lang.Object]
        [c #=(java.lang.Class/forName "[Ljava.lang.Object;") java.lang.Object]))
    ((fn rec [^Class c]
       (when c
         (cons c (rec (.getSuperclass c)))))
     c)))

(defn pref
  ([] nil)
  ([a] a)
  ([^Class a ^Class b]
     (if (.isAssignableFrom a b) b a)))

(defn find-impl [protocol x]
  (if (instance? (:on-interface protocol) x)
    x
    (let [c (class x)
          impl #(get (:impls protocol) %)]
      (or (impl c)
          (and c (or (first (remove nil? (map impl (butlast (super-chain c)))))
                     (when-let [t (reduce pref (filter impl (disj (supers c) Object)))]
                       (impl t))
                     (impl Object)))))))

(defn find-method [protocol methodk x]
  (get (find-impl protocol x) methodk))

(defn cache-method [cache x ^Class cls ^clojure.lang.IFn interf]
  (let [che @cache
        f (if (.isInstance cls x)
            interf
            (find-method (.protocol che) (.methodk che) x))]
    (when-not f
      (throw (IllegalArgumentException. (str "No implementation of method: " (.methodk che)
                                             " of protocol: " (:var (.protocol che))
                                             " found for class: " (if (nil? x) "nil" (.getName (class x)))))))
    (swap! cache #'clojure.core/expand-method-impl-cache (class x) f)
    f))
