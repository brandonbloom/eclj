(ns eclj.reader
  (:require [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :refer
             (input-stream-push-back-reader indexing-push-back-reader)]))

;;;TODO: Override evaulator for *read-eval*, etc.

(defn form-seq [stream path]
  (let [eof (Object.)
        rdr (-> (input-stream-push-back-reader stream)
                (indexing-push-back-reader 1 path))]
    ((fn rec []
       (lazy-seq
         (let [form (reader/read rdr false eof)]
           (when-not (identical? eof form)
             (cons form (rec)))))))))
