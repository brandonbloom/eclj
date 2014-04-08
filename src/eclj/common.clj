(ns eclj.common)

;;TODO: Install handler that converts all effects in to errors.
(defmacro pure [x] x)

(defrecord Syntax [head form env])
