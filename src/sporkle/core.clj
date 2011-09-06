(ns sporkle.core)

(defn byte-stream-seq
  "Returns a lazy sequence of bytes from a java.io.InputStream"
  [^java.io.InputStream stream]
  (when-let [b (.read stream)]
    (cons b (lazy-seq (byte-stream-seq stream)))))
