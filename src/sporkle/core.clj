(ns sporkle.core)

(defn byte-stream-seq
  "Returns a lazy sequence of bytes from a java.io.InputStream"
  [^java.io.InputStream stream]
  (let [b (.read stream)]
    (if (= b -1)
      ()
      (cons b (lazy-seq (byte-stream-seq stream))))))
