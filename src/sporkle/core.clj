(ns sporkle.core)

(defn byte-stream-seq
  "Returns a lazy sequence of bytes from a java.io.InputStream"
  [^java.io.InputStream stream]
  (let [b (.read stream)]
    (if (= b -1)
      ()
      (cons b (lazy-seq (byte-stream-seq stream))))))

(defn bytes-to-integral-type
  "Given a big-endian stream of bytes, convert those to a Java integral type of the correct size and value"
  
  ([bytes]
      (if (empty? bytes)
        nil
        (bytes-to-integral-type 0 bytes)))

  ([acc bytes]
      (if (empty? bytes)
        acc
        (recur (+ (first bytes) (bit-shift-left acc 8)) (rest bytes)))))
