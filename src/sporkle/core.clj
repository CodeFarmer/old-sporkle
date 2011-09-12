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

(defn unpack-struct
  "Given a list of pairs of [:key integer] and a seq, return a vec containing 1) the unused portion of the seq and 2) a map whose keys are all the keys from the pairs, plus seqs of n consecutive items from aseq each (in the order they appear in avec)"
  ([avec aseq]
     (unpack-struct {} avec aseq))
  ([amap avec aseq]
     (let [[field-key field-size] (first avec) field-data (take field-size aseq)]
       (cond
        (nil? field-key) [amap aseq] ;; this is the return value
        (< field-size (count field-data)) (throw (IndexOutOfBoundsException. (str "Ran out of stream unpacking struct field " field-key ", needed " field-size ", got " field-data)))
        (= 1 field-size) (recur (assoc amap field-key (first field-data)) (rest avec) (rest aseq))
        :else (recur (assoc amap field-key field-data) (rest avec) (drop field-size aseq))))))

