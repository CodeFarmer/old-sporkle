(ns sporkle.core)


(defn byte-stream-seq
  "Returns a lazy sequence of bytes from a java.io.InputStream"

  [^java.io.InputStream stream]
  (let [b (.read stream)]
    (if (= b -1)
      ()
      (cons b (lazy-seq (byte-stream-seq stream))))))


(defn bytes-to-unsigned-integral-type
  
  "Given a big-endian stream of bytes, convert those to a long of the correct value"
  
  ([bytes]
     (if (empty? bytes)
       nil
       (bytes-to-unsigned-integral-type 0 bytes)))

  ([acc bytes]
     (if (empty? bytes)
       acc
       (recur (+ (first bytes) (bit-shift-left acc 8)) (rest bytes)))))

(defn bytes-to-integral-type

  "Given a big-endian stream of bytes, convert those to a long of the correct signed value"
  
  ([bytes]
     (if (empty? bytes)
       nil
       (if (zero? (bit-and 0x80 (first bytes)))
         (bytes-to-unsigned-integral-type 0 bytes)
         (bytes-to-integral-type 0 (cons (- (first bytes) 0x80) (rest bytes))))))

  ([acc bytes]
     (if (empty? bytes)
       (- acc)
       (recur (+ (first bytes) (bit-shift-left acc 8)) (rest bytes)))))


(defn byte-from-unsigned [i]
  "Try and coerce an integer in the range Byte.MIN_VALUE, Byte.MAX_VALUE from an unsigned integer between 0x00 and 0xFF. Behaviour is undefined outside those values!"
  (if (> i Byte/MAX_VALUE) ; try and assume it's unsigned
    (+ 0x80 (- i))
    i))


(def MAGIC_BYTES         [0xCA 0xFE 0xBA 0xBE])
(def MAJOR_VERSION_BYTES [0x00 0x32])
(def MINOR_VERSION_BYTES [0x00 0x00])


(defn unpack-struct
  "Given a list of [:key integer] and a seq, return a vec containing 1) a map whose keys are all the keys from the pairs, plus seqs made of from the requisite numbers of bytes from aseq each (in the order they appear in avec), and 2) the remainder of the seq.

Partial applications conform to the expectations of read-stream-maplets."
  
  ([avec aseq]
     (unpack-struct {} avec aseq))

  ([amap avec aseq]
     
     (let [[field-key field-size & flags] (first avec)
           field-data (take field-size aseq)]
       
       (cond
        (nil? field-key) [amap aseq] ;; this is the return value
        (< field-size (count field-data)) (throw (IndexOutOfBoundsException. (str "Ran out of stream unpacking struct field " field-key ", needed " field-size ", got " field-data)))
        :else (recur (assoc amap field-key field-data) (rest avec) (drop field-size aseq))))))


;; Dammit the threading macro is *so* close to what I need
(defn read-stream-maplets
  "Given a (seq of (functions that take a seq and return a map plus the unread portion of the seq)) and an input seq, return the combination of all the maps created by running the functions in series, each on the remaining seq left after its predecessor, starting with the input seq.

Returns a pair [map, remainder], so it can nest within itself"

  ([funcs bytes]
     (read-stream-maplets {} funcs bytes))

  ([acc funcs bytes]
     (let [f (first funcs)]
       (if (nil? f) [acc bytes]
           (let [[maplet remainder] (f bytes)]
             (recur (merge acc maplet) (rest funcs) remainder))))))


(defn each-with-index [aseq]
  "Given a seq, return a seq of pairs (vectors) containing the elements of the seq, and the index at which they appear"
  (map vector aseq (iterate inc 0)))


;; FIXME write a test
(defn write-bytes [stream byte-seq]
  "Write the bytes from byte-seq into stream, and return stream"
  (doseq [b byte-seq] (.write stream b))
  stream)


(defn two-byte-index [i]
  [(bit-and 0xFF00 i) (bit-and 0x00FF i)])


