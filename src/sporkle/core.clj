(ns sporkle.core)


(defn byte-from-unsigned 
  "Try and coerce an integer in the range Byte.MIN_VALUE, Byte.MAX_VALUE from an unsigned long between 0x00 and 0xFF. Behaviour is undefined outside those values!"
  [i]
  (let [sign (bit-and 0x80 i)
        size (bit-and 0x7F i)]
    (- size sign)))

(defn int-from-unsigned 
  "Try and coerce an integer in the range Integer.MIN_VALUE, Integer.MAX_VALUE from an unsigned long between 0x00000000 and 0xFFFFFFFF. Behaviour is undefined outside those values!"
  [i]
  (let [sign (bit-and 0x80000000 i)
        size (bit-and 0x7FFFFFFF i)]
    (- size sign)))

(defn byte-stream-seq
  "Returns a lazy sequence of bytes from a java.io.InputStream. NOTE that these are actually unsigned ints that only have 8 significant bits"

  [^java.io.InputStream stream]
  (let [b (.read stream)]
    (if (= b -1)
      ()
      (cons b (lazy-seq (byte-stream-seq stream))))))

(defn read-stream-to-byte-array 
  "FIXME this should not be necessary; either that or ditch byte-stream-seq (which maybe doesn't really need to be lazy). Or at least buffer it, this will be slow reading big things (will it ever read big things?)."
  [stream]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (loop [b (.read stream)]
      (if (= b -1)
        (.toByteArray baos)
        (do (.write baos b)
            (recur (.read stream)))))))

(defn bytes-to-unsigned-integral-type
  
  "Given a big-endian stream of bytes, convert those to a long of the correct value assuming the target value is positive. NOTE also expects that 'bytes' actually means 'unsigned ints with 8 significant bits'"
  
  ([bytes]
     (if (empty? bytes)
       nil
       (bytes-to-unsigned-integral-type 0 bytes)))

  ([acc bytes]
     (if (empty? bytes)
       acc
       (recur (+ (first bytes) (bit-shift-left acc 8)) (rest bytes)))))

(defn bytes-to-int
  [bytes]
  (int-from-unsigned (bytes-to-unsigned-integral-type bytes)))

(defn int-to-byte-pair [i]
  [(bit-and i 0xFF00) (bit-and i 0x00FF)])

;; FIXME move to classfile?
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


;; This is for locating constants by name and returning the index by
;; which to refer to them in the class
(defn each-with-index
  "Given a seq, return a seq of pairs (vectors) containing the elements of the seq, and the index at which they appear."
  [aseq]
  (map vector aseq (iterate inc 0)))


(defn write-bytes
  "Write the bytes from byte-seq into stream, and return stream"
  [stream byte-seq]
  (doseq [b byte-seq] (.write stream b))
  stream)


(defn two-byte-index
  "Given an integer, return a byte pair as used by the classfile format to describe an array index"
  [i]
  [(bit-and 0xFF00 i) (bit-and 0x00FF i)])


(defn four-byte-count
  "Given an integer, return a byte quad, used by the classfile format to describe a count or size"
  [i]
  [(bit-and 0xFF000000 i) (bit-and 0x00FF00 i) (bit-and 0x0000FF00 i) (bit-and 0x000000FF i)])
