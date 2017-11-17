(ns sporkle.test.core
  (:require [sporkle.core :refer :all])
  (:require [clojure.test :refer [deftest is testing]])
  (:require [clojure.java.io :as io])
  (:import  [java.io ByteArrayOutputStream]))


(deftest test-byte-stream-seq
  (testing "byte-stream-seq's seqiness"
    (let [cs (byte-stream-seq (io/input-stream "test/fixtures/Nothing.class"))]
      (testing "when the seq is new"
        (is (= 0xCA (first cs)) "we should be able to get a byte from the stream")
        (is (= 188 (count cs)) "the seq have the right number of bytes"))
      (testing "after some bytes have been taken"
        (is (= '(0xCA 0xFE) (take 2 cs)) "we should be able to retake bytes (ie., behave like a seq and not a stream")))))


(deftest test-bytes-to-int
  
  (testing "bytes-to-int"

    (testing "given four bytes"
      (let [b4 (bytes-to-int [0 0 0 4])]
        (is (= java.lang.Long (type b4)) "should return a boxed long")
        (is (= 4 b4) "should return the correct low byte number"))
      (let [b (bytes-to-int [0x00 0x00 0x01 0x00])]
        (is (= 0x00000100 b) "should return the correct 3rd-byte integer"))
      (let [b (bytes-to-int [0x09 0x12 0xF4 0x2A])]
        (is (= java.lang.Long (type b)) "should return a long")
        (is (= 0x0912F42A b) "should return the correct multi-byte integer"))
      (let [b (bytes-to-int [0xff 0xff 0xff 0xff])]
        (is (= java.lang.Long (type b)) "should return a long")
        (is (= -1 b) "should return the correct signed integer")))
  
    
    
    (testing "Clojure 1.3 behavior"
        (let [b (bytes-to-long [0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF])]
          (is (= java.lang.Long (type b)) "should return a long")
          (is (= -1 b)) "should return the correctly signed long")
        (let [b (bytes-to-long [0xFF 0x12 0xF4 0x2A 0x09 0x12 0xF4 0x2])]
          (is (= java.lang.Long (type b)) "should not overflow into BigInteger even when bigger than Long.MAX_VALUE")))))


(deftest test-byte-from-unsigned
  
  (testing "for values within the normal range for signed byte"
    (is (= 0 (byte-from-unsigned 0))
        "0 should be the easiest case")
    (is (= 5 (byte-from-unsigned 5))
        "Working for small numbers")
    (is (= 127 (byte-from-unsigned 127))
        "127 is the highest positive number"))

  (testing "negative numbers and edge cases"

    (is (= -128 (byte-from-unsigned 0x80)) "0x80 is the sign bit and minimum value")
    (is (= -127 (byte-from-unsigned 0x81)) "0x81 is one greater than the minimum value")
    (is (= -1   (byte-from-unsigned 0xFF)) "0xFF is the highes negative number, or -1")))


(deftest test-bytes-to-long
  (testing "for values between 128 and 255"

    (is (= 0xFE (bytes-to-long [0xFE]))))
  (testing "for values between 0 and 127"
    (is (= 0x0E (bytes-to-long [0x0E])))
    (is (= 0x7F (bytes-to-long [0x7F]))))

  (testing "given eight bytes"
    (let [b (bytes-to-long [0x09 0x12 0xF4 0x2A 0x09 0x12 0xF4 0x02])]
      (is (= java.lang.Long (type b)) "should return a long")
      (is (= 653853357300184066 b)) "should return the correct multi-byte long")))


(deftest test-unpack-struct

  (testing "reading a simple struct from a stream with some trailing bytes"

    (let [[java-class remainder]
          (unpack-struct
            [[:magic 4] [:minor-version 2 bytes-to-long] [:major-version 2 bytes-to-long]]
            [0xCA 0xFE 0xBA 0xBE 0x00 0x00 0x00 0x32  0x00 0x0D 0x0A 0x00])]
      (is (= (:magic java-class) [0xCA 0xFE 0xBA 0xBE]) "first 4 bytes should be 0xCAFEBABE")
      (is (= (:minor-version java-class) 0x0000) "Next two bytes are the minor version, set to 0x0000")
      (is (= (:major-version java-class) 0x0032) "Final two bytes are the major version, set to 0x0032")
      (is (= [0x00 0x0D 0x0A 0x00] remainder) "should return the remainder as a seq for further processing")))
  
  (testing "reading a struct with single-item fields"

    (let [[amap remainder] (unpack-struct [[:front-two 2] [:middle-one 1] [:back-two 2]] [1 2 3 4 5])]
      (is (seq? (:front-two amap)) "multi-byte fields should be seqs")
      (is (= [3] (:middle-one amap)) "single-item fields should no longer be atomic")))

  (testing "reading a struct with handlers"

    (let [[amap remainder] (unpack-struct [[:front-two 2 bytes-to-long] [:middle-one 1 bytes-to-long] [:back-two 2]] [1 2 3 4 5])]
      (is (= 0x0102 (:front-two amap)) "multi-byte fields be handled")
      (is (= 3 (:middle-one amap)) "single-item fields should be handled"))))

;; TODO consider making this read-bytes-and-return-map-plus-remainder form
;; into a macro, just for kicks
(deftest test-read-stream-maplets

  (testing "the correct ordering of reads along a stream"

    (letfn [(f [seq] [{:f (first seq)} (rest seq)])
            (g [seq] [{:g (first seq)} (rest seq)])
            (h [seq] [{:h (take 2 seq)} (drop 2 seq)])]
      (let [[amap _] (read-stream-maplets [f g h] [1 2 3 4 5])]
        (is (= (:f amap) 1)     "should read f first")
        (is (= (:g amap) 2)     "should read g second")
        (is (= (:h amap) [3 4]) "should read h third"))
      (let [[amap _] (read-stream-maplets [f h g] [1 2 3 4 5])]
        (is (= (:f amap) 1)     "should read f first")
        (is (= (:g amap) 4)     "should read g third")
        (is (= (:h amap) [2 3]) "should read h second")))))


(deftest test-each-with-index
  (is (= [] (each-with-index [])) "should return empty seq when input is empty")
  (is (= [[:a 0] [:b 1] [nil 2] [:c 3]] (each-with-index [:a :b nil :c]))))


(deftest test-write-bytes
  
  (with-open [stream (ByteArrayOutputStream.)]
    (is (= stream (write-bytes stream [1 2 3]))
        "write-bytes should return the stream it is writing to"))

  (testing "with arguments < 0x80"
    (with-open [stream (ByteArrayOutputStream.)]
      (is (= [1 2 3 4 5] (seq (.toByteArray (write-bytes stream [1 2 3 4 5])))))))

  (testing "with arguments > 0x80"
    (with-open [stream (ByteArrayOutputStream.)]
      (is (= [-0x80 -0x7F 0x20 -0x01] (seq (.toByteArray (write-bytes stream [0x80 0x81 0x20 0xFF])))) "Written bytes should be signed interpretations"))
    (with-open [stream (ByteArrayOutputStream.)]
      (is (= [-54 -2 -70 -66] (seq (.toByteArray (write-bytes stream MAGIC_BYTES)))) "Written bytes should match the known file values for the classfile magic number"))))
