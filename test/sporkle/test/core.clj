(ns sporkle.test.core
  (:use [sporkle.core])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))

(deftest test-byte-stream-seq
  (testing "byte-stream-seq's seqiness"
    (let [cs (byte-stream-seq (io/input-stream "test/fixtures/Nothing.class"))]
      (testing "when the seq is new"
        (is (= 0xCA (first cs)) "we should be able to get a byte from the stream")
        (is (= 188 (count cs)) "the seq have the right number of bytes"))
      (testing "after some bytes have been taken"
        (is (= '(0xCA 0xFE) (take 2 cs)) "we should be able to retake bytes (ie., behave like a seq and not a stream")))))


;; TODO this currently ignores sign! Java doesn't... ;)
(deftest test-bytes-to-integral-type
  
  (testing "bytes-to-integral-type"

    (testing "given no bytes"
      (is (nil? (bytes-to-integral-type ()))))

    (testing "given four bytes"
      (let [b4 (bytes-to-integral-type [0 0 0 4])]
        (is (= java.lang.Long (type b4)) "should return a boxed long")
        (is (= 4 b4) "should return the correct low byte number"))
      (let [b (bytes-to-integral-type [0x00 0x00 0x01 0x00])]
        (is (= 0x00000100 b) "should return the correct 3rd-byte integer"))
      (let [b (bytes-to-integral-type [0x09 0x12 0xF4 0x2A])]
        (is (= java.lang.Long (type b)) "should return a long")
        (is (= 0x0912F42A) "should return the correct multi-byte integer"))
      (let [b (bytes-to-integral-type [0x80 0x00 0x00 0x01])]
        (is (= java.lang.Long (type b)) "should return a long")
        (is (= -1 b) "should return the correct signed integer")))
  
    (testing "given eight bytes"
      (let [b (bytes-to-integral-type [0x09 0x12 0xF4 0x2A 0x09 0x12 0xF4 0x02])]
        (is (= java.lang.Long (type b)) "should return a long")
        (is (= 653853357300184066 b)) "should return the correct multi-byte long"))
    
    (testing "Clojure 1.3 behavior"
      (let [b (bytes-to-integral-type [0x80 0x00 0x00 0x00 0x00 0x00 0x00 0x01])]
        (is (= java.lang.Long (type b)) "should return a long")
        (is (= -1 b)) "should return the correctly signed long")
      (let [b (bytes-to-integral-type [0xFF 0x12 0xF4 0x2A 0x09 0x12 0xF4 0x2])]
        (is (= java.lang.Long (type b)) "should not overflow into BigInteger even when bigger than Long.MAX_VALUE")))))


(deftest test-unpack-struct

  (testing "reading a simple struct from a stream with some trailing bytes"

    (let [[java-class remainder]
          (unpack-struct
            [[:magic 4 :unsigned] [:minor-version 2 :unsigned] [:major-version 2 :unsigned]]
            [0xCA 0xFE 0xBA 0xBE 0x00 0x00 0x00 0x32  0x00 0x0D 0x0A 0x00])]
      (is (= (:magic java-class) 0xCAFEBABE) "first 4 bytes should be 0xCAFEBABE")
      (is (= (:minor-version java-class) 0x0000) "Next two bytes are the minor version, set to 0x0000")
      (is (= (:major-version java-class) 0x0032) "Final two bytes are the major version, set to 0x0032")
      (is (= [0x00 0x0D 0x0A 0x00] remainder) "should return the remainder as a seq for further processing")))
  
  (testing "reading a struct with single-item fields"

    (let [[amap remainder] (unpack-struct [[:front-two 2] [:middle-one 1] [:back-two 2]] [1 2 3 4 5])]
      (is (not (seq? (:front-two amap))) "multi-byte fields should no longer be seqs")
      (is (= 3 (:middle-one amap)) "single-item fields should be atomic"))))

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
  (is (= [[:a 0] [:b 1] [nil 2] [:c 3]] (each-with-index [:a :b nil :c]))))
