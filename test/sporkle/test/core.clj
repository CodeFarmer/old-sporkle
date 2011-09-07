(ns sporkle.test.core
  (:use [sporkle.core])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))

(deftest test-byte-stream-seq
  (testing "byte-stream-seq's seqiness"
    (let [cs (byte-stream-seq (io/input-stream "test/fixtures/Nothing.class"))]
      (testing "when the seq is new"
        (is (= 0xCA (first cs)) "we should be able to get a byte from the stream")
        (is (= 188 (count cs)) "the seq should return the byte count of the stream"))
      (testing "after some bytes have been taken"
        (is (= '(0xCA 0xFE) (take 2 cs)) "we should be able to retake bytes (ie., behave like a seq and not a stream")))))
