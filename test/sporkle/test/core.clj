(ns sporkle.test.core
  (:use [sporkle.core])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))

(deftest test-byte-stream-seq
  (testing "byte-stream-seq"
    (let [cs (byte-stream-seq (io/input-stream "test/fixtures/Nothing.class"))]
      (testing "getting a single byte"
        (is (= 0xCA (first cs)) "we should be able to get a byte from the stream"))
      (testing "getting bytes after some have been taken"
        (is (= '(0xCA 0xFE) (take 2 cs)) "we should be able to retake bytes (ie., behave like a seq and not a stream")))))
