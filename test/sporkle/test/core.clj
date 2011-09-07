(ns sporkle.test.core
  (:use [sporkle.core])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))

(deftest test-byte-stream-seq ;; FIXME: write
  (let [cs (byte-stream-seq (io/input-stream "test/fixtures/Nothing.class"))]
    (is (= 0xCA (first cs)) "We should be able to get a single byte from the stream")
    (is (= '(0xCA 0xFE) (take 2 cs)) "Having taken a byte, we should be able to take it again (ie., behave like a seq and not a stream")))
