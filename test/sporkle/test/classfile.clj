(ns sporkle.test.classfile
  (:use [sporkle.classfile])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))

;; (deftest read-constant-pool-entry)

(deftest test-read-utf8-entry
  (testing "Reading from a known UTF8 constant from a classfile, with some trailing bytes"
    ;; actually it's the string "Nothing.java"
    (let [[count entry] (read-utf8-entry
                         [0x01 0x00 0x0C 0x4E  0x6F 0x74 0x68 0x69
                          0x6E 0x67 0x2E 0x6A  0x61 0x76 0x61 0x0C
                          ;; 0x61 is actually the last byte ^ of the entry
                          00 04])]
      (is (= 15 count)                   "should correctly return the number of bytes read")
      (is (= CONSTANT_Utf8 (:tag entry)) "should correctly set the tag")
      (is (= 12 (:length entry))         "should read the two-byte length correctly")
      (is (= [0x4E 0x6F 0x74 0x68 0x69 0x6E 0x67 0x2E 0x6A 0x61 0x76 0x61] (:bytes entry))                          "should read the correct bytes")
      (is (= "Nothing.java"   (apply str (map char (:bytes entry))))
                                         "should give us back bytes that can be turned into a Java String"))))


