(ns sporkle.test.bytecode
  (:use [sporkle.bytecode])
  (:use [clojure.test]))

(deftest test-syms-to-opcodes
  (is (= [:sipush 0x11 2 1] (syms-to-opcodes :sipush))))

(deftest test-bytes-to-opcodes
  (is (= [:sipush 0x11 2 1] (bytes-to-opcodes 0x11))))

