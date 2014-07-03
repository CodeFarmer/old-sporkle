(ns sporkle.test.bytecode
  (:require [sporkle.bytecode :refer :all])
  (:require [clojure.test :refer [deftest is testing]]))

(deftest test-syms-to-opcodes
  (is (= [:sipush 0x11 2 1 nil] (syms-to-opcodes :sipush))))

(deftest test-bytes-to-opcodes
  (is (= [:sipush 0x11 2 1 nil] (bytes-to-opcodes 0x11))))

