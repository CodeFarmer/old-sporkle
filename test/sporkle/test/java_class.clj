(ns sporkle.test.java-class
  
  (:use [clojure.test])
  (:use [sporkle.core])
  (:use [sporkle.java-class])
  (:use [sporkle.classfile])
  (:require [clojure.java.io :as io]))

(deftest test-minimal-class
  (testing "that the creation of a minimal class should fill out the required keys in the class-file struct"

    (let [clazz (java-class "Nothing")]
      (is (map? clazz)
          "java-class should return a Map")
      
      (is (vector? (:constant-pool clazz))
          "java-class should have a vector constant pool")

      (is (vector? (:methods clazz))
          "java-class should have a methods vector")
      
      (is (not (nil? (:this-class clazz)))
          "java-class should have a this-class member"))))


(deftest test-cp-find
  (testing "With s real class constant pool"
    (let [constant-pool (:constant-pool (read-java-class (byte-stream-seq (io/input-stream "test/fixtures/Nothing.class"))))]
      
      (is (= 11 (cp-find constant-pool {:tag [CONSTANT_Utf8] :bytes (seq (.getBytes "Nothing"))}))
          "The string 'Nothing' should be stored in a UTf8 constant with index 11")
      (is (nil? (cp-find constant-pool {:tag [CONSTANT_Utf8] :bytes (seq (.getBytes "Not a real Utf8 constant"))}))
          "cp-find shoudl return nil when it can't find a constant"))))


(deftest test-include-constant
  (testing "with an empty constant pool"

    (let [constant-pool []]

      (let [const {:tag [CONSTANT_Utf8] :bytes (seq (.getBytes "Nothing"))}
            [new-constant-pool index] (include-constant constant-pool const)]

        (is (= 1 index)
            "New item should always have index 1")
        (is (= 1 (cp-find new-constant-pool const))
            "And the item should indeed be found in the index"))))

  (testing "with an entry already in the pool"

    (let [const         {:tag [CONSTANT_Utf8] :bytes (seq (.getBytes "Nothing"))}
          other-const   {:tag [CONSTANT_Utf8] :bytes (seq (.getBytes "Something"))}
          constant-pool [const]]

      (let [[new-constant-pool index] (include-constant constant-pool const)]

        (is (= constant-pool new-constant-pool)
            "constant pool should remain unchanged when an already-included constant is reincluded")
        (is (= 1 index)
            "correct index should be returned for the existing constant"))

      (let [[new-constant-pool index] (include-constant constant-pool other-const)]

        (is (= 2 index)
            "constant should have new index at the end of the pool")
        (is (= 2 (cp-find new-constant-pool other-const))
            "constant should be findable at the end of the pool")))))


(deftest test-minimal-meaningful-class
  (testing "that the creation of a minimal class should have minimal, self-consistent fields")

  (let [clazz (java-class "Nothing")]

    (is (= "Nothing" (class-name clazz))
        "this-class member should correctly resolve to the name")))
