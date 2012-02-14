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

      (is (= 12 (cp-find constant-pool {:tag CONSTANT_Utf8 :bytes (seq (.getBytes "Nothing"))}))
          "The string 'Nothing' should be stored in a UTf8 constant"))))

(deftest test-include-constant
  (testing "with an empty constant pool")

  (testing "with some entries already in the pool"))


(deftest test-minimal-meaningful-class
  (testing "that the creation of a minimal class should have minimal, self-consistent fields")

  (let [clazz (java-class "Nothing")]

    (comment (is (= "Nothing" (class-name clazz)
                    "this-class member should correctly resolve to the name")))))
