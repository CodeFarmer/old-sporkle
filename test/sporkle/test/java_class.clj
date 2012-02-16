(ns sporkle.test.java-class
  
  (:use [clojure.test])
  (:use [sporkle.core])
  (:use [sporkle.java-class])
  (:use [sporkle.classfile])
  (:require [clojure.java.io :as io]))


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


(deftest test-minimal-class
  (testing "that the creation of a minimal class should fill out the required keys in the class-file struct"

    (let [clazz (java-class "Nothing")]
      (is (map? clazz)
          "java-class should return a Map")
      
      (is (coll? (:constant-pool clazz))
          "java-class should have a vector constant pool")

      (is (coll? (:methods clazz))
          "java-class should have a methods vector")

      (is (coll? (:interfaces clazz))
          "java-class should have an interfaces vector")

      (is (coll? (:fields clazz))
          "java-class should have a fields vector")
      
      (is (not (nil? (:this-class clazz)))
          "java-class should have a this-class member")
      (is (= 2 (count (:this-class clazz)))
          "java-class this-class should be two bytes long")

      (is (not (nil? (:super-class clazz)))
          "java-class should have a super-class")
      (is (= 2 (count (:super-class clazz)))
          "java-class super-class should be two bytes long")

      (is (not (nil? (:major-version clazz)))
          "java-class should have a format major version")

      (is (not (nil? (:minor-version clazz)))
          "java-class should have a format minor version")

      (is (= [0xCA 0xFE 0xBA 0xBE] (:magic clazz))
          "java-class should have the magic number 0xCAFEBABE")

      (is (not (nil? (:access-flags clazz)))
          "java-class should have some access flags")
      (is (= 2 (count (:access-flags clazz)))
          "java-class access flags should be two bytes long")

      (is (coll? (:attributes clazz))
          "java-class should have an attributes vector"))))


(deftest test-minimal-meaningful-class
  (testing "that the creation of a minimal class should have minimal, self-consistent fields")

  (let [clazz (java-class "Nothing")]

    (is (= "Nothing" (class-name clazz))
        "this-class member should correctly resolve to the name")

    (is (= "java/lang/Object" (super-class-name clazz))
        "super-class member should correctly resolve to Object")

    ;; TODO implement write-java-class and find out!
    ;; NEXT though, implement disassemble-bytecode and look at Nothing.<init>()
    (comment "Is this strictly necessary?"
             (is (= 1 (count (:methods clazz)))
                 "class should exactly have one method"))

    (comment (is (= "<init>" (get-name clazz (first (:methods clazz))))
                 "class' only method should be a constructor"))

    (is (empty? (:fields clazz))
        "Minimal class should have no fields")))


(deftest test-write-class-header
  (with-open [stream (java.io.ByteArrayOutputStream.)]
    (is (= stream (write-class-header stream))
        "write-class-header should return the stream it writes to"))

  (with-open [stream (java.io.ByteArrayOutputStream.)]
    (is (= (map byte-from-unsigned [0xCA 0xFE 0xBA 0xBE 0x00 0x00 0x00 0x32])
           (seq (.toByteArray (write-class-header stream))))
        "write-class-header should write a valid and correct class header")))
