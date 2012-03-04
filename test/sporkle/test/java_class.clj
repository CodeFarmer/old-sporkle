(ns sporkle.test.java-class
  
  (:use [clojure.test])
  (:use [sporkle.core])
  (:use [sporkle.java-class])
  (:use [sporkle.classfile])
  (:use [sporkle.test.support])
  (:require [clojure.java.io :as io])
  (:import  [java.io ByteArrayOutputStream])
  (:import  [sporkle.test ByteLoader]))


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

    (is (empty? (:interfaces clazz)))
    (is (empty? (:methods clazz)))
    (is (empty? (:fields clazz))
        "Minimal class should have no fields")))


(deftest test-implement-interface
  (let [clazz (implement-interface (java-class "Nothing") "java/io/Serializable")
        constant-pool (:constant-pool clazz)]
    (is (= 1 (count (:interfaces clazz)))
        "class should now have one interface")
    (is (= "java/io/Serializable" (get-name constant-pool (get-constant constant-pool (bytes-to-integral-type (first (:interfaces clazz))))))
        "first interface should be an index that points to something in the constant pool with the right name")))


(deftest test-write-class-header
  (with-open [stream (ByteArrayOutputStream.)]
    (is (= stream (write-class-header stream))
        "write-class-header should return the stream it writes to"))

  (with-open [stream (ByteArrayOutputStream.)]
    (is (= (map byte-from-unsigned [0xCA 0xFE 0xBA 0xBE 0x00 0x00 0x00 0x32])
           (seq (.toByteArray (write-class-header stream))))
        "write-class-header should write a valid and correct class header")))

(deftest test-write-java-class
  
  (with-open [stream (ByteArrayOutputStream.)]
    (is (= stream (write-java-class stream (java-class "Nothing")))
        "write-java-class should return the stream it writes to"))

  (comment (testing "reading back what we've written out"

              (with-open [stream (ByteArrayOutputStream.)]

                (let [class-map-src  (java-class "Nothing")
                      bytes          (.toByteArray (write-java-class stream class-map-src))
                      class-map-dest (read-java-class bytes)]
                  (is (= class-map-src class-map-dest) "Should be able to unpack the same class from bytes we've put in"))))))


(deftest test-write-simplest-complete-class
  (testing "writing of a complete, mostly-empty classfile that can be loaded by the JVM"
    (with-open [stream (ByteArrayOutputStream.)]
      
      (let [bytes (.toByteArray (write-java-class stream (java-class "Nothing")))
            clazz (.loadBytes (ByteLoader.) bytes)]
        
        (is (class? clazz)
            "written bytes should be loadable into a Java class")
        (is (not (.isAssignableFrom java.io.Serializable clazz))
            "minimal class should not be Serializable")))))


(deftest test-write-class-with-interface
  (testing "writing of a complete, mostly-empty classfile that can be loaded by the JVM"
    (with-open [stream (ByteArrayOutputStream.)]
      
      (let [bytes (.toByteArray (write-java-class stream (implement-interface (java-class "Nothing") "java/io/Serializable")))
            clazz (.loadBytes (ByteLoader.) bytes)]
        (is (.isAssignableFrom java.io.Serializable clazz)
            "class should be Serializable, since we declared the interface")))))
