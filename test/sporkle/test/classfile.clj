(ns sporkle.test.classfile
  (:use [sporkle.core])
  (:use [sporkle.classfile])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))

;; (deftest read-constant-pool-entry)

(deftest test-read-constant-pool-entry

  (testing "Reading from a known UTF8 constant from a classfile, with some trailing bytes"

    (let [[entry rest] (read-constant-pool-entry
                        [0x01 0x00 0x0C 0x4E  0x6F 0x74 0x68 0x69
                         0x6E 0x67 0x2E 0x6A  0x61 0x76 0x61 0x0C
                         ;; 0x61 is actually the last byte ^ of the entry
                         00 04])]
                          
      (is (= [0x0C 0x00 0x04] rest)
          "should correctly return the remaining bytes")
      (is (= CONSTANT_Utf8 (:tag entry))
          "should correctly set the tag")
      (is (= 12 (:length entry))
          "should read the two-byte length correctly")
      (is (= [0x4E 0x6F 0x74 0x68 0x69 0x6E 0x67 0x2E 0x6A 0x61 0x76 0x61] (:bytes entry))
          "should read the correct bytes")
      (is (= "Nothing.java" (apply str (map char (:bytes entry))))
          "should give us back bytes that can be turned into a Java String")))
  
  (testing "Reading an integer constant, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x03 0x00 0x10 0x01 0x01 0xFF])]
      
      (is (= [0xFF] rest)                          "should correctly return the remaining bytes")
      (is (= [0x00 0x10 0x01 0x01] (:bytes entry)) "should read only the two integer bytes")))

  (testing "reading a method ref entry, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x0A 0x00 0x03 0x00 0x0A])]
    (is (= CONSTANT_Methodref (:tag entry)) "should record the correct tag")
    (is (= [0x00 0x03] (:class-index entry)) "should record the class index bytes"))

  (testing "reading a field ref entry, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x09 0x00 0x03 0x00 0x0A])]
      (is (= CONSTANT_Fieldref (:tag entry)) "should record the correct tag")
      (is (= [0x00 0x03] (:class-index entry)) "should record the class index bytes")
      (is (= [0x00 0x0A] (:name-and-type-index entry "should record the name and type index bytes"))))

    (testing "reading an interface method ref entry, with a trailing byte"

      (let [[entry rest] (read-constant-pool-entry [0x0B 0x00 0x03 0x00 0x0A])]
        (is (= CONSTANT_InterfaceMethodref (:tag entry)) "should record the correct tag")
        (is (= [0x00 0x03] (:class-index entry)) "should record the class index bytes")
        (is (= [0x00 0x0A] (:name-and-type-index entry "should record the name and type index bytes"))))))


  (testing "Reading from a constant with an unknown tag value"
    
    (is (thrown? IllegalArgumentException (read-constant-pool-entry [0x0D 0xFF 0xFF]))))))


(deftest test-read-constant-pool-maplet

  (testing "reading the constant pool bytes from a small class"

    (let [[constant-pool-maplet remainder] (read-constant-pool-maplet (drop 8 (byte-stream-seq (io/input-stream "test/fixtures/Nothing.class"))))]
      (let [pool (:constant-pool constant-pool-maplet)]
        (is (= 12 (count pool)) "should read the right number of constants")
        (is (every? #(contains? % :tag) pool) "should return a seq of objects with tag fields")
        (is (= 0x0A (:tag (first pool))) "should have the constant pool objects in the right order"))
      ;; IMPLEMENT ME
      )))


(comment (deftest test-read-attribute

    (testing "reading a generic attribute from a byte stream with trailing bytes"

      (let [[attr remainder] (read-attribute [0x00 0x01 0x00 0x00 0x00 0x03 0x0A 0x0B 0x0C 0x0D])]
        (is (= [0x00 0x01] (:attribute-name-index attr)) "first two bytes should be the atribute name index")
        (is (= [0x0A 0x0B 0x0C] (:info attr)) "info should contain the right three bytes")
        (is (= [0x0D] remainder) "should correctly leave the remaining byte")))))


(deftest test-read-attributes-maplet

  (testing "reading an empty attribute list"
    
    (is (= [{:attributes []} [0x01 0x02] (read-attributes-maplet [0x00 0x00 0x01 0x02])]) "should return the byte stream, minus the first two zero bytes that display the count"))
  
  (testing "reading a simple attribute list, two attributes long, with some trailing bytes")

  (let [[attributes-maplet remainder] (read-attributes-maplet [0x00 0x02 0x00 0x01 0x00 0x00 0x00 0x03 0x0A 0x0B 0x0C 0x00 0x01 0x00 0x00 0x00 0x01 0x0A 0x0B 0x0C])]
    
    (let [attr-list (:attributes attributes-maplet)]
      (is (= 2 (count attr-list)) "should return two attributes in the list"))

    (is (= [0x0B 0x0C] remainder) "should leave the correct remainder")))

(deftest test-read-java-class

  (testing "reading a simple class"

    (let [java-class (read-java-class (byte-stream-seq (io/input-stream "test/fixtures/Nothing.class")))]

      (is (= [0xCA 0xFE 0xBA 0xBE] (:magic java-class)) "gotta get the magic number right")
      (is (= [0x00 0x00] (:minor-version java-class))   "minor version number of the class file")
      (is (= [0x00 0x32] (:major-version java-class))   "major version number of the class file")
      (is (= 12 (count (:constant-pool java-class)))    "should read the correct number of constant pool entries")
      ;; some more plain fields
      (let [access-flags (bytes-to-integral-type (:access-flags java-class))]
        (is (= (bit-or ACC_PUBLIC ACC_SUPER) access-flags) "class should be public with no other modifiers (except the fearsome ACC_SUPER)"))
      (is (= 2 (count (:this-class java-class))) "should have two bytes for its this-class constant reference")
      (is (= 2 (count (:super-class java-class))) "should have two bytes for its superclass constant reference")
      ;; and the interfaces...
      (is (= 0 (count (:interfaces java-class))) "should have no interfaces, because it's so simple")
      (is (not (nil? (:interfaces java-class))) "should still have something in the interfaces field though")
      (is (= 0 (count (:fields java-class))) "should have no fields")
      (is (not (nil? (:fields java-class))) "should still have something in the fields field though")
      (is (= 1 (count (:methods java-class))) "should have one method (<init>")
      (is (not (nil? (:methods java-class))) "should still have something in the method fields")
      ))
  
  (testing "reading a Java class with some marker interfaces"

    (let [java-class (read-java-class (byte-stream-seq (io/input-stream "test/fixtures/SerializableNothing.class")))]
      (= 2 (count (:interfaces java-class)) "The class should have two entries in its interface list")))

  (testing "reading a Java class with some fields"
    
    (let [java-class (read-java-class (byte-stream-seq (io/input-stream "test/fixtures/FieldNothing.class")))]
      (is  (= 2 (count (:fields java-class))) "The class should have two entries in its field list"))
    
    )

  (testing "reading a Java class with a method"

    (let [java-class (read-java-class (byte-stream-seq (io/input-stream "test/fixtures/MethodNothing.class")))]

      (is (= 2 (count (:methods java-class))) "the class should return one method"))))


