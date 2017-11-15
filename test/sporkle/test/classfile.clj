(ns sporkle.test.classfile
  (:require [sporkle.core
             :refer [byte-from-unsigned bytes-to-long byte-stream-seq]])
  (:require [sporkle.classfile :refer :all])
  (:require [sporkle.constant-pool
             :refer [constant-value cp-entry-value cp-find-utf8 tag CONSTANT_Double CONSTANT_Fieldref CONSTANT_Float CONSTANT_InterfaceMethodref CONSTANT_Long CONSTANT_MethodHandle CONSTANT_Methodref CONSTANT_String CONSTANT_Utf8]])
  (:require  [clojure.test :refer [deftest is testing]])
  (:require [clojure.java.io :as io]))

;; this file now tests a bunch of constant-pool stuff, maybe split?

(deftest test-read-constant-pool-entry

  (testing "Reading from a known UTF8 constant from a classfile, with some trailing bytes"

    (let [[entry rest](read-constant-pool-entry
                        [0x01 0x00 0x0C 0x4E  0x6F 0x74 0x68 0x69
                         0x6E 0x67 0x2E 0x6A  0x61 0x76 0x61 0x0C
                         ;; 0x61 is actually the last byte ^ of the entry
                         00 04])]
                          
      (is (= [0x0C 0x00 0x04] rest)
          "should correctly return the remaining bytes")
      (is (= CONSTANT_Utf8 (tag entry))
          "should correctly set the tag")
      (is (= 12 (count (:bytes entry)))
          "should read the two-byte length correctly")))
  
  (testing "Reading an integer constant, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x03 0x00 0x10 0x01 0x01 0xFF])]
      
      (is (= [0xFF] rest)
          "should correctly return the remaining bytes")
      (is (= 0x00100101 (cp-entry-value nil entry))
          "should the two bytes into an integer")))

  (testing "reading a method ref entry, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x0A 0x00 0x03 0x00 0x0A 0x01])]
      (is (= CONSTANT_Methodref (tag entry))
          "should record the correct tag")
      (is (= [0x00 0x03] (:class-index entry))
          "should record the class index bytes")
      (is (= [0x00 0x0A] (:name-and-type-index entry))
          "should record the name and type index")
      (is (= [0x01] rest)
          "should leave the remainder of the stream correctly"))

  (testing "reading a field ref entry, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x09 0x00 0x03 0x00 0x0A])]
      (is (= CONSTANT_Fieldref (tag entry))
          "should record the correct tag")
      (is (= [0x00 0x03] (:class-index entry))
          "should record the class index bytes")
      (is (= [0x00 0x0A] (:name-and-type-index entry))
          "should record the name and type index bytes"))

    (testing "reading an interface method ref entry, with a trailing byte"

      (let [[entry rest] (read-constant-pool-entry [0x0B 0x00 0x03 0x00 0x0A])]
        (is (= CONSTANT_InterfaceMethodref (tag entry))
            "should record the correct tag")
        (is (= [0x00 0x03] (:class-index entry))
            "should record the class index bytes")
        (is (= [0x00 0x0A] (:name-and-type-index entry))
            "should record the name and type index bytes"))))


  (testing "reading a string entry with trailing bytes"
    (let [[entry rest] (read-constant-pool-entry [0x08 0x00 0x05 0x10 0x00])]
      (is (= CONSTANT_String (tag entry))
          "should record the correct tag")
      (is (= [0x00 0x05] (:string-index entry))
          "should record the string-index bytes")
      (is (= [0x10 0x00] rest)
          "should leave the correct remainder")))

  (testing "Reading a float constant, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x04 0x40 0x20 0x00 0x00 0xFF])]

      (is (= CONSTANT_Float (tag entry))
          "should read the correct tag")
      (is (= [0x40 0x20 0x00 0x00] (:bytes entry))
          "should have four bytes")
      (is (= [0xFF] rest)
          "should correctly return the remaining byte")
      (is (= 2.5 (cp-entry-value nil entry))
          "should be able to read the bytes into a float")))

  (testing "Reading a long constant, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x05 0x00 0x00 0x00 0x00 0x40 0x20 0x00 0x00 0xFF])]

      (is (= CONSTANT_Long (tag entry))
          "should read the correct tag")
      (is (= [0x00 0x00 0x00 0x00] (:high-bytes entry))
          "should have four high bytes")
      (is (= [0x40 0x20 0x00 0x00] (:low-bytes entry))
          "should have four low bytes")
      (is (= [0xFF] rest)
          "should correctly return the remaining byte")))

  (testing "Reading a double constant, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x06 0x00 0x00 0x00 0x00 0x40 0x20 0x00 0x00 0xFF])]

      (is (= CONSTANT_Double (tag entry))
          "should read the correct tag")
      (is (= [0x00 0x00 0x00 0x00] (:high-bytes entry))
          "should have four high bytes")
      (is (= [0x40 0x20 0x00 0x00] (:low-bytes entry))
          "should have four low bytes")
      (is (= [0xFF] rest)
          "should correctly return the remaining byte")))
  
  (testing "reading from a constant with an unknown tag value"
    
    (is (thrown? IllegalArgumentException (read-constant-pool-entry [0x0D 0xFF 0xFF]))))))


(deftest test-read-constant-pool-maplet

  (testing "reading the constant pool bytes from a small class"

    (let [[constant-pool-maplet remainder] (read-constant-pool-maplet (drop 8 (byte-stream-seq (io/input-stream "test/fixtures/Nothing.class"))))]
      (let [pool (:constant-pool constant-pool-maplet)]
        (is (= 12 (count pool))
            "should read the right number of constants")
        (is (every? #(contains? % :tag) pool)
            "should return a seq of objects with tag fields")
        (is (= 0x0A (tag (first pool)))
            "should have the constant pool objects in the right order"))))

  (testing "reading the constant pool from a class with wide constants"

    (let [[constant-pool-maplet remainder] (read-constant-pool-maplet (drop 8 (byte-stream-seq (io/input-stream "test/fixtures/LongFieldStaticInit.class"))))
          constant-pool (:constant-pool constant-pool-maplet)]

      (is (= 18 (count constant-pool))
          "The constant pool has only 18 constants (no obsolete spacer)"))))

(deftest test-read-attribute

  (testing "reading a generic attribute from a byte stream with trailing bytes"

    (let [[attr remainder] (read-attribute nil [0x00 0x01 0x00 0x00 0x00 0x03 0x0A 0x0B 0x0C 0x0D])]
      (is (= [0x00 01] (:attribute-name-index attr))
          "first two bytes should be the atribute name index")
      (is (= [0x0A 0x0B 0x0C] (:info attr))
          "info should contain the right three bytes")
      (is (= [0x0D] remainder)
          "should correctly leave the remaining byte"))))


(deftest test-read-attributes-maplet

  (testing "reading an empty attribute list"
    
    (is (= [{:attributes []} [0x01 0x02]]
           (read-attributes-maplet nil [0x00 0x00 0x01 0x02]))
        "should return the byte stream, minus the first two zero bytes that display the count"))
  
  (testing "reading a simple attribute list, two attributes long, with some trailing bytes")

  ;; nil cp to read-attributes-maplet, we don't care about their actual contents
  (let [[attributes-maplet remainder] (read-attributes-maplet nil [0x00 0x02 0x00 0x01 0x00 0x00 0x00 0x03 0x0A 0x0B 0x0C 0x00 0x01 0x00 0x00 0x00 0x01 0x0A 0x0B 0x0C])]
    
    (let [attr-list (:attributes attributes-maplet)]
      (is (= 2 (count attr-list))
          "should return two attributes in the list"))

    (is (= [0x0B 0x0C] remainder)
        "should leave the correct remainder")))


(deftest test-read-field-or-method-info

  (testing "reading a known field info with 4 trailing bytes"

    ;; note nil cp arg to f-o-m-info, no attributes
    (let [[field remainder] (read-field-or-method-info nil [0x00 0x01 0x00 0x04 0x00 0x05 0x00 0x00 0x00 0x01 0x00 0x06])]

      (is (= [] (:attributes field))
          "should have no attributes")
      (is (= [0x00 0x01] (:access-flags field))
          "should select the correct access flag bytes")
      (is (= [0x00 0x04] (:name-index field))
          "should select the correct name index bytes")
      (is (= [0x00 0x05] (:descriptor-index field))
          "should select the correct descriptor-index bytes")
      (is (= [0x00 0x01 0x00 0x06] remainder)
          "Should return the correct remainder bytes")))
  
  (testing "reading a known method info with 4 trailing bytes"

    ;; nil leads to incorrect Code attribute unpacking, but we just care that it's there today
    (let [[method remainder] (read-field-or-method-info nil [0x00 0x01 0x00 0x04 0x00 0x05 0x00 0x01 0x00 0x06 0x00 0x00 0x00 0x1d 0x00 0x01 0x00 0x01 0x00 0x00 0x00 0x05 0x2a 0xb7 0x00 0x01 0xb1 0x00 0x00 0x00 0x01 0x00 0x07 0x00 0x00 0x00 0x06 0x00 0x01 0x00 0x00 0x00 0x01 0x00 0x01 0x00 0x08])]
      
      (is (= [0x00 0x01] (:access-flags method))
          "should select the correct access flag bytes")
      (is (= [0x00 0x04] (:name-index method))
          "should select the correct name index bytes")
      (is (= [0x00 0x05] (:descriptor-index method))
          "should select the correct descriptor-index bytes")
      (is (= 1 (count (:attributes method)))
          "should have one (Code) attribute")
      (is (= [0x00 0x01 0x00 0x08] remainder)
          "should return the correct remainder bytes"))))


(deftest test-read-java-class

  (testing "reading a simple class"

    (let [java-class (read-java-class-file "test/fixtures/Nothing.class")]

      (is (= 0xCAFEBABE (bytes-to-long (:magic java-class)))
          "gotta get the magic number right")
      (is (= 0x0000 (bytes-to-long (:minor-version java-class)))
          "minor version number of the class file")
      (is (= 0x0032 (bytes-to-long (:major-version java-class)))
          "major version number of the class file")
      (is (= 12 (count (:constant-pool java-class)))
          "should read the correct number of constant pool entries")
      
      (is (= ACC_SUPER (access-flags java-class))
          "class have no modifiers (except the fearsome ACC_SUPER)")
      
      (is (= 2 (count (:this-class java-class)))
          "should have two bytes for its this-class constant reference")
      (is (= 2 (count (:super-class java-class)))
          "should have two bytes for its superclass constant reference")
      
      ;; and the interfaces...
      (is (= 0 (count (:interfaces java-class)))
          "should have no interfaces, because it's so simple")
      (is (not (nil? (:interfaces java-class)))
          "should still have something in the interfaces field though")
      (is (= 0 (count (:fields java-class)))
          "should have no fields")
      (is (not (nil? (:fields java-class)))
          "should still have something in the fields field though")
      (is (= 1 (count (:methods java-class)))
          "should have one method (<init>")
      (is (not (nil? (:methods java-class)))
          "should still have something in the method fields")

      (is (= "Nothing" (class-name java-class))
          "should be able to retrieve the class name")))

  
  (testing "reading a Java class with some marker interfaces"

    (let [java-class (read-java-class-file "test/fixtures/SerializableNothing.class")]
      (= 2 (count (:interfaces java-class))
         "The class should have two entries in its interface list")))

  (testing "reading a Java class with some fields"
    
    (let [java-class (read-java-class-file "test/fixtures/FieldNothing.class")]
      (is  (= 2 (count (:fields java-class)))
           "The class should have two entries in its field list")))

  (testing "reading a Java class with a method"

    (let [java-class (read-java-class-file "test/fixtures/MethodNothing.class")]
      (is (= 2 (count (:methods java-class)))
          "the class should return one method plus constructor")))

  (testing "reading a Java class with attributes"

    (let [java-class (read-java-class-file "test/fixtures/Nothing.class")]
      (is  (= 1 (count (:attributes java-class)))
           "The class should have one attribute (SourceFile)")
      (is (not (empty? (:info (first (:attributes java-class)))))
          "The first attribute should have info (ie., not be truncated as in issue #7"))))

;; information about fields or methods or... anything with a :name-index
(deftest test-get-name
  (let [java-class (read-java-class-file "test/fixtures/Nothing.class")]
    (is (= "<init>" (get-name (:constant-pool java-class) (first (:methods java-class)))))))

;; this test is for various little bits of get-whatever functionality for the annoyingly differently-structured bits of the classfile struct
(deftest test-getters
  (let [java-class (read-java-class-file "test/fixtures/Nothing.class")
        constant-pool (:constant-pool java-class)]
    (is (= "<init>" (get-name constant-pool (first (:methods java-class)))))
    (is (= "()V" (descriptor constant-pool (first (:methods java-class)))))

    (is (= "SourceFile" (attribute-name constant-pool (first (:attributes java-class))))
        "attribute-name should return 'SourceFile' for a classfile known to have it as a first attrib")))


(deftest test-read-code-attribute

  (testing "Unpacking the code attribute of an empty <init> method in an empty class"
    
    (let [java-class (read-java-class-file "test/fixtures/Nothing.class")
          constant-pool (:constant-pool java-class)
          code-attrib (attribute-named constant-pool (first (:methods java-class)) "Code")]

      (is (not (nil? code-attrib))
          "Code attribute should not be nil")

      (is (= (bytes-to-long (:attribute-name-index code-attrib))
             (cp-find-utf8 constant-pool "Code"))
          "Code attribute should have name 'Code'")

      (is (= [0x00 0x01] (:max-stack code-attrib))
          "Code attribute should now have a max-stack field")
      (is (= [0x00 0x01] (:max-locals code-attrib))
          "Code attribute should now have a max-locals field")

      (is (coll? (:code code-attrib))
          "Code attribute should now have a code bytes field")
      (is (= 5 (count (:code code-attrib)))
          "Code field should be 5 bytes long")

      (is (coll? (:exception-table code-attrib))
          "Code attribute should have an exception handler table")
      (is (empty? (:exception-table code-attrib))
          "Basic class doesn't have any exception handlers")

      (is (coll? (:attributes code-attrib))
          "Code attribute should itself have attributes")
      (is (not (nil? (attribute-named constant-pool code-attrib "LineNumberTable")))
          "The code attribute from the example code happens to have a line number table"))))

;; TODO: write a test for line number tables, local variables, exceptions and so on.


;; This is going to end up quite a large test, but only as g-c-v gets implemented (once I figure out what it should really return).
(deftest test-constant-value 
  
  (let [clazz (read-java-class-file "test/fixtures/ClassWithAllConstantPoolTypes.class")
        constant-pool (:constant-pool clazz)]
    
    (testing "utf-8 constant"
      (is (= "java/util/Set" (constant-value constant-pool 57))
          "The 57th constant is the canonical name of java.util.Set, and should be returned as a String"))

    (comment "TODO and aaaaalll the rest of them, just need to figure out what shape they should be")))

(deftest test-java-8-class-reading 
  
  (let [clazz (read-java-class-file "test/fixtures/Java8Lambda.class")
        constant-pool (:constant-pool clazz)]
    (testing "loading the new constant pool struct types"
      (is (some #(= CONSTANT_MethodHandle (first (:tag %))) constant-pool)))
    ))


;; class writing

(deftest test-write-class-header
  (with-open [stream (java.io.ByteArrayOutputStream.)]
    (is (= stream (write-class-header stream))
        "write-class-header should return the stream it writes to"))

  (with-open [stream (java.io.ByteArrayOutputStream.)]
    (is (= (map byte-from-unsigned [0xCA 0xFE 0xBA 0xBE 0x00 0x00 0x00 0x32])
           (seq (.toByteArray (write-class-header stream))))
        "write-class-header should write a valid and correct class header")))



(deftest test-constant-pool-entry-bytes

  (testing "Utf8 constant"
    (is (= [1 0 6 60 105 110 105 116 62] (constant-pool-entry-bytes {:tag [1] :bytes [60 105 110 105 116 62]}))
        "should convert utf8 into tag byte, two-byte index, then content"))

  (testing "integer constant"
    ;; consider making assertions about incoming field lengths?
    (is (= [3 24 109 0 1] (constant-pool-entry-bytes {:tag [3] :bytes [24 109 0 1]}))
        "should convert integer simply as a tag and the four bytes"))

  (testing "method-ref constant"
    ;; field-ref and interface-method-ref use the same code
    (is (= [10 0 3 0 10] (constant-pool-entry-bytes {:name-and-type-index [0 10], :class-index [0 3], :tag [10]}))))
  (testing "field-ref constant"
    (is (= [9 0 3 0 10] (constant-pool-entry-bytes {:name-and-type-index [0 10], :class-index [0 3], :tag [9]}))))
  (testing "interface-method-ref constant"
    (is (= [11 0 3 0 10] (constant-pool-entry-bytes {:name-and-type-index [0 10], :class-index [0 3], :tag [11]}))))

  (testing "class constant"
    (is (= [7 0 11] (constant-pool-entry-bytes {:name-index [0 11] :tag [7]}))))

  (testing "name-and-type constant"
    (is (= [12 0 4 0 5] (constant-pool-entry-bytes {:descriptor-index [0 5], :name-index [0 4], :tag [12]}))
        "name-and-type constant should be ordered tag, name-index, descriptior-index"))
  
  (testing "string constant"
    (is (= [8 0 16] (constant-pool-entry-bytes {:string-index [0 16] :tag [8]}))
        "string constant should be ordered tag, string-index"))
  
  (testing "float constant"
    (is (= [4 64 32 0 0] (constant-pool-entry-bytes {:bytes [64 32 0 0] :tag [4]}))
        "float constant should be ordered tag, bytes"))

  (testing "long constant"
    (is (= [5 00 00 34 23] (constant-pool-entry-bytes {:high-bytes [00 00] :low-bytes [34 23] :tag [5]}))
        "long constant is tag, two high bytes, two low bytes"))

  (testing "double constant"
    (is (= [6 04 00 34 23] (constant-pool-entry-bytes {:high-bytes [04 00] :low-bytes [34 23] :tag [6]}))
        "double constant is tag, two high bytes, two low bytes")))




