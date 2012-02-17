(ns sporkle.test.classfile
  (:use [sporkle.core])
  (:use [sporkle.classfile])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))


(deftest test-read-constant-pool-entry

  (testing "Reading from a known UTF8 constant from a classfile, with some trailing bytes"

    (let [[entry rest] (read-constant-pool-entry
                        [0x01 0x00 0x0C 0x4E  0x6F 0x74 0x68 0x69
                         0x6E 0x67 0x2E 0x6A  0x61 0x76 0x61 0x0C
                         ;; 0x61 is actually the last byte ^ of the entry
                         00 04])]
                          
      (is (= [0x0C 0x00 0x04] rest)
          "should correctly return the remaining bytes")
      (is (= CONSTANT_Utf8 (tag entry))
          "should correctly set the tag")
      (is (= 12 (count (:bytes entry)))
          "should read the two-byte length correctly")
      (is (= "Nothing.java" (constant-value nil entry))
          "should give us back bytes that can be turned into a Java String")))
  
  (testing "Reading an integer constant, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x03 0x00 0x10 0x01 0x01 0xFF])]
      
      (is (= [0xFF] rest)
          "should correctly return the remaining bytes")
      (is (= 0x00100101 (constant-value nil entry))
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
      (is (= [0x10 0x00])
          "should leave the correct remainder")))

  (testing "Reading a float constant, with a trailing byte"

    (let [[entry rest] (read-constant-pool-entry [0x04 0x40 0x20 0x00 0x00 0xFF])]

      (is (= CONSTANT_Float (tag entry))
          "should read the correct tag")
      (is (= [0x40 0x20 0x00 0x00] (:bytes entry))
          "should have four bytes")
      (is (= [0xFF] rest)
          "should correctly return the remaining bytes")
      (is (= 2.5 (constant-value nil entry))
          "should be able to read the bytes into a float")))

  ;; TODO
  (comment (testing "reading a long constant"))

  ;; TODO
  (comment (testing "reading a double constant"))
  
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

      (is (= 19 (count constant-pool))
          "The constant pool has only 18 constants, but there should be a spacer for the float")
      (is (= [:spacer] (:tag (nth constant-pool 2)))
          "The float spacer should be at cp-index 3"))))

(deftest test-read-attribute

  (testing "reading a generic attribute from a byte stream with trailing bytes"

    (let [[attr remainder] (read-attribute [0x00 0x01 0x00 0x00 0x00 0x03 0x0A 0x0B 0x0C 0x0D])]
      (is (= 0x0001 (:attribute-name-index attr))
          "first two bytes should be the atribute name index")
      (is (= [0x0A 0x0B 0x0C] (:info attr))
          "info should contain the right three bytes")
      (is (= [0x0D] remainder)
          "should correctly leave the remaining byte"))))


(deftest test-read-attributes-maplet

  (testing "reading an empty attribute list"
    
    (is (= [{:attributes []} [0x01 0x02]
            (read-attributes-maplet [0x00 0x00 0x01 0x02])])
        "should return the byte stream, minus the first two zero bytes that display the count"))
  
  (testing "reading a simple attribute list, two attributes long, with some trailing bytes")

  (let [[attributes-maplet remainder] (read-attributes-maplet [0x00 0x02 0x00 0x01 0x00 0x00 0x00 0x03 0x0A 0x0B 0x0C 0x00 0x01 0x00 0x00 0x00 0x01 0x0A 0x0B 0x0C])]
    
    (let [attr-list (:attributes attributes-maplet)]
      (is (= 2 (count attr-list))
          "should return two attributes in the list"))

    (is (= [0x0B 0x0C] remainder)
        "should leave the correct remainder")))


(deftest test-read-field-or-method-info

  (testing "reading a known field info with 4 trailing bytes"

    (let [[field remainder] (read-field-or-method-info [0x00 0x01 0x00 0x04 0x00 0x05 0x00 0x00 0x00 0x01 0x00 0x06])]

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
    
    (let [[method remainder] (read-field-or-method-info [0x00 0x01 0x00 0x04 0x00 0x05 0x00 0x01 0x00 0x06 0x00 0x00 0x00 0x1d 0x00 0x01 0x00 0x01 0x00 0x00 0x00 0x05 0x2a 0xb7 0x00 0x01 0xb1 0x00 0x00 0x00 0x01 0x00 0x07 0x00 0x00 0x00 0x06 0x00 0x01 0x00 0x00 0x00 0x01 0x00 0x01 0x00 0x08])]
      
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

      (is (= 0xCAFEBABE (bytes-to-unsigned-integral-type (:magic java-class)))
          "gotta get the magic number right")
      (is (= 0x0000 (bytes-to-integral-type (:minor-version java-class)))
          "minor version number of the class file")
      (is (= 0x0032 (bytes-to-integral-type (:major-version java-class)))
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
          "the class should return one method"))))

;; information about fields or methods or... anything with a :name-index
(deftest test-get-name
  (let [java-class (read-java-class-file "test/fixtures/Nothing.class")]
    (is (= "<init>" (get-name java-class (first (:methods java-class)))))))

(deftest test-getters
  (let [java-class (read-java-class-file "test/fixtures/Nothing.class")]
    (is (= "<init>" (get-name java-class (first (:methods java-class)))))
    (is (= "()V" (descriptor java-class (first (:methods java-class)))))))


(deftest test-unpack-code-attribute

  (testing "Unpacking the code attribute of an empty <init> method in an empty class"
    
    (let [java-class (read-java-class-file "test/fixtures/Nothing.class")
          code-attrib (unpack-code-attribute (attribute-named java-class (first (:methods java-class)) "Code"))]

      (is (not (nil? code-attrib))
          "Code attribute should not be nil")

      (is (= (:attribute-name-index code-attrib)
             (cp-find-utf8 (:constant-pool java-class) "Code"))
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
      (is (not (nil? (attribute-named java-class code-attrib "LineNumberTable")))
          "The code attribute from the example code happens to have a line number table"))))

;; TODO: write a test for line number tables, local variables, exceptions and so on.


(deftest test-friendly-code

  (testing "simple code with fixed operands"
    (let [code-bytes [42 183 0 1 177]]

      (is (= [:aload_0
              :invokespecial 0 1
              :return] ; this is the empty constructor from my local javac

             (friendly-code code-bytes))))))

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

  (comment (testing "long constant"))
  (comment (testing "double constant")))



(comment (deftest test-write-class
           (testing "Reading a class produced by javac and writing it back as a class that can then be loaded by the classloader")))


