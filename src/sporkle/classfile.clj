(ns sporkle.classfile
  (:use [sporkle.core]))

;; ClassFile {
;; 	u4 magic;
;; 	u2 minor_version;
;; 	u2 major_version;
;; 	u2 constant_pool_count;
;; 	cp_info constant_pool[constant_pool_count-1];
;; 	u2 access_flags;
;; 	u2 this_class;
;; 	u2 super_class;
;; 	u2 interfaces_count;
;; 	u2 interfaces[interfaces_count];
;; 	u2 fields_count;
;; 	field_info fields[fields_count];
;; 	u2 methods_count;
;; 	method_info methods[methods_count];
;; 	u2 attributes_count;
;; 	attribute_info attributes[attributes_count];
;; }

;; access flags

(def ACC_PUBLIC	      0x0001) ;; Class, method: may be accessed from outside its package.
(def ACC_PRIVATE      0x0002) ;; Class, method: accessible only within the defining class.
(def ACC_PROTECTED    0x0004) ;; Method: may be accessed within subclasses. method
(def ACC_STATIC       0x0008) ;; Method

(def ACC_FINAL	      0x0010) ;; Class: no subclasses allowed. Method: may not be overridden

; beware shared flag value
(def ACC_SUPER	      0x0020) ;; Class: treat superclass methods specially when invoked by the invokespecial instruction.
(def ACC_SYNCHRONIZED 0x0020) ;; Method: invocation is wrapped in a monitor lock.

(def ACC_NATIVE       0x0100) ;; Method: native method.
(def ACC_INTERFACE    0x0200) ;; Class: is an interface, not a class.
(def ACC_ABSTRACT     0x0400) ;; Class: may not be instantiated. Method: no implementation is provided
(def ACC_STRICT       0x0800) ;; Method: floating-point mode is FP-strict


;; constant pool entries

(def CONSTANT_Class               7)
(def CONSTANT_Fieldref	          9)
(def CONSTANT_Methodref	         10)
(def CONSTANT_InterfaceMethodref 11)
(def CONSTANT_String              8)

(def CONSTANT_Integer             3)
(def CONSTANT_Float               4)
(def CONSTANT_Long                5)
(def CONSTANT_Double              6)

(def CONSTANT_NameAndType        12)
(def CONSTANT_Utf8                1)

;; conforms to the expectations of read-stream-maplets
;; FIXME document this
(defmulti read-constant-pool-entry first)

(defmethod read-constant-pool-entry CONSTANT_Utf8 [bytes]
  (let [[tag & rest] bytes]
    (let [length (bytes-to-integral-type (take 2 rest))]
      [{:tag tag :length length :bytes (take length (drop 2 rest))}
       (drop (+ length 2) rest)])))

(defmethod read-constant-pool-entry CONSTANT_Integer [bytes]
  (unpack-struct [[:tag 1] [:bytes 4]] bytes))

; NOTE these next three are the same at the moment but that will change
(defmethod read-constant-pool-entry CONSTANT_Methodref [bytes]
  (unpack-struct [[:tag 1] [:class-index 2] [:name-and-type-index 2]] bytes))
(defmethod read-constant-pool-entry CONSTANT_Fieldref [bytes]
  (unpack-struct [[:tag 1] [:class-index 2] [:name-and-type-index 2]] bytes))
(defmethod read-constant-pool-entry CONSTANT_InterfaceMethodref [bytes]
  (unpack-struct [[:tag 1] [:class-index 2] [:name-and-type-index 2]] bytes))

(defmethod read-constant-pool-entry CONSTANT_Class [bytes]
  (unpack-struct [[:tag 1] [:name-index 2]] bytes))

(defmethod read-constant-pool-entry CONSTANT_NameAndType [bytes]
  (unpack-struct [[:tag 1] [:name-index 2] [:descriptor-index 2]] bytes))

(defmethod read-constant-pool-entry :default [bytes]
  (throw (IllegalArgumentException. (str "Unhandled constant pool tag " (format "0x%02X" (first bytes))))))

;;

(defn read-constant-pool-maplet
  "Return a map containing the key :constant-pool, and a seq of constant pool entries. Conforms to the expectations of read-stream-maplets."

  ([bytes]
     ;; I have no idea why the actual count is equal to the count field minus one
     (read-constant-pool-maplet () (dec (bytes-to-integral-type (take 2 bytes))) (drop 2 bytes)))

  ([acc count bytes]
     (if (= 0 count) [{:constant-pool acc} bytes]
         (let [[entry remaining-bytes] (read-constant-pool-entry bytes)]
              (recur (cons entry acc) (dec count) remaining-bytes)))))

;; the interface list is a list of byte pairs, interpreted as indexes into the constant pool
;; FIXME this needs an individual unit test (not just in test-read-java-class)
(defn read-interface-list-maplet
  "Return a map containing the key :interfaces, and a seq of interface references. Conforms to the expectations of read-stream-maplets."

  ([bytes]
     (read-interface-list-maplet () (bytes-to-integral-type (take 2 bytes)) (drop 2 bytes)))
  
  ([acc count bytes]
     (if (= 0 count) [{:interfaces acc} bytes]
         (recur (cons (take 2 bytes) acc) (dec count) (drop 2 bytes)))))


(defn read-attribute [bytes]
  (let [name-index (take 2 bytes) count (bytes-to-integral-type (take 4 (drop 2 bytes))) remainder (drop 6 bytes)]
    [{:attribute-name-index name-index :info (take count remainder)} (drop count remainder)]))


;; IMPLEMENT ME
(defn read-attributes-maplet

  ([bytes]
     
     (let [count (bytes-to-integral-type (take 2 bytes)) remainder (drop 2 bytes)]
       (read-attributes-maplet () count remainder)))

  ([acc count bytes]
     (if (zero? count) [{:attributes acc} bytes]
         (let [[attr remainder] (read-attribute bytes)]
           (recur (cons attr acc) (dec count) remainder)))))


(defn read-field-descriptor [bytes]
  (read-stream-maplets
   [#(unpack-struct [[:access-flags 2] [:name-index 2] [:descriptor-index 2]] %)
    read-attributes-maplet]
   bytes))


;; IMPLEMENT ME
(defn read-field-list-maplet

  ([bytes]
     (let [count (bytes-to-integral-type (take 2 bytes)) remainder (drop 2 bytes)]
       (read-field-list-maplet () count remainder)))

  ([acc count bytes]
     (if (zero? count) [{:fields acc} bytes]
         (let [[field remainder] (read-field-descriptor bytes)]
           (recur (cons field acc) (dec count) remainder)))))

;; the overall stream-to-class function
(defn read-java-class [bytes]

  (first
   (read-stream-maplets
    [#(unpack-struct [[:magic 4] [:minor-version 2] [:major-version 2]] %)
     read-constant-pool-maplet
     #(unpack-struct [[:access-flags 2] [:this-class 2] [:super-class 2]] %)
     read-interface-list-maplet
     read-field-list-maplet]
    bytes)))
