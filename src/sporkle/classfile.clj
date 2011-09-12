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


;; the overall stream-to-class funtion
(defn read-java-class [bytes]

  (read-stream-maplets
   [#(unpack-struct [[:magic 4] [:minor-version 2] [:major-version 2]] %)
    read-constant-pool-maplet]
   bytes))
