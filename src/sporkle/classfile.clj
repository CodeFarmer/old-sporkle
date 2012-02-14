(ns sporkle.classfile  
  (:use sporkle.core)
  (:require [clojure.string :as str]))

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

(def ^:const ACC_PUBLIC	      0x0001) ;; Class, method: may be accessed from outside its package.
(def ^:const ACC_PRIVATE      0x0002) ;; Class, method: accessible only within the defining class.
(def ^:const ACC_PROTECTED    0x0004) ;; Method: may be accessed within subclasses. method
(def ^:const ACC_STATIC       0x0008) ;; Method

(def ^:const ACC_FINAL	      0x0010) ;; Class: no subclasses allowed. Method: may not be overridden

; beware shared flag value
(def ^:const ACC_SUPER	      0x0020) ;; Class: treat superclass methods specially when invoked by the invokespecial instruction.
(def ^:const ACC_SYNCHRONIZED 0x0020) ;; Method: invocation is wrapped in a monitor lock.

(def ^:const ACC_NATIVE       0x0100) ;; Method: native method.
(def ^:const ACC_INTERFACE    0x0200) ;; Class: is an interface, not a class.
(def ^:const ACC_ABSTRACT     0x0400) ;; Class: may not be instantiated. Method: no implementation is provided
(def ^:const ACC_STRICT       0x0800) ;; Method: floating-point mode is FP-strict


;; constant pool entries

(def ^:const CONSTANT_Class               7)
(def ^:const CONSTANT_Fieldref	          9)
(def ^:const CONSTANT_Methodref	         10)
(def ^:const CONSTANT_InterfaceMethodref 11)
(def ^:const CONSTANT_String              8)

(def ^:const CONSTANT_Integer             3)
(def ^:const CONSTANT_Float               4)
(def ^:const CONSTANT_Long                5)
(def ^:const CONSTANT_Double              6)

(def ^:const CONSTANT_NameAndType        12)
(def ^:const CONSTANT_Utf8                1)

;; conforms to the expectations of read-stream-maplets
;; FIXME document this
(defmulti read-constant-pool-entry first)

(defmethod read-constant-pool-entry CONSTANT_Utf8 [bytes]
  (let [[tag & rest] bytes]
    (let [length (bytes-to-integral-type (take 2 rest))]
      [{:tag [tag] :bytes (take length (drop 2 rest))}
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
  (throw (IllegalArgumentException. (str "Unable to read in constant pool entry with tag " (format "0x%02X" (first bytes))))))

;; getting constant values usefully

(defn read-attribute [bytes]
  (let [name-index (bytes-to-integral-type (take 2 bytes)) count (bytes-to-integral-type (take 4 (drop 2 bytes))) remainder (drop 6 bytes)]
    [{:attribute-name-index name-index :info (take count remainder)} (drop count remainder)]))


(defn read-struct-list-maplet

  ([key readfn bytes]

     (let [count (bytes-to-integral-type (take 2 bytes)) remainder (drop 2 bytes)]
       (read-struct-list-maplet [] count key readfn remainder)))

  ([acc count key readfn bytes]
     (if (zero? count) [{key acc} bytes]
         (let [[descriptor remainder] (readfn bytes)]
           (recur (conj acc descriptor) (dec count) key readfn remainder)))))


;; this one comes up a few times
(defn read-attributes-maplet [bytes]
  (read-struct-list-maplet :attributes read-attribute bytes))


;; the constant pool, annoyingly, has a different way of expressing its length than every other struct list
(defn read-constant-pool-maplet [bytes]
  (let [count (dec (bytes-to-integral-type (take 2 bytes))) remainder (drop 2 bytes)]
    (read-struct-list-maplet [] count :constant-pool read-constant-pool-entry remainder)))


(defn read-field-or-method-info [bytes]
  (read-stream-maplets
   [#(unpack-struct [[:access-flags 2] [:name-index 2] [:descriptor-index 2]] %)
    read-attributes-maplet]
   bytes))


;; an interface index is just a byte pair
(defn read-byte-pair [bytes]
  [(take 2 bytes) (drop 2 bytes)])


;; for quick-hack debugging
(defn -print-some-stream-bytes [bytes]
  (println (map #(format "0x%02x" %) (take 256 bytes)))
  [{} bytes])

;; the overall stream-to-class function
(defn read-java-class [bytes]

  (first
   (read-stream-maplets
    [#(unpack-struct [[:magic 4 :unsigned] [:minor-version 2 :unsigned] [:major-version 2 :unsigned]] %)
     read-constant-pool-maplet
     #(unpack-struct [[:access-flags 2] [:this-class 2] [:super-class 2]] %)
     #(read-struct-list-maplet :interfaces read-byte-pair %)
     #(read-struct-list-maplet :fields     read-field-or-method-info %)
     #(read-struct-list-maplet :methods    read-field-or-method-info %)
     read-attributes-maplet]
    bytes)))

;; and now some classfile byte extraction goodness

(defn tag [unpacked-struct]
  (bytes-to-integral-type (:tag unpacked-struct)))

(defn access-flags [unpacked-struct]
  (bytes-to-integral-type (:access-flags unpacked-struct)))

;; pass the constant pool, as some things evaluate to indices into it which should be followed
(defmulti constant-value #(tag %2))

;; it would be nice to be able to this,
;; for the case when you know you won't need to follow references - that is, primitives and
;; CONSTANT_Utf8s
;; (defn constant-value [pool-entry]
;;  (constant-value pool-entry nil))

(defmethod constant-value CONSTANT_Utf8 [java-class pool-entry]
  (str/join (map char (:bytes pool-entry))))

(defmethod constant-value CONSTANT_Integer [java-class pool-entry]
  (bytes-to-integral-type (:bytes pool-entry)))

(defmethod constant-value :default [java-class pool-entry]
  (throw (IllegalArgumentException. (str "Unable to interpret constant pool entry with tag " (format "0x%02X" (:tag pool-entry))))))

;; this is necessary for two reasons
;; 1) The indices are 1-based!
;; 2) Some constant pool entries count for two spaces!
;;
;; therefore, this implementation WILL change - it is WRONG right now
(defn get-constant [java-class index]
  (nth (:constant-pool java-class) (dec index)))

;; for something with a name-index, get its name
(defn get-name [java-class thing]
  "For anything that has a name-index in its struct, return the string represented in the class by that name-index. If no name-index, return nil.

NOTE not called 'name' like the others of its ilk in order not to clash"

  (if (nil? (:name-index thing))
    nil
    (constant-value java-class (get-constant java-class (bytes-to-integral-type (:name-index thing))))))

;; FIXME everything below needs a test

(defn descriptor [java-class meth]
  "Return the descriptor string for a method (or anything else with a descriptor-index"
  (constant-value java-class (get-constant java-class (bytes-to-integral-type (:descriptor-index meth)))))

(defn class-name [java-class]
  (let [this-class (:this-class java-class)]
    (if (nil? this-class)
      nil
      (get-name java-class (get-constant java-class (bytes-to-integral-type (:this-class java-class)))))))