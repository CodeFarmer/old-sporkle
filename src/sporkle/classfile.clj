(ns sporkle.classfile  
  (:use sporkle.core)
  (:use sporkle.bytecode)
  (:require [clojure.java.io :as io])
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


;; some classfile byte extraction goodness

(defn tag [unpacked-struct]
  (let [t (:tag unpacked-struct)]
    (if (keyword? (first t))
      (first t)
      (bytes-to-integral-type (:tag unpacked-struct)))))

(defn access-flags [unpacked-struct]
  (bytes-to-integral-type (:access-flags unpacked-struct)))


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

(defmethod read-constant-pool-entry CONSTANT_String [bytes]
  (unpack-struct [[:tag 1] [:string-index 2]] bytes))

(defmethod read-constant-pool-entry CONSTANT_Float [bytes]
  (unpack-struct [[:tag 1] [:bytes 4]] bytes))

(defmethod read-constant-pool-entry CONSTANT_Long [bytes]
  (unpack-struct [[:tag 1] [:high-bytes 4] [:low-bytes 4]] bytes))
(defmethod read-constant-pool-entry CONSTANT_Double [bytes]
  (unpack-struct [[:tag 1] [:high-bytes 4] [:low-bytes 4]] bytes))

;; This is almost certainly fucked and wrong
(comment (defmethod read-constant-pool-entry 0 [bytes]
           (unpack-struct [[:tag 1] [:bytes 2]] bytes)))

(defmethod read-constant-pool-entry :default [bytes]
  (let [tag (first bytes)]
    (if (nil? tag)
      (throw (IllegalArgumentException. (str "Unable to read in constant pool entry with nil tag")))
      (throw (IllegalArgumentException. (str "Unable to read in constant pool entry with tag " (format "0x%02X" (first bytes))))))))

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


(defn read-cp-entry-list-maplet
  "This should be read-struct-list-maplet, except that for long and double constants you need to bump the index twice. No, me either."
  ;; "In retrospect, making 8-byte constants take two constant pool entries was a poor choice." :P
  [acc count key readfn bytes]
  (if (zero? count) [{key acc} bytes]
      (let [[descriptor remainder] (readfn bytes)]
        (if (#{CONSTANT_Double CONSTANT_Long} (tag descriptor))
          ;; "The constant_pool table is indexed from 1 to constant_pool_count-1."
          (recur (conj (conj acc descriptor) {:tag [:spacer]}) (- count 2) key readfn remainder)
          (recur (conj acc descriptor) (dec count) key readfn remainder)))))

;; the constant pool, annoyingly, has a different way of expressing its length than every other struct list
(defn read-constant-pool-maplet [bytes]
  (let [count (dec (bytes-to-integral-type (take 2 bytes))) remainder (drop 2 bytes)]
    (read-cp-entry-list-maplet [] count :constant-pool read-constant-pool-entry remainder)))


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

(defn read-java-class-file [filename]
  "Convenience method; read a java-class map from a named file"
  (with-open [stream (io/input-stream filename)]
    (read-java-class (byte-stream-seq stream))))


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

(defmethod constant-value CONSTANT_Float [java-class pool-entry]
  (Float/intBitsToFloat (bytes-to-integral-type (:bytes pool-entry))))

;; FIXME this borked before Float was implemented, why?
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


;; dissassembly
(defn friendly-code
  ([code-data]
     "Given the raw bytes of a code attribute, return them as a vector of :opcode keywords with the correct number of integer arguments per opcode.

(friendly-code (:code (unpack-code-attribute (attribute-named clazz (first (:methods clazz)) \"Code\"))))"

     (friendly-code [] code-data))

  ([acc code-data]

     (if (empty? code-data)
       acc
       (let [opcode-byte (first code-data)
             remainder   (rest code-data)
             [opcode-name _ argwidth _] (get bytes-to-opcodes opcode-byte)]
         
         (recur (into (conj acc opcode-name) (take argwidth remainder)) (drop argwidth remainder))))))




;; FIXME everything below needs a test
;; FIXME everything below needs a test
;; FIXME everything below needs a test
;; FIXME everything below needs a test


(defn read-code-maplet [bytes]
  (let [count (bytes-to-integral-type (take 4 bytes))
        info (take count (drop 4 bytes))]
    [{:code info} (drop (+ 4 count) bytes)]))


(defn descriptor [java-class meth]
  "Return the descriptor string for a method (or anything else with a descriptor-index"
  (constant-value java-class (get-constant java-class (bytes-to-integral-type (:descriptor-index meth)))))


;; consider making this internal
(defn indexed-name [java-class index-bytes]
  "Given a java-class and a two-byte index, find the constant pointed to by the index (for example a Class or a NameAndType), and resolve its name-index attribute to a string"
  (get-name java-class (get-constant java-class (bytes-to-integral-type index-bytes))))


(defn class-name [java-class]
  (let [this-class (:this-class java-class)]
    (if (nil? this-class)
      nil
      (indexed-name java-class this-class))))

;; FIXME REFACTOR this is the same as class-name
(defn super-class-name [java-class]
  (let [super-class (:super-class java-class)]
    (if (nil? super-class)
      nil
      (indexed-name java-class super-class))))


(defn interface-names [java-class]
  "Return an array of strings that are the qualified classnames of a class' implemented interfaces"
  (map (partial indexed-name java-class) (:interfaces java-class)))


(defn cp-find [constant-pool value-map]
  "Find the cp-index into constant pool where a constant with particular contents can be found (remember, cp-indices start at one and have other potentially annoying behaviour)."
  
  (loop [indexed-cp (each-with-index constant-pool)]
    (if (empty? indexed-cp)
      nil
      (let [[c i] (first indexed-cp)]
        (if (= c value-map)
          (inc i)
          (recur (rest indexed-cp)))))))


(defn cp-find-utf8 [constant-pool string]
  "Shortcut to cp-find for UTF-8 strings, which is a very common case"
  (cp-find constant-pool {:tag [CONSTANT_Utf8] :bytes (seq (.getBytes string))}))


(defn attribute-named [java-class thing name]
  "Retrieve the :attributes member of thing whose attribute-name-index corresponds to the location of 'name' in the constant pool of java-class"
  (when-let [idx (cp-find-utf8 (:constant-pool java-class) name)]
    (loop [attribs (seq (:attributes thing))]
      (if (empty? attribs)
        nil
        (let [attrib (first attribs)]
          (if (= idx (:attribute-name-index attrib))
            attrib
            (recur (rest attribs))))))))


(defn read-exception-table-maplet [bytes]
  (read-struct-list-maplet :exception-table #(unpack-struct [[:start-pc 2] [:end-pc 2] [:handler-pc 2] [:catch-type 2]] %) bytes))


(defn unpack-code-attribute [attribute]
  "Unpack a code-attribute structure from the info field of a more general attribute (you know you want to do this because you used (get-attribute-named \"Code\"))"

  ;; TODO implement unpacking of LineNumberTable and LocalVariableTable sub-attributes?
  (into attribute
    (first
     (read-stream-maplets
      [#(unpack-struct [[:max-stack 2] [:max-locals 2]] %)
       read-code-maplet
       read-exception-table-maplet
       read-attributes-maplet]
       (:info attribute)))))


;; writing classfiles

;; has test
(defn write-class-header [stream]
  (write-bytes stream (map byte-from-unsigned MAGIC_BYTES))
  (write-bytes stream (map byte-from-unsigned MINOR_VERSION_BYTES))
  (write-bytes stream (map byte-from-unsigned MAJOR_VERSION_BYTES)))

;; pool-writey stuff all needs tests
(defn write-pool [stream writefn pool]
  (write-bytes (two-byte-index (count pool)))
  (doseq [p pool]
    (writefn stream p)))


(defmulti constant-pool-entry-bytes tag)

(defmethod constant-pool-entry-bytes CONSTANT_Utf8 [cp-entry]
  ;; not convinced this is a great idea
  (flatten [(:tag cp-entry) (two-byte-index (count (:bytes cp-entry))) (:bytes cp-entry)]))

(defmethod constant-pool-entry-bytes CONSTANT_Integer [cp-entry]
  ;; take 4 ensures you don't over/underrun in case of mangled fields, is this actually useful?
  (into (:tag cp-entry) (take 4 (:bytes cp-entry))))
(defmethod constant-pool-entry-bytes CONSTANT_Float [cp-entry]
  (into (:tag cp-entry) (take 4 (:bytes cp-entry))))

(defn ref-bytes [cp-entry]
  ;; these are all already starting to look very repetitive, and they also duplicate information
  ;; in read-constant-pool-entry - FIXME
  (flatten [(:tag cp-entry) (take 2 (:class-index cp-entry)) (take 2 (:name-and-type-index cp-entry))]))

(defmethod constant-pool-entry-bytes CONSTANT_Methodref [cp-entry]
  (ref-bytes cp-entry))
(defmethod constant-pool-entry-bytes CONSTANT_Fieldref [cp-entry]
  (ref-bytes cp-entry))
(defmethod constant-pool-entry-bytes CONSTANT_InterfaceMethodref [cp-entry]
  (ref-bytes cp-entry))

(defmethod constant-pool-entry-bytes CONSTANT_Class [cp-entry]
  (into (:tag cp-entry) (:name-index cp-entry)))

(defmethod constant-pool-entry-bytes CONSTANT_NameAndType [cp-entry]
  (flatten [(:tag cp-entry) (take 2 (:name-index cp-entry)) (take 2 (:descriptor-index cp-entry))]))

(defmethod constant-pool-entry-bytes CONSTANT_String [cp-entry]
  (into (:tag cp-entry) (:string-index cp-entry)))

(defmethod constant-pool-entry-bytes CONSTANT_Long [cp-entry]
  (flatten [(:tag cp-entry) (:high-bytes cp-entry) (:low-bytes cp-entry)]))
(defmethod constant-pool-entry-bytes CONSTANT_Double [cp-entry]
  (flatten [(:tag cp-entry) (:high-bytes cp-entry) (:low-bytes cp-entry)]))

(defmethod constant-pool-entry-bytes :spacer [cp-entry]
  [])
  
(defmethod constant-pool-entry-bytes :default [cp-entry]
  (throw (IllegalArgumentException. (str "Unable to make flat bytes for pool entry with tag " (format "0x%02X" (tag cp-entry))))))

; unit test?
(defn write-constant-pool-entry [stream entry]
  (write-bytes stream (constant-pool-entry-bytes entry)))

(defn write-interface-list [stream])
(defn write-field-list [stream])
(defn write-method-list [stream])
(defn write-attribute-list [stream])

(defn write-java-class [stream java-class]
  
  (write-class-header   stream)
  (write-pool           stream write-constant-pool-entry (:constant-pool java-class))
  (write-bytes          stream (:access-flags java-class))
  (write-bytes          stream (:this-class   java-class))
  (write-bytes          stream (:super-class  java-class))
  
  (write-interface-list stream (:interfaces   java-class))
  (write-field-list     stream (:fields       java-class))
  (write-method-list    stream (:methods      java-class))
  (write-attribute-list stream (:attributes   java-class)))
