(ns sporkle.java-class
  (:use [sporkle.core])
  (:use [sporkle.classfile]))


(defn cp-with-constant [constant-pool constant-info]
  "Given a pool and a constant structure, return the constant pool containing the constant (adding it if necessary), and its index"
  (if-let [index (cp-find constant-pool constant-info)]
    [constant-pool index]
    [(conj constant-pool constant-info) (+ 1 (count constant-pool))]))

;; constant pool methods, consider consolidating after you know what shape they are
(defn cp-with-utf8 [constant-pool string]
  "Given a constant pool and a string, return a vector containing the constant pool with the Utf8 constant, and the cp index"
  (cp-with-constant constant-pool {:tag [CONSTANT_Utf8] :bytes (seq (.getBytes string))}))


(defn cp-with-class [constant-pool string]
  
  "Given a constant pool and a string, return a vector containing the constant pool containing the string as a Utf8 constant and a Class constant whose name-index aligns with it, and the cp index of the class constant"
  
  (let [[new-cp idx] (cp-with-utf8 constant-pool string)]
    (cp-with-constant new-cp {:tag [CONSTANT_Class] :name-index (two-byte-index idx)})))

(defn java-class

  ([class-name]
     (java-class class-name "java/lang/Object"))

  ([class-name super-class-name]

     ;; this serial redefinition of cp is definitely bad style. But what's the good style? thread macro?
     
     (let [[cp this-idx]  (cp-with-class [] class-name)
           [cp super-idx] (cp-with-class cp super-class-name)]
       
       {:magic         MAGIC_BYTES
        :major-version MINOR_VERSION_BYTES
        :minor-version MAJOR_VERSION_BYTES
        :access-flags  (two-byte-index ACC_SUPER)
        :constant-pool cp
        :methods       []
        :interfaces    []
        :fields        []
        :attributes    []
        :this-class    (two-byte-index this-idx)
        :super-class   (two-byte-index super-idx)})))


(defn jc-implementing-interface [java-class interface-class-name]

  (let [[cp idx] (cp-with-class (:constant-pool java-class) interface-class-name)
        interfaces (:interfaces java-class)
        idx-bytes (two-byte-index idx)]

    (if (some #(= idx-bytes %) interfaces)
      java-class

      (assoc java-class :constant-pool cp :interfaces (conj interfaces idx-bytes)))))


(defn jc-with-field [java-class access-flags type-spec field-name]
  (let [cp (:constant-pool java-class)
        fields (:fields java-class)
        [cp name-index] (cp-with-utf8 cp field-name)
        [cp descriptor-index] (cp-with-utf8 cp type-spec)
        field-descriptor {:access-flags     (int-to-byte-pair access-flags)
                          :name-index       (int-to-byte-pair name-index)
                          :descriptor-index (int-to-byte-pair descriptor-index)
                          :attributes []}]
    (assoc java-class
      :constant-pool cp
      :fields (if (some #(= % field-descriptor) fields)
                fields
                (conj fields field-descriptor)))))
