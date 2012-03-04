(ns sporkle.java-class
  (:use [sporkle.core])
  (:use [sporkle.classfile]))


(defn include-constant [constant-pool constant-info]
  "Given a pool and a constant structure, return the constant pool containing the constant (adding it if necessary), and its index"
  (if-let [index (cp-find constant-pool constant-info)]
    [constant-pool index]
    [(conj constant-pool constant-info) (+ 1 (count constant-pool))]))


;; cp methods, consider consilidating after you know what shape they are
(defn cp-add-utf8 [constant-pool string]
  "Given a constant pool and a string, return a vector containing the constant pool with the Utf8 constant, and the cp index"
  (include-constant constant-pool {:tag [CONSTANT_Utf8] :bytes (seq (.getBytes string))}))


(defn cp-add-class [constant-pool string]
  
  "Given a constant pool and a string, return a vector containing the constant pool containing the string as a Utf8 constant and a Class constant whose name-index aligns with it, and the cp index of the class constant"
  
  (let [[new-cp idx] (cp-add-utf8 constant-pool string)]
    (include-constant new-cp {:tag [CONSTANT_Class] :name-index (two-byte-index idx)})))



(defn java-class

  ([class-name]
     (java-class class-name "java/lang/Object"))

  ([class-name super-class-name]

     ;; this serial redefinition of cp is definitely bad style. But what's the good style? thread macro?
     
     (let [[cp this-idx]  (cp-add-class [] class-name)
           [cp super-idx] (cp-add-class cp super-class-name)]
       
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


(defn implement-interface [java-class interface-class-name]

  (let [[cp idx] (cp-add-class (:constant-pool java-class) interface-class-name)
        interfaces (:interfaces java-class)
        idx-bytes (two-byte-index idx)]

    (if (some #(= idx-bytes %) interfaces)
      java-class

      (assoc java-class :constant-pool cp :interfaces (conj interfaces idx-bytes)))))

