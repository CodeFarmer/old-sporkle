(ns sporkle.java-class
  (:require [sporkle.core
             :refer [bytes-to-long int-to-byte-pair two-byte-index MAGIC_BYTES MAJOR_VERSION MINOR_VERSION]])
  (:require [sporkle.classfile
             ;; FIXME: does cp-with-code-attribute go in constant-pool?
             :refer [cp-with-code-attribute ACC_PUBLIC ACC_SUPER]])
  (:require [sporkle.constant-pool
             :refer [cp-with-class cp-with-utf8]]))


(defn public [thing]

  ;; FIXME I am beginning to think having all the byte pairs and so on live in the classfile struct was a bad idea. Maybe just encode and decode at read and write time?
  (assoc thing :access-flags (bit-or (get thing :access-flags 0) ACC_PUBLIC)))


(defn java-class

  ([class-name]
     (java-class class-name "java/lang/Object"))

  ([class-name super-class-name]

     ;; this serial redefinition of cp is definitely bad style. But what's the good style? thread macro?
     
     (let [[cp this-idx]  (cp-with-class [] class-name)
           [cp super-idx] (cp-with-class cp super-class-name)]
       
       {:magic         MAGIC_BYTES
        :major-version MINOR_VERSION
        :minor-version MAJOR_VERSION
        :access-flags  ACC_SUPER
        :constant-pool cp
        :methods       []
        :interfaces    []
        :fields        []
        :attributes    []
        :this-class    this-idx
        :super-class   super-idx})))


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
        field-descriptor {:access-flags     access-flags
                          :name-index       (int-to-byte-pair name-index)
                          :descriptor-index (int-to-byte-pair descriptor-index)
                          :attributes []}]
    (assoc java-class
      :constant-pool cp
      :fields (if (some #(= % field-descriptor) fields)
                fields
                (conj fields field-descriptor)))))



;; FIXME share more code with field-desc
(defn jc-with-method [java-class
                      access-flags
                      method-name
                      method-desc
                      max-stack
                      max-locals
                      opcodes]
  (let [cp (:constant-pool java-class)
        methods (:methods java-class)
        [cp name-index] (cp-with-utf8 cp method-name)
        [cp descriptor-index] (cp-with-utf8 cp method-desc)
        [cp code-attribute] (cp-with-code-attribute cp max-stack max-locals opcodes)
        method-descriptor {:access-flags     access-flags
                           :name-index       name-index
                           :descriptor-index descriptor-index
                           ;; NOTE this is incomplete
                           :attributes [code-attribute]}]

    (assoc java-class
      :constant-pool cp
      :methods (if (some #(= % method-descriptor) methods)
                methods
                (conj methods method-descriptor)))))


;; FIXME this defaults to Object, rather than the superclass if it can be found
(defn jc-with-empty-constructor [java-class access-flags]

  (jc-with-method java-class access-flags "<init>" "()V" 1 1
    [:aload_0
     :invokespecial "java/lang/Object" "<init>" "()V"
     :return]))
