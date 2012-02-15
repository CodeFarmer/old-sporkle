(ns sporkle.java-class
  (:use [sporkle.core])
  (:use [sporkle.classfile]))


(defn two-byte-index [i]
  [(bit-and 0xFF00 i) (bit-and 0x00FF i)])


(defn cp-find [constant-pool value-map]
  "Find the cp-index into constant pool where a constant with particular contents can be found (remember, cp-indices start at one and have other potentially annoying behaviour)."
  
  (loop [indexed-cp (each-with-index constant-pool)]
    (if (empty? indexed-cp)
      nil
      (let [[c i] (first indexed-cp)]
        (if (= c value-map)
          (inc i)
          (recur (rest indexed-cp)))))))


(defn include-constant [constant-pool constant-info]
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


(defn java-class [class-name]

  ;; this serial redefinition of cp is definitely bad style. But what's the good style?
  
  (let [[cp this-idx]  (cp-add-class [] class-name)
        [cp super-idx] (cp-add-class cp "java/lang/Object")]
    
    {:magic         [0xCA 0xFE 0xBA 0xBE]
     :major-version [0x00 0x32]
     :minor-version [0x00 0x00]
     :access-flags  (two-byte-index ACC_SUPER)
     :constant-pool cp
     :methods       []
     :interfaces    []
     :fields        []
     :attributes    []
     :this-class    (two-byte-index this-idx)
     :super-class   (two-byte-index super-idx)}))
