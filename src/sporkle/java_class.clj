(ns sporkle.java-class
  (:use [sporkle.core]))

(defn java-class [class-name]
  {:magic         [0xCA 0xFE 0xBA 0xBE]
   :constant-pool []
   :methods       []
   :this-class    [0x00 0x01]})


(defn cp-find [constant-pool value-map]
  "Find the cp-index into constant pool where a constant with particular contents can be found"
  
  (let [indexed-cp (each-with-index constant-pool)]

    ))

;; cp methods, consider consilidating after you know what shape they are
(defn cp-add-utf8 [constant-pool string]
  "Given a constant pool and a string, return a vector containing the constant pool with the Utf8 constant, and the cp index"
  [constant-pool (count constant-pool)])