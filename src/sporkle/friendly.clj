(ns sporkle.friendly 
  (:use sporkle.core)
  (:use sporkle.bytecode)
  (:use sporkle.classfile)
  (:require [clojure.java.io :as io]))

;; dissassembly

;; TODO rewrite using threading macros, this is awful
(def friendly-attr)

(defn friendly-code
  ([code-data]
     "Given the raw bytes of a code attribute, return them as a vector of :opcode keywords with the correct number of integer arguments per opcode.

(friendly-code (:code (unpack-code-attribute (attribute-named (:constant-pool clazz) (first (:methods clazz)) \"Code\"))))"

     (friendly-code [] code-data))

  ([acc code-data]

     (if (empty? code-data)
       acc
       (let [opcode-byte (first code-data)
             remainder   (rest code-data)
             [opcode-name _ argwidth _] (get bytes-to-opcodes opcode-byte)]
         
         (recur (into (conj acc opcode-name) (take argwidth remainder)) (drop argwidth remainder))))))

(defn -attr-with-attrs [const-pool attr]
  (if-let [attr-attrs (:attributes attr)]
    (assoc attr :attributes (map (partial friendly-attr const-pool) attr-attrs))
    attr))

(defn -attr-with-code [const-pool attr]
  (if-let [code-block (:code attr)]
    (assoc attr :code (friendly-code code-block))
    attr))

(defn -attr-with-name [const-pool attr]
  (if (:attribute-name-index attr)
    (assoc (dissoc attr :attribute-name-index) :name (attribute-name const-pool attr))
    attr))


(defn friendly-attr [const-pool attr]
  "Given an unpacked attribute object, output the form describing the attribute with its name string."
  (->> attr
       (-attr-with-name const-pool)
       (-attr-with-attrs const-pool)
       (-attr-with-code const-pool)))


