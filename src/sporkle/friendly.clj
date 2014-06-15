(ns sporkle.friendly 
  (:use sporkle.core)
  (:use sporkle.bytecode)
  (:use sporkle.classfile)
  (:use sporkle.constant-pool)
  (:require [clojure.java.io :as io]))

;; 
(defn -constant-pool-tag-constants []
  "Get the constant pool symbol names, and their associated symbols."
  (filter #(.startsWith (str (first %)) "CONSTANT") (ns-publics 'sporkle.constant-pool)))

(defn -get-symbol-for-constant-tag [tag-number]
  "Yeah, it's a kludge. Locates the constant symbol representing a particular tag name, if it exists"
  (first
   ;; this first-filter thing *surely* has an actual function for it.
   (first (filter #(= tag-number (var-get (second %))) (-constant-pool-tag-constants))))) 

;; dissassembly

(defn friendly-cp-entry [constant-pool entry]
  (list 'cp-entry (-get-symbol-for-constant-tag (first (:tag entry))) (cp-entry-value constant-pool entry)))

(def friendly-attr)

;; TODO rewrite using threading macros, this is awful
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


