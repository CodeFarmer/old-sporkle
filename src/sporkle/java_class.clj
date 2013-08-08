(ns sporkle.java-class
  (:use [sporkle.core])
  (:use [sporkle.classfile])
  (:use [sporkle.bytecode]))


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


(defn cp-with-name-and-type [constant-pool name descriptor]

  (let [[new-cp name-index]       (cp-with-utf8 constant-pool name)
        [new-cp descriptor-index] (cp-with-utf8 new-cp descriptor)]

    (cp-with-constant new-cp {:tag [CONSTANT_NameAndType] :name-index (two-byte-index name-index) :descriptor-index (two-byte-index descriptor-index)})))


(defn cp-with-method [constant-pool class-name method-name descriptor]

  (let [[new-cp class-index]   (cp-with-class constant-pool class-name)
        [new-cp name-and-type-index] (cp-with-name-and-type new-cp method-name descriptor)]

    (cp-with-constant new-cp {:tag [CONSTANT_Methodref] :class-index (two-byte-index class-index) :name-and-type-index (two-byte-index name-and-type-index)})))


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

;; Return an updated constant pool, plus opcodes converted into a byte vector according to that constant pool

(defn opcodes-to-code-bytes

  ([constant-pool opcodes]

      (opcodes-to-code-bytes constant-pool opcodes []))

  ([constant-pool opcodes acc]

     (if (empty? opcodes)
       [constant-pool acc]
       (let [sym (first opcodes)
             op-info (get syms-to-opcodes sym)]

         (if (not (keyword? sym))

           (throw (IllegalArgumentException. (str "Expected bytecode operation key, got '" sym "'")))
           
           (if (nil? op-info)
             
             (throw (IllegalArgumentException. (str "Unable to find opcode info for '" sym "'")))
             
             (let [[name code argwidth stack-delta] op-info]
               
               (if (= 0 argwidth)
                 (recur constant-pool (drop 1 opcodes) (conj acc code))
                 (throw (IllegalArgumentException. "Opcodes that take arguments are not supported yet"))))))))))


;; FIXME add exception tables
(defn cp-with-code-attribute [cp max-stack max-locals opcodes]

  "Return a suitably updated constant pool, plus a Code Attribute (see JVM spec section 4.7.3) corresponding to the opcodes (translated into a byte array). Presently returns no exception table or further attributes."

  (let [[cp name-index] (cp-with-utf8 cp "Code")
        [cp code-bytes] (opcodes-to-code-bytes cp opcodes)
        code-count (count code-bytes)]

    [cp
     {:attribute-name-index   (int-to-byte-pair name-index)
      ;; FIXME this assumes no exception table etc
      :attribute-length       (four-byte-count (+ code-count 12))
      :max-stack              (int-to-byte-pair max-stack)
      :max-locals             (int-to-byte-pair max-locals)
      :code-length            (four-byte-count code-count)
      :code                   code-bytes
      :exception-table-length [0x00 0x00]
      :exception-table        []
      :attributes-count       [0x00 0x00]
      :attributes             []}]))


;; FIXME share more code with field-desc
(defn jc-with-method [java-class
                      access-flags
                      method-desc
                      method-name
                      max-stack
                      max-locals
                      opcodes]
  (let [cp (:constant-pool java-class)
        methods (:methods java-class)
        [cp name-index] (cp-with-utf8 cp method-name)
        [cp descriptor-index] (cp-with-utf8 cp method-desc)
        [cp code-attribute] (cp-with-code-attribute cp max-stack max-locals opcodes)
        method-descriptor {:access-flags     (int-to-byte-pair access-flags)
                           :name-index       (int-to-byte-pair name-index)
                           :descriptor-index (int-to-byte-pair descriptor-index)
                           ;; NOTE this is incomplete
                           :attributes [code-attribute]}]

    (assoc java-class
      :constant-pool cp
      :methods (if (some #(= % method-descriptor) methods)
                methods
                (conj methods method-descriptor)))))


(defn jc-with-empty-constructor [java-class access-flags]

  (jc-with-method java-class access-flags "()V" "<init>" 0 1
    [:aload_0
     :invokespecial "$superInit"
     :return]) )