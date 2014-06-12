(ns sporkle.test.friendly
  (:use [sporkle.friendly])
  (:use [sporkle.classfile])
  (:use [clojure.test]))

(deftest test-friendly-code

  (testing "simple code with fixed operands"
    (let [code-bytes [42 183 0 1 177]]

      (is (= [:aload_0
              :invokespecial 0 1
              :return] ; this is the empty constructor from my local javac

             (friendly-code code-bytes)))))

  (testing "longer code"
    (let [code-bytes [43 185 0 14 1 0 62 44 182 0 15 185 0 16 1 0 54 4 29 21 4 96 172]]

      (is (= [:aload_1
              :invokeinterface 0 14 1 0
              :istore_3
              :aload_2
              :invokevirtual 0 15
              :invokeinterface 0 16 1 0
              :istore 4
              :iload_3
              :iload 4
              :iadd
              :ireturn] ; ClassWithAllConstantPoolType.countListAndHashMap()
             
             (friendly-code code-bytes))))))

(deftest test-friendly-attr
  (let [java-class (read-java-class-file "test/fixtures/Nothing.class")
        constant-pool (:constant-pool java-class)
        code-attrib (attribute-named constant-pool (first (:methods java-class)) "Code")
        friendly-code-attrib (friendly-attr constant-pool code-attrib)]

    (testing "Given an attribute we know has a name, when we call test-friendly-attr then it should format it with the correct name"
      (is (= "Code" (:name friendly-code-attrib)) ))

    (testing "Overall attribute layout" 
        
      (is (= '{:name "Code",
               :attributes ({:name "LineNumberTable",
                             :info (0 1 0 0 0 1)}),
               :exception-table [],
               :code [:aload_0 :invokespecial 0 1 :return],
               :max-stack (0 1),
               :max-locals (0 1)}

             friendly-code-attrib)))))
