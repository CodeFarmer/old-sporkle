(ns sporkle.test.constant_pool
  (:require [sporkle.classfile :refer [read-java-class-file]])
  (:require [sporkle.constant-pool :refer :all])
  (:require [clojure.test :refer [deftest is testing]]))

(deftest test-constant-pool-value

  (testing "Long constant pool value"
    
    (let [clazz (read-java-class-file "test/fixtures/LongFieldStaticInit.class")
          cp (:constant-pool clazz)
          long-entry (nth cp 1)]

      (is (= 0x02 (cp-entry-value cp long-entry))))
    
    (let [long-entry {:low-bytes [0 0 0 2], :high-bytes [0 3 0 0], :tag 5}]
      (is (= 0x0003000000000002 (cp-entry-value [] long-entry)))))

  (testing "Double constant pool value"
    
    (let [clazz (read-java-class-file "test/fixtures/DoubleFieldStaticInit.class")
          cp (:constant-pool clazz)
          double-entry (nth cp 1)]

      (is (= (Double. "2.19") (cp-entry-value cp double-entry))))
    
    (let [long-entry {:low-bytes [0 0 0 2], :high-bytes [0 3 0 0], :tag 5}]
      (is (= 0x0003000000000002 (cp-entry-value [] long-entry)))))

  (testing "Float constant pool value"
    
    (let [clazz (read-java-class-file "test/fixtures/FloatFieldStaticInit.class")
          cp (:constant-pool clazz)
          float-entry (nth cp 1)]

      (is (= (Float. "2.19") (cp-entry-value cp float-entry)))
      (is (= (Float. "2.19") (:bytes float-entry))))))


(deftest test-cp-nth
  (testing "Simple case of a constant pool with only single-width entries")

  (testing "Complicated case of a constant pool containing a Long constant"

    (let [clazz (read-java-class-file "test/fixtures/LongFieldStaticInit.class")
          cp (:constant-pool clazz)
          long-entry (cp-nth cp 2)
          ;; there is no 3rd because long-entry is doublewide
          field-entry (cp-nth cp 4)]

      (is (= CONSTANT_Long (:tag long-entry)))
      (is (= CONSTANT_Fieldref (:tag field-entry))))))
