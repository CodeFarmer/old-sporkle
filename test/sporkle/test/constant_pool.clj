(ns sporkle.test.constant_pool
  (:use [sporkle.classfile])
  (:use sporkle.constant-pool)
  (:use [clojure.test]))

(deftest test-constant-pool-value

  (testing "Long constant pool value"
    
    (let [clazz (read-java-class-file "test/fixtures/LongFieldStaticInit.class")
          cp (:constant-pool clazz)
          long-entry (nth cp 1)]

      (is (= 0x02 (cp-entry-value cp long-entry))))
    
    (let [long-entry {:low-bytes [0 0 0 2], :high-bytes [0 3 0 0], :tag [5]}]
      (is (= 0x0003000000000002 (cp-entry-value [] long-entry))))))

