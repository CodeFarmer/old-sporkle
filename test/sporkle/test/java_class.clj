(ns sporkle.test.java-class
  (:use [clojure.test])
  (:use [sporkle.java-class])
  (:use [sporkle.classfile]))

(deftest test-minimal-class
  (testing "The creation of an empty class, with default everything"

    (let [clazz (java-class "Nothing")]
      (is (map? clazz)
          "java-class should return a Map")
      (is (vector? (:constant-pool clazz))
          "java-class should have a vector constant pool")
      (is (not (nil? (:this-class clazz)))
          "java-class should have a this-class member")
      (is (= "Nothing" (class-name clazz))
          "this-class member should resolve to a name"))))

