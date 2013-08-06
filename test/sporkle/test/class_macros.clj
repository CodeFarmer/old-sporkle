(ns sporkle.test.class-macros
  
  (:use [clojure.test])
  (:use [sporkle.java-class])
  (:use [sporkle.test.support]))

(comment (deftest test-simple-class
           
           (testing "A simple example of the build-class macro"
             
             (let [clazz
                   (build-class Foo
                                
                                (static
                                 (final
                                  (field
                                   MESSAGE java.lang.String "Hello, World.")))
                                
                                (public
                                 (static
                                  (method
                                   main [(array java.lang.String)]
                                   ;; (178 0 2
                                   ;;  18 3
                                   ;;  182 0 4
                                   ;;  177)
                                   [:getstatic     System/out
                                    :ldc           "Hello, world." ;; compiler seems to optimise this to not load the field
                                    :invokevirtual java.io.PrintStream/println
                                    :return]))))]))))
