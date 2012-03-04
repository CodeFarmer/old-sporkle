(ns sporkle.test.examples.simple-class
  (:use [sporkle.class-file]))

(java-class "sporkle/SimpleClass"
            
  :implements [java.io.Serializable, sporkle.SimpleInterface]
  :extends    [java.lang.String]

  (public
   (field x Integer)
   (field y String))

  (public
   (static
    (final
     (field NAME String))))

  (public
   (method "countListAndHashMap" [java.util.List java.util.HashMap]
           [:aload_1
            :invokeinterface (method-ref java.util.List "size" "()I")
            :istore_3
            :aload_2
            :invokevirtual 0 15
            :invokeinterface 0 16 1 0
            :istore 4
            :iload_3
            :iload 4
            :iadd
            :ireturn])))