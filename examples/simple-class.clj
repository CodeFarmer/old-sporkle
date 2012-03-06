(ns sporkle.test.examples.simple-class
  (:use [sporkle.class-file]))

(java-class

 "sporkle/SimpleClass"
            
 (implements [java.io.Serializable, sporkle.SimpleInterface])
 (extends    [java.lang.String])

 (static
  (final
   (field NAME String)))


 (public
   
  (let [size (interface-method-ref (interface-method-ref int java.util.List "size" []) 1 0)]
     
    (method int "countListAndHashMap" [java.util.List java.util.HashMap]
             
            [:aload_1
             :invokeinterface size 1 0
             :istore_3
             :aload_2
             :invokevirtual   (method-ref java.util.Set java.util.HashMap "keySet" [])
             ;; I wonder, could I compact this and the let-size down to Collection.size()?
             :invokeinterface (interface-method-ref int java.util.Set "size" []) 1 0
             :istore          4
             :iload_3
             :iload           4
             :iadd
             :ireturn]))))
