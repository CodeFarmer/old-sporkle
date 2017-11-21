## Sporkle: Pure-Clojure Java classfile assembler and disassembler

Written as a compare-and-contrast programming exercise, loosely based on
Pork (http://github.com/CodeFarmer/pork).

Has three main parts:

1. *Classfile read/write* - parsing JVM class file streams into structs that correspond to the format laid out in the [JVM spec](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html), and writing those structs back into byte streams.

2. Class struct creation with a sorta-DSL - creating classfile structs (above) by describing in a friendly form the fields and attributes of the class, as well as a pseudo-bytecode JCM assembler for describing methods.

3. Friendly view of classfile structs - the reverse process of 2. above, creating a human-readable view onto the classfile struct that should eventually be the same DSL, so we can run the view through a reader and get Java classes.

## Current State

This works at the moment:

```clojure
(jc-with-method
  (jc-with-empty-constructor
    (public (java-class "Nothing"))
    ACC_PUBLIC)
  ACC_PUBLIC
  "doNothing"
  "()V"
  0
  1 ;; this
  [:return])
```

Ugly eh? This is the plan:

```clojure
(public
  (java-class "Nothing"
    (private
      (field "x" java.lang.Object)
    (public
      (method :void "doNothing" [] [:return]))
      (method java.lang.Object "getX" []
        [:aload_0 
         :getfield 0 2
         :areturn]))))
```

The shortiss term goal is feature parity with Pork, so support for
exception tables, smart arguments for JVM bytecode instructions, line
number tables and so on will be next once the syntax is nice.

## License

Copyright (C) 2011 Joel Gluth

Distributed under the Eclipse Public License, the same as Clojure.

