(ns sporkle.bytecode
  (:require [sporkle.constant-pool :refer [cp-with-method]])
  (:require [sporkle.core :refer [two-byte-index]]))


;; bc-args-fns

;; FIXME implement the ability to take a method (Java or, even better, Clojre syntax)
;; FIXME syntaxt checking - type and number
;; FIXME syntax checking for method arg descriptors
;; FIXME have the opcode dispatcher check the number of bytes :)
(defn bc-method-ref
  "Encode a method ref - to start with this must be encoded as a class name, a method name and a method descriptor. For example, 'java.lang.String', '<init>', '()V'"
  [pool aseq]
  (let [[class-str method-str method-arg-descriptor-str & remainder] aseq
        [pool descriptor-index] (cp-with-method pool class-str method-str method-arg-descriptor-str)]

    [pool (two-byte-index descriptor-index) remainder]))


;; this is lifted verbatim from Pork's jopcode.py... unfortunately I can't use that to wiggle out from mistakes, because I wrote that too :(
(def ^:const OPCODES
  [
   ;; [:name opcode argwidth stack-delta bc-arg-fn]
   ;; bc-arg-fn is a function that takes a constant pool and a seq, and returns the pool, the remainder of the seq, plus an index or indices encoded as the right number of bytes to be the args for the operation
   [:aaload          0x32 0 -1 nil]
   [:aastore         0x53 0 -3 nil]
   [:aconst_null     0x01 0  1 nil]

   ;; aload can't be used to load returnAddress
   [:aload           0x19 1  1 nil]
   [:aload_0         0x2a 0  1 nil]
   [:aload_1         0x2b 0  1 nil]
   [:aload_2         0x2c 0  1 nil]
   [:aload_3         0x2d 0  1 nil]
   [:anewarray       0xbd 2  0 nil]
   [:areturn         0xb0 0 -1 nil]
   [:arraylength     0xbe 0  0 nil]

   ;; astore is used to write returnAddress as well as reference
   [:astore          0x3a 1 -1 nil]
   [:astore_0        0x4b 0 -1 nil]
   [:astore_1        0x4c 0 -1 nil]
   [:astore_2        0x4d 0 -1 nil]
   [:astore_3        0x4e 0 -1 nil]
   [:athrow          0xbf 0  0 nil]

   [:baload          0x33 0 -1 nil]
   [:bastore         0x54 0 -3 nil]
   [:bipush          0x10 1  1 nil]

   [:caload          0x34 0 -1 nil]
   [:castore         0x55 0 -3 nil]
   [:checkcast       0xc0 2  0 nil]

   [:d2f             0x90 0 -1 nil] ; shrinkage
   [:d2i             0x8e 0 -1 nil]
   [:d2l             0x8f 0  0 nil]
   [:dadd            0x63 0 -2 nil]
   [:daload          0x31 0  0 nil] ; two one-word args -> one two-word
   [:dastore         0x52 0 -4 nil]
   [:dcmpg           0x98 0 -3 nil] ; vice versa
   [:dcmpl           0x97 0 -3 nil]
   [:dconst_0        0x0e 0  2 nil]
   [:dconst_1        0x0f 0  2 nil]
   [:ddiv            0x6f 0 -2 nil]
   [:dload           0x18 1  2 nil]
   [:dload_0         0x26 0  2 nil]
   [:dload_1         0x27 0  2 nil]
   [:dload_2         0x28 0  2 nil]
   [:dload_3         0x29 0  2 nil]
   [:dmul            0x6b 0 -2 nil]
   [:dneg            0x77 0  0 nil]
   [:drem            0x73 0 -2 nil]
   [:dreturn         0xaf 0 -2 nil] ; TODO sort out how to describe returns
   [:dstore          0x39 1 -2 nil]
   [:dstore_0        0x47 0 -2 nil]
   [:dstore_1        0x48 0 -2 nil]
   [:dstore_2        0x49 0 -2 nil]
   [:dstore_3        0x4a 0 -2 nil]
   [:dsub            0x67 0 -2 nil]
   [:dup             0x59 0  1 nil]
   [:dup_x1          0x5a 0  1 nil]
   [:dup_x2          0x5b 0  1 nil] ; beware type categories
   [:dup2            0x5c 0  2 nil] ;
   [:dup2_x1         0x5d 0  2 nil] ; 
   [:dup2_x2         0x5e 0  2 nil] ;

   [:f2d             0x8d 0  1 nil]
   [:f2i             0x8b 0  0 nil]
   [:f2l             0x8c 0  1 nil]
   [:fadd            0x62 0 -1 nil]
   [:faload          0x30 0 -1 nil]
   [:fastore         0x51 0 -3 nil]
   [:fcmpg           0x96 0 -1 nil]
   [:fcmpl           0x95 0 -1 nil]
   [:fconst_0        0x0b 0  1 nil]
   [:fconst_1        0x0c 0  1 nil]
   [:fconst_2        0x0d 0  1 nil]
   [:fdiv            0x6e 0 -1 nil]
   [:fload           0x17 1  1 nil]
   [:fload_0         0x22 0  1 nil]
   [:fload_1         0x23 0  1 nil]
   [:fload_2         0x24 0  1 nil]
   [:fload_3         0x25 0  1 nil]
   [:fmul            0x6a 0 -1 nil]
   [:fneg            0x76 0  0 nil]
   [:frem            0x72 0 -1 nil]
   [:freturn         0xae 0 -1 nil]
   [:fstore          0x38 1 -1 nil]
   [:fstore_0        0x43 0 -1 nil]
   [:fstore_1        0x44 0 -1 nil]
   [:fstore_2        0x45 0 -1 nil]
   [:fstore_3        0x46 0 -1 nil]
   [:fsub            0x66 0 -1 nil]

   [:getfield        0xb4 2  1 nil] ; delta actually 0 or 1
   [:getstatic       0xb2 2  2 nil] ; 1 or 2
   [:goto            0xa7 2  0 nil]
   [:goto_w          0xc8 4  0 nil]

   [:i2b             0x91 0  0 nil]
   [:i2c             0x92 0  0 nil]
   [:i2d             0x87 0  1 nil]
   [:i2f             0x86 0  0 nil]
   [:i2l             0x85 0  1 nil]
   [:i2s             0x93 0  0 nil]
   [:iadd            0x60 0 -1 nil]
   [:iaload          0x2e 0 -1 nil]
   [:iand            0x7e 0 -1 nil]
   [:iastore         0x4f 0 -3 nil]
   ;; bipush <i>
   [:iconst_m1       0x02 0  1 nil]
   [:iconst_0        0x03 0  1 nil]
   [:iconst_1        0x04 0  1 nil]
   [:iconst_2        0x05 0  1 nil]
   [:iconst_3        0x06 0  1 nil]
   [:iconst_4        0x07 0  1 nil]
   [:iconst_5        0x08 0  1 nil]
   [:idiv            0x6c 0 -1 nil]
   [:if_acmpeq       0xa5 2 -2 nil]
   [:if_acmpne       0xa6 2 -2 nil]
   ;; integer comparison
   [:if_icmpeq       0x9f 2 -2 nil]
   [:if_icmpne       0xa0 2 -2 nil]
   [:if_icmplt       0xa1 2 -2 nil]
   [:if_icmpge       0xa2 2 -2 nil]
   [:if_icmpgt       0xa3 2 -2 nil]
   [:if_icmple       0xa4 2 -2 nil]
   ;; integer comparison with zero
   [:ifeq            0x99 2 -1 nil]
   [:ifge            0x9c 2 -1 nil]
   [:ifgt            0x9d 2 -1 nil]
   [:ifle            0x9e 2 -1 nil]
   [:iflt            0x9b 2 -1 nil]
   [:ifne            0x9a 2 -1 nil]
   [:ifnonnull       0xc7 2 -1 nil]
   [:ifnull          0xc6 2 -1 nil]
   [:iinc            0x84 2  0 nil]
   ;; iload
   [:iload           0x15 1  1 nil] ; wide allowed
   [:iload_0         0x1a 0  1 nil] ; wide allowed
   [:iload_1         0x1b 0  1 nil] ; wide allowed
   [:iload_2         0x1c 0  1 nil] ; wide allowed
   [:iload_3         0x1d 0  1 nil] ; wide allowed
   [:imul            0x68 0 -1 nil]
   [:ineg            0x74 0  0 nil]
   [:instanceof      0xc1 2  0 nil]
   ;; FIXME stack pop count for args also rule that operand 3 must be zero?
   [:invokeinterface 0xb9 4  nil nil]
   ;; FIXME stack pop count for args
   [:invokespecial   0xb7 2  nil bc-method-ref]
   ;; FIXME stack pop count for args
   [:invokestatic    0xb8 2  nil nil]
   ;; FIXME stack pop count for args
   [:invokevirtual   0xb6 2  nil nil]
   [:ior             0x80 0 -1 nil]
   [:irem            0x70 0 -1 nil]
   ;; What happens if theres a float on the stack instead? Find out.
   [:ireturn         0xac 0 -1 nil] ; return int
   [:ishl            0x78 0 -1 nil]
   [:ishr            0x7a 0 -1 nil]
   
   ;; istore
   [:istore          0x36 1 -1 nil]
   [:istore_0        0x3b 0 -1 nil]
   [:istore_1        0x3c 0 -1 nil]
   [:istore_2        0x3d 0 -1 nil]
   [:istore_3        0x3e 0 -1 nil]
   [:isub            0x64 0 -1 nil]
   [:iushr           0x7c 0 -1 nil]
   [:ixor            0x82 0 -1 nil]

   [:jsr             0xa8 2  1 nil]
   [:jsr_w           0xc9 4  1 nil]

   [:l2d             0x8a 0  0 nil]
   [:l2f             0x89 0 -1 nil] ; shrinker
   [:l2i             0x88 0 -1 nil] ; 
   [:ladd            0x61 0 -2 nil]
   [:laload          0x2f 0  0 nil]
   [:land            0x7f 0 -2 nil]
   [:lastore         0x50 0 -4 nil]
   [:lcmp            0x94 0 -3 nil]
   [:lconst_0        0x09 0  2 nil]
   [:lconst_1        0x0a 0  2 nil]
   [:ldc             0x12 1  1 nil]
   [:ldc_w           0x13 2  1 nil]
   [:ldc2_w          0x14 2  2 nil]
   [:ldiv            0x6d 0 -2 nil]
   [:lload           0x16 1  2 nil]
   [:lload_0         0x1e 0  2 nil]
   [:lload_1         0x1f 0  2 nil]
   [:lload_2         0x20 0  2 nil]
   [:lload_3         0x21 0  2 nil]
   [:lmul            0x69 0 -2 nil]
   [:lneg            0x75 0  0 nil]
   ;; TODO figure out how to implement lookupswitch.
   ;; [:lookupswitch 0xab nil -1 nil]
   [:lor             0x81 0 -2 nil]
   [:lrem            0x71 0 -2 nil]
   ;; TODO returns clear stack ignore their deltas or..?
   [:lreturn         0xad 0 -2 nil]
   [:lshl            0x79 0 -1 nil]
   [:lshr            0x7b 0 -1 nil]
   [:lstore          0x37 1 -2 nil]
   [:lstore_0        0x3f 0 -2 nil]
   [:lstore_1        0x40 0 -2 nil]
   [:lstore_2        0x41 0 -2 nil]
   [:lstore_3        0x42 0 -2 nil]
   [:lsub            0x65 0 -2 nil]
   [:lushr           0x7d 0 -1 nil]
   [:lxor            0x83 0 -1 nil]

   [:monitorenter    0xc2 0 -1 nil]
   [:monitorexit     0xc3 0 -1 nil]
   [:multinewarray   0xc5 3  nil nil] ; stack pop count from dimensions?

   [:new             0xbb 2  1 nil]
   [:newarray        0xbc 1  0 nil]
   [:nop             0x00 0  0 nil]

   [:pop             0x57 0 -1 nil]
   [:pop2            0x58 0 -2 nil]
   [:putfield        0xb5 2 -2 nil]
   [:putstatic       0xb3 2 -1 nil]

   [:ret             0xa9 1  0 nil]
   [:return          0xb1 0  0 nil] ; return void

   [:saload          0x35 0 -1 nil]
   [:sastore         0x56 0 -3 nil]
   [:sipush          0x11 2  1 nil]
   [:swap            0x5f 0  0 nil] ; note lack of swap2
])


;; {:aaload  [:aaload          0x32 0 -1]
;;  :aastore [:aastore         0x53 0 -3]
;;  ... }
(def syms-to-opcodes
  (apply hash-map (interleave (map first OPCODES) OPCODES)))

;; {0x32  [:aaload          0x32 0 -1]
;;  0x53 | [:aastore         0x53 0 -3]
;;  ... }
(def bytes-to-opcodes
  (apply hash-map (interleave (map second OPCODES) OPCODES)))


