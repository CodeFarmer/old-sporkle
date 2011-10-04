(ns sporkle.bytecode)

;; this is lifted verbatim frrom Pork's jopcode.py... unfortunately I can't use that to wiggle out from mistakes, because I wrote that too :(
(def #^const OPCODES
  [
   ;; [:name opcode argwidth stack-delta]
   [:aaload          0x32 0 -1]
   [:aastore         0x53 0 -3]
   [:aconst_null     0x01 0  1]

   ;; aload cant be used to load returnAddress
   [:aload           0x19 1  1]
   [:aload_0         0x2a 0  1]
   [:aload_1         0x2b 0  1]
   [:aload_2         0x2c 0  1]
   [:aload_3         0x2d 0  1]
   [:anewarray       0xbd 2  0]
   [:areturn         0xb0 0 -1]
   [:arraylength     0xbe 0  0]

   ;; astore is used to write returnAddress as well as reference
   [:astore          0x3a 1 -1]
   [:astore_0        0x4b 0 -1]
   [:astore_1        0x4c 0 -1]
   [:astore_2        0x4d 0 -1]
   [:astore_3        0x4e 0 -1]
   [:athrow          0xbf 0  0]

   [:baload          0x33 0 -1]
   [:bastore         0x54 0 -3]
   [:bipush          0x10 1  1]

   [:caload          0x34 0 -1]
   [:castore         0x55 0 -3]
   [:checkcast       0xc0 2  0]

   [:d2f             0x90 0 -1] ; shrinkage
   [:d2i             0x8e 0 -1]
   [:d2l             0x8f 0  0]
   [:dadd            0x63 0 -2]
   [:daload          0x31 0  0] ; two one-word args -> one two-word
   [:dastore         0x52 0 -4]
   [:dcmpg           0x98 0 -3] ; vice versa
   [:dcmpl           0x97 0 -3]
   [:dconst_0        0x0e 0  2]
   [:dconst_1        0x0f 0  2]
   [:ddiv            0x6f 0 -2]
   [:dload           0x18 1  2]
   [:dload_0         0x26 0  2]
   [:dload_1         0x27 0  2]
   [:dload_2         0x28 0  2]
   [:dload_3         0x29 0  2]
   [:dmul            0x6b 0 -2]
   [:dneg            0x77 0  0]
   [:drem            0x73 0 -2]
   [:dreturn         0xaf 0 -2] ; TODO sort out how to describe returns
   [:dstore          0x39 1 -2]
   [:dstore_0        0x47 0 -2]
   [:dstore_1        0x48 0 -2]
   [:dstore_2        0x49 0 -2]
   [:dstore_3        0x4a 0 -2]
   [:dsub            0x67 0 -2]
   [:dup             0x59 0  1]
   [:dup_x1          0x5a 0  1]
   [:dup_x2          0x5b 0  1] ; beware type categories
   [:dup2            0x5c 0  2] ;
   [:dup2_x1         0x5d 0  2] ; 
   [:dup2_x2         0x5e 0  2] ;

   [:f2d             0x8d 0  1]
   [:f2i             0x8b 0  0]
   [:f2l             0x8c 0  1]
   [:fadd            0x62 0 -1]
   [:faload          0x30 0 -1]
   [:fastore         0x51 0 -3]
   [:fcmpg           0x96 0 -1]
   [:fcmpl           0x95 0 -1]
   [:fconst_0        0x0b 0  1]
   [:fconst_1        0x0c 0  1]
   [:fconst_2        0x0d 0  1]
   [:fdiv            0x6e 0 -1]
   [:fload           0x17 1  1]
   [:fload_0         0x22 0  1]
   [:fload_1         0x23 0  1]
   [:fload_2         0x24 0  1]
   [:fload_3         0x25 0  1]
   [:fmul            0x6a 0 -1]
   [:fneg            0x76 0  0]
   [:frem            0x72 0 -1]
   [:freturn         0xae 0 -1]
   [:fstore          0x38 1 -1]
   [:fstore_0        0x43 0 -1]
   [:fstore_1        0x44 0 -1]
   [:fstore_2        0x45 0 -1]
   [:fstore_3        0x46 0 -1]
   [:fsub            0x66 0 -1]

   [:getfield        0xb4 2  1] ; delta actually 0 or 1
   [:getstatic       0xb2 2  2] ; 1 or 2
   [:goto            0xa7 2  0]
   [:goto_w          0xc8 4  0]

   [:i2b             0x91 0  0]
   [:i2c             0x92 0  0]
   [:i2d             0x87 0  1]
   [:i2f             0x86 0  0]
   [:i2l             0x85 0  1]
   [:i2s             0x93 0  0]
   [:iadd            0x60 0 -1]
   [:iaload          0x2e 0 -1]
   [:iand            0x7e 0 -1]
   [:iastore         0x4f 0 -3]
   ;; bipush <i>
   [:iconst_m1       0x02 0  1]
   [:iconst_0        0x03 0  1]
   [:iconst_1        0x04 0  1]
   [:iconst_2        0x05 0  1]
   [:iconst_3        0x06 0  1]
   [:iconst_4        0x07 0  1]
   [:iconst_5        0x08 0  1]
   [:idiv            0x6c 0 -1]
   [:if_acmpeq       0xa5 2 -2]
   [:if_acmpne       0xa6 2 -2]
   ;; integer comparison
   [:if_icmpeq       0x9f 2 -2]
   [:if_icmpne       0xa0 2 -2]
   [:if_icmplt       0xa1 2 -2]
   [:if_icmpge       0xa2 2 -2]
   [:if_icmpgt       0xa3 2 -2]
   [:if_icmple       0xa4 2 -2]
   ;; integer comparison with zero
   [:ifeq            0x99 2 -1]
   [:ifge            0x9c 2 -1]
   [:ifgt            0x9d 2 -1]
   [:ifle            0x9e 2 -1]
   [:iflt            0x9b 2 -1]
   [:ifne            0x9a 2 -1]
   [:ifnonnull       0xc7 2 -1]
   [:ifnull          0xc6 2 -1]
   [:iinc            0x84 2  0]
   ;; iload
   [:iload           0x15 1  1] # wide allowed
   [:iload_0         0x1a 0  1] # wide allowed
   [:iload_1         0x1b 0  1] # wide allowed
   [:iload_2         0x1c 0  1] # wide allowed
   [:iload_3         0x1d 0  1] # wide allowed
   [:imul            0x68 0 -1]
   [:ineg            0x74 0  0]
   [:instanceof      0xc1 2  0]
   ;; FIXME stack pop count for args also rule that operand 3 must be zero?
   [:invokeinterface 0xb9 4  None]
   ;; FIXME stack pop count for args
   [:invokespecial   0xb7 2  None]
   ;; FIXME stack pop count for args
   [:invokestatic    0xb8 2  None]
   ;; FIXME stack pop count for args
   [:invokevirtual   0xb6 2  None]
   [:ior             0x80 0 -1]
   [:irem            0x70 0 -1]
   ;; What happens if theres a float on the stack instead? Find out.
   [:ireturn         0xac 0 -1] # return int
   [:ishl            0x78 0 -1]
   [:ishr            0x7a 0 -1]
   
   ;; istore
   [:istore          0x36 1 -1]
   [:istore_0        0x3b 0 -1]
   [:istore_1        0x3c 0 -1]
   [:istore_2        0x3d 0 -1]
   [:istore_3        0x3e 0 -1]
   [:isub            0x64 0 -1]
   [:iushr           0x7c 0 -1]
   [:ixor            0x82 0 -1]

   [:jsr             0xa8 2  1]
   [:jsr_w           0xc9 4  1]

   [:l2d             0x8a 0  0]
   [:l2f             0x89 0 -1] ; shrinker
   [:l2i             0x88 0 -1] ; 
   [:ladd            0x61 0 -2]
   [:laload          0x2f 0  0]
   [:land            0x7f 0 -2]
   [:lastore         0x50 0 -4]
   [:lcmp            0x94 0 -3]
   [:lconst_0        0x09 0  2]
   [:lconst_1        0x0a 0  2]
   [:ldc             0x12 1  1]
   [:ldc_w           0x13 2  1]
   [:ldc2_w          0x14 2  2]
   [:ldiv            0x6d 0 -2]
   [:lload           0x16 1  2]
   [:lload_0         0x1e 0  2]
   [:lload_1         0x1f 0  2]
   [:lload_2         0x20 0  2]
   [:lload_3         0x21 0  2]
   [:lmul            0x69 0 -2]
   [:lneg            0x75 0  0]
   ;; TODO figure out how to implement lookupswitch.
   ;; [:lookupswitch 0xab None -1]
   [:lor             0x81 0 -2]
   [:lrem            0x71 0 -2]
   ;; TODO returns clear stack ignore their deltas or..?
   [:lreturn         0xad 0 -2]
   [:lshl            0x79 0 -1]
   [:lshr            0x7b 0 -1]
   [:lstore          0x37 1 -2]
   [:lstore_0        0x3f 0 -2]
   [:lstore_1        0x40 0 -2]
   [:lstore_2        0x41 0 -2]
   [:lstore_3        0x42 0 -2]
   [:lsub            0x65 0 -2]
   [:lushr           0x7d 0 -1]
   [:lxor            0x83 0 -1]

   [:monitorenter    0xc2 0 -1]
   [:monitorexit     0xc3 0 -1]
   [:multinewarray   0xc5 3  nil] ; stack pop count from dimensions?

   [:new             0xbb 2  1]
   [:newarray        0xbc 1  0]
   [:nop             0x00 0  0]

   [:pop             0x57 0 -1]
   [:pop2            0x58 0 -2]
   [:putfield        0xb5 2 -2]
   [:putstatic       0xb3 2 -1]

   [:ret             0xa9 1  0]
   [:return          0xb1 0  0] ; return void

   [:saload          0x35 0 -1]
   [:sastore         0x56 0 -3]
   [:sipush          0x11 2  1]
   [:swap            0x5f 0  0] ; note lack of swap2
   ])
