(ns sporkle.core)

;; ClassFile {
;; 	u4 magic;
;; 	u2 minor_version;
;; 	u2 major_version;
;; 	u2 constant_pool_count;
;; 	cp_info constant_pool[constant_pool_count-1];
;; 	u2 access_flags;
;; 	u2 this_class;
;; 	u2 super_class;
;; 	u2 interfaces_count;
;; 	u2 interfaces[interfaces_count];
;; 	u2 fields_count;
;; 	field_info fields[fields_count];
;; 	u2 methods_count;
;; 	method_info methods[methods_count];
;; 	u2 attributes_count;
;; 	attribute_info attributes[attributes_count];
;; }


;; constant pool entries

(def CONSTANT_Class               7)
(def CONSTANT_Fieldref	          9)
(def CONSTANT_Methodref	         10)
(def CONSTANT_InterfaceMethodref 11)
(def CONSTANT_String              8)
(def CONSTANT_Integer             3)
(def CONSTANT_Float               4)
(def CONSTANT_Long                5)
(def CONSTANT_Double              6)
(def CONSTANT_NameAndType        12)
(def CONSTANT_Utf8                1)

;; access flags

(def ACC_PUBLIC	      0x0001) ;; Class, method: may be accessed from outside its package.
(def ACC_PRIVATE      0x0002) ;; Class, method: accessible only within the defining class.
(def ACC_PROTECTED    0x0004) ;; Method: may be accessed within subclasses. method
(def ACC_STATIC       0x0008) ;; Method

(def ACC_FINAL	      0x0010) ;; Class: no subclasses allowed. Method: may not be overridden

; beware shared flag value
(def ACC_SUPER	      0x0020) ;; Class: treat superclass methods specially when invoked by the invokespecial instruction.
(def ACC_SYNCHRONIZED 0x0020) ;; Method: invocation is wrapped in a monitor lock.

(def ACC_NATIVE       0x0100) ;; Method: native method.
(def ACC_INTERFACE    0x0200) ;; Class: is an interface, not a class.
(def ACC_ABSTRACT     0x0400) ;; Class: may not be instantiated. Method: no implementation is provided
(def ACC_STRICT       0x0800) ;; Method: floating-point mode is FP-strict

