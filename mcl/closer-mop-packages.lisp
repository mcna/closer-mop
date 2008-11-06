(in-package :cl-user)

(defpackage #:closer-mop
  (:use #:common-lisp)
  (:nicknames #:c2mop)

  #-(or clozure-common-lisp openmcl)
  (:shadow #:defclass #:standard-class #:typep #:subtypep)
  #-(or clozure-common-lisp openmcl)
  (:export #:defclass #:standard-class #:typep #:subtypep)

  #+(or clozure-common-lisp openmcl)
  (:shadow #:defgeneric #:ensure-generic-function #:standard-generic-function)
  #+(or clozure-common-lisp openmcl)
  (:export #:defgeneric #:ensure-generic-function #:standard-generic-function)

  (:import-from #:ccl

   #:classp

   #:direct-slot-definition
   #:effective-slot-definition
   #:eql-specializer
   #:forward-referenced-class
   #:funcallable-standard-class
   #:funcallable-standard-object
   #:metaobject
   #:slot-definition
   #:specializer
   #:standard-accessor-method
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-reader-method
   #:standard-slot-definition
   #:standard-writer-method

   #:accessor-method-slot-definition
   #:add-dependent
   #:add-direct-method
   #:add-direct-subclass
   #:class-default-initargs
   #:class-direct-default-initargs
   #:class-direct-slots
   #:class-direct-subclasses
   #:class-direct-superclasses
   #:class-finalized-p
   #:class-precedence-list
   #:class-prototype
   #:class-slots
   #-(or clozure-common-lisp openmcl mcl) #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #:compute-default-initargs
   #-(or clozure-common-lisp openmcl) #:compute-discriminating-function
   #:compute-effective-method
   #:compute-effective-slot-definition
   #:compute-slots
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:ensure-class
   #:ensure-class-using-class
   #:ensure-generic-function-using-class
   #:eql-specializer-object
   #:extract-lambda-list
   #:extract-specializer-names
   #:finalize-inheritance
   #:find-method-combination
   #:funcallable-standard-instance-access
   #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #:intern-eql-specializer
   #-(or clozure-common-lisp openmcl mcl) #:make-method-lambda
   #:map-dependents
   #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #:reader-method-class
   #:remove-dependent
   #:remove-direct-method
   #:remove-direct-subclass
   #+(or clozure-common-lisp openmcl) #:set-funcallable-instance-function
   #:slot-boundp-using-class
   #:slot-definition-allocation
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-location
   #:slot-definition-name
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-type
   #:slot-makunbound-using-class
   #:slot-value-using-class
   #:specializer-direct-generic-functions
   #:specializer-direct-methods
   #:standard-instance-access
   #:update-dependent
   #:validate-superclass
   #:writer-method-class)

  (:export
   #:direct-slot-definition
   #:effective-slot-definition
   #:eql-specializer
   #:forward-referenced-class
   #:funcallable-standard-class
   #:funcallable-standard-object
   #:metaobject
   #:slot-definition
   #:specializer
   #:standard-accessor-method
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-reader-method
   #:standard-slot-definition
   #:standard-writer-method

   #:classp
   #:ensure-finalized
   #:ensure-method
   #:fix-slot-initargs
   #:required-args
   #:subclassp

   #:accessor-method-slot-definition
   #:add-dependent
   #:add-direct-method
   #:add-direct-subclass
   #:class-default-initargs
   #:class-direct-default-initargs
   #:class-direct-slots
   #:class-direct-subclasses
   #:class-direct-superclasses
   #:class-finalized-p
   #:class-precedence-list
   #:class-prototype
   #:class-slots
   #-(or clozure-common-lisp openmcl mcl) #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #:compute-default-initargs
   #:compute-discriminating-function
   #:compute-effective-method
   #:compute-effective-slot-definition
   #:compute-slots
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:ensure-class
   #:ensure-class-using-class
   #:ensure-generic-function-using-class
   #:eql-specializer-object
   #:extract-lambda-list
   #:extract-specializer-names
   #:finalize-inheritance
   #:find-method-combination
   #:funcallable-standard-instance-access
   #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #:intern-eql-specializer
   #-(or clozure-common-lisp openmcl mcl) #:make-method-lambda
   #:map-dependents
   #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #:reader-method-class
   #:remove-dependent
   #:remove-direct-method
   #:remove-direct-subclass
   #+(or clozure-common-lisp openmcl) #:set-funcallable-instance-function
   #:slot-boundp-using-class
   #:slot-definition-allocation
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-location
   #:slot-definition-name
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-type
   #:slot-makunbound-using-class
   #:slot-value-using-class
   #:specializer-direct-generic-functions
   #:specializer-direct-methods
   #:standard-instance-access
   #-(or clozure-common-lisp openmcl) #:subtypep
   #-(or clozure-common-lisp openmcl) #:typep
   #:update-dependent
   #:validate-superclass
   #:writer-method-class))
