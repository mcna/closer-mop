
:allegro7.0
:compute-default-initargs -> :compute-default-initargs-allegro (no)
:defgeneric-calls-find-method-combination (no)
:defmethod-calls-make-method-lambda (no)
:dependent-protocol-for-generic-functions (fixed)
:extensible-allocation (fixed)
:function-invocation-calls-compute-applicable-methods (redef sgf)
:function-invocation-calls-compute-applicable-methods-using-classes (redef sgf)
:function-invocation-calls-compute-effective-method (redef sgf)
:make-method-lambda (no)
:method-functions-take-processed-parameters (no)
:method-lambdas-are-processed (no)
:multiple-qualifiers (no)
:reinitialize-instance-calls-compute-discriminating-function (fixed)
:setf-class-name-calls-reinitialize-instance (no)
:setf-generic-function-name-class-reinitialize-intance (no)
:slot-boundp-using-class-specialized-on-slot-definition (fixed)
:slot-makunbound-using-class-specialized-on-slot-definition (fixed)
:standard-class-and-funcallable-standard-class-are-compatible (no)
:t-is-always-a-valid-superclass (no)

:allegro8.0b
:compute-default-initargs -> :compute-default-initargs-allegro (no)
:defgeneric-calls-find-method-combination (no)
:defmethod-calls-make-method-lambda (no)
:dependent-protocol-for-generic-functions (fixed)
:extensible-allocation (fixed)
:function-invocation-calls-compute-applicable-methods (redef sgf)
:function-invocation-calls-compute-applicable-methods-using-classes (redef sgf)
:function-invocation-calls-compute-effective-method (redef sgf)
:make-method-lambda (no)
:method-functions-take-processed-parameters (no)
:method-lambdas-are-processed (no)
:reinitialize-instance-calls-compute-discriminating-function (fixed)
:setf-class-name-calls-reinitialize-instance (no)
:setf-generic-function-name-class-reinitialize-intance (no)
:slot-boundp-using-class-specialized-on-slot-definition (fixed)
:slot-makunbound-using-class-specialized-on-slot-definition (fixed)
:standard-class-and-funcallable-standard-class-are-compatible (no)
:t-is-always-a-valid-superclass (no)

:clisp2.35-2.36
:accessor-method-initialized-with-function (no)
:add-method-calls-compute-discriminating-function (no)
:compute-slots-requested-slot-order-honoured (no)
:defmethod-calls-make-method-lambda (no)
:extensible-allocation (no)
:forward-referenced-class-changed-by-change-class (no)
:initialize-instance-calls-compute-discriminating-function (no)
:make-method-lambda (no)
:method-initialized-with-function (no)
:method-lambdas-are-processed (no)
:reinitialize-instance-calls-compute-discriminating-function (no)
:remove-method-calls-compute-discriminating-function (no)

:clisp2.37
:accessor-method-initialized-with-function (no)
:add-method-calls-compute-discriminating-function (no)
:compute-slots-requested-slot-order-honoured (no)
:defmethod-calls-make-method-lambda (no)
:forward-referenced-class-changed-by-change-class (no)
:initialize-instance-calls-compute-discriminating-function (no)
:make-method-lambda (no)
:method-initialized-with-function (no)
:method-lambdas-are-processed (no)
:reinitialize-instance-calls-compute-discriminating-function (no)
:remove-method-calls-compute-discriminating-function (no)

:cmu
:accessor-method-initialized-with-function (fixed)
:accessor-method-initialized-with-lambda-list (fixed)
:accessor-method-initialized-with-slot-definition (fixed)
:accessor-method-initialized-with-specializers (fixed)
:anonymous-classes (fixed)
:class-initialization-calls-reader-method-class (fixed)
:class-initialization-calls-writer-method-class (fixed)
:discriminating-functions-can-be-closures (no)
:discriminating-functions-can-be-funcalled (no)
:documentation-passed-to-effective-slot-definition-class (redef sc)
:effective-slot-definition-initialized-with-documentation (redef sc)
:method-initialized-with-function (no)
:multiple-slot-options-passed-as-list-to-direct-slot-definition-class (fix with fix-slot-initargs)
:reinitialize-instance-calls-compute-discriminating-function (fixed)
:setf-class-name-calls-reinitialize-instance (no)
:setf-generic-function-name-calls-reinitialize-instance (no)
:standard-class-and-funcallable-standard-class-are-compatible (no)

:lispworks4.4
:add-method-calls-add-direct-method (fixed)
:add-method-calls-compute-discriminating-function (no)
:add-method-calls-remove-method (fixed)
:add-method-updates-specializer-direct-generic-functions (fixed)
:allocation-passed-to-effective-slot-definition-class (instead :flags-passed-to-effective-slot-definition-class)
:class-initialized-with-direct-default-initargs (instead: conditionalization)
:class-reinitialization-calls-remove-direct-subclass (fixed)
:compute-applicable-methods-using-classes (redef sgf)
:compute-default-initargs (probably not / redef finalize-inheritance)
:defgeneric-calls-find-method-combination (no)
:direct-superclasses-by-default-empty (not fixed, but direct superclasses are automatically adjusted)
:effective-slot-definition-initialized-with-allocation (instead :effective-slot-definition-initialized-with-flags)
:eql-specializer (partially fixed)
:eql-specializer-object (fixed)
:eql-specializers-are-objects (no)
:extensible-allocation (no)
:finalize-inheritance-calls-compute-default-initargs (probably not / redef finailze-inheritance)
:find-method-combination (partially fixed)
:funcallable-standard-instance-access (no)
:function-invocation-calls-compute-applicable-methods (redef sgf)
:function-invocation-calls-compute-applicable-methods-using-classes (redef sgf)
:generic-function-initialized-with-declarations (map from generic-function-initialized-with-declare)
:initialize-instance-calls-compute-discriminating-function (no)
:intern-eql-specializer (partially fixed)
:make-method-lambda (partially fixed)
:method-functions-take-processed-parameters (no)
:reinitialize-instance-calls-compute-discriminating-function (no)
:remove-method-calls-compute-discriminating-function (no)
:remove-method-calls-remove-direct-method (fixed)
:setf-class-name-calls-reinitialize-instance (no)
:setf-generic-function-name-calls-reinitialize-instance (no)
:setf-slot-value-using-class-specialized-on-slot-definition (fixed)
:slot-boundp-using-class-specialized-on-slot-definition (fixed)
:slot-makunbound-using-class-specialized-on-slot-definition (fixed)
:slot-reader-calls-slot-value-using-class (fixed)
:slot-value-using-class-specialized-on-slot-definition (fixed)
:slot-writer-calls-slot-value-using-class (fixed)
:specializer (no)
:specializer-direct-generic-functions (fixed)
:standard-class-and-funcallable-standard-class-are-compatible (no)
:standard-instance-access (no)

:mcl
:add-method-calls-compute-discriminating-function (no)
:compute-applicable-methods-using-classes (no)
:defmethod-calls-generic-function-method-class (no)
:defmethod-calls-make-method-lambda (no)
:discriminating-functions-can-be-closures (no)
:discriminating-functions-can-be-funcalled (no)
:funcallable-standard-object (no)
:function-invocation-calls-compute-applicable-methods (no)
:function-invocation-calls-compute-applicable-methods-using-classes (no)
:function-invocation-calls-compute-effective-method (no)
:generic-function-initialized-with-declarations (no)
:generic-functions-can-be-empty (no)
:initialize-instance-calls-compute-discriminating-function (no)
:make-method-lambda (no)
:method-functions-take-processed-parameters (no)
:method-lambdas-are-processed (no)
:reinitialize-instance-calls-compute-discriminating-function (no)
:remove-method-calls-compute-discriminating-function (no)
:set-funcallable-instance-function (no)
:setf-generic-function-name (no)
:setf-generic-function-name-calls-reinitialize-instance (no)
-
:compute-slots-requested-slot-order-honoured (no)
:direct-slot-definition (fixed)
:direct-superclasses-by-default-empty (not fixed, but direct superclasses are automatically adjusted, not for funcallable-standard-class though)
:effective-slot-definition (fixed)
:eql-specializer (fixed)
:extensible-allocation (no)
:multiple-slot-options-passed-as-list-to-direct-slot-definition-class (fix with fix-slot-initargs)
:setf-class-name-calls-reinitialize-instance (no)
:slot-definition (fixed)
:standard-slot-definition (fixed)

:openmcl
:add-method-calls-compute-discriminating-function (no)
:compute-applicable-methods-using-classes (no)
:defmethod-calls-generic-function-method-class (no)
:defmethod-calls-make-method-lambda (no)
:discriminating-functions-can-be-closures (no)
:discriminating-functions-can-be-funcalled (no)
:funcallable-standard-object (no)
:function-invocation-calls-compute-applicable-methods (no)
:function-invocation-calls-compute-applicable-methods-using-classes (no)
:function-invocation-calls-compute-effective-method (no)
:generic-functions-can-be-empty (no)
:initialize-instance-calls-compute-discriminating-function (no)
:make-method-lambda (no)
:method-functions-take-processed-parameters (no)
:method-lambdas-are-processed (no)
:reinitialize-instance-calls-compute-discriminating-function (no)
:remove-method-calls-compute-discriminating-function (no)
-
:compute-slots-requested-slot-order-honoured (no)
:eql-specializer (fixed)

:sbcl
:accessor-method-initialized-with-function (fixed)
:accessor-method-initialized-with-lambda-list (fixed)
:accessor-method-initialized-with-slot-definition (fixed)
:accessor-method-initialized-with-specializers (fixed)
:anonymous-classes (fixed)
:class-initialization-calls-reader-method-class (fixed)
:class-initialization-calls-writer-method-class (fixed)
:method-initialized-with-function (no)
