(in-package :closer-mop)

;; Some utility functions.

(defun required-args (lambda-list &optional (collector #'identity))
  (loop for arg in lambda-list
        until (member arg lambda-list-keywords)
        collect (funcall collector arg)))

(defun ensure-finalized (class &optional (errorp t))
  (if (typep class 'class)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (when errorp (error "~S is not a class." class)))
  class)

(defun subclassp (class superclass)
  (flet ((get-class (class) (etypecase class
                              (class class)
                              (symbol (find-class class)))))
    
      (loop with class = (get-class class)
            with superclass = (get-class superclass)
            
            for superclasses = (list class)
            then (set-difference 
                  (union (class-direct-superclasses current-class) superclasses)
                  seen)

            for current-class = (first superclasses)

            while current-class
            
            if (eq current-class superclass) return t
            else collect current-class into seen
            
            finally (return nil))))

;; We need a new standard-generic-function for various things.

(cl:defclass standard-generic-function (cl:standard-generic-function)
  ((argument-order :accessor argument-order)
   (initial-methods :initform '()))
  (:metaclass clos:funcallable-standard-class))

#|
(defun ensure-generic-function
       (name &rest args
             &key (generic-function-class 'cl:standard-generic-function)
             &allow-other-keys)
  (declare (dynamic-extent args))
  (when (fboundp name)
    (let ((function (fdefinition name)))
      (unless (typep function 'generic-function)
        (cerror "Discard existing definition and create generic function."
                "~S is already fbound, but not as a generic function." name)
        (fmakunbound name))))
  (if (fboundp name)
    (let ((function (fdefinition name)))
      (apply #'ensure-generic-function-using-class
             function name args))
    (apply #'ensure-generic-function-using-class nil name
           :generic-function-class generic-function-class
           args)))
|#

;; We need a new standard-class for various things.

(cl:defclass standard-class (cl:standard-class)
  ())

;; validate-superclass for metaclass classes is a little bit
;; more tricky than for class metaobject classes because
;; we don't want to make all standard-classes compatible to
;; each other.

;; Our validate-superclass may get passed a class-prototype
;; as its second argument, so don't expect its readers to
;; yield useful information. (In ANSI parlance, "the
;; consequences are undefined...")

(cl:defmethod validate-superclass
           ((class standard-class)
            (superclass cl:standard-class))
  (or (when (eq (class-of class) (find-class 'standard-class))
        (or (eq (class-of superclass) (find-class 'cl:standard-class))
            (eq (class-of superclass) (find-class 'standard-class))))
      (call-next-method)
      (when (eq (class-of superclass) (find-class 'cl:standard-class))
        (validate-superclass class (class-prototype (find-class 'standard-class))))))

;; We need a new funcallable-standard-class for various things.

(cl:defclass funcallable-standard-class (clos:funcallable-standard-class)
  ())

;; See the comment on validate-superclass for standard-class above.

(cl:defmethod validate-superclass
           ((class funcallable-standard-class)
            (superclass clos:funcallable-standard-class))
  (or (when (eq (class-of class) (find-class 'funcallable-standard-class))
        (or (eq (class-of superclass) (find-class 'clos:funcallable-standard-class))
            (eq (class-of superclass) (find-class 'funcallable-standard-class))))
      (call-next-method)
      (when (eq (class-of superclass) (find-class 'clos:funcallable-standard-class))
        (validate-superclass class (class-prototype (find-class 'funcallable-standard-class))))))

#+lispworks5
(cl:defmethod validate-superclass
           ((class funcallable-standard-class)
            (superclass (eql (find-class 'funcallable-standard-object))))
  t)

;; We also need a new funcallable-standard-object because the default one
;; is not an instance of clos:funcallable-standard-class.

#-lispworks5
(cl:defclass funcallable-standard-object (clos:funcallable-standard-object)
  ()
  (:metaclass clos:funcallable-standard-class))

;; The following code ensures that possibly incorrect lists of direct
;; superclasses are corrected.

#-lispworks5
(defun modify-superclasses (direct-superclasses &optional (standardp t))
  (if (null direct-superclasses)
    (list (if standardp
            (find-class 'standard-object)
            (find-class 'funcallable-standard-object)))
    (let ((standard-object (if standardp
                             (find-class 'standard-object)
                             (find-class 'clos:funcallable-standard-object))))
      (if (eq (car (last direct-superclasses)) standard-object)
        (if standardp
          direct-superclasses
          (append (butlast direct-superclasses)
                  (list (find-class 'funcallable-standard-object))))
        (remove standard-object direct-superclasses)))))

;; During class re/initialization, we take care of the following things:
;; - Optimization of slot accessors is deactivated.
;; - Lists of direct superclasses are corrected.
;; - Removal of direct subclasses.

(cl:defmethod initialize-instance :around
  ((class standard-class) &rest initargs
   #-lispworks5 &key
   #-lispworks5 (direct-superclasses ()))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method class
         #-lispworks5 :direct-superclasses
         #-lispworks5 (modify-superclasses direct-superclasses)
         :optimize-slot-access nil
         initargs))

(cl:defmethod reinitialize-instance :around
  ((class standard-class) &rest initargs
   #-lispworks5 &key
   #-lispworks5 (direct-superclasses () direct-superclasses-p))
  (declare (dynamic-extent initargs))
  #-lispworks5
  (progn
    (when direct-superclasses-p
      (setq direct-superclasses (modify-superclasses direct-superclasses))
      (loop for superclass in (copy-list (class-direct-superclasses class))
            unless (member superclass direct-superclasses)
            do (remove-direct-subclass superclass class)))
    (if direct-superclasses-p
      (apply #'call-next-method class
             :direct-superclasses direct-superclasses
             :optimize-slot-access nil
             initargs)
      (apply #'call-next-method class
             :optimize-slot-access nil
             initargs)))
  #+lispworks5
  (apply #'call-next-method class
         :optimize-slot-access nil
         initargs))

(cl:defmethod initialize-instance :around
  ((class funcallable-standard-class) &rest initargs
   #-lispworks5 &key
   #-lispworks5 (direct-superclasses ()))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method class
         #-lispworks5 :direct-superclasses
         #-lispworks5 (modify-superclasses direct-superclasses nil)
         :optimize-slot-access nil
         initargs))

(cl:defmethod reinitialize-instance :around
  ((class funcallable-standard-class) &rest initargs
   #-lispworks5 &key
   #-lispworks5 (direct-superclasses () direct-superclasses-p))
  (declare (dynamic-extent initargs))
  #-lispworks5
  (progn
    (when direct-superclasses-p
      (setq direct-superclasses (modify-superclasses direct-superclasses nil))
      (loop for superclass in (copy-list (class-direct-superclasses class))
            unless (member superclass direct-superclasses)
            do (remove-direct-subclass superclass class)))
    (if direct-superclasses-p
      (apply #'call-next-method class
             :direct-superclasses direct-superclasses
             :optimize-slot-access nil
             initargs)
      (apply #'call-next-method class
             :optimize-slot-access nil
             initargs)))
  #+lispworks5
  (apply #'call-next-method class
         :optimize-slot-access nil
         initargs))

;; The following is necessary for forward-referenced-classes.
;; Since we replace the original funcallable-standard-object with
;; a new one, we have to prevent LispWorks from trying to use
;; the original one when forward-ferenced-classes are resolved.

#-lispworks5
(cl:defmethod change-class :around
  ((class forward-referenced-class)
   (new-class funcallable-standard-class)
   &rest initargs
   &key (direct-superclasses ()))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method class new-class
         :optimize-slot-access nil
         :direct-superclasses (modify-superclasses direct-superclasses nil)
         initargs))

;;; In LispWorks, the slot accessors (slot-value-using-class, etc.) are specialized
;;; on slot names instead of effective slot definitions. In order to fix this,
;;; we need to rewire the slot access protocol.

(cl:defmethod slot-value-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find slot (class-slots class)
                     :test #'eq
                     :key #'slot-definition-name)))
    (if slotd
      (slot-value-using-class class object slotd)
      (slot-missing class object slot 'slot-value))))

(cl:defmethod slot-value-using-class
           ((class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (slot-value-using-class
   (load-time-value (class-prototype (find-class 'cl:standard-class)))
   object
   (slot-definition-name slotd)))

(cl:defmethod (setf slot-value-using-class)
           (new-value (class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find slot (class-slots class)
                     :test #'eq
                     :key #'slot-definition-name)))
    (if slotd
      (setf (slot-value-using-class class object slotd)
            new-value)
      (slot-missing class object slot 'setf new-value))))

(cl:defmethod (setf slot-value-using-class)
           (new-value (class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (setf (slot-value-using-class
         (load-time-value (class-prototype (find-class 'cl:standard-class)))
         object
         (slot-definition-name slotd))
        new-value))

(cl:defmethod slot-boundp-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find slot (class-slots class)
                     :test #'eq
                     :key #'slot-definition-name)))
    (if slotd
      (slot-boundp-using-class class object slotd)
      (slot-missing class object slot 'slot-boundp))))

(cl:defmethod slot-boundp-using-class
           ((class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (slot-boundp-using-class
   (load-time-value (class-prototype (find-class 'cl:standard-class)))
   object
   (slot-definition-name slotd)))

(cl:defmethod slot-makunbound-using-class
           ((class standard-class) object (slot symbol))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (let ((slotd (find slot (class-slots class)
                     :test #'eq
                     :key #'slot-definition-name)))
    (if slotd
      (slot-makunbound-using-class class object slotd)
      (slot-missing class object slot 'slot-makunbound))))

(cl:defmethod slot-makunbound-using-class
           ((class standard-class) object (slotd standard-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (slot-makunbound-using-class
   (load-time-value (class-prototype (find-class 'cl:standard-class)))
   object
   (slot-definition-name slotd)))

;; In LispWorks, eql specializers are lists. We cannot change this
;; but we can soften some of the incompatibilities.

(deftype eql-specializer ()
  '(or eql-specializer*
       (satisfies clos:eql-specializer-p)))

(cl:defgeneric eql-specializer-object (eql-specializer)
  (:method ((cons cons))
   (if (clos:eql-specializer-p cons)
     (cadr cons)
     (error "~S is not an eql-specializer." cons))))

(defun intern-eql-specializer (object)
  `(eql ,object))

(cl:defclass eql-specializer* (metaobject)
  ((obj :reader eql-specializer-object
        :initarg eso
        :initform (error "Use intern-eql-specializer to create eql-specializers."))
   (direct-methods :reader specializer-direct-methods
                   :accessor es-direct-methods
                   :initform ())))

(defvar *eql-specializers* (make-hash-table))

(defun intern-eql-specializer* (object)
  (or (gethash object *eql-specializers*)
      (sys:with-hash-table-locked *eql-specializers*
        (or (gethash object *eql-specializers*)
            (setf (gethash object *eql-specializers*)
                  (make-instance 'eql-specializer* 'eso object))))))

(cl:defmethod add-direct-method ((specializer eql-specializer*) (method method))
  (pushnew method (es-direct-methods specializer)))

(cl:defmethod remove-direct-method ((specializer eql-specializer*) (method method))
  (removef (es-direct-methods specializer) method))

(cl:defgeneric specializer-direct-generic-functions (specializer)
  (:method ((class class))
   (remove-duplicates
    (mapcar #'method-generic-function
            (specializer-direct-methods class))))
  (:method ((eql-specializer eql-specializer*))
   (remove-duplicates
    (mapcar #'method-generic-function
            (specializer-direct-methods eql-specializer))))
  (:method ((cons cons))
   (specializer-direct-generic-functions
    (intern-eql-specializer*
     (eql-specializer-object cons)))))

;; The following method ensures that remove-method is called.

#-lispworks5
(cl:defmethod add-method :before ((gf standard-generic-function) (method method))
  (when-let (old-method (find-method gf (method-qualifiers method)
                                     (method-specializers method) nil))
    (remove-method gf old-method)))

;; The following two methods ensure that add/remove-direct-method is called,
;; and that the dependent protocol for generic function works.

(cl:defmethod add-method :after ((gf standard-generic-function) (method method))
  (loop for specializer in (method-specializers method)
        if (consp specializer)
        do (add-direct-method
            (intern-eql-specializer*
             (eql-specializer-object specializer))
            method)
        #-lispworks5 else
        #-lispworks5 do
        #-lispworks5 (add-direct-method specializer method))
  #+lispworks4.3
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'add-method method))))

(cl:defmethod remove-method :after ((gf standard-generic-function) (method method))
  (loop for specializer in (method-specializers method)
        if (consp specializer)
        do (remove-direct-method
            (intern-eql-specializer*
             (eql-specializer-object specializer))
            method)
        #-lispworks5 else
        #-lispworks5 do
        #-lispworks5 (remove-direct-method specializer method))
  #+lispworks4.3
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

(cl:defgeneric find-method-combination (gf combi combi-options)
  (:method ((gf generic-function) (combi symbol) combi-options)
   (when combi-options
     (error "This implementation of find-method-combination cannot handle method combination options."))
   (clos::find-a-method-combination-type combi)))

(declaim (inline m-function))

(defun m-function (m)
  (method-function m))

(define-compiler-macro m-function (m)
  (handler-case (method-function m)
    (error () `(the function (method-function (the method ,m))))))

(defun compute-argument-order (gf nof-required-args)
  (loop with specialized-count = (make-array nof-required-args :initial-element 0)
        
        for method in (generic-function-methods gf) do
        (loop for specializer in (method-specializers method)
              for index from 0
              unless (eq specializer (find-class 't))
              do (incf (svref specialized-count index)))

        finally
  
        (loop for arg in (generic-function-argument-precedence-order gf)
              for pos = (position arg (generic-function-lambda-list gf))
              when (> (svref specialized-count pos) 0)
              collect pos into argument-order
              finally (setf (argument-order gf) (coerce argument-order 'simple-vector)))))

(cl:defmethod compute-applicable-methods-using-classes ((gf standard-generic-function) classes)
  (labels ((subclass* (spec1 spec2 arg-spec)
             (let ((cpl (class-precedence-list arg-spec)))
               (declare (type list cpl))
               (find spec2 (the list (cdr (member spec1 cpl :test #'eq))) :test #'eq)))
           (method-more-specific-p (m1 m2)
             (declare (type method m1 m2))
             (loop for n of-type fixnum across (argument-order gf)
                   for spec1 = (nth n (method-specializers m1))
                   for spec2 = (nth n (method-specializers m2))
                   unless (eq spec1 spec2)
                   return (subclass* spec1 spec2 (nth n classes)))))
    (let ((applicable-methods
           (sort
            (loop for method of-type method in (the list (generic-function-methods gf))
                  when (loop for class in classes
                             for specializer in (the list (method-specializers method))
                             if (typep specializer 'eql-specializer)
                             do (when (typep (eql-specializer-object specializer) class)
                                  (return-from compute-applicable-methods-using-classes (values '() nil)))
                             else if (not (subclassp class specializer)) return nil
                             finally (return t))
                  collect method)
            #'method-more-specific-p)))
      (values applicable-methods t))))

(cl:defgeneric make-method-lambda (generic-function method lambda-expression environment)
  (:method ((gf generic-function) (method standard-method) lambda-expression environment)
   (destructuring-bind
       (lambda (&rest args) &body body)
       lambda-expression
     (declare (ignore lambda))
     (loop with documentation = :unbound
           for (car . cdr) = body then cdr
           while (or (and cdr (stringp car))
                     (and (consp car) (eq (car car) 'declare)))
           if (stringp car)
           do (setf documentation
                    (if (eq documentation :unbound) car
                      (error "Too many documentation strings in lambda expression ~S."
                             lambda-expression)))
           else append (loop for declaration in (cdr car) 
                             if (eq (car declaration) 'ignore)
                             collect `(ignorable ,@(cdr declaration))
                             and collect `(dynamic-extent ,@(cdr declaration))
                             else collect declaration) into declarations
           finally (multiple-value-bind
                       (method-lambda method-args)
                       (clos:make-method-lambda
                        gf method args declarations
                        `(progn ,car ,@cdr)
                        environment)
                     (if (eq documentation :unbound)
                       (return (values method-lambda method-args))
                       (return (values
                                `(lambda ,(cadr method-lambda)
                                   ,documentation
                                   ,@(cddr method-lambda))
                                method-args)))))))
  (:method ((gf standard-generic-function) (method standard-method) lambda-expression environment)
   (declare (ignore environment) (optimize (speed 3) (space 0) (compilation-speed 0)))
   (when (only-standard-methods gf)
     (return-from make-method-lambda (call-next-method)))
   (with-unique-names (args next-methods more-args method-function)
     (values
      `(lambda (,args ,next-methods &rest ,more-args)
         (declare (dynamic-extent ,more-args)
                  (ignorable ,args ,next-methods ,more-args))
         (flet ((call-next-method (&rest args)
                  (declare (dynamic-extent args))
                  (if ,next-methods
                    (apply (method-function (first ,next-methods))
                           (if args args ,args) (rest ,next-methods) ,more-args)
                    (apply #'no-next-method
                           (getf ,more-args :generic-function)
                           (getf ,more-args :method)
                           (if args args ,args))))
                (next-method-p () (not (null ,next-methods))))
           (declare (inline call-next-method next-method-p)
                    (ignorable #'call-next-method #'next-method-p))
           (flet ((,method-function ,@(rest lambda-expression)))
             (declare (inline ,method-function))
             (apply #',method-function ,args))))
      '()))))

(cl:defgeneric compute-effective-method-function (gf effective-method options)
  (:method ((gf generic-function) effective-method options)
   (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
   (when options
     (cerror "Ignore these options."
             "This version of compute-effective-method-function does not support method combination options: ~S"
             options))
   (let ((all-t-specializers (required-args (generic-function-lambda-list gf)
                                            (constantly (find-class 't))))
         (args (gensym)))
     (labels ((transform-effective-method (form)
                (if (atom form) form
                  (case (car form)
                    (call-method (transform-effective-method
                                  (let ((the-method (transform-effective-method (cadr form)))
                                        (method-var (gensym)))
                                    `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
                                       (let ((,method-var ,the-method))
                                         (declare (ignorable ,method-var))
                                         (funcall (m-function ,(if (typep the-method 'method)
                                                                 the-method method-var))
                                                  ,args
                                                  ,@(let ((subforms
                                                           (loop for subform in (the list (cddr form))
                                                                 collect `',subform)))
                                                      (if subforms subforms '(())))
                                                  :generic-function ,gf
                                                  :method ,(if (typep the-method 'method)
                                                             the-method method-var)))))))
                    (make-method (when (cddr form)
                                   (error "Incorrect make-method form: ~S." form))
                                 (multiple-value-bind
                                     (method-lambda method-options)
                                     (make-method-lambda
                                      gf (class-prototype (generic-function-method-class gf))
                                      `(lambda (&rest ,args)
                                         (declare (dynamic-extent ,args) (ignorable ,args))
                                         ,(transform-effective-method (cadr form))) nil)
                                   (apply #'make-instance
                                          (generic-function-method-class gf)
                                          :qualifiers '()
                                          :specializers all-t-specializers
                                          :lambda-list (generic-function-lambda-list gf)
                                          :function (compile nil method-lambda)
                                          method-options)))
                    (t (mapcar #'transform-effective-method (the list form)))))))
       (let ((emf-lambda `(lambda (&rest ,args)
                            (declare (dynamic-extent ,args) (ignorable ,args))
                            ,(transform-effective-method effective-method))))
         (multiple-value-bind (function warnings failure)
             (compile nil emf-lambda)
           (declare (ignore warnings))
           (assert (not failure))
           function))))))

(defun get-emf (gf args nof-required-args)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (let ((applicable-methods (compute-applicable-methods gf (subseq args 0 nof-required-args))))
    (if applicable-methods
      (multiple-value-bind
          (effective-method options)
          (compute-effective-method
           gf (generic-function-method-combination gf)
           applicable-methods)
        (compute-effective-method-function gf effective-method options))
      (lambda (&rest args)
        (declare (dynamic-extent args))
        (apply #'no-applicable-method gf args)))))

(defun get-emf-using-classes (gf args classes nof-required-args)
  (declare (type generic-function gf) (type list args classes)
           (optimize (speed 3) (space 0) (compilation-speed 0)))
  (multiple-value-bind
      (applicable-methods validp)
      (compute-applicable-methods-using-classes gf classes)
    (unless validp
      (setq applicable-methods
            (compute-applicable-methods gf (subseq args 0 nof-required-args))))
    (values
     (if applicable-methods
       (multiple-value-bind
           (effective-method options)
           (compute-effective-method
            gf (generic-function-method-combination gf)
            applicable-methods)
         (compute-effective-method-function gf effective-method options))
       (lambda (&rest args)
         (declare (dynamic-extent args))
         (apply #'no-applicable-method gf args)))
     validp)))

(defvar *standard-gfs*
  (list #'compute-applicable-methods #'compute-applicable-methods-using-classes
        #'compute-effective-method #'compute-effective-method-function
        #'make-method-lambda))

(defun only-standard-methods (gf &rest other-gfs)
  (declare (dynamic-extent other-gfs) (optimize (speed 3) (space 0) (compilation-speed 0)))
  (loop for other-gf in (or other-gfs *standard-gfs*)
        always (loop for method in (generic-function-methods other-gf)
                     for specializer = (first (method-specializers method))
                     if (and (typep specializer 'class)
                             (subclassp specializer (find-class 'standard-generic-function))
                             (not (eq specializer (find-class 'standard-generic-function)))
                             (typep gf specializer))
                     return nil
                     else if (and (typep specializer 'eql-specializer)
                                  (eql (eql-specializer-object specializer) gf))
                     return nil
                     finally (return t))))

(defun methods-all-the-same-specializers (gf)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (loop with template = (first (generic-function-methods gf))
        for method in (rest (generic-function-methods gf))
        always (loop for spec1 in (method-specializers template)
                     for spec2 in (method-specializers method)
                     always (etypecase spec1
                              (class (etypecase spec2
                                       (class (eq spec1 spec2))
                                       (eql-specializer nil)))
                              (eql-specializer
                               (etypecase spec2
                                 (class nil)
                                 (eql-specializer
                                  (eql (eql-specializer-object spec1)
                                       (eql-specializer-object spec2)))))))))

(cl:defmethod compute-discriminating-function ((gf standard-generic-function))
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (let ((nof-required-args (length (required-args (generic-function-lambda-list gf))))
        discriminator)
    (compute-argument-order gf nof-required-args)
    (flet ((discriminate (emf-setter args &optional (classes (loop for arg in args
                                                                   repeat nof-required-args
                                                                   collect (class-of arg))))
             (declare (type list args classes) (type function emf-setter))
             (multiple-value-bind (emf validp) (get-emf-using-classes gf args classes nof-required-args)
               (funcall emf-setter (if validp emf (lambda (&rest args)
                                                    (declare (dynamic-extent args))
                                                    (apply (the function (get-emf gf args nof-required-args)) args))))
               (apply (the function emf) args))))
      (when (only-standard-methods gf #'compute-applicable-methods #'compute-applicable-methods-using-classes)
        (setq discriminator
              (if (only-standard-methods gf #'compute-effective-method #'compute-effective-method-function #'make-method-lambda)
                (call-next-method)
                (cond ((null (generic-function-methods gf))
                       (lambda (&rest args)
                         (declare (dynamic-extent args))
                         (apply #'no-applicable-method gf args)))
                      ((methods-all-the-same-specializers gf)
                       (let ((specializers (method-specializers (first (generic-function-methods gf))))
                             (effective-method-function nil))
                         (declare (type list specializers))
                         (lambda (&rest args)
                           (declare (dynamic-extent args) (optimize (speed 3) (safety 0) (debug 0)
                                                                    (compilation-speed 0)))
                           (cond ((loop for arg in args
                                        for spec in specializers
                                        always (etypecase spec
                                                 (class (typep arg spec))
                                                 (eql-specializer (eql arg (eql-specializer-object spec)))))
                                  (if effective-method-function
                                    (apply (the function effective-method-function) args)
                                    (discriminate (lambda (emf) (setq effective-method-function emf)) args)))
                                 (t (apply #'no-applicable-method gf args))))))
                      ((= (length (argument-order gf)) 1)
                       (let ((dispatch-argument-index (svref (argument-order gf) 0))
                             (emfs (make-hash-table :test #'eq)))
                         (declare (type hash-table emfs) (type fixnum dispatch-argument-index))
                         (lambda (&rest args)
                           (declare (dynamic-extent args) (optimize (speed 3) (safety 0) (debug 0)
                                                                    (compilation-speed 0)))
                           (let* ((dispatch-class (class-of (nth dispatch-argument-index args)))
                                  (effective-method-function (gethash dispatch-class emfs)))
                             (if effective-method-function
                               (apply (the function effective-method-function) args)
                               (discriminate (lambda (emf) (setf (gethash dispatch-class emfs) emf)) args))))))))))
      (if discriminator discriminator
        (let ((emfs (make-hash-table :test #'equal)))
          (declare (type hash-table emfs))
          (lambda (&rest args)
            (declare (dynamic-extent args) (optimize (speed 3) (safety 0) (debug 0)
                                                     (compilation-speed 0)))
            (let* ((classes (loop for arg in args
                                  repeat nof-required-args
                                  collect (class-of arg)))
                   (effective-method-function (gethash (the list classes) emfs)))
              (if effective-method-function
                (apply (the function effective-method-function) args)
                (discriminate (lambda (emf) (setf (gethash (the list classes) emfs) emf)) args classes)))))))))

(defmacro defgeneric (&whole form name (&rest args) &body options)
  (unless (every #'consp options)
    (error "Illegal generic function options in defgeneric form ~S." form))
  (let ((non-standard (member :generic-function-class options :key #'car :test #'eq))
        (options-without-methods (remove :method options :key #'car :test #'eq)))
    `(progn
       (let ((generic-function (ignore-errors (fdefinition ',name))))
         (when (and generic-function (typep generic-function 'standard-generic-function))
           (loop for method in (slot-value generic-function 'initial-methods)
                 do (remove-method generic-function method))))
       ,(if non-standard
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (cl:defgeneric ,name ,args ,@options-without-methods))
          `(progn
             (eval-when (:compile-toplevel)
               (cl:defgeneric ,name ,args ,@options-without-methods))
             (eval-when (:load-toplevel :execute)
               (let ((dspec:*redefinition-action* :quiet))
                 (cl:defgeneric ,name ,args ,@options)))))
       (let ((generic-function (fdefinition ',name)))
         ,(when non-standard
            `(setf (slot-value generic-function 'initial-methods)
                   (list ,@(loop for method-spec in (remove :method options :key #'car :test-not #'eq)
                                 collect `(defmethod ,name ,@(cdr method-spec))))))
         generic-function))))

(defun create-gf-lambda-list (method-lambda-list)
  (loop with stop-keywords = '#.(remove '&optional lambda-list-keywords)
        for arg in method-lambda-list
        until (member arg stop-keywords)
        collect arg into gf-lambda-list
        finally (return (let (rest)
                          (cond ((member '&key method-lambda-list)
                                 (nconc gf-lambda-list '(&key)))
                                ((setq rest (member '&rest method-lambda-list))
                                 (nconc gf-lambda-list (subseq rest 0 2)))
                                (t gf-lambda-list))))))

(defun extract-specializers (specialized-args form)
  (loop for specializer-name in (extract-specializer-names specialized-args)
        collect (typecase specializer-name
                  (symbol `(find-class ',specializer-name))
                  (class specializer-name)
                  (cons (cond
                         ((> (length specializer-name) 2)
                          (error "Invalid specializer ~S in defmethod form ~S."
                                 specializer-name form))
                         ((eq (car specializer-name) 'eql)
                          `(intern-eql-specializer ,(cadr specializer-name)))
                         (t (error "Invalid specializer ~S in defmethod form ~S."
                                   specializer-name form))))
                  (t (error "Invalid specializer ~S in defmethod form ~S."
                            specializer-name form)))))

(defun load-method (name gf-lambda-list type qualifiers specializers lambda-list function options)
  (let* ((gf (if (fboundp name) (fdefinition name)
               (ensure-generic-function name :lambda-list gf-lambda-list :generic-function-class type)))
         (method (apply #'make-instance
                        (generic-function-method-class gf)
                        :qualifiers qualifiers
                        :specializers specializers
                        :lambda-list lambda-list
                        :function function
                        options)))
    (add-method gf method)
    method))

(define-condition defmethod-without-generic-function (style-warning)
  ((name :initarg :name :reader dwg-name))
  (:report (lambda (c s) (format s "No generic function present when encountering a defmethod for ~S. Assuming it will be an instance of standard-generic-function." (dwg-name c)))))

(define-symbol-macro warn-on-defmethod-without-generic-function t)

(defmacro defmethod (&whole form name &body body &environment env)
  (loop with generic-function = (when (fboundp name) (fdefinition name))
        
        initially
        (when (macroexpand 'warn-on-defmethod-without-generic-function env)
          (unless generic-function
            (warn 'defmethod-without-generic-function :name name)))
        (unless (typep generic-function 'standard-generic-function)
          (return-from defmethod `(cl:defmethod ,@(cdr form))))
        (when (only-standard-methods generic-function)
          (return-from defmethod `(cl:defmethod ,@(cdr form))))

        for tail = body then (cdr tail)
        until (listp (car tail))
        collect (car tail) into qualifiers
        finally
        (destructuring-bind
            ((&rest specialized-args) &body body) tail
          (loop with documentation = :unbound
                for (car . cdr) = body then cdr
                while (or (stringp car)
                          (and (consp car) (eq (car car) 'declare)))
                if (stringp car)
                do (setq documentation
                         (if (eq documentation :unbound) car
                           (error "Too many documentation strings for defmethod form ~S." form)))
                else append (cdr car) into declarations
                finally
                (let* ((lambda-list (extract-lambda-list specialized-args))
                       (gf-lambda-list (create-gf-lambda-list lambda-list))
                       (specializers (extract-specializers specialized-args form)))
                  (multiple-value-bind
                      (method-lambda method-options)
                      (make-method-lambda generic-function
                                          (class-prototype (generic-function-method-class generic-function))
                                          `(lambda ,lambda-list
                                             (declare ,@declarations)
                                             (declare (ignorable ,@(loop for arg in specialized-args
                                                                         until (member arg lambda-list-keywords)
                                                                         when (consp arg) collect (car arg))))
                                             (block ,(if (consp name) (cadr name) name) ,car ,@cdr))
                                          env)
                    (return-from defmethod
                      `(load-method ',name ',gf-lambda-list ',(type-of generic-function)
                                    ',qualifiers (list ,@specializers) ',lambda-list
                                    (function ,method-lambda) ',method-options))))))))


(defun ensure-method (gf lambda-expression 
                         &key (method-class (generic-function-method-class gf))
                         (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly (find-class 't)))))
  (multiple-value-bind
      (method-lambda method-args)
      (make-method-lambda
       gf (class-prototype method-class)
       lambda-expression ())
    (let ((method (apply #'make-instance
                         method-class
                         :qualifiers qualifiers
                         :lambda-list lambda-list
                         :specializers specializers
                         :function (compile nil method-lambda)
                         method-args)))
      (add-method gf method)
      method)))


;; The following can be used in direct-slot-definition-class to get the correct initargs
;; for a slot. Use it like this:
;;
;; (defmethod direct-slot-definition-class
;;            ((class my-standard-class) &rest initargs)
;;   (declare (dynamic-extent initargs))
;;   (destructuring-bind
;;       (&key key-of-interest &allow-other-keys)
;;       (fix-slot-initargs initargs)
;;     ...))

(defvar *standard-slot-keys*
  '(:name :documentation
    :initargs :initform :initfunction
    :readers :writers))

(defun fix-slot-initargs (initargs)
  initargs)

;; Provide standard-instance-access and funcallable-standard-instance-access

(declaim (inline standard-instance-access
                 (setf standard-instance-access)))

(defun standard-instance-access (instance location)
  (clos::fast-standard-instance-access instance location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (clos::fast-standard-instance-access instance location) new-value))

(declaim (inline funcallable-instance-access))

(defun funcallable-instance-access (instance location &rest args)
  (declare (dynamic-extent args))
  (let* ((class (class-of instance))
         (slot (find location (class-slots class)
                     :key #'slot-definition-location)))
    (if slot
      (apply #'clos::funcallable-instance-access instance (slot-definition-name slot) args)
      (error "There is no slot with location ~S for instance ~S." location instance))))

(defun funcallable-standard-instance-access (instance location)
  (funcallable-instance-access instance location))

(defun (setf funcallable-standard-instance-access) (new-value instance location)
  (funcallable-instance-access instance location new-value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
