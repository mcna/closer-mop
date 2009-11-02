(in-package :closer-mop)

;; Some utility functions.

(defun classp (thing)
  (typep thing 'class))

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

(defun ensure-method (gf lambda-expression 
                         &key (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly 't))))
  (funcall (compile nil `(lambda () 
                           (defmethod ,(generic-function-name gf) ,@qualifiers
                             ,(loop for specializer in specializers
                                    for (arg . rest) on lambda-list
                                    collect `(,arg ,specializer) into args
                                    finally (return (nconc args rest)))
                             ,@(cddr lambda-expression))))))

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


;;;; New generic functions.

(declaim (inline m-function))

(defun m-function (m)
  (method-function m))

(define-compiler-macro m-function (m)
  (handler-case (method-function m)
    (error () `(the function (method-function real-method-function (the method ,m))))))

(defclass standard-generic-function (cl:standard-generic-function)
  ((initial-methods :initform '()))
  (:metaclass funcallable-standard-class))

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
              finally (return-from compute-argument-order
                        (coerce argument-order 'simple-vector)))))

(cl:defgeneric make-method-lambda (generic-function method lambda-expression environment)
  (:method ((gf standard-generic-function) (method standard-method) lambda-expression environment)
   (declare (ignore environment) (optimize (speed 3) (space 0) (compilation-speed 0)))
   (let ((args (gensym)) (next-methods (gensym))
         (more-args (gensym)) (method-function (gensym)))
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
   (when (or (cdr (assoc :arguments options))
             (cdr (assoc :duplicates options)))
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
        #'compute-effective-method #'compute-effective-method-function))

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
              (if (only-standard-methods gf #'compute-effective-method #'compute-effective-method-function)
                (call-next-method)
                (let ((argument-order (compute-argument-order gf nof-required-args)))
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
                        ((= (length argument-order) 1)
                         (let ((dispatch-argument-index (svref argument-order 0))
                               (emfs (make-hash-table :test #'eq)))
                           (declare (type hash-table emfs) (type fixnum dispatch-argument-index))
                           (lambda (&rest args)
                             (declare (dynamic-extent args) (optimize (speed 3) (safety 0) (debug 0)
                                                                      (compilation-speed 0)))
                             (let* ((dispatch-class (class-of (nth dispatch-argument-index args)))
                                    (effective-method-function (gethash dispatch-class emfs)))
                               (if effective-method-function
                                 (apply (the function effective-method-function) args)
                                 (discriminate (lambda (emf) (setf (gethash dispatch-class emfs) emf)) args)))))))))))
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
               (cl:defgeneric ,name ,args ,@options))))
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

(defmacro defmethod (&whole form name &body body &environment env)
  (loop with generic-function = (when (fboundp name) (fdefinition name))
        
        initially
        (unless generic-function
          (warn "No generic function ~S present when encountering macroexpansion of defmethod. Assuming it will be an instance of standard-generic-function." name))
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
