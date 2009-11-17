(in-package :closer-mop)

;; Some internal utility functions.

(define-modify-macro nconcf (&rest lists) nconc)

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

;; We need a new standard-class for various things.

(defclass standard-class (cl:standard-class)
  ())

;; validate-superclass for metaclass classes is a little bit
;; more tricky than for class metaobject classes because
;; we don't want to make all standard-classes compatible to
;; each other.

;; Our validate-superclass may get passed a class-prototype
;; as its second argument, so don't expect its readers to
;; yield useful information. (In ANSI parlance, "the
;; consequences are undefined...")

(defmethod validate-superclass
           ((class standard-class)
            (superclass cl:standard-class))
  (or (when (eq (class-of class) (find-class 'standard-class))
        (or (eq (class-of superclass) (find-class 'cl:standard-class))
            (eq (class-of superclass) (find-class 'standard-class))))
      (call-next-method)
      (when (eq (class-of superclass) (find-class 'cl:standard-class))
        (validate-superclass class (class-prototype (find-class 'standard-class))))))

(defmethod ccl::create-reader-method-function
           ((class standard-class)
            (reader-method-class standard-reader-method)
            (dslotd standard-direct-slot-definition))
  (let ((slot-name (slot-definition-name dslotd)))
    (compile nil `(lambda (object) (slot-value object ',slot-name)))))

(defmethod ccl::create-writer-method-function
             ((class standard-class)
              (writer-method-class standard-writer-method)
              (dslotd standard-direct-slot-definition))
    (let ((slot-name (slot-definition-name dslotd)))
      (compile nil `(lambda (new-value object)
                      (setf (slot-value object ',slot-name) new-value)))))

(defgeneric typep (object type)
  (:method (object type)
   (cl:typep object type))
  (:method (object (type class))
   (member (class-of object)
           (class-precedence-list type))))
  
(defgeneric subtypep (type1 type2)
  (:method (type1 type2)
   (cl:subtypep type1 type2))
  (:method ((type1 class) (type2 symbol))
   (let ((class2 (find-class type2 nil)))
     (if class2
       (member class2 (class-precedence-list type1))
       (cl:subtypep type1 type2))))
  (:method ((type1 symbol) (type2 class))
   (let ((class1 (find-class type1 nil)))
     (if class1
       (member type2 (class-precedence-list class1))
       (cl:subtypep type1 type2))))
  (:method ((type1 class) (type2 class))
   (member type2 (class-precedence-list type1))))

(defun ensure-method (gf lambda-expression 
                         &key (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly (find-class 't)))))
  (eval `(defmethod ,(generic-function-name gf) ,@qualifiers
           ,(loop for specializer in specializers
                  for (arg . rest) on lambda-list
                  collect `(,arg ,specializer) into args
                  finally (return (nconc args rest)))
           ,@(cddr lambda-expression))))

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

(defun fix-slot-initargs (initargs) initargs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
