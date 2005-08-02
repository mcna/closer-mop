(in-package :closer-mop)

;; We need a new standard-generic-function for various things.

(cl:defclass standard-generic-function (cl:standard-generic-function)
  ()
  (:metaclass clos:funcallable-standard-class))

;; The following ensures that the new standard-generic-function is used.

(defun ensure-generic-function
       (name &rest args
             &key (generic-function-class 'standard-generic-function)
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

;; The following macro ensures that the new standard-generic-function
;; is used by default.

(defmacro defgeneric (name (&rest args) &body options)
  (if (member :generic-function-class options :key #'car)
      `(cl:defgeneric ,name ,args ,@options)
    `(cl:defgeneric ,name ,args ,@options
       (:generic-function-class standard-generic-function))))

;;; The following three methods ensure that the dependent protocol
;;; for generic function works.

;; The following method additionally ensures that
;; compute-discriminating-function is triggered.

(cl:defmethod reinitialize-instance :after
  ((gf standard-generic-function) &rest initargs)
  (declare (dynamic-extent initargs))
  (set-funcallable-instance-function
   gf (compute-discriminating-function gf))
  (map-dependents
   gf (lambda (dep) (apply #'update-dependent gf dep initargs))))

(cl:defmethod add-method :after
  ((gf standard-generic-function) method)
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'add-method method))))

(cl:defmethod remove-method :after
  ((gf standard-generic-function) method)
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

(defun ensure-method (gf lambda-expression 
                         &key (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (loop for arg in lambda-list
                                             until (member arg lambda-list-keywords)
                                             collect (find-class 't))))
  (funcall (compile nil `(lambda ()
                           (defmethod ,(generic-function-name gf) ,@qualifiers
                             ,(loop for (arg . rest) on lambda-list
                                    for specializer in specializers
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
