(in-package :closer-mop)

(defun ensure-method (gf lambda-expression 
                         &key (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (loop for arg in lambda-list
                                             until (member arg lambda-list-keywords)
                                             collect (find-class 't))))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
