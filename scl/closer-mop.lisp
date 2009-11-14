(in-package :closer-mop)

(define-modify-macro nconcf (&rest lists) nconc)

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

(declaim (inline classp))

(defun classp (thing)
  (typep thing 'class))

(cl:defgeneric add-direct-method (specializer method)
  (:method ((specializer standard-object) (method method))))

(cl:defgeneric remove-direct-method (specializer method)
  (:method ((specializer standard-object) (method method))))

(defvar *dependents* (make-hash-table :test #'eq))

(cl:defgeneric add-dependent (metaobject dependent)
  (:method ((metaobject standard-class) dependent)
    (pushnew dependent (gethash metaobject *dependents*)))
  (:method ((metaobject funcallable-standard-class) dependent)
    (pushnew dependent (gethash metaobject *dependents*)))
  (:method ((metaobject standard-generic-function) dependent)
    (pushnew dependent (gethash metaobject *dependents*))))

(cl:defgeneric remove-dependent (metaobject dependent)
  (:method ((metaobject standard-class) dependent)
    (setf (gethash metaobject *dependents*)
	  (delete metaobject (gethash metaobject *dependents*))))
  (:method ((metaobject funcallable-standard-class) dependent)
    (setf (gethash metaobject *dependents*)
	  (delete metaobject (gethash metaobject *dependents*))))
  (:method ((metaobject standard-generic-function) dependent)
    (setf (gethash metaobject *dependents*)
	  (delete metaobject (gethash metaobject *dependents*)))))

(cl:defgeneric map-dependents (metaobject function)
  (:method ((metaobject standard-class) function)
    (mapc function (gethash metaobject *dependents*)))
  (:method ((metaobject funcallable-standard-class) function)
    (mapc function (gethash metaobject *dependents*)))
  (:method ((metaobject standard-generic-function) function)
    (mapc function (gethash metaobject *dependents*))))

(cl:defgeneric update-dependent (metaobject dependent &rest initargs))

(cl:defmethod reinitialize-instance :after ((metaobject metaobject) &rest initargs)
  (declare (dynamic-extent initargs))
  (map-dependents
   metaobject (lambda (dep) (apply #'update-dependent metaobject dep initargs))))

(cl:defmethod add-method :after
  ((gf standard-generic-function) method)
  (loop for specializer in (method-specializers method)
        do (add-direct-method specializer method))
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'add-method method))))

(cl:defmethod remove-method :after
  ((gf standard-generic-function) method)
  (loop for specializer in (method-specializers method)
	do (remove-direct-method specializer method))
  (map-dependents
   gf (lambda (dep) (update-dependent gf dep 'remove-method method))))

(define-condition defmethod-without-generic-function (style-warning)
  ((name :initarg :name :reader dwg-name))
  (:report (lambda (c s) (format s "No generic function present when encountering a defmethod for ~S. Assuming it will be an instance of standard-generic-function." (dwg-name c)))))

(define-symbol-macro warn-on-defmethod-without-generic-function t)

(defmacro defmethod (&whole form name &body body &environment env)
  (declare (ignore body))
  (let ((generic-function (when (fboundp name) (fdefinition name))))
    (when (macroexpand 'warn-on-defmethod-without-generic-function env)
      (unless generic-function
	(warn 'defmethod-without-generic-function :name name)))
    `(cl:defmethod ,@(cdr form))))

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
  (let* ((counts (loop with counts
		       for (key nil) on initargs by #'cddr
		       do (incf (getf counts key 0))
		       finally (return counts)))
	 (keys-to-fix (loop for (key value) on counts by #'cddr
			    if (> value 1) collect key)))
    (if keys-to-fix
	(let ((multiple-standard-keys
	       (intersection keys-to-fix *standard-slot-keys*)))
	  (if multiple-standard-keys
	      (error "Too many occurences of ~S in slot initargs ~S."
		     multiple-standard-keys initargs)
	      (loop with fixed-keys
		    for (key value) on initargs by #'cddr
		    if (member key keys-to-fix)
		    do (nconcf (getf fixed-keys key) (list value))
		    else nconc (list key value) into fixed-initargs
		    finally (return (nconc fixed-initargs fixed-keys)))))
	initargs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
