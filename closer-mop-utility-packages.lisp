(in-package :cl-user)

(macrolet ((define-closer-common-lisp-package ()
             (loop with symbols = (nunion (loop for sym being the external-symbols of :common-lisp
                                                if (find-symbol (symbol-name sym) :c2mop)
                                                collect it
                                                else collect sym)
                                          (loop for sym being the external-symbols of :c2mop
                                                collect sym))
                   with map = '()
                   for symbol in symbols do
                   (push (symbol-name symbol)
                         (getf map (symbol-package symbol)))
                   finally (return 
                            `(defpackage #:closer-common-lisp
                               (:nicknames #:c2cl)
                               (:use)
                               ,@(loop for (package symbols) on map by #'cddr
                                       collect `(:import-from ,(package-name package) ,@symbols))
                               (:export ,@(mapcar #'symbol-name symbols)))))))
  (define-closer-common-lisp-package))

(defpackage #:closer-common-lisp-user
  (:nicknames #:c2cl-user)
  (:use #:closer-common-lisp))
