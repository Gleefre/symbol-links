(in-package #:symbol-links)

;;; HACK! [ thanks to phoe ]

(defparameter *hacked!* nil)

(defconstant +%find-symbol+ (fdefinition 'system::find-symbol*))

(defun enable-hack! ()
  (unless *hacked!*
    (let ((lispworks:*handle-warn-on-redefinition* nil))
      (defun system::find-symbol* (string length package)
        (multiple-value-bind (symbol where)
            (funcall +%find-symbol+ string length package)
          (if (and *follow-symbol-links* where (symbolp symbol))
              (values (%follow-symbol-link symbol) where)
              (values symbol where)))))
    (setf *hacked!* t)))

(defun disable-hack! ()
  (when *hacked!*
    (let ((lispworks:*handle-warn-on-redefinition* nil))
      (defun system::find-symbol* (string length package)
        (funcall +%find-symbol+ string length package)))
    (setf *hacked!* nil)
    t))
