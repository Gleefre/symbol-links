(in-package #:symbol-links)

;;; HACK!

(defparameter *hacked!* nil)

(defconstant +%find-symbol+ (fdefinition 'sb-impl::%find-symbol))

(defun enable-hack! ()
  (unless *hacked!*
    (sb-ext:with-unlocked-packages (#:sb-impl)
      (let ((sb-ext:*muffled-warnings* 'sb-kernel:redefinition-warning))
        (defun sb-impl::%find-symbol (string length package)
          (multiple-value-bind (symbol where)
              (funcall +%find-symbol+ string length package)
            (if (and *follow-symbol-links* where (symbolp symbol))
                (values (%follow-symbol-link symbol) where)
                (values symbol where))))))
    (setf *hacked!* t)))

(defun disable-hack! ()
  (when *hacked!*
    (sb-ext:with-unlocked-packages (#:sb-impl)
      (defun sb-impl::%find-symbol (string length package)
        (funcall +%find-symbol+ string length package)))
    (setf *hacked!* nil)
    t))
