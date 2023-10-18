(in-package #:symbol-links)

;;; HACK!

(defparameter *hacked!* nil)

(defconstant +%find-symbol+ (if (boundp '+%find-symbol+)
                                (symbol-value '+%find-symbol+)
                                (fdefinition 'ccl::%find-symbol)))

(defun enable-hack! ()
  (unless *hacked!*
    (let ((ccl:*warn-if-redefine-kernel* nil))
      (defun ccl::%find-symbol (string length package)
        (multiple-value-bind (symbol where internal-offset external-offset)
            (funcall +%find-symbol+ string length package)
          (if (and *follow-symbol-links* where (symbolp symbol))
              (values (%follow-symbol-link symbol) where internal-offset external-offset)
              (values symbol where internal-offset external-offset)))))
    (setf *hacked!* t)))

(defun disable-hack! ()
  (when *hacked!*
    (let ((ccl:*warn-if-redefine-kernel* nil))
      (defun ccl::%find-symbol (string length package)
        (funcall +%find-symbol+ string length package)))
    (setf *hacked!* nil)
    t))
