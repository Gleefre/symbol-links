(in-package #:symbol-links)

;;; HACK!

(defparameter *hacked!* nil)

(defconstant +%find-symbol+ (if (boundp '+%find-symbol+)
                                (symbol-value '+%find-symbol+)
                                (fdefinition 'lisp::find-symbol*)))

(defun enable-hack! ()
  (unless *hacked!*
    (ext:without-package-locks
      (defun lisp::find-symbol* (string length package)
        (multiple-value-bind (symbol where)
            (funcall +%find-symbol+ string length package)
          (if (and *follow-symbol-links* where (symbolp symbol))
              (values (%follow-symbol-link symbol) where)
              (values symbol where)))))
    (setf *hacked!* t)))

(defun disable-hack! ()
  (when *hacked!*
    (ext:without-package-locks
      (defun lisp::find-symbol* (string length package)
        (funcall +%find-symbol+ string length package)))
    (setf *hacked!* nil)
    t))
