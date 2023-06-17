(defpackage #:symbol-links
  (:use #:cl)
  (:export #:enable-hack!
           #:disable-hack!
           #:add-symbol-link
           #:remove-symbol-link
           #:symbol-link
           #:symbol-linked-by
           #:*follow-symbol-links*))

(in-package #:symbol-links)

;;; Conditions

(define-condition symbol-link-error (error)
  ((symbol :reader symbol-link-symbol :initarg :symbol)
   (link :reader symbol-link-link :initarg :link)))

(define-condition cyclic-symbol-link (symbol-link-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
             "Attempt to create a cyclic symbol link (~S~{ -> ~S~})"
             (symbol-link-symbol condition)
             (loop for link = (symbol-link-link condition)
                   then (symbol-link link)
                   collect link
                   until (eq link (symbol-link-symbol condition)))))))

(define-condition conflicting-symbol-link (symbol-link-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
             "Conflicting link already exists (~S -> ~S, not ~S)"
             (symbol-link-symbol condition)
             (symbol-link (symbol-link-symbol condition))
             (symbol-link-link condition)))))

;;; Links

(defparameter *symbol->symbol-link* (make-hash-table))
(defparameter *symbol->symbol-linked-by* (make-hash-table))

(defun %symbol-link (symbol)
  (gethash symbol *symbol->symbol-link*))

(defun (setf %symbol-link) (value symbol)
  (if (symbolp value)
      (setf (gethash symbol *symbol->symbol-link*) value)
      (remhash symbol *symbol->symbol-link*)))

(defun %symbol-linked-by (symbol)
  (gethash symbol *symbol->symbol-linked-by*))

(defun (setf %symbol-linked-by) (value symbol)
  (setf (gethash symbol *symbol->symbol-linked-by*) value))

(defun %remove-symbol-link (symbol)
  (multiple-value-bind (link link-p) (%symbol-link symbol)
    (when link-p
      (setf (%symbol-linked-by link) (remove symbol (%symbol-linked-by link)))
      (setf (%symbol-link symbol) 0)
      t)))

(defun %add-symbol-link (from to)
  (multiple-value-bind (link link-p) (%symbol-link from)
    (when link-p
      (if (eq link to)
          (return-from %add-symbol-link nil)
          (restart-case
              (error 'conflicting-symbol-link :symbol from :link to)
            (continue ()
              :report "Continue (link is not created)"
              (return-from %add-symbol-link nil))
            (drop ()
              :report "Remove existing link"
              (%remove-symbol-link from))))))
  (if (loop for sym = to then (%symbol-link sym)
            thereis (eq from sym)
            while (nth-value 1 (%symbol-link sym)))
      (restart-case
          (error 'cyclic-symbol-link :symbol from :link to)
        (continue ()
          :report "Continue (link is not created)"))
      (progn
        (setf (%symbol-link from) to)
        (push from (%symbol-linked-by to))
        t)))

(defun %follow-symbol-link (symbol)
  (loop
    (multiple-value-bind (link link-p) (%symbol-link symbol)
      (unless link-p (return symbol))
      (setf symbol link))))

;;; API

(defun add-symbol-link (from to)
  (check-type from symbol)
  (check-type to symbol)
  (%add-symbol-link from to))

(defun remove-symbol-link (symbol)
  (check-type symbol symbol)
  (%remove-symbol-link symbol))

(defun symbol-link (symbol)
  (check-type symbol symbol)
  (%symbol-link symbol))

(defun symbol-linked-by (symbol)
  (check-type symbol symbol)
  (nth-value 0 (%symbol-linked-by symbol)))

(defparameter *follow-symbol-links* t)

;;; HACK!

(defparameter *hacked!* nil)

(defconstant +%find-symbol+ (fdefinition 'sb-impl::%find-symbol))

(defun enable-hack! ()
  (unless *hacked!*
    (sb-ext:with-unlocked-packages (#:sb-impl)
      (defun sb-impl::%find-symbol (string length package)
        (multiple-value-bind (symbol where)
            (funcall +%find-symbol+ string length package)
          (if (and *follow-symbol-links* where (symbolp symbol))
              (values (%follow-symbol-link symbol) where)
              (values symbol where)))))
    (setf *hacked!* t)))

(defun disable-hack! ()
  (when *hacked!*
    (sb-ext:with-unlocked-packages (#:sb-impl)
      (defun sb-impl::%find-symbol (string length package)
        (funcall +%find-symbol+ string length package)))
    (setf *hacked!* nil)))
