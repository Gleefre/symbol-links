(in-package #:symbol-links)

(defmacro without-symbol-links (&body body)
  `(let ((*follow-symbol-links* nil))
     ,@body))

(defmacro link (from to)
  `(add-symbol-link ',from ',to))

(defmacro unlink (symbol)
  `(remove-symbol-link ',symbol))

(defmacro relink (from to)
  `(progn
     (remove-symbol-link ',from)
     (add-symbol-link ',from ',to)))
