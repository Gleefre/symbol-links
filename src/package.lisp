(defpackage #:symbol-links
  (:use #:cl)
  (:export #:enable-hack!
           #:disable-hack!
           #:add-symbol-link
           #:remove-symbol-link
           #:symbol-link
           #:symbol-linked-by
           #:*follow-symbol-links*)
  (:export #:without-symbol-links
           #:link
           #:unlink
           #:relink))
