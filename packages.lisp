(defpackage :util
  (:use :cl)
  (:export :let-it-be :with-gensyms :it :concat-str :awhile :last1 :in))

(defpackage :abackquote
  (:use :cl :util)
  (:export :enable-abackquote))
