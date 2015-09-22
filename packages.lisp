(defpackage :util
  (:use :cl)
  (:export :letitbe :with-gensyms :it :concat-str :awhile :last1 :in))

(defpackage :abackquote
  (:use :cl :util))
