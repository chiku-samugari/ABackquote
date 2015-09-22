;;; abackquote.asd
;;; system definition for ABackquote
;;;
;;; Author: chiku (Takehiko Nawata, samugari.penguin@gmail.com)
;;; License: MIT License
(defpackage :abackquote.asd
  (:use :cl :asdf))

(in-package :abackquote.asd)

(defsystem abackquote
  :name "ABackquote"
  :version "0.6.0"
  :maintainer "Takehiko Nawata"
  :author "Takehiko Nawata"
  :license "MIT License"
  :description "Extended version of Sharp-Backquote introduced in ``Let Over the Lambda''"
  :long-description "Extended version of Sharp-Backquote introduced in ``Let Over the Lambda''"
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "abackquote")))
