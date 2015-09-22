;;;; Sharp backquote reader macro
;;; The #` reader macro introduced in Let Over the Lambda is an
;;; excellent stuff. However, implicit insertion of anaphoras is more
;;; preferable.
(in-package :abackquote)

(defun terminating-character-p (ch)
  (or (in ch #\Tab #\Newline #\Linefeed #\Page #\Return #\Space)
      ;;; Actually speaking, TERMINATING-MACRO-CHARACTER-P is desirable
      ;;; here. But it is conceptually impossible in Common Lisp because
      ;;; Common Lisp does not offer any way to check if a macro
      ;;; character is terminating or non-terminating.
      (get-macro-character ch)))

;;; TODO: Colon should be properly handled.
;;; TODO: Padding 0 must be rejected.
(defun read-suffix (strm)
  ;; Here, the radix must be 10. The definition of anaphora is the
  ;; basis. Even if *READ-BASE* is not 10.
  (do (clst
        (banged? (if (char= (peek-char nil strm t nil t) #\!)
                   (read-char strm t nil t)))
        (ch (peek-char nil strm t nil t) (peek-char nil strm t nil t)))
    ((not (digit-char-p ch))
     ;; TODO: what about a1\\1? Should it be an anaphora?
     (cond ((terminating-character-p ch)
            (if clst
              (values (coerce (nreverse clst) 'string) t banged?)
              (values "" nil banged?)))
           (t (values (coerce (nreverse clst) 'string) nil banged?))))
    (push (read-char strm nil t nil) clst)))

(defun intern-anaphora (asuffix)
  (intern (format nil "A~D" asuffix)))

(defun unread-str (strm &rest strdesg-lst)
  (make-concatenated-stream
    (make-string-input-stream
      (reduce (lambda (part item)
                (concat-str part (string item)))
              strdesg-lst :initial-value ""))
    strm))

(defparameter *saved-readtable* nil)

(defun gen-reader-macro-setup-code (ch local-var shared-var)
  (with-gensyms (strm c suffix anaphora? banged?)
    `(set-macro-character
       ,ch
       (lambda (,strm ,c)
         (declare (ignore ,c))
         (multiple-value-bind (,suffix ,anaphora? ,banged?)
           (read-suffix ,strm)
           (cond ((and ,anaphora? ,banged?)
                  (car (pushnew (intern-anaphora ,suffix) ,shared-var)))
                 (,anaphora?
                   (car (pushnew (intern-anaphora ,suffix) ,local-var)))
                 (t (let ((*readtable* *saved-readtable*))
                      (read (unread-str ,strm ,ch (if ,banged?  "!" "") ,suffix)
                            t nil t))))))
       t)))

;;; On WITH-ANAPHORA-PICKING expansion : SYMBOL-MACROLET form is build.
;;; Since the body of each symbol macro is never evaluated, we have to
;;; use this approach to program the expansion result of symbol macros.
;;; At this expansion, LET form is not expanded. That form is expanded
;;; on next expansion, expansion of symbol macros.
(defmacro with-anaphora-picking (local-var shared-var prefix-chars &body body)
  "This macro binds *READTABLE* to a copy of current readtable and set
   the anaphora picking function to all the characters given as the
   PREFIX-CHARS. The picked anaphoras are collected to the specified
   variable VAR. The forms given as BODY will be evaluated on READ time
   under the environment.
   "
  `(symbol-macrolet
     ((setup-reader-macros
        (progn ,@(mapcar (lambda (ch)
                           (gen-reader-macro-setup-code ch local-var shared-var))
                         prefix-chars))))
     (if (null *saved-readtable*)
       (let ((*saved-readtable* *readtable*)
             (*readtable* (copy-readtable))
             nest
             ,shared-var
             ,local-var)
         (declare (special ,shared-var))
         setup-reader-macros
         ,@body)
       (let ((*readtable* (copy-readtable))
             (nest t)
             ,local-var)
         (declare (special ,shared-var))
         setup-reader-macros
         ,@body))))

(let ((bqreader-fn (get-macro-character #\` (copy-readtable nil))))
  (defun |#`-reader| (strm c n)
    (declare (ignore c))
    (if n
      `(lambda ,(loop :for i :from 0 :upto (1- n)
                      :collect (intern-anaphora i))
         ,(funcall bqreader-fn strm nil))
      (with-anaphora-picking anaphoras shared-anaphoras (#\a #\A)
        (let ((expr (funcall bqreader-fn strm nil)))
          `(lambda ,(sort (union anaphoras (unless nest shared-anaphoras)) #'<=
                          :key (lambda (a) (parse-integer (remove-if-not #'digit-char-p (symbol-name a)))))
             ,expr))))))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)

(read-from-string "#`(print (list ,a0 ,a1 ,a3))")
(read-from-string "#4`(print (list ,a0 a1 a3))")
(read-from-string "#0`(print (list ,a0 a1 a3))")
(read-from-string "#`(print (list ,a0 a1 a3))")
(read-from-string "#`(print (list ,a0 ,a1 ,(progn `(foo ,a0) `(bar ,a2))))")
(read-from-string "#`(print (list ,a0 ,a1 ,(progn #`(foo ,a4) `(bar ,a2))))")
(read-from-string "#`(print (list ,a0 ,a1 ,(progn #`(foo ,a!4) #`(bar ,a2))))")
(read-from-string "#`(print (list ,abc ,a1 ,a3))")
(let ((*read-base* 4))
  (read-from-string "#`(print (list ,a10 (bc ,a2) ,a1 ,a0 a4))"))
(read-from-string "#`(print (list ,a2bc ,a1 ,a3))")
(read-from-string "#`(print (list ,a\\2\\bc ,a1 ,a3))")
(read-from-string "#`(print (list ,a\\2bc ,a1 ,a3))")

(read-from-string "#`(list ,a0)")
(read-from-string "#`(list ,A0)")
(read-from-string "#`(list ,a1 ,a0)")
(read-from-string "#`(list ,a\\1 ,a0)")
(read-from-string "#`(list ,A\\1 ,a0)")
(read-from-string "#`(list ,a1\\1 ,a0)")

(read-from-string "#`(print (list ,a0 ,a1 ,(progn #`(foo ,a!4 #`(baz a!1)) #`(bar ,a2))))")

(read-from-string "#`(list ,a0 a!d)")
(read-from-string "#`(list ,a01)")
