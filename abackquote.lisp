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
(defun read-suffix (strm)
  ;; Here, the radix must be 10. The definition of anaphora is the
  ;; basis. Even if *READ-BASE* is not 10.
  (let (bangs)
    (awhile (char= (peek-char nil strm t nil t) #\!)
      (push (read-char strm t nil t) bangs))
    (do (clst
          (ch (peek-char nil strm t nil t) (peek-char nil strm t nil t)))
      ((not (digit-char-p ch))
       ;; TODO: what about a1\\1? Should it be an anaphora?
       (values (coerce (reverse clst) 'string)
               (and (terminating-character-p ch)
                    clst
                    (or (equal clst '(#\0))
                        (char/= (last1 clst) #\0)))
               (length bangs)))
      (push (read-char strm nil t nil) clst))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-reader-macro-setup-code (ch local-var shared-var)
    (with-gensyms (strm c suffix anaphora? bangcount)
      `(set-macro-character
         ,ch
         (lambda (,strm ,c)
           (declare (ignore ,c))
           (multiple-value-bind (,suffix ,anaphora? ,bangcount)
             (read-suffix ,strm)
             (cond ((and ,anaphora? (< 0 ,bangcount))
                    (let-it-be (intern-anaphora ,suffix)
                      (pushnew it (nth ,bangcount ,shared-var))))
                   (,anaphora?
                     (let-it-be (intern-anaphora ,suffix)
                       (pushnew (intern-anaphora ,suffix) ,local-var)))
                   (t (let ((*readtable* *saved-readtable*))
                        (read (unread-str
                                ,strm ,ch
                                (make-string ,bangcount :initial-element #\!)
                                ,suffix)
                              t nil t))))))
         t))))

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
             (,shared-var (list nil))
             ,local-var)
         (declare (special ,shared-var))
         setup-reader-macros
         ,@body)
       (let ((*readtable* (copy-readtable))
             ,local-var)
         (declare (special ,shared-var))
         (push () ,shared-var)
         setup-reader-macros
         (unwind-protect
           (progn
             ,@body)
           (pop ,shared-var))))))

(defun anaphora-order (anaphora)
  (parse-integer (remove-if-not #'digit-char-p (symbol-name anaphora))))

(let ((bqreader-fn (get-macro-character #\` (copy-readtable nil))))
  (defun |#`-reader| (strm c n)
    (declare (ignore c))
    (if n
      `(lambda ,(loop :for i :from 0 :upto (1- n)
                      :collect (intern-anaphora i))
         ,(funcall bqreader-fn strm nil))
      (with-anaphora-picking anaphoras shared-anaphoras (#\a #\A)
        (let ((expr (funcall bqreader-fn strm nil)))
          `(lambda ,(sort (union anaphoras (car shared-anaphoras)) #'<=
                          :key #'anaphora-order)
             ,expr))))))

(defun enable-abackquote (&optional (disp-char #\#) (sub-char #\`))
  (set-dispatch-macro-character disp-char sub-char #'|#`-reader|))
