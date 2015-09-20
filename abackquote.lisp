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

(defun read-suffix (strm)
  ;; Here, the radix must be 10. The definition of anaphora is the
  ;; basis. Even if *READ-BASE* is not 10.
  (do (clst
        (ch (peek-char nil strm t nil t) (peek-char nil strm t nil t)))
    ((not (digit-char-p ch))
     ;; TODO: what about a1\\1? Should it be an anaphora?
     (cond ((terminating-character-p ch)
             (if clst
               (values (coerce (nreverse clst) 'string) t)
               (values "" nil)))
           (t (values (coerce (nreverse clst) 'string) nil))))
    (push (read-char strm nil t nil) clst)))

(defun intern-anaphora (asuffix)
  (intern (format nil "A~D" asuffix)))

(let ((bqreader-fn (get-macro-character #\` (copy-readtable nil))))
  (defun |#`-reader| (strm c n)
    (declare (ignore c))
    (if n
      `(lambda ,(loop :for i :from 0 :upto (1- n)
                      :collect (intern-anaphora i))
         ,(funcall bqreader-fn strm nil))
      (let ((*readtable* (copy-readtable))
            anaphoras)
        (set-macro-character
          #\a (lambda (strm c)
                (declare (ignore c))
                (multiple-value-bind (suffix anaphora?)
                  (read-suffix strm)
                  (if anaphora?
                    (car (pushnew (intern-anaphora suffix) anaphoras))
                    (let ((fake (make-concatenated-stream
                                  (make-string-input-stream (concat-str "\\A" suffix))
                                  strm)))
                      (read fake t nil t)))))
          t)
        (let ((expr (funcall bqreader-fn strm nil)))
          `(lambda ,(sort anaphoras #'<=
                          :key (lambda (a) (parse-integer (subseq (symbol-name a) 1))))
             ,expr))))))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)

(read-from-string "#`(print (list ,a0 ,a1 ,a3))")
(read-from-string "#4`(print (list ,a0 a1 a3))")
(read-from-string "#0`(print (list ,a0 a1 a3))")
(read-from-string "#`(print (list ,a0 a1 a3))")
(read-from-string "#`(print (list ,a0 ,a1 ,(progn `(foo ,a0) `(bar ,a2))))")
(read-from-string "#`(print (list ,a0 ,a1 ,(progn #`(foo ,a4) `(bar ,a2))))")
(read-from-string "#`(print (list ,a0 ,a1 ,(progn #`(foo ,a4) #`(bar ,a2))))")
(read-from-string "#`(print (list ,abc ,a1 ,a3))")
(let ((*read-base* 4))
  (read-from-string "#`(print (list ,a10 (bc ,a2) ,a1 ,a0 a4))"))
(read-from-string "#`(print (list ,a2bc ,a1 ,a3))")

(read-from-string "#`(list ,a0)")
(read-from-string "#`(list ,a1 ,a0)")
(read-from-string "#`(list ,a\\1 ,a0)")
(read-from-string "#`(list ,a1\\1 ,a0)")
