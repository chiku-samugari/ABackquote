;;;; Sharp backquote reader macro
;;; The #` reader macro introduced in Let Over the Lambda is an
;;; excellent stuff. However, implicit insertion of anaphoras is more
;;; preferable.
(in-package :abackquote)

(defun anaphora-suffix-p (strm)
  ;; Here, base must be 10. The definition of anaphora is the basis.
  ;; Even if *READ-BASE* is not 10.
  ;; TODO: FIXME!
  (digit-char-p (peek-char nil strm t nil t)))

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
                (if (anaphora-suffix-p strm)
                  (car (pushnew (intern-anaphora (read strm t nil t))
                                anaphoras))
                  (let ((fake (make-concatenated-stream
                                (make-string-input-stream "\\A")
                                strm)))
                    (read fake t nil t))))
          t)
        (let ((expr (funcall bqreader-fn strm nil)))
          `(lambda ,(sort anaphoras #'string<= :key #'symbol-name)
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
(read-from-string "#`(print (list ,a (bc ,a2) ,a1 ,a0))")
