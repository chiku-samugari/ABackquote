;;;; Sharp backquote reader macro
;;; The #` reader macro introduced in Let Over the Lambda is an
;;; excellent stuff. However, implicit insertion of anaphoras is more
;;; preferable.
(in-package :sharp-backquote)

(defun anaphorap (sym)
  " Retruns T if the name of the given symbol ``SYM'' is
   a member of following regular language:
   A[1-9][0-9]*|A0
   "
  (let ((name (symbol-name sym)))
    (and (<= 2 (length name))
         (string= (take name) "A")
         (every #'digit-char-p (drop name))
         (or (= (length name) 2)
             ;; Disables padding 0
             (not (char= (elt name 1) #\0))))))

;;; For example, Clozure CL does not need this function. Unquoted
;;; expressions are not something special structure.
(defun unquotedp (obj)
  #+sbcl
  (if (sb-impl::comma-p obj)
    (values t (sb-impl::comma-expr obj))))

(mapcar #'unquotedp (flatten (read-from-string "'(lambda (x) `(print ,a))")))

(mapcar #'unquotedp (flatten (read-from-string "'(lambda (x) ``(print ,,a))")))

(mapcar #'unquotedp (flatten (read-from-string "(lambda (x) `(print ,(`,a)))")))

(defun anaphora-list (tree)
  " Returns a list that is composed of symbols whose ANAPHORAP check is
   T. Each symbol appears only once in the list and the list is sorted
   by the name.
   "
  (labels ((collect (tree)
             (filter (lambda (obj)
                       (or (and (symbolp obj) (anaphorap obj) obj)
                           #+sbcl
                           (multiple-value-bind (? expr)
                             (unquotedp obj)
                             (and ? (collect expr)))))
                     (flatten tree))))
    (sort (remove-duplicates (flatten (collect tree)))
          #'string<= :key #'symbol-name)))

(anaphora-list (read-from-string "(lambda (x) `(print ,(`,a0)))"))

(anaphora-list (read-from-string "`(print (list ,a0 ,a1 ,a3))"))

(let ((bqreader-fn (get-macro-character #\` (copy-readtable nil))))
  (defun |#?-reader| (strm c n)
    (declare (ignore c))
    (let ((expr (funcall bqreader-fn strm nil)))
      `(lambda ,(if n
                  (loop :for i :from 0 :upto (1- n)
                        :collect (intern (format nil "A~A" i)))
                  (anaphora-list expr))
         ,expr))))

(set-dispatch-macro-character #\# #\? #'|#?-reader|)

(read-from-string "#?(print (list ,a0 ,a1 ,a3))")
(read-from-string "#4?(print (list ,a0 a1 a3))")
(read-from-string "#0?(print (list ,a0 a1 a3))")
(read-from-string "#?(print (list ,a0 a1 a3))")
(read-from-string "#?(print (list ,a0 ,a1 ,(progn `(foo ,a0) `(bar ,a2))))")
