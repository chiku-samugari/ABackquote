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
(defun unquoted-symbol-p (obj)
  #+sbcl
  (if (sb-impl::comma-p obj)
    (let ((expr (sb-impl::comma-expr obj)))
      (if (symbolp expr)
        (values t expr)
        (unquoted-symbol-p expr)))))

(mapcar #'unquoted-symbol-p (flatten (read-from-string "'(lambda (x) `(print ,a))")))

(mapcar #'unquoted-symbol-p (flatten (read-from-string "'(lambda (x) ``(print ,,a))")))

(defun anaphora-list (tree)
  " Returns a list that is composed of symbols whose ANAPHORAP check is
   T. Each symbol appears only once in the list and the list is sorted
   by the name.
   "
  (sort (remove-duplicates
          (filter (lambda (obj)
                    (or (and (symbolp obj) (anaphorap obj) obj)
                        #+sbcl
                        (multiple-value-bind (result expr)
                          (unquoted-symbol-p obj)
                          (and result expr))))
                  (flatten tree)))
        #'string<= :key #'symbol-name))

(defun new-sharpbq-reader (strm c n)
  (declare (ignore c))
  (let ((expr (funcall (get-macro-character #\`) strm nil)))
    `(lambda ,(if n
                (map0-n (lambda (n) (intern (format nil "A~A" n))) (1- n))
                (anaphora-list expr))
       ,expr)))

;;; Try it out by register it as #?
(set-dispatch-macro-character #\# #\? #'sharpbq-reader)

(read-from-string "#?(print (list ,a0 ,a1 ,a3))")
(read-from-string "#4?(print (list ,a0 a1 a3))")
