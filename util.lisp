(in-package :abackquote)

(defmacro let-it-be (expr &body body)
  `(let ((it ,expr))
     ,@body
     it))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect
          (let ((name n))
            (if (atom name)
              `(,name (gensym ,(symbol-name name)))
              `(,(car name) (gensym (or ,(cadr name) "G"))))))
     ,@body))

(defun concat-str (&rest strs)
    (apply #'concatenate 'string strs))

(defmacro awhile (form &body body)
  `(do ((it ,form ,form))
     ((not it))
     ,@body))

(defun last1 (lst) (car (last lst)))

(defmacro in (obj &rest choices)
  (with-gensyms (insym)
    `(let ((,insym ,obj))
       (or ,@(mapcar (lambda (c) `(eql ,insym ,c)) choices)))))
