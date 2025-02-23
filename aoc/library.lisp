(ql:quickload "str")
(in-package str)
(ql:quickload "fset")
(ql:quickload "iterate")
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload "cl-ppcre")


(defpackage foo
  (:use :cl)
  (:local-nicknames (:i :iterate)))
(in-package foo)



(defun file-string (x)
  (with-open-file (stream x)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


(defun inp (x) (file-string x))


; read input as lists
(defun inp_list (x) (with-open-file (input x)
   (loop for line = (read-line input nil)
      while line 
          collect (str:words line))))

; flatten list with alexandria
(defun flattened_inp (x)  (mapcar #'alexandria:flatten (split-sequence:split-sequence 
           NIL x :test #'equal
             :remove-empty-subseqs t)))

; maximum of list
(defun max-lst-using-max (lst-in)
  (reduce #'max lst-in)) 