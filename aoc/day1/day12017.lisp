(ql:quickload "str")
(ql:quickload "alexandria")
(in-package str)
(defparameter *my-hash* (make-hash-table))

(defun file-string (path)
  (with-open-file (stream path)
    (let (
    (data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defparameter path "day1/input.txt")

(defparameter inp (file-string "day1/testdata.txt"))

(defparameter inp_list (with-open-file (input path)
   (mapcar #'parse-integer (alexandria:flatten (loop for line = (read-line input nil)
      while line 
          collect (str:words line))))))

inp_list

(defparameter keys
(loop for x in inp_list
    append
    (loop for y in inp_list
      collect (list x y)
          )
    )
)
; part 1
(apply '* (car
(rassoc 2020
(pairlis keys (mapcar #'(lambda (x) (apply #'+ x)) 
(loop for x in inp_list
    append
    (loop for y in inp_list
      collect (list x y)
          )
    )
)
)
:test #'equal)))

(loop for z in inp_list 
append
(loop for x in inp_list
    append
    (loop for y in inp_list
      collect (list x y)
          )
    )
)
