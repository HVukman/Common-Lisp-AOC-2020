(ql:quickload "str")
(in-package str)


(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defparameter inp (file-string "testdata.txt"))



(defparameter inp_list (with-open-file (input "testdata.txt")
   (loop for line = (read-line input nil)
      while line 
          collect (str:words line))))



(defun valid_func (x)
     (let (
         ( amount (str:count-substring (string (char (second x) 0)) (third x)))
         ( min_max (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" (car x))) )
         )
     ; (print amount)
      (if (and (<= (car min_max) amount ) (>= (cadr min_max) amount ))
          1
          0)
      )
   )

(print (reduce #'+
(loop for x in inp_list
      collect (valid_func x))
           ))


(defun valid_func2 (x)
     (let (
         ( relevant_char (char (second x) 0))
         ( first_last (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" (car x))) )
         )
      (print relevant_char)
      (print first_last)
      (if (not (equal (equal relevant_char (char (third x) 
               (- (car first_last) 1) )) (equal relevant_char (char (third x) 
               (- (cadr first_last) 1) ))))
          1
          0)
      )
   )

(print (reduce #'+
(loop for x in inp_list
      collect (valid_func2 x))))