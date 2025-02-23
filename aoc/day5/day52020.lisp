(load "library.lisp")
(in-package foo)
(defparameter path "day5/testdata.txt")


(defparameter input_lists (inp_list "day5/input/input.txt"))


; testcase
(defparameter test (caar (flattened_inp (inp_list "day5/testdata.txt"))))

test
(equal (char test 0) #\F)

(defparameter seats (alexandria:iota 128))
(defparameter rows (alexandria:iota 8))

; for rows
(defun searchrow (x y)
    (if (and (< 1 (length x)) (< 0 (length y)) )
    (if (equal (char y 0) #\F) 
        (searchrow (subseq x 0 (/ (length x) 2) ) (subseq y 1) )
        (searchrow (subseq x (/ (length x) 2) ) (subseq y 1) )
        )
    (car x)
    )
)
; for columns
(defun searchcol (x y)
    (if (and (< 1 (length x)) (< 0 (length y)) )
    (if (equal (char y 0) #\L) 
        (searchcol (subseq x 0 (/ (length x) 2) ) (subseq y 1) )
        (searchcol (subseq x (/ (length x) 2) ) (subseq y 1) )
        )
    (car x)
    )
)
; parse rows and columns as binaries
; mind edge cases
(defun parserow (x)
    (cond
     (
     (equal x "BBBBBBB") 
         0
        )
     (
     (equal x "FFFFFFF")
     127
     )
     (
      (car (list (parse-integer (substitute #\1 #\B 
        (substitute #\0 #\F x)) :radix 2))))
     )
    ) 
    

(defun parsecol (x)
    (cond
     (
      (equal x "RRR")
         7
     )
     (
      (equal x "LLL")
         0
     )
     (
      (car (list (parse-integer (substitute #\1 #\L 
        (substitute #\0 #\R x)) :radix 2)))
     )
     )
    )


; maximum using max function in library.lisp
( max-lst-using-max  (i:iterate (i:for pass in input_lists) 
              (i:collect ( + (* 8 (searchrow seats (subseq (car pass) 0 7)))
                        (searchcol rows (subseq (car pass) 7)) )
              )))

( max-lst-using-max  (i:iterate (i:for pass in input_lists) 
              (i:collect ( + (* 8 (parserow (subseq (car pass) 0 7)))
                        (parsecol (subseq (car pass) 7)) )
              )))
; part 2
; collect row, column and id as pairs
(defparameter ids 
              (let (
                    (ids (i:iterate (i:for pass in input_lists) 
                        (i:collect
                       
                        ( + (* 8 (parserow (subseq (car pass) 0 7)))
                        (parsecol (subseq (car pass) 7)) )
                                         )))
              
                   
                        )
                  ids     
                  ))
              



seatsids
(defparameter seatsids (alexandria:iota 998))


(defparameter missingids (remove-if #'(lambda (x) 
                 (equal nil (member x ids)))
                  seatsids))

; ignore the first missing seat, since the seat is not in front
(defparameter missingidsdummy (cdr missingids))

; check for missing seat id and return the seat
(print (i:iterate (i:for i from 1 below (length missingidsdummy))
            (if (not (equal 1 (- (nth (+ i 1) missingidsdummy) (nth i missingidsdummy))))
                (i:leave  (nth (+ i 1) missingidsdummy) )
               )      
           ))


