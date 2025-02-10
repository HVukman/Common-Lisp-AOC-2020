(ql:quickload "str")
(in-package str)
(ql:quickload "iterate")
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload "cl-ppcre")

; part 1
(defpackage foo
  (:use :cl)
  (:local-nicknames (:i :iterate)))
(in-package foo)

(defparameter path "day4/input.txt")

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


(defparameter inp (file-string path))
(print inp)

; read input as lists
(defparameter inp_list (with-open-file (input path)
   (loop for line = (read-line input nil)
      while line 
          collect (str:words line))))

; flatten list
(defparameter flattened_inp (mapcar #'alexandria:flatten (split-sequence:split-sequence 
           NIL inp_list :test #'equal
             :remove-empty-subseqs t)))

flattened_inp 

; list of required fields
(defparameter required_fields
             (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))


(defun check-valid (x)
 (equal NIL (member NIL (let (
      (test (remove "cid"  
              (i:iterate (i:for var in x)
              (i:collect (subseq var 0 3))
            ) :test #'string=)
            )
          )
         ; check if each required field is in test
        (i:iterate (i:for xx in required_fields) 
             (i:collect (member xx test :test #'string=))    
                  )
             ))))
      
    
(print (count T (i:iterate (i:for passport in flattened_inp) 
              (i:collect (check-valid passport))
           )))     


; part two

; check if conditions are met, nothing special
(defparameter eye_colors (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth"))

(defun valid_birth (x)
  (let (
        (year (parse-integer x :junk-allowed t)))
    (if year
        (and (> year 1919) (< year 2003))
        )
  ))

(defun valid_issue (x)
  (let (
        (year (parse-integer x :junk-allowed t)))
    (if year
        (and (> year 2009) (< year 2021))
        )
  ))

(defun valid_eyr (x)
  (let (
        (year (parse-integer x :junk-allowed t)))
    (if year
        (and (> year 2019) (< year 2031))
        )
  ))

(defun valid_height (x)
       (if (or
            (= (length x) 4)
            (= (length x) 5))
       (let* (
              (strlength (length x))
              (end (subseq x (- strlength 2) )))
              (cond 
                  ( (string= end "in" ) 
                      (let ( (num (parse-integer (subseq x 0 2) :junk-allowed t)))
                            (if num 
                                (and (> num 58) (< num 77))
                              )
                          )
                  )
                  ( (string= end "cm" )
                      (let ( (num (parse-integer (subseq x 0 3) :junk-allowed t)))
                            (if num 
                                (and (> num 149) (< num 194))
                              )
                          )
                        
                  )
                   (t nil)
              )
              ))
       )

(defun valid_hair (x)
      (if (and (equal 7 (length x)) (string= "#" (char x 0) ))
          (i:iterate (i:for char in (coerce (subseq x 1) 'list))
            
                (cond (
                     (not (digit-char-p char))
                            (if (not (char<= #\a char #\f))     
                         (i:leave nil)
                             )
                         
                         )
                 )
                 (i:finally (return T))
                 )
      )
)

(defun valid_eye (x)
    (not (equal nil (member x eye_colors :test #'string=))))

(defun valid_pass (x)
      (if (equal (length x) 9)
          (i:iterate (i:for char in (coerce (subseq x 1) 'list))
            (cond (
                    (not (digit-char-p char))
                           
                            (i:leave nil)
                         
                  )
                 )
                 (i:finally (return T))
                 )
      )
  )

(defun valid_fields (x)
  (let
      ((field (subseq x 0 3))
       (value (subseq x 4)))
      
        (cond 
          ( 
              (string= field "byr")
              (valid_birth value)
          )
          ( 
              (string= field "iyr")
              (valid_issue value)
          )
          ( 
              (string= field "eyr")
              (valid_eyr value)
          )
          ( 
              (string= field "hgt")
              (valid_height value)
          )
          ( 
              (string= field "hcl")
              (valid_hair value)
          )
          ( 
              (string= field "ecl")
              (valid_eye value)
          )
          ( 
              (string= field "pid")
              
              (valid_pass value)
          )
          ( 
              (string= field "cid")
              T
          )
         
        ) 
       )
      )  
  
; iterate over fields if valid passports
(defun check_valid_and_field (x)
      (if (check-valid x)
        (i:iterate (i:for field in x)
                  (cond (
                    (equal nil (valid_fields field))   
                         (i:leave nil)
                  )
                 )
                 (i:finally (return T))
              )
      )
  )

(print (count T (i:iterate (i:for passport in flattened_inp) 
              (i:collect 
                (check_valid_and_field passport)
            )
           )))


; test cases
(valid_birth "19a20")

(valid_issue "2010")

(valid_eyr "2021")

(valid_height "59in")

(valid_hair "#123a56")

(valid_eye "amba")

(print (valid_pass "087499704"))

(check-valid (car flattened_inp))

(print (stringp (nth 0 (car flattened_inp))))

(print (nth 6 (car flattened_inp)))

(print (check-valid (car flattened_inp)))

(print (valid_fields (nth 6 (car flattened_inp))))