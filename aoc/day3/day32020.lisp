(ql:quickload "str")
(in-package str)
(ql:quickload "iterate")
(ql:quickload :alexandria)


; part 1
(defpackage foo
  (:use :cl)
  (:local-nicknames (:i :iterate)))
(in-package foo)




(defparameter inp_list (with-open-file (input "day3/input.txt")
   (loop for line = (read-line input nil)
      while line 
          collect (str:words line))))

(defparameter string_length (length(caar inp_list)))

; collect chars in a simple loop with modulo division
(defparameter dummy (i:iterate (i:for i from 1 to 
        (- (length inp_list) 1) 
                  )
            (i:collect 
                (char (car (nth i inp_list))  
                        (mod (* i 3) string_length))              
             ;  (list i  (nth (mod (- i 1)  (length increment)   ) increment) )
             )
))


(print (length (remove #\. dummy)))



; part 2



(defparameter increase (list (list 1 1) (list 3 1) (list 5 1) (list 7 1) ) )

; loop over step sizes except last one
(defparameter dummy2 (i:iterate (i:for var in increase )
    (let ((step (car var))
          (bystep 1))
      (i:collect 
            (i:iterate (i:for i from bystep to 
                  (- (length inp_list) 1) 
                      by bystep )
                          (i:collect 
                              (char (car (nth i inp_list))  
                                      (mod (* i step) string_length))              
                          ;  (list i  (nth (mod (- i 1)  (length increment)   ) increment) )
                          )
            )
      )  
    )   
  ))

; seperate loop for higher steps
(defparameter dummy3
    (let (
          (bystep 2))
            (i:iterate (i:for i from 2 to 
                  (- (length inp_list) 1) 
                      by bystep )
                          (i:collect 
                              (char (car (nth i  inp_list))  
                                      (mod (/ i 2) string_length))              
                          ;  (list i  (nth (mod (- i 1)  (length increment)   ) increment) )
                          )
            )
       
    )  
) 
  

(defparameter dummy4 (apply #'* (i:iterate (i:for var in dummy2)
           (i:collect (length (remove #\. var)))
           )) )

(print (* dummy4 (length (remove #\. dummy3))))