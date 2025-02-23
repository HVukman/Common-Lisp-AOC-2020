(load "library.lisp")
(in-package foo)

(defparameter path "day6/input/testdata.txt")


(defparameter input_lists (inp_list path))

(defparameter flattenedlists
             (mapcar #'alexandria:flatten (split-sequence:split-sequence nil input_lists
                                        :test #'equal
                                      :remove-empty-subseqs t)))

; part 1
(reduce #'+
    (mapcar #'(lambda (x) (count T x)) 
       (i:iter (i:for i in flattenedlists ) 
        (i:collect (i:iter (i:for j in-sequence "abcdefghijklmnopqrstuvwxyz" )
                        (i:collect  (not (equal nil 
                            (member j (mapcar #'(lambda (x) (find j x)) i)))))
                    )
        )
    )
    )
)

; part 2
; function for all questions answered
(defun all_questions (charac input) 
    (equal nil (member nil 
       (mapcar #'(lambda (x) (equal charac x)) (member charac
                    (mapcar #'(lambda (x) (find charac x)) input))))))
 
   (i:iter (i:for i in flattenedlists ) 
        (i:collect (i:iter (i:for j in-sequence "abc" )
                        (i:collect  (not (equal nil 
                            (all_questions j i)
                            )
                    )
        )
    )))


