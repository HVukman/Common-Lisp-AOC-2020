(load "library.lisp")
(in-package foo)

(defparameter path_str "day7/input/input.txt")

(defparameter path path_str)


(defparameter input_lists (inp_list path))

(defparameter flattenedlists
             (mapcar #'alexandria:flatten (split-sequence:split-sequence nil input_lists
                                        :test #'equal
                                      :remove-empty-subseqs t)))



(defun set_value (x)
  (let* 
      (
       (dummy (mapcar #'(lambda (x) (str:split " " x))
        (str:split "," (concat-space x) )
        ))
       (key (concat-space (list (car (car dummy)) (cadr (car dummy)))))
       (rest (cdr dummy))
       (value (cons (parse-integer (nth 4 (car dummy))) 
                     (concat-space (list (nth 5 (car dummy)) (nth 6 (car dummy))))
                  )
        )
       (value_rest (i:iterate (i:for entry in rest)
        (i:collect        
            (cons (parse-integer (nth 1 entry)) 
                    (concat-space (list (nth 2 entry ) (nth 3 entry))))
              )       
      ))
        )
   
    (setf (gethash 
            key *hash-bag*) (cons value value_rest))
    )
  
  )


(defun set_key (x)
  (let 
      (
       (dummy (mapcar #'(lambda (x) (str:split " " x))
        (str:split "," (concat-space x) )
        ))
       )
     
      ; if equal one bag contains no other bag
      (if (and (not (parse-integer (nth 4 (car dummy)) :junk-allowed t)) (equal 1 (length dummy)))
          (setf (gethash 
            (concat-space (list (nth 0 (car dummy)) 
                                    (nth 1 (car dummy)))) *hash-bag*) 0)
          (set_value x)
      )
    )
  )

(defparameter *hash-bag* (make-hash-table :test 'equal))

(i:iterate (i:for bagg in input_lists)
           (set_key bagg))

(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))

(maphash #'print-hash-entry *hash-bag*)





(defun check_gold (xx yy)
  (if (listp (gethash xx *hash-bag*))
  (if (equal nil (member yy (gethash xx *hash-bag*)))
  (if (shiny_gold? xx)
      T
      (if (and (equal nil (member xx yy)) (check_key xx))
          (loop for i in (gethash xx *hash-bag*)
                  do  
                  ;(print (cdr i))
                  ; add i to list to keep track
                  ;(print (cons xx yy ))
                  ; (print (member xx yy))
                  (check_gold (cdr i) (cons xx yy))
                      when (check_gold (cdr i) (cons xx yy))
                      return T
          )
      )
  )
  )
  )
)

; checks if shiny gold is in values
(defun shiny_gold? (x) 
  (not (equal nil (member T 
    (mapcar (lambda (y) (equal  (cdr y) "shiny gold" ))
      (gethash x *hash-bag*)
      ))))
  )

(defun check_key (xx)
  (and (equal nil 
              (loop for i in (gethash xx *hash-bag*)
        do (equal xx (cdr i)) 
        )))
                   (or (not (equal (gethash xx *hash-bag*) 0) ) 
                      (not (equal nil (gethash xx *hash-bag*)))))



; print solution
(print (count T (loop for i in (alexandria:hash-table-keys *hash-bag*)
        collect (check_gold i '())
)))

; part 2
; gethash i while not 0 collect all





(defparameter bags 0)

(defun get-entries2 (x)
  (if (listp (gethash x *hash-bag*))
      (loop for x in (gethash x *hash-bag*)
            do 
            (dotimes (i (car x))
                (setq bags (+ 1 bags))
                ;(print (cdr (car (gethash x *hash-bag*))))
                (get-entries2 (cdr x))
            )
      )
  )
)

(get-entries2 "shiny gold")
(print bags)