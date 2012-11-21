(defun order (x y)
	(cond 		
		((and (numberp x) (numberp y))   ;if x and y are numbers
			(cond 
				((> x y) (list y x))     ;if x>y return list with x in back of y
							;else if x<y return list with x in front of y
				((< x y) (list x y))
							;else make list of x
				(t (list x))
		))
		(t (list x y)))		;make list of x and y

)

(defun combine-recursive(x y) 
	(cond 
		((and (not (listp x)) (not (listp y))) ;if both x and y are not lists then order them 
			(order x y) 
		) 	 
		((or (equal nil x) (equal nil y)) ;if both x and y null do nothing
			()
		)			  
		((and (listp x) (listp y))     ;if lists x and y get cons of first 2 elements in list and sort by order then call function recursively
			(cons (order (car x) (car y)) (combine-recursive (cdr x) (cdr y))) 
		)
	)
)
			         
(defun combine-iterative(x y) 
  (tagbody
   (go point-a)
   
   point-a
   (cond ((or (equal nil x) (equal nil y)) (go point-end))
	 (t (go point-b))
	 )

   point-b
   (cond ((and (not (listp x)) (not (listp y))) (order x y))
	 (t (go point-c))
	 )

   point-c
   (cond ((and (listp x) (listp y)) (cons (order (car x) (car y)) (go point-a)))
	 (t (go point-end))
	 )
   
   point-end ()
   )
)


(defun combine-mapcar(x y)
	(cond 
		((or (equal nil x) (equal nil y))
			()
		)
		((and (listp x) (listp y)) ;if x and y are lists go through them and order each element store result in new list
			(mapcar #'order x y)
		) 
		(t (order x y))		;else order them
	)
)
