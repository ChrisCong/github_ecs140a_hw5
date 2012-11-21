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

(defun itr-helper(x y)
	(do ((keep x (rest keep)) (keep2 y (rest keep2)) 
		(restore nil (append restore (list (order (first keep) (first keep2))))))
	((null keep) restore)
	return restore
	)
)

(defun combine-iterative(x y)
	(cond 
		((or (equal nil x) (equal nil y))
			()
		)
		((and (listp x) (listp y) (itr-helper x y)))
		(t (order x y))
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

;;;;;;;;;;;;;;;;;;;;;;part2;;;;;;;;;;;;;;;;;;;;

(defun remove-if-recursive(f x) 
        (cond
		((null x)	;;if nothing do nothing 
			()
		)
		((not (funcall f (first x)))	;;if first is not function remove first x
			(cons (first x) (remove-if-recursive f (rest x)))
		)
		(t (remove-if-recursive f (rest x)))	;;else recurse through and remove rest of list x
        )
)


(defun remove-if-mapcar(f x)
	(cond 
		(setq helper mapcar #'(lambda (x) 
			(if (not (funcall f x)) (list x))) x)
	)
			(apply #'append helper)
)


