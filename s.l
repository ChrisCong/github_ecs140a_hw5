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
