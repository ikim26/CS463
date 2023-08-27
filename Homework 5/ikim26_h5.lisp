;;  Name: Isaac Kim
;;  G#:   G01201648
;;  (other header-comments you'd like to add)

;;----------------------------------------------------------------------------------

(defun prime-factors (n) ;;call helper function to keep track of divisor
	(prime-factors-helper n n 2 NIL))

(defun prime-factors-helper (x og divisor factors)
	(if (<= divisor (* 2 (sqrt og)))    ;;if divisor is less than 2 * sqrt of og value
		(if (= 0 (mod x divisor))    ;;if x is divisible by our prime factor
			(prime-factors-helper (floor x divisor) og divisor (append factors (list divisor)))   ;;recurively call prime-factor-helper
			(if (evenp divisor)
				(prime-factors-helper x og (+ divisor 1) factors)    ;;else (divisor is not a factor and even) increment divisor and call again
		        (prime-factors-helper x og (+ divisor 2) factors)    ;;else (divisor is not a factor and odd) increment divisor by 2 and call again
		        )
			)
		(if (null factors)  ;;if we reach here, and factors is null, it means x was prime
            (list og)  ;;return og value
            factors  ;;return list of factors
			)
		)
	)

;;----------------------------------------------------------------------------------

(defun coprime (a b) ;;call helper function with prime factors of both parameters
	(coprime-helper (prime-factors a) (prime-factors b)))

(defun coprime-helper (xs ys)   ;;helper for coprime
    (if (null xs) ;;if list of factors is empty, return true (base case)
        T
        (if (coprime-helper2 (first xs) ys) ;;call 2nd helper with each element of xs to compare with ys
        	(coprime-helper (rest xs) ys) ;;recursive call for loop
        	NIL) ;;if we get here, we have shared factors so not coprime
        )
	)

(defun coprime-helper2 (x ys) ;;2nd helper for coprime
    (if (null ys) ;;if ys is empty, return true (base case)
    	T
        (if (= x (first ys))  ;;if there is a shared factor, return false (NIL)
            NIL
            (coprime-helper2 x (rest ys))  ;;recursive call to loop through ys
        	)
    	)
	)

;;----------------------------------------------------------------------------------

(defun trib (n) ;;call helper with first 3 trib numbers
    (trib-helper n (list 1 1 1)))

(defun trib-helper (n seq)  ;;helper for trib
    (if (> n 2)  ;;if n > 2
        (trib-helper (- n 1) (append (list (+ (nth 0 seq) (+ (nth 1 seq) (nth 2 seq)))) seq))  ;;add number based on sum of left most 3 nums in array
        (nth 0 seq) ;;once we reach nth number, return latest addition to array
        )
	)

;;----------------------------------------------------------------------------------

(defun max-two (xs) ;;call helper with sorted array
	(max-two-helper (insertion-sort xs)))

(defun max-two-helper (xs)  ;;helper for max-two
	(if (null xs)  ;;if list is empty, return NIL
        NIL
        (if (> (list-length xs) 1)  ;;if list has 2 or more elements
            (list (nth (- (list-length xs) 1) xs) (nth (- (list-length xs) 2) xs))  ;;return last two elements since array is sorted from least to greatest
            (list (nth 0 xs))  ;;otherwise, return first element in array
        	)
		)
	)

;; insertion sort ------------------------------------------------------------------
;; (taken from https://cs.gmu.edu/~marks/463/slides/4.lisp_notes/more_examples.lisp)

(defun insert-one (v xs)
  "inserts one number in increasing order."
  (if (null xs) (cons v NIL)
      (if (> v (first xs)) (cons (first xs) (insert-one v (rest xs)))
	  (cons v xs))))

(defun insert-helper (ans todo-list)
  "insert everything from our todo-list into ans"
  (if (null todo-list) ans
      (let ((one-more-placed (insert-one (first todo-list) ans)))
	(insert-helper one-more-placed (rest todo-list)))))

(defun insertion-sort (xs)
  (insert-helper NIL xs))

;;----------------------------------------------------------------------------------

(defun reversed (xs)  ;;call helper for reversed
	(reversed-helper xs NIL))

(defun reversed-helper (xs ys)  ;;reversed helper function
    (if (null xs)  ;;if xs is empty, return ys (base case)
        ys
        (reversed-helper (rest xs) (append (list (first xs)) ys))  ;;append first element of xs to beginning of ys
    	)
	)

;;----------------------------------------------------------------------------------

(defun clockwise (grid) 
	(if (null grid)  ;;empty array (base case), return NIL
        NIL
		(clockwise-helper (list-length grid) (list-length (nth 0 grid)) grid NIL)  ;;otherwise, call helper function
		)
	)

(defun clockwise-helper (i j xs r)  ;;helper for clockwise
    (if (= 0 i)  ;;base case (empty array)
        r  ;;return r
        (if (> j 0)  ;;(while) if j > 0
            (clockwise-helper2 0 j xs NIL r)  ;;call helper function and start at i = 0
            r
        	)
    	)
	)

(defun clockwise-helper2 (i j xs temp r)  ;;2nd helper for clockwise
    (if (> i (- (list-length xs) 1))  ;;(while) if i > len(xs) - 1
    	(clockwise-helper i (- j 1) xs (append (list temp) r))  ;;if here, we are done with outer array, add new array to list of arrays
    	(clockwise-helper2 (+ i 1) j xs (append (list (nth (- j 1) (nth i xs))) temp) r)  ;;for each element in outer array, make new array from jth element
    	)
	)

;;----------------------------------------------------------------------------------

(defun any (bs) 
    (if (null bs)  ;;if bs is empty (base case)
        NIL  ;;return NIL
        (if (first bs)  ;;if first element is true
            T  ;;return true
            (any (rest bs))  ;;otherwise, keep looping (recursively)
        	)
    	)
	)

;;----------------------------------------------------------------------------------

(defun select (p xs)  ;;call helper for select
	(select-helper p xs NIL))

(defun select-helper (p xs ys)  ;;helper for select
    (if (null xs)  ;;if array is empty (base case)
        ys  ;;return ys
    	(if (funcall p (first xs))  ;;loop through elements of xs, see if first element of xs passes p
            (select-helper p (rest xs) (append ys (list (first xs))))  ;;if passes, add to ys
            (select-helper p (rest xs) ys)  ;;otherwise, go next
    		)	
    	)
	)

;;----------------------------------------------------------------------------------

(defun zip-with (f xs ys)  ;;call helper function that has array to store results
	(zip-with-helper f xs ys NIL))

(defun zip-with-helper (f xs ys zs)  ;;helper function for zip with
    (if (or (null xs) (null ys))  ;;base cases (empty xs or ys)
        zs  ;;return zs
        (zip-with-helper f (rest xs) (rest ys) (append zs (list (funcall f (first xs) (first ys)))))  ;;apply function to each of the elements in the arrays and add to zs
    	)
	)

;;----------------------------------------------------------------------------------

(defun augdentity (r c) 
	(augdentity-helper 0 r c NIL))

(defun augdentity-helper (i r c arr)
    (if (>= i r) ;;if i is equal to num of rows
        arr  ;;return array
        (augdentity-helper2 i 0 r c arr NIL)  ;;otherwise, fill ith column with 2nd helper
    	)
	)

(defun augdentity-helper2 (i j r c arr temp)  ;;2nd helper for augdentity
    (if (< j c)  ;;if j < c
        (if (= i j)  ;;if i == j
            (augdentity-helper2 i (+ j 1) r c arr (append temp (list 1)))  ;;if same col and row, put 1
            (augdentity-helper2 i (+ j 1) r c arr (append temp (list 0)))  ;;otherwise, put 0
        	)
        (augdentity-helper (+ i 1) r c (append arr (list temp)))
    	)
	)
;;----------------------------------------------------------------------------------
