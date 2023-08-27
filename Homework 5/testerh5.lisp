;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STUDENTS: use in the following way:
;;
;; (1) via manual commands:
;;
;;    os-prompt$ clisp -q
;;    [1]> (load "yourcode.lisp")
;;    T
;;    [2]> (load "tester5h.lisp")
;;    T
;;    [3]> (run-tests)



;;    [1]> (load "tester5h.lisp")
;;    T
;;    [2]> (main '("YOURFILE.lisp"))
;;    ....
;;
;; (2) via command line options (sbcl provides these, clisp does not...)
;;
;;    os-prompt$ sbcl --load "tester5h.lisp" --eval '(progn (main (list "YOURFILE.lisp")) (sb-ext:quit))'
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *numwrong* 0)
(defvar *haserrored* nil)

;;(declaim (optimize (SPACE 3) (DEBUG 2))) ; to turn on tco



;(load "h5.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This section builds up the testing framework.


(defparameter *tests* nil)

(defun repeat-character (char len)
  (let ((str (make-array 0 :element-type 'character :adjustable t
			 :fill-pointer t)))
    (loop for i from 0 below len do
	 (vector-push-extend char str))
    str))

(defun run-tests ()
  (setf *numwrong* 0)
  (mapcar
   (lambda (test-pair)
     (destructuring-bind (name fn) test-pair
       (let* ((top-str (format nil "====Running ~a====~%" name))
	      (bot-str (repeat-character #\= (length top-str))))
	 (format t "~a" top-str)
	 (funcall fn)
	 
	 (format t "~a~%" bot-str))))
   *tests*)
  (if (equalp *numwrong* 0) (format t "OK~%")
      (format t "FAILED (failures=~D)" *numwrong*)))

(defun main (args)
  (load (first args))
  (run-tests))

(defmacro with-gensyms-v2 ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; Macro used to create test functions
(defmacro define-test (function &body in-outs)
  ;; Gensym for the result from the function
  (let ((name (intern
	       (string-upcase
		(format nil "test-~a"
			(string function))))))
    (with-gensyms-v2 (res)
      ;; Define the function with the name test-<FUNCTION>
      `(progn (defun ,name
		  ;; Test function takes no arguments
		  ()
		;; Set up all the function calls and test harnesses
		,@(mapcar
		   (lambda (pair)
		     `(progn
			;; Call the function, store the result
			(let ((,res
			       (handler-case 
				   (apply #',function (list ,@(first pair)))
				 (condition (caught-err)
				   (progn
				     (incf *numwrong*)
				     (setf *haserrored* t)
				     (print caught-err))))))
			  ;; If it is the expected value...
			  (if (equal ,res ,(second pair))
			      ;; Print a PASSED message
			      (setf *haserrored* nil) ; don't print a passed message
			      ;; Otherwise, Show what failed
			      (if *haserrored*
				  (progn
				    (setf *haserrored* nil)
				    (format t "ERROR: Given \"~{~a~^ ~}\" called function coughed and died."
					    (list ,@(first pair)))
				    ())
				  (progn
				    (format t "FAIL: Expected \"~a\" got \"~a\"~%"
					    ,(second pair)
					    ,res)
				    ;; and reduce grade.
				    (incf *numwrong*)))))))
		   ;; in-outs is the list of list pairs that define the test
		   in-outs))
	      (push (list ',name #',name) *tests*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This section creates tests for each function, with one test per line inside.

(define-test prime-factors
  ((2)    '(2))
  ((4)    '(2 2))
  ((5)    '(5))
  ((50)   '(2 5 5))
  ((66)   '(2 3 11))
  ((100)  '(2 2 5 5))
  ((463)  '(463))
  ((512)  '(2 2 2 2 2 2 2 2 2))
  ((117)  '(3 3 13))
  ((1117) '(1117))
  ((44100) '(2 2 3 3 5 5 7 7))
)

(define-test coprime
  ((  2   3) T)
  ((  5  10) NIL)
  (( 10  15) NIL)
  (( 15  10) NIL)
  (( 50  61) T)
  ((100 200) NIL)
  (( 97  98) T)
  (( 66 201) NIL)
  ((367 463) T)
  ((330 463) T)
  ((867 5309) T) 
)

(define-test  trib
  ((0) 1)
  ((2) 1)
  ((3) 3)
  ((4) 5)
  ((5) 9)
  ((6) 17)
  ((10) 193)
  ((12) 653)
  ((13) 1201)
  ((30) 37895489)
  ((35) 797691075)
)

(define-test max-two
  (('(1 2 3 4 5 1 2))     '(5 4))
  (('(3 6 1 2 6 4))       '(6 6))
  (('(-3 -4 -5 -2 -10))   '(-2 -3))
  (('(5))                 '(5))
  (('())                  '())
  (('(-1 -2 50 -3 14 0 -10))  '(50 14))
  (('(-3 -4 -5 -3 -4 -5))     '(-3 -3))
  (('(1 0 -1))                '(1 0))
  (('(123456789123456789123456789))  '(123456789123456789123456789))
  (('(1 4 2 6 5 23 4 5 12 15))  '(23 15))
  (('(-1 0 -2))                '(0 -1))
)


(define-test reversed
        (('()) '())
    (('(5)) '(5))
    (('(2 6)) '(6 2))
    (('(4 6 3)) '(3 6 4))
    (('('(1 2 3) '(5) '(6 4 2))) '('(6 4 2) '(5) '(1 2 3)))
    (('(0 10 100 1000)) '(1000 100 10 0))
    (('(-1 3 0 -2 4)) '(4 -2 0 3 -1))
    (('(T NIL NIL)) '(NIL NIL T))
    (('(#\a #\b #\c #\d)) '(#\d #\c #\b #\a))
    (('('(1 2 3) '(4 5) '(6 7 8))) '('(6 7 8) '(4 5) '(1 2 3)))
    (('(1 2 3 5 4)) '(4 5 3 2 1))
    
)


(define-test clockwise
  (((list '(1 2) '(3 4))) (list '(3 1) '(4 2)))
  ( ((list '(5))) (list '(5)) )
  ( (NIL) NIL )
  ( ((list '(1 2 3) '(4 5 6) )) (list '(4 1) '(5 2) '(6 3)) )
  ( ((list '(1 2) '(3 4) '(5 6) )) (list '(5 3 1) '(6 4 2) ) )
  ( ((list '(1 2 3 4 5) )) (list '(1) '(2) '(3) '(4) '(5) ) )
  ( ((list '(1) '(2) '(3) '(4) '(5) )) (list '(5 4 3 2 1) ) )
  ( ((list '(1 1 1) '(2 2 2) '(3 3 3) )) (list '(3 2 1) '(3 2 1) '(3 2 1) ) )
  ( ((list '(4 4) '(4 4))) (list '(4 4) '(4 4) ) )
  ( ((list '(1 2 3 2 1) '(2 3 4 3 2) '(3 4 5 4 3) '(2 3 4 3 2) '(1 2 3 2 1))) (list '(1 2 3 2 1) '(2 3 4 3 2) '(3 4 5 4 3) '(2 3 4 3 2) '(1 2 3 2 1) ))

  )


(define-test any
    ((NIL)                NIL)
    (('(NIL))             NIL)
    (('(T))               T  )
    (('(T NIL))           T  )
    (('(NIL NIL))         NIL)
    (('(T NIL T))         T  )
    (('(NIL NIL T))       T  )
    (('(T T T T T))       T  )
    (('(NIL T T T NIL))   T  )
    (('(NIL NIL NIL NIL NIL NIL)) NIL)
)


(define-test select
  (('evenp '(1 2 3 4 5))  '(2 4))
  (('oddp  '(1 2 3 4 5))  '(1 3 5))
  (((lambda (x) (and (< 5 x) (< x 10)))  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))  '(6 7 8 9))
  (((lambda (x) (coprime x 5)) '(4 5 6 7 8 9 10 11 12))  '(4 6 7 8 9 11 12) )
  (((lambda (xs) (any xs)) (list '(T NIL T) '(T T) '(NIL NIL) '(T)))  (list '(T NIL T) '(T T) '(T)) )
  (((lambda (x) (= 5 x)) '(7 6 5 4 3 4 5 6 7))  '(5 5))
  (('evenp '(1 3 5 7 9))  NIL)
  (('evenp NIL)  NIL)
  (('evenp '(10 100))  '(10 100))
  (('oddp '(20 21 22))  '(21))
)


(define-test zip-with
  ((#'+  '(1 2 3 4) '(10 10 10 10)) '(11 12 13 14))
  ((#'+  '(1 2 3 4) '(5 6 7 8)) '(6 8 10 12))
  ((#'*  '(2 3 4)  '(5 5 5 5 5))  '(10 15 20))
  ((#'*  '(2 3 4 5 6 7 8)  '(5 5 5))  '(10 15 20))
  ((#'*  '(1 2 3 4 5)  '())  '())
  ((#'*  '()  '(2 3 4 5 56 6 7))  '())
  ((#'*  '() '())  '())
  ((#'=  '(1 2 3 4 5) '(1 1 3 3 3))  '(T NIL T NIL NIL))
  (('coprime  '(330 367 463) '(463 463 463 463))  '(T T NIL))
  ((#'*  '(1 2 3) '(3 2 1))  '(3 4 3))
)


(define-test augdentity
    ((1 1) '((1)) )
    ((2 2) '((1 0) (0 1)) )
    ((3 3) '((1 0 0) (0 1 0) (0 0 1)) )
    ((4 4) '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)) )
    ((2 3) '((1 0 0) (0 1 0)) )
    ((3 2) '((1 0) (0 1) (0 0)) )
    ((3 5) '((1 0 0 0 0) (0 1 0 0 0) (0 0 1 0 0)) )
    ((5 2) '((1 0) (0 1) (0 0) (0 0) (0 0)) )
    ((1 6) '((1 0 0 0 0 0)) )
    ((6 1) '((1) (0) (0) (0) (0) (0)) )
)

;; (load "yourfile.lisp")
;; (run-tests)
