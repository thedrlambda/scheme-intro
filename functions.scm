; Plan
; - Functions
; - Lists
; - Symbols
; - Quassi

(define plus2 
  (lambda (n) (+ 2 n)))

(define plus 
  (lambda (a) 
    (lambda (b) 
      (+ a b))))
; f a b <> (f a) b
; ((f a) b)

(define fac 
  (lambda (n) 
    (if (zero? n)
      1
      (* n (fac (- n 1))))))

(define fib
  (lambda (n)
    (if (zero? n)
	0
	(if (zero? (- n 1))
	    1
	    (+ (fib (- n 1)) (fib (- n 2)))))))

(define gcd
  (lambda (a b)
    (if (= a b)
	a
	(if (< a b)
	    (gcd a (- b a))
	    (gcd (- a b) b)))))


(define make_rat
  (lambda (n d)
    (let ((g (gcd n d)))
      (cons (/ n g)
	    (/ d g)))))
(define get_num
  (lambda (r)
    (car r)))
(define get_denom
  (lambda (r)
    (cdr r)))


(define make_rat
  (lambda (n d)
    (lambda (selector)
      (let ((g (gcd n d)))
	(if (zero? selector)
	    (/ n g)
	    (/ d g))))))
(define get_num
  (lambda (r)
    (r 0)))
(define get_denom
  (lambda (r)
    (r 1)))


(define make_rat
  (lambda (n d)
    (lambda (selector)
      (let ((g (gcd n d)))
	(selector (/ n g) (/ d g))))))
(define get_num
  (lambda (r)
    (r (lambda (n d) n))))
(define get_denom
  (lambda (r)
    (r (lambda (n d) d))))


(define make_rat
  (lambda (n d)
    (lambda (selector)
      ((lambda (g)
	 (selector (/ n g) (/ d g)))
       (gcd n d)))))
(define get_num
  (lambda (r)
    (r (lambda (n d) n))))
(define get_denom
  (lambda (r)
    (r (lambda (n d) d))))


(define rat1 (make_rat 5 3))
(define rat2 (make_rat 60 44))


(define mult_rat
  (lambda (r1 r2)
    (make_rat
     (* (get_num r1) (get_num r2))
     (* (get_denom r1) (get_denom r2)))))


(define fac_helper
  (lambda (n acc)
    (if (zero? n)
	acc
	 (fac_helper (- n 1) (* acc n)))))
(define fac
  (lambda (n)
    (fac_helper n 1)))

;    n     a1 a2     
; 0  1  1  2  3  5  8
(define fib_tail
  (lambda (n a1 a2)
    (if (zero? n)
	a1
	(fib_tail (- n 1) a2 (+ a1 a2)))))
    

(define list+2
  (lambda (ns)
    (if (null? ns)
	'()
	(cons (+ (car ns) 2) (list+2 (cdr ns))))))

(define plus-n-list
  (lambda (n ns)
    (if (null? ns)
	'()
	(cons (+ (car ns) n) (plus-n-list n (cdr ns))))))

(define op-n-list
  (lambda (op n ns)
    (if (null? ns)
	'()
	(cons (op (car ns) n) (op-n-list op n (cdr ns))))))

(define map
  (lambda (op ns)
    (if (null? ns)
	'()
	(cons (op (car ns)) (map op (cdr ns))))))



(define sum-list
  (lambda (ns)
    (if (null? ns)
	0
	(+ (car ns) (sum-list (cdr ns))))))

(define op-list
  (lambda (op ns)
    (if (null? ns)
	0
	(op (car ns) (op-list op (cdr ns))))))

(define fold-right
  (lambda (op base ns)
    (if (null? ns)
	base
	(op (car ns) (fold-right op base (cdr ns))))))

(define fold-left
  (lambda (op base ns)
    (letrec ((loop
	      (lambda (base ns)
		(if (null? ns)
		    base
		    (loop (op base (car ns)) (cdr ns))))))
      (loop base ns))))


(define make_rat
  (lambda (n d)
    (cons 'rat (cons n d))))
(define get_num
  (lambda (r)
    (cadr r)))
(define get_denom
  (lambda (r)
    (cddr r)))

(define make_num
  (lambda (n)
    (cons 'num n)))
(define get_value
  (lambda (num)
    (cdr num)))
(define num?
  (lambda (num)
    (and (pair? num)
;	 (symbol? (car num))
	 (eq? 'num (car num)))))

(define value-of
  (lambda (n)
    (if (num? n)
	(get_value n)
	(/ (get_num n) (get_denom n)))))


(define map
  (lambda (op ns)
    (if (null? ns)
	'()
	(cons `(,op ,(car ns)) (map op (cdr ns))))))
(define map_helper
  (lambda (op ns)
    (cons 'list (map op ns))))


(define demo
  (lambda (xs)
    (if (null? (cdr xs))
	(car xs)
	`(if (< ,(car xs) ,(cadr xs))
	    ,(demo (cons (car xs) (cddr xs)))
	    ,(demo (cdr xs))))))

(define demo2
  (lambda (xs)
    `(let ((min (lambda (a b) (if (< a b) a b))))
      ,(letrec ((loop 
		(lambda (xs)
		  (if (null? (cdr xs))
		      (car xs)
		      `(min ,(car xs) ,(loop (cdr xs)))))))
	(loop xs)))))
