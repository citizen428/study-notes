;; Exercise 1.3
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-largest-2 x y z)
  (apply sum-of-squares (cdr (sort (list x y z) <))))

;; Exercise 1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess (sqrt-iter (improve guess x)
                       x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x) 
  (< (abs (- (improve guess x) guess)) 
     (abs (* guess .001)))) 
 
(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.8

(define (square x)
  (* x x))

(define (good-enough? guess x) 
  (< (abs (- (improve guess x) guess)) 
     (abs (* guess .001)))) 

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x)
                 x)))

(define (cube-root x)
  (cbrt-iter 1.0 x))

