;; Exercise 1.11

; recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (sub1 n))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

; iterative

(define (f n) 
  (define (f-iter a b c n) 
    ;; a = f(n - 1), b = f(n - 2), c = f(n - 3). 
    ;; return a + 2b + 3c 
    (if (< n 3) 
        a 
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1)))) 
  (if (< n 3) 
      n 
      (f-iter 2 1 0 n))) 

;; Exercise 1.12

(define (pascal-sums lst)
  (if (empty? (cdr lst))
      '()
      (cons (+ (car lst) (cadr lst))
            (pascal-sums (cdr lst)))))

(define (pascal-triangle depth)
  (define (triangle-iter lst n acc)
    (cond ((zero? n)
           (reverse acc))
          (else
           (triangle-iter `(1 ,@(pascal-sums lst) 1)
                          (sub1 n)
                          (cons lst acc)))))
  (triangle-iter '(1) depth '()))
