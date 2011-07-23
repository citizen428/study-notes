#lang scheme

;;; 1. Toys

;; also in mzlib/compat, but without null? check
(define (atom? x)
  (and
   (not (pair? x))
   (not (null? x))))

;;; 2. Do It, Do It Again, and Again, and Again...

(define (lat? l)
  (cond
   ((null? l) #t)
   ((atom? (car l)) (lat? (cdr l)))
   (else #f)))

(define (member? a lat)
  (cond
   ((null? lat) #f)
   (else (or (equal? (car lat) a)
             (member? a (cdr lat))))))

;;; 3. Cons the Magnificient

;; redefined in chapter 8
;;(define (rember a lat)
;;  (cond
;;    ((null? lat) '())
;;    ((equal? (car lat) a) (cdr lat))
;;    (else (cons (car lat) (rember a (cdr lat))))))

(define (firsts l)
  (cond
   ((null? l) '())
   (else (cons (caar l)
               (firsts (cdr l))))))

;; the next 3 functions get redefined in chapter 8
;;(define (insertR new old lat)
;;  (cond
;;    ((null? lat) '())
;;    ((equal? (car lat) old) (cons old (cons new (cdr lat))))
;;    (else (cons (car lat) (insertR new old (cdr lat))))))

;;(define (insertL new old lat)
;;  (cond
;;    ((null? lat) '())
;;    ((equal? (car lat) old) (cons new lat))
;;    (else (cons (car lat) (insertL new old (cdr lat))))))

;;(define (subst new old lat)
;;  (cond
;;    ((null? lat) '())
;;    ((equal? (car lat) old) (cons new (cdr lat)))
;;    (else (cons (car lat) (subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond
   ((null? lat)
    '())
   ((or (equal? (car lat)
                o1)
        (equal? (car lat)
                o2))
    (cons new (cdr lat)))
   (else
    (cons (car lat)
          (subst2 new o1 o2 (cdr lat))))))

(define (multirember a lat)
  (cond
   ((null? lat)
    '())
   ((equal? (car lat) a)
    (multirember a (cdr lat)))
   (else
    (cons (car lat)
          (multirember a (cdr lat))))))

(define (multiinsertR new old lat)
  (cond
   ((null? lat)
    '())
   ((equal? (car lat) old)
    (cons old
          (cons new
                (multiinsertR new old (cdr lat)))))
   (else
    (cons (car lat)
          (multiinsertR new old (cdr lat))))))

(define (multiinsertL new old lat)
  (cond
   ((null? lat)
    '())
   ((equal? (car lat) old)
    (cons new
          (cons old
                (multiinsertL new old (cdr lat)))))
   (else
    (cons (car lat)
          (multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
  (cond
   ((null? lat)
    '())
   ((equal? (car lat) old)
    (cons new
          (multisubst new old (cdr lat))))
   (else
    (cons (car lat)
          (multisubst new old (cdr lat))))))

;;; 4. Number Games

(define (o+ n m)
  (cond
   ((zero? m) n)
   (else (add1 (o+ n (sub1 m))))))

(define (o- n m)
  (cond
   ((zero? m) n)
   (else (sub1 (o- n (sub1 m))))))

(define (addtup tup)
  (cond
   ((null? tup) 0)
   (else (o+ (car tup) (addtup (cdr tup))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (x n (sub1 m)))))))

(define (tup+ tup1 tup2)
  (cond
   ((null? tup1)
    tup2)
   ((null? tup2)
    tup1)
   (else
    (cons (o+ (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2))))))

(define (o> n m)
  (cond
   ((zero? n) #f)
   ((zero? m) #t)
   (else (o> (sub1 n) (sub1 m)))))

(define (o< n m)
  (cond
   ((equal? n m) #f)
   (else (not (o> n m)))))

(define (o= n m)
  (cond
   ((or (o> n m) (o< n m)) #f)
   (else #t)))

(define (oexpt n m)
  (cond
   ((zero? n) 0)
   ((zero? m) 1)
   (else (x n (oexpt n (sub1 m))))))

(define (oquotient n m)
  (cond
   ((o< n m) 0)
   (else (add1 (oquotient (o- n m) m)))))

(define (olength lat)
  (cond
   ((null? lat) 0)
   (else (add1 (olength (cdr lat))))))

(define (pick n lat)
  (cond
   ((zero? (sub1 n))
    (car lat))
   (else
    (pick (sub1 n) (cdr lat)))))

;; second version, using one? defined below
(define (rempick n lat)
  (cond
   ((one? n)
    (cdr lat))
   (else
    (cons (car lat)
          (rempick (sub1 n) (cdr lat))))))

(define (no-nums lat)
  (cond
   ((null? lat)
    '())
   (else
    (cond ((number? (car lat))
           (no-nums (cdr lat)))
          (else
           (cons (car lat)
                 (no-nums (cdr lat))))))))

(define (all-nums lat)
  (cond
   ((null? lat)
    '())
   (else
    (cond
     ((number? (car lat))
      (cons (car lat) (all-nums (cdr lat))))
     (else
      (all-nums (cdr lat)))))))

;; use equal? instead of this
(define (eqan? a1 a2)
  (cond
   ((and (number? a1) (number? a2)) (= a1 a2))
   ((or (number? a1) (number? a2)) #f)
   (else (eq? a1 a2))))

(define (occur a lat)
  (cond
   ((null? lat)
    0)
   (else
    (cond
     ((equal? (car lat) a)
      (add1 (occur a (cdr lat))))
     (else
      (occur a (cdr lat)))))))

(define (one? n)
  (= n 1))

;; 5. *Oh My Gawd*: It's Full of Stars

(define (rember* a l)
  (cond
   ((null? l)
    '())
   ((atom? (car l))
    (cond
     ((equal? (car l) a)
      (rember* a (cdr l)))
     (else
      (cons (car l)
            (rember* a (cdr l))))))
   (else
    (cons (rember* a (car l))
          (rember* a (cdr l))))))

(define (insertR* new old l)
  (cond
   ((null? l)
    '())
   ((atom? (car l))
    (cond
     ((equal? (car l) old)
      (cons old (cons new (insertR* new old (cdr l)))))
     (else
      (cons (car l)
            (insertR* new old (cdr l))))))
   (else
    (cons (insertR* new old (car l))
          (insertR* new old (cdr l))))))

(define (occur* a l)
  (cond
   ((null? l)
    0)
   ((atom? (car l))
    (cond
     ((equal? (car l) a)
      (add1 (occur* a (cdr l))))
     (else
      (occur* a (cdr l)))))
   (else
    (o+ (occur* a (car l))
        (occur* a (cdr l))))))

(define (subst* new old l)
  (cond
   ((null? l)
    '())
   ((atom? (car l))
    (cond
     ((equal? (car l) old)
      (cons new (subst* new old (cdr l))))
     (else
      (cons (car l)
            (subst* new old (cdr l))))))
   (else
    (cons (subst* new old (car l))
          (subst* new old (cdr l))))))

(define (insertL* new old l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((equal? (car l) old)
      (cons new
            (cons old
                  (insertL* new old (cdr l)))))
     (else
      (cons (car l)
            (insertL* new old (cdr l))))))
   (else
    (cons (insertL* new old (car l))
          (insertL* new old (cdr l))))))

(define (member* a l)
  (cond
   ((null? l)
    #f)
   ((atom? (car l))
    (or (equal? (car l) a)
        (member* a (cdr l))))
   (else
    (or (member* a (car l))
        (member* a (cdr l))))))

(define (leftmost l)
  (cond
   ((atom? (car l)) (car l))
   (else (leftmost (car l)))))

(define (eqlist? l1 l2)
  (cond
   ((and (null? l1)
         (null? l2))
    #t)
   ((or (null? l1)
        (null? l2))
    #f)
   (else
    (and (equal? (car l1)
                 (car l2))
         (eqlist? (cdr l1) (cdr l2))))))

;; generalized remove member
(define (rember2 s l)
  (cond
   ((null? l)
    '())
   ((equal? (car l) s)
    (cdr l))
   (else
    (cons (car l)
          (rember s (cdr l))))))

;;; 6. Shadows

(define (numbered? aexp)
  (cond
   ((atom? aexp)
    (number? aexp))
   (else
    (and (numbered? (car aexp))
         (numbered? (caddr aexp))))))

(define (1st-sub-exp aexp)
  (car aexp))

(define (2nd-sub-exp aexp)
  (caddr aexp))

(define (operator aexp)
  (cadr aexp))

;; redefined in chapter 8
;;(define (value nexp)
;;  (cond
;;    ((atom? nexp) nexp)
;;    ((eq? (operator nexp) '+) (o+ (value (1st-sub-exp nexp))
;;                                  (value (2nd-sub-exp nexp))))
;;    ((eq? (operator nexp) 'x) (x (value (1st-sub-exp nexp))
;;                                 (value (2nd-sub-exp nexp))))
;;    (else
;;     (oexpt (value (1st-sub-exp nexp))
;;            (value (2nd-sub-exp nexp))))))

(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

;;; 7. Friends and Relations

(define (set? lat)
  (cond
   ((null? lat)
    #t)
   ((member? (car lat) (cdr lat))
    #f)
   (else
    (set? (cdr lat)))))

(define (makeset lat)
  (cond
   ((null? lat)
    '())
   (else
    (cons (car lat)
          (makeset (multirember (car lat)
                                (cdr lat)))))))

(define (subset? set1 set2)
  (cond
   ((null? set1)
    #t)
   (else
    (and (member? (car set1) set2)
         (subset? (cdr set1) set2)))))

(define (eqset? set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond
   ((null? set1)
    #f)
   (else
    (or (member? (car set1) set2)
        (intersect? (cdr set1) set2)))))

(define (intersect set1 set2)
  (cond
   ((null? set1)
    '())
   ((member? (car set1) set2)
    (cons (car set1)
          (intersect (cdr set1) set2)))
   (else
    (intersect (cdr set1) set2))))

(define (union set1 set2)
  (cond
   ((null? set1)
    set2)
   ((member? (car set1) set2)
    (union (cdr set1) set2))
   (else
    (cons (car set1)
          (union (cdr set1) set2)))))

(define (intersectall l-set)
  (cond
   ((null? (cdr l-set))
    (car l-set))
   (else
    (intersect (car l-set)
               (intersectall (cdr l-set))))))

(define (a-pair? x)
  (cond
   ((atom? x) #f)
   ((null? x) #f)
   ((null? (cdr x)) #f)
   ((null? (cddr x)) #t)
   (else #f)))

(define (first p) (car p))

(define (second p) (cadr p))

(define (build s1 s2)
  (cons s1
        (cons s2 '())))

(define (third l)
  (caddr l))

(define (fun? rel)
  (set? (firsts rel)))

(define (revpair pair)
  (build (second pair) (first pair)))

(define (revrel rel)
  (cond
   ((null? rel)
    '())
   (else
    (cons (revpair (car rel))
          (revrel (cdr rel))))))

(define (seconds l)
  (cond
   ((null? l)
    '())
   (else
    (cons (cadar l)
          (seconds (cdr l))))))

(define (fullfun? fun)
  (set? (seconds fun)))

;; alternative to fullfun?
(define (one-to-one? fun)
  (fun? (revrel fun)))

;;; 8. Lambda the Ultimate

(define (rember-f test?)
  (lambda (a l)
    (cond
     ((null? l)
      '())
     ((test? (car l) a)
      (cdr l))
     (else
      (cons (car l)
            ((rember-f test?) a (cdr l)))))))

(define (eq-c? a)
  (lambda (x)
    (eq? x a)))

(define (insertL-f test?)
  (lambda (new old l)
    (cond
     ((null? l)
      '())
     ((test? (car l) old)
      (cons new
            (cons old
                  (cdr l))))
     (else
      (cons (car l)
            ((insertL-f test?) new old (cdr l)))))))

(define (insertR-f test?)
  (lambda (new old l)
    (cond
     ((null? l)
      '())
     ((test? (car l) old)
      (cons old
            (cons new
                  (cdr l))))
     (else
      (cons (car l)
            ((insertR-f test?) new old (cdr l)))))))

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

(define (seqS new old l)
  (cons new l))

(define (insert-g seq)
  (lambda (new old l)
    (cond
     ((null? l)
      '())
     ((equal? (car l) old)
      (seq new old (cdr l)))
     (else
      (cons (car l)
            ((insert-g seq) new old (cdr l)))))))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(define subst
  (insert-g
   (lambda (new old l)
     (cons new l))))

(define (rember a l)
  ((insert-g
    (lambda (new old l) l)) #f a l))

(define (atom-to-function x)
  (cond
   ((equal? x '+) o+)
   ((equal? x '-) o-)
   (else oexpt)))

;; redefined in chapter 10
;;(define (value nexp)
;;  (cond
;;   ((atom? nexp) nexp)
;;   (else
;;    ((atom-to-function (operator nexp))
;;     (value (1st-sub-exp nexp))
;;     (value (2nd-sub-exp nexp))))))

(define (multirember-f test?)
  (lambda (a lat)
    (cond
     ((null? lat)
      '())
     ((test? (car lat) a)
      ((multirember-f test?) a (cdr lat)))
     (else
      (cons (car lat)
            ((multirember-f test?) a (cdr lat)))))))

(define (multiremberT test? lat)
  (cond
   ((null? lat)
    '())
   ((test? (car lat))
    (multiremberT test? (cdr lat)))
   (else
    (cons (car lat)
          (multiremberT test? (cdr lat))))))

(define (multirember&co a lat col)
  (cond
   ((null? lat)
    (col '() '()))
   ((equal? (car lat) a)
    (multirember&co a
                    (cdr lat)
                    (lambda (newlat seen)
                      (col newlat
                           (cons (car lat) seen)))))
   (else
    (multirember&co a
                    (cdr lat)
                    (lambda (newlat seen)
                      (col (cons (car lat) newlat) seen))))))

(define (multiinsertLR new oldL oldR lat)
  (cond
   ((null? lat)
    '())
   ((equal? (car lat) oldL)
    (cons new
          (cons oldL
                (multiinsertLR new oldL oldR (cdr lat)))))
   ((equal? (car lat) oldR)
    (cons oldR
          (cons new
                (multiinsertLR new oldL oldR (cdr lat)))))
   (else
    (cons (car lat)
          (multiinsertLR oldL oldR (cdr lat))))))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond
   ((null? lat)
    (col '() 0 0))
   ((equal? (car lat) oldL)
    (multiinsertLR&co new
                      oldL
                      oldR
                      (cdr lat)
                      (lambda (newlat L R)
                        (col (cons new
                                   (cons oldL newlat))
                             (add1 L)
                             R))))
   ((equal? (car lat) oldR)
    (multiinsertLR&co new
                      oldL
                      oldR
                      (cdr lat)
                      (lambda (newlat L R)
                        (col (cons oldR
                                   (cons new newlat))
                             L
                             (add1 R)))))
   (else
    (multiinsertLR&co new
                      oldL
                      oldR
                      (cdr lat)
                      (lambda (newlat L R)
                        (col (cons (car lat)
                                   newlat)
                             L
                             R))))))

(define (evens-only* l)
  (cond
   ((null? l)
    '())
   ((atom? (car l))
    (cond
     ((even? (car l))
      (cons (car l)
            (evens-only* (cdr l))))
     (else (evens-only* (cdr l)))))
   (else
    (cons (evens-only* (car l))
          (evens-only* (cdr l))))))

(define (evens-only*&co l col)
  (cond
   ((null? l)
    (col '() 1 0))
   ((atom? (car l))
    (cond
     ((even? (car l))
      (evens-only*&co (cdr l)
                      (lambda (newl p s)
                        (col (cons (car l) newl)
                             (* (car l) p)
                             s))))
     (else (evens-only*&co (cdr l)
                           (lambda  (newl p s)
                             (col newl
                                  p
                                  (+ (car l) s)))))))
   (else
    (evens-only*&co (car l)
                    (lambda (al ap as)
                      (evens-only*&co (cdr l)
                                      (lambda (dl dp ds)
                                        (col (cons al dl)
                                             (* ap dp)
                                             (+ as ds)))))))))

(define (the-last-friend newl product sum)
  (cons sum (cons product newl)))

;;; 9. ...and Again, and Again, and Again,...

(define (looking a lat)
  (keep-looking a
                (pick 1 lat)
                lat))

(define (keep-looking a sorn lat)
  (cond
   ((number? sorn)
    (keep-looking a
                  (pick sorn lat)
                  lat))
   (else
    (equal? sorn a))))

;; this will loop forever
(define (eternity x)
  (eternity x))

(define (shift pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

(define (align pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (align (shift pora)))
   (else
    (build (first pora)
           (align (second pora))))))

(define (length* pora)
  (cond
   ((atom? pora) 1)
   (else
    (+ (length* (first pora))
       (length* (second pora))))))

(define (weight* pora)
  (cond
   ((atom? pora)
    1)
   (else
    (+ (* (weight* (first pora)) 2)
       (weight* (second pora))))))

(define (shuffle pora)
  (cond
   ((atom? pora)
    pora)
   ((a-pair? (first pora))
    (shuffle (revpair pora)))
   (else
    (build (first pora)
           (shuffle (second pora))))))

;; Collatz function
(define (C n)
  (cond
   ((one? n)
    1)
   (else
    (cond
     ((even? n)
      (C (/ n 2)))
     (else
      (C (add1 (* 3 n))))))))

;; Ackermann function
(define (A n m)
  (cond
   ((zero? n)
    (add1 m))
   ((zero? m)
    (A (sub1 n) 1))
   (else
    (A (sub1 n)
       (A n (sub1 m))))))

;; applicative-order Y combinator
;; definition in lambda calculus
;; (lambday.(lambdax.y(xx)(lambdax.y)(xx)))
;; applied to a function F:
;; YF = (lambdax.F(xx))(lambdax.R(xx))
;; further reduction:
;; R((lambdax.R(xx))(lambdax.R(xx)))
;; this means that
;; YR = R(YR)
(define (Y le)
  ((lambda (f) (f f))
   (lambda (f)
     (le (lambda (x) ((f f) x))))))

;;; 10. What Is the Value of All of This?

;; an entry is a pair of lists of equal length
;; where the first list must be a set
(define new-entry build)

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

(define (lookup-in-entry-help name names values entry-f)
  (cond
   ((null? names)
    (entry-f name))
   ((equal? (car names) name)
    (car values))
   (else
    (lookup-in-entry-help name
                          (cdr names)
                          (cdr values)
                          entry-f))))

;; a table (aka environment) is a list of entries
(define extend-table cons)

(define (lookup-in-table name table table-f)
  (cond
   ((null? table)
    (table-f name))
   (else
    (lookup-in-entry name
                     (car table)
                     (lambda (name)
                       (lookup-in-table name
                                        (cdr table)
                                        table-f))))))

(define (expression-to-action e)
  (cond
   ((atom? e) (atom-to-action e))
   (else (list-to-action e))))

(define (atom-to-action e)
  (cond
   ((number? e) *const)
   ((equal? e #t) *const)
   ((equal? e #f) *const)
   ((equal? e 'cons) *const)
   ((equal? e 'car) *const)
   ((equal? e 'cdr) *const)
   ((equal? e 'null?) *const)
   ((equal? e 'equal?) *const)
   ((equal? e 'atom?) *const)
   ((equal? e 'zero?) *const)
   ((equal? e 'add1) *const)
   ((equal? e 'sub1) *const)
   ((equal? e 'number?) *const)
   (else *identifier)))

(define (list-to-action e)
  (cond
   ((atom? (car e))
    (cond
     ((equal? (car e) 'quote)
      *quote)
     ((equal? (car e) 'lambda)
      *lambda)
     ((equal? (car e) 'cond)
      *cond)
     (else
      *application)))
   (else
    *application)))

;; this approximates eval
(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*const e table)
  (cond
   ((number? e) e)
   ((equal? e #t) #t)
   ((equal? e #f) #f)
   (else (build 'primitive e))))

(define (*quote e table)
  (text-of e))

(define text-of second)

(define (*identifier e table)
  (lookup-in-table e table initial-table))

;; let's hope we don't need this
(define (initial-table name)
  (car '()))

(define (*lambda e table)
  (build 'non-primitive
         (cons table
               (cdr e))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define (evcon lines table)
  (cond
   ((else? (question-of (car lines)))
    (meaning (answer-of (car lines))
             table))
   ((meaning (question-of (car lines))
             table)
    (meaning (answer-of (car lines))
             table))
   (else
    (evcon (cdr lines) table))))

(define (else? x)
  (cond
   ((atom? x) (equal? x 'else))
   (else #f)))

(define question-of first)

(define answer-of second)

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define cond-lines-of cdr)

(define (evlis args table)
  (cond
   ((null? args)
    '())
   (else
    (cons (meaning (car args) table)
          (evlis (cdr args) table)))))

(define (*application e table)
  (apply
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define function-of car)

(define arguments-of cdr)

(define (primitive? l)
  (equal? (first l) 'primitive))

(define (non-primitive? l)
  (equal? (first l) 'non-primitive))

;; approximating apply
(define (apply fun vals)
  (cond
   ((primitive? fun)
    (apply-primitive (second fun)
                     vals))
   ((non-primitive? fun)
    (apply-closure (second fun)
                   vals))))

(define (apply-primitive name vals)
  (cond
   ((equal? name 'cons)
    (cons (first vals) (second vals)))
   ((equal? name 'car)
    (car (first vals)))
   ((equal? name 'cdr)
    (cdr (first vals)))
   ((equal? name 'null?)
    (null? (first vals)))
   ((equal? name 'equal?)
    (equal? (first vals) (second vals)))
   ((equal? name 'atom?)
    (:atom? (first vals)))
   ((equal? name 'zero?)
    (zero? (first vals)))
   ((equal? name 'add1)
    (add1 (first vals)))
   ((equal? name 'sub1)
    (sub1 (first vals)))
   ((equal? name 'number?)
    (number? (first vals)))))

(define (:atom? x)
  (cond
   ((atom? x) #t)
   ((null? x) #f)
   ((equal? (car x) 'primitive) #t)
   ((equal? (car x) 'non-primitive) #t)
   (else #f)))

(define (apply-closure closure vals)
  (meaning (body-of closure)
           (extend-table
            (new-entry (formals-of closure)
                       vals)
            (table-of closure))))
