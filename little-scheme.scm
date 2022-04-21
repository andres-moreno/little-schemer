;; code for the Toys chapter of The Little Schemer

;; missing definitions
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (sub1 x)
  (- x 1))

(define (add1 x)
  (+ x 1))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (member? a (cdr lat)))))))


(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a) (cdr lat))
          (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (car (car l))
                      (firsts (cdr l)))))))

;; (firsts '((a b c) (1 2 3) (one two three))) (a 1 one)

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

;; (insertR 'topping 'fudge '(ice cream with fudge for dessert))
;; (insertR 'jalapeno 'and '(tacos tamales and salsa))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

;; (insertL 'topping 'fudge '(ice cream with fudge for dessert))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'cat 'dog '(the brown dog jumps over the lazy fox))

