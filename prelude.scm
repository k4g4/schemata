(define (not expr)
    (cond
        (expr #f)
        (else #t)))

;;(define (>= x y)
;;    (not (< x y)))
;;(define (<= x y)
;;    (not (> x y)))

(define (sqrt x)
    (define e .00001)
    (define (avg x y) (/ (+ x y) 2))
    (define (approx x y)
        (define upper (+ x e))
        (define lower (- x e))
        (and (<= y upper) (>= y lower)))
    (define (sqrt_ guess)
        (if (approx (* guess guess) x)
            guess
            (sqrt_ (avg guess (/ x guess)))))
    (sqrt_ (/ x 2)))
