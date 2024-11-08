(define (not expr)
    (cond
        (expr #f)
        (else #t)))

(define (xor a b)
    (cond
        ((and a b) #f)
        ((not (or a b)) #f)
        (else #t)))

;;(define (>= x y)
;;    (not (< x y)))
;;(define (<= x y)
;;    (not (> x y)))

(define (even? x) (= (remainder x 2) 0))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (pow x p)
    (define (pow_ res p)
        (if (= p 0) res (pow_ (* res x) (- p 1))))
    (pow_ 1 p))

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

(define (abs n)
    (if (< n 0) (- n) n))

(define (fib n)
    (define (fib_ a b i)
        (if (= i 1)
            a
            (fib_ b (+ a b) (- i 1))))
    (fib_ 0 1 n))

(define (identity x) x)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))