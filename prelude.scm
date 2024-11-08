(define (not expr)
    (cond
        (expr #f)
        (else #t)))

(define (xor a b)
    (cond
        ((and a b) #f)
        ((not (or a b)) #f)
        (else #t)))

(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (= (remainder x 2) 1))
(define (zero? x) (= x 0))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (expt base power)
    (define (expt_ res p)
        (if (= p 0) res (expt_ (* res base) (- p 1))))
    (expt_ 1 power))

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

(define (list . items) items)

(define nil (list))

(define (length list)
    (define (length_ at count)
        (if (null? at)
            count
            (length_ (cdr at) (+ count 1))))
    (length_ list 0))

(define (map proc list)
    (if (null? list)
        list
        (cons
            (proc (car list))
            (map proc (cdr list)))))

(define (filter pred list)
    (if (null? list)
        list
        (let ((rest (filter pred (cdr list))))
            (if (pred (car list))
                (cons (car list) rest)
                rest))))

(define (reduce combiner list)
    (if (null? (cdr list))
        (car list)
        (combiner
            (car list)
            (reduce combiner (cdr list)))))

(define (reverse l)
    (define (reverse_ res rem)
        (if (null? rem)
            res
            (reverse_ (cons (car rem) res) (cdr rem))))
    (reverse_ nil l))

