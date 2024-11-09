(define true #t)
(define false #f)

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

(define (quotient dividend divisor)
    (if (and (integer? dividend) (integer? divisor))
        (truncate (/ dividend divisor))
        (error "Arguments must be integers")))

(define (log10 operand) (log operand 10))
(define (log2 operand) (log operand 2))

(define (fib n)
    (define (fib_ a b i)
        (if (= i n)
            a
            (fib_ b (+ a b) (+ i 1))))
    (fib_ 0 1 0))

(define (identity x) x)

(define (compose f g) (lambda (x) (f (g x))))

(define (curry2 f)
    (lambda (x1)
        (lambda (x2) (f x1 x2))))
(define (curry3 f)
    (lambda (x1)
        (lambda (x2)
            (lambda (x3) (f x1 x2 x3)))))
(define (curry4 f)
    (lambda (x1)
        (lambda (x2)
            (lambda (x3)
                (lambda (x4) (f x1 x2 x3 x4))))))
(define (curry5 f)
    (lambda (x1)
        (lambda (x2)
            (lambda (x3)
                (lambda (x4)
                    (lambda (x5) (f x1 x2 x3 x4 x5)))))))

(define (flip f) (lambda (x y) (f y x)))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (list . items) items)

(define nil '())

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
    (define (reduce_ res rem)
        (if (null? rem)
            res
            (reduce_ (combiner res (car rem)) (cdr rem))))
    (reduce_ (car list) (cdr list)))

(define (fold combiner init list)
    (if (null? list)
        init
        (combiner
            (fold combiner init (cdr list))
            (car list))))

(define (reverse l)
    (define (reverse_ res rem)
        (if (null? rem)
            res
            (reverse_ (cons (car rem) res) (cdr rem))))
    (reverse_ nil l))

(define (any pred list)
    (define (any_ at)
        (if (null? at) 
            #f
            (if (pred (car at)) #t (any_ (cdr at)))))
    (any_ list))

(define (all pred list)
    (not (any (compose not pred) list)))

(define (for-each proc list)
    (if (not (null? list))
        (let ()
            (proc (car list))
            (for-each proc (cdr list)))))

(define (count-leaves tree)
    (cond
        ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else
            (+
                (count-leaves (car tree))
                (count-leaves (cdr tree))))))

(define (range from to step)
    (if ((if (> step 0) >= <=) from to)
        nil
        (cons from (range (+ from step) to step))))

(define (append . lists)
    (define (append2 acc list)
        (if (null? list)
            acc
            (cons (car list) (append2 acc (cdr list)))))
    (fold append2 nil lists))

(define (max n . ns)
    (fold (lambda (x y) (if (> x y) x y)) n ns))
(define (min n . ns)
    (fold (lambda (x y) (if (< x y) x y)) n ns))

(define (memq item x)
    (cond
        ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (displayln item)
    (display item)
    (newline))

(define (print . items)
    (for-each
        (lambda (item)
            (if (string? item)
                (begin
                    (display "\"")
                    (display item)
                    (display "\""))
                (display item))
            (display " "))
        items)
    (newline))
