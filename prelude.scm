(define (if pred then alt)
    (cond
        (pred then)
        (else alt)))

(define (not expr)
    (cond
        (expr #f)
        (else #t)))

;;(define (>= x y)
;;    (not (< x y)))
;;(define (<= x y)
;;    (not (> x y)))
