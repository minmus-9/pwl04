; from sicp

(def (memo-proc proc)
    (let (
        (already-run? ())
        (result ())
    )
    (lambda ()
        (if
            (not already-run?)
            (do (set! result (proc))
                (set! already-run? #t)
                result
            )
            result
        )
    ))
)

(define the-empty-stream ())
(define stream-null? null?)

(def (stream-ref s n)
     (if
        (equal? n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (sub n 1))
    )
)

(def (stream-map proc s)
    (if
        (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
            (stream-map proc (stream-cdr s)))
    )
)

(def (stream-for-each proc s)
    (if
        (stream-null? s)
        'done
        (do
            (proc (stream-car s))
            (stream-for-each proc (stream-cdr s))
        )
    )
)

(special delay (lambda (x)
    (eval `(memo-proc (lambda () ,x)) 1)
))

(def (force x) (x))

(special cons-stream (lambda (x y)
    (eval `(cons ,x (memo-proc (lambda () ,y))) 1)
))

(def (stream-car x) (car x))
(def (stream-cdr x) (force (cdr x)))

(def (stream-filter pred stream)
    (cond
        ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
            (cons-stream (stream-car stream)
                (stream-filter pred (stream-cdr stream))))
        (#t (stream-filter pred (stream-cdr stream)))
    )
)





(def (fibgen a b) (cons-stream a (fibgen b (add a b))))
(define fibs (fibgen 0 1))

(while (lambda ()
    (do
        ;(print (stream-car fibs))
        (set! fibs (stream-cdr fibs))
        () ;#t
    )
))

(def (stream-counter start)
    (cons-stream
        start
        (stream-counter (add start 1))
    )
)

(def (sieve stream)
    (def (divisible? x y) (equal? (mod x y) 0))
    (cons-stream
        (stream-car stream)
        (sieve
            (stream-filter
                (lambda (x)
                  (not (divisible? x (stream-car stream)))
                )
                (stream-cdr stream)
            )
        )
    )
)
(define primes (sieve (stream-counter 2)))
;(stream-ref primes 50)  ;; almost 6.5 sec to compute!
;(stream-for-each print primes)


(def (stream-enumerate-interval low high)
    (if
        (lt? high low)
        the-empty-stream
        (cons-stream low
            (stream-enumerate-interval (add low 1) high))
    )
)

;(stream-for-each print (stream-enumerate-interval 1 10))

(def (map2 proc lst)
    (if
        (null? lst)
        ()
        (cons (proc (car lst))
              (map2 proc (cdr lst)))
    )
)

(def (print-ident x) (print x) x)

(def (stream-map proc & argstreams)
    (if
        (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map2 stream-car argstreams))
            (apply stream-map (cons proc (map2 stream-cdr argstreams)))
        )
    )
)
    
(define ones (cons-stream 1 ones))
(def (add-streams s1 s2) (stream-map print s1 s2))
(stream-for-each print (add-streams ones ones))
(define integers
    (cons-stream 1 (add-streams ones integers)))

(stream-for-each print integers)
