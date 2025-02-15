; "calculate" pi as 4 * atan(1) using Taylor series (this is very slow)
(def (pi1 _)
    (define z 1)
    (define k 3)
    (define s -1.0)
    (def (f c & _) (if (lt? k 25000) (c c) ()));print (mul z 4))))
    (f  (call/cc (lambda (cc) cc))
        (set! z (add z (div s k)))
        (set! k (add k 2))
        (set! s (neg s))
    )
)
(timeit pi1 1)

;    k, a, b, a1, b1 = 2, 4, 1, 12, 4
;    while True:
;        # Next approximation
;        p, q, k = k*k, 2*k+1, k+1
;        a, b, a1, b1 = a1, b1, p*a+q*a1, p*b+q*b1
;        # Print common digits
;        d, d1 = a//b, a1//b1
;        while d == d1:
;            output(d)
;            a, a1 = 10*(a%b), 10*(a1%b1)
;            d, d1 = a//b, a1//b1

(def (pi2 & _)
    (define k 2)
    (define a 4)
    (define b 1)
    (define a1 12)
    (define b1 4)
    (define d ())
    (define d1 ())
    (def (next)
        (define p (mul k k))
        (define q (add (mul k 2) 1))
        (set! k (add k 1))
        (define t1 (add (mul p a) (mul q a1)))
        (define t2 (add (mul p b) (mul q b1)))
        (set! a a1)
        (set! b b1)
        (set! a1 t1)
        (set! b1 t2)
        (set! d (div a b))
        (set! d1 (div a1 b1))
        (while inner)
        (if
            (lt? k 20)
            #t
            ()
        )
    )
    (def (inner)
        (if
            (equal? d d1)
            (do
                ;(print d)
                (set! a  (mul 10 (mod a b)))
                (set! a1 (mul 10 (mod a1 b1)))
                (set! d  (div a b))
                (set! d1 (div a1 b1))
                #t
            )
            ()
        )
    )
    (while next)
)
(timeit pi2 100)
