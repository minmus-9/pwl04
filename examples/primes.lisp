;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prime sieve

(def (primes)
    (define pl '(2))
    (define t pl)
    (def (q x)
        (define n (cons x ()))
        (set-cdr! t n)
        (set! t n)
    )
    (define i 1)
    (define ok #t)
    (define l pl)
    (def (inner)
        (define p (car l))
        (set! l (cdr l))
        (if
            (null? l)
            ()
            (if 
                (equal? 0 (mod i p))
                (do (set! ok ()) ())
                (if (lt? i (mul p p)) () #t)
            )
        )
    )
    (def (outer)
        (set! i (add i 2))
        (set! ok #t)
        (set! l pl)
        (while inner)
        (if ok (do (q i) pl) ())
    )
    (def (driver)
        (while (lambda () (not (outer))))
        pl
    )
    driver
)

(define g (primes))
(def (h _) (g));(print (g)))
(for h 1 400 1)
