; from sicp

(def (deriv exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp)
            (if
                (same-variable? exp var)
                1
                0
            )
        )
        ((sum? exp)
            (make-sum
                (deriv (addend exp) var)
                (deriv (augend exp) var)
            )
        )
        ((product? exp)
            (make-sum
                (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var)
                )
                (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp)
                )
            )
        )
        (#t (error "unknown expression type: DERIV" exp))
    )
)

(def (number? x)
    (or
        (eq? (type x) 'integer)
        (eq? (type x) 'float)
    )
)

(def (symbol? x)
    (eq? (type x) 'symbol)
)

(def (variable? x) (symbol? x))

(def (same-variable? x y)
    (and
        (variable? x)
        (variable? y)
        (eq? x y)
    )
)

(def (zero? x)
    (and (number? x) (equal? x 0))
)

(def (one? x)
    (and (number? x) (equal? x 1))
)

(def (make-sum x y)
    (cond
        ((zero? x) y)
        ((zero? y) x)
        ((and (number? x) (number? y)) (add x y))
        (#t (list 'add x y))
    )
)

(def (make-product x y)
    (cond
        ((or (zero? x) (zero? y)) 0)
        ((one? x) y)
        ((one? y) x)
        ((and (number? x) (number? y)) (mul x y))
        (#t (list 'mul x y))
    )
)

(def (sum? x)
    (and
        (pair? x)
        (eq? (car x) 'add)
    )
)
(def (addend x) (cadr x))
(def (augend x) (caddr x))

(def (product? x)
    (and
        (pair? x)
        (eq? (car x) 'mul)
    )
)
(def (multiplier x) (cadr x))
(def (multiplicand x) (caddr x))

(deriv '(add x 3) 'x)
(deriv '(mul x y) 'x)
(deriv '(mul (mul x y) (add x 3)) 'x)
