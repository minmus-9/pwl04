; from sicp

(
    (lambda (n)
        (
            (lambda (fact)
                (fact fact n)
            )   (lambda (ft k) (if (equal? k 1) 1 (mul k (ft ft (sub k 1)))))
        )
    )
10)




(define cons (lambda (x y) (lambda (m) (m x y))))

(special cons (lambda (__special_cons_x__ __special_cons_y__)
    (eval `(lambda (__special_cons_m__) (__special_cons_m__ ,__special_cons_x__ ,__special_cons_y__)) 1)
))

(define car  (lambda (z) (z (lambda (p q) p))))
(define cdr  (lambda (z) (z (lambda (p q) q))))

(define list-ref (lambda (items n)
    (if (equal? n 0)
        (car items)
        (list-ref (cdr items) (sub n 1))
    )
))

(define map (lambda (proc items)
    (if (null? items)
        ()
        (cons (proc (car items)) (map proc (cdr items)))
    )
))

(define scale-list (lambda (items factor) (map (lambda (x) (* x factor)) items)))

(define add-lists (lambda (list1 list2)
    (cond
        ((null? list1) list2)
        ((null? list2) list1)
        (#t (cons
            (add (car list1) (car list2))
            (add-lists (cdr list1) (cdr list2))))
    )
))

(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))
(print (car integers))
(print (cdr integers))
(print (cadr integers))
(print (cdr (cdr integers)))

;; this takes 74 seconds!!!
;(list-ref integers 17)
