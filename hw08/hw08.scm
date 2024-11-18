(define (ascending? s)
    (if (null? s) 
        #t
        (if (null? (cdr (cdr s)))
            (if (<= (car s) (car (cdr s)))
                #t
                #f
            )
            (if (<= (car s) (car (cdr s)))
                (ascending? (cdr s))
                #f
            )
        )
    )
)

(define (my-filter pred s) 
    (if (null? s) 
        nil
        (if (null? (cdr s))
            (if (pred (car s))
                (list (car s))
                (list )
            )
            (if (pred (car s))
                (append (list (car s)) (my-filter pred (cdr s)))
                (my-filter pred (cdr s))
            )
        )
    )
)

(define (interleave lst1 lst2) 
    (cond 
        ((and (null? lst1) (not (null? lst2))) lst2)
        ((and (null? lst2) (not (null? lst1))) lst1)
        ((and (null? lst1) (null? lst2)) '())
        (else 
            (append 
                (list (car lst1))
                (list (car lst2))
                (interleave (cdr lst1) (cdr lst2))
            )
        )
    )
)

(define (isInList? lst x)
    (if (null? lst) 
        #f
        (if (= (car lst) x)
            #t
            (isInList? (cdr lst) x)
        )
    )
)

(define (no-repeats s) 
    (define (helper seen rest)
        (if (null? rest)
            seen
            (if (isInList? seen (car rest))
                (helper seen (cdr rest)) ;skip the current
                (helper (append seen (list (car rest))) (cdr rest))
            )
        )
    ) 
  (helper '() s) ;init seen is empty
)