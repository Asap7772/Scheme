(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests)
)

(define (zip pairs)
  (cond
    ((null? pairs) (cons nil (cons nil nil)))
    (else
        (begin
            (define s "\n")
            (define x (zip (cdr pairs)))
            (define firstElem (cons (car(car pairs)) (car x)))
            (define secondElem (cons (cons (cadr (car pairs)) (cadr x))nil))
            (cons firstElem secondElem)
        )
    )
  )
)

;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (helper s index)
    (if (null? s)
      nil
      (cons (cons index (cons (car s) nil)) (helper (cdr s) (+ index 1)))
    )
  )
  (helper s 0)
)
  ; END PROBLEM 16

;; Problem 17
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
    ; BEGIN PROBLEM 17
    (cond
        ((null? denoms) nil)
        ((= total (car denoms)) (append (cons (cons total nil) nil) (list-change total (cdr denoms))))
        ((> (car denoms) total) (list-change total (cdr denoms)))
        (else
            (append
                (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                (list-change total (cdr denoms))
            )
        )
    )
)

  ; END PROBLEM 17

;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (cons form (cons params (let-to-lambda body)))
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (define params (let-to-lambda (car (zip values))))
           (define numbers (let-to-lambda (cadr (zip values))))
           (define evaluatedBody (let-to-lambda body))
           ; (display expr)
           ; (display '"\n")
           ; (display '"\n")
           ; (display body)
           ; (display '"\n")
           ; (display '"\n")
           ; (display evaluatedBody)
           ; (display '"\n")
           ; (display '"\n")
           (cons (cons 'lambda (cons params evaluatedBody)) numbers)
           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
         (map (lambda (x) (let-to-lambda x)) expr)
         ; END PROBLEM 18
         )))
