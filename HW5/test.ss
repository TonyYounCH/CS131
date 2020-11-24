#lang racket
(provide expr-compare)

#| don't forget to include the lines above!
	otherwise there might be troubles
	this file is directly runnable by either
	    racket FILENAME.ss
	or, open it in DrRacket and click run

    Also, it can be loaded from racket using

    (require "FILENAME.ss")

    for basic syntax introduction, please see slides and hello.ss
|#

; hint on judging lambda
(define (lambda? x)
  (member x '(lambda λ)))

(define (is_lambda x)
  (if (or (equal? x 'lambda) (equal? x 'λ)) #t #f))

(define (second_layer_exchange val to_val x)
  (if (empty? x)
      x
      (if (pair? x)
          (if (equal? 'quote (car x))
              x
              (if (list? (car x))
                  x
                  (if (equal? val (car x))
                      (cons to_val (second_layer_exchange val to_val (cdr x)))
                      (cons (car x) (second_layer_exchange val to_val (cdr x))))))
          (if (equal? val x)
              to_val
              x)
     )
  )
)
(define (exchange val to_val x)
  (if (empty? x)
      x
      (if (pair? x)
          (if (equal? 'quote (car x))
              x
              (if (list? (car x))
                  (cons (exchange val to_val (car x)) (exchange val to_val (cdr x)))
                  (if (equal? val (car x))
                      (cons to_val (exchange val to_val (cdr x)))
                      (cons (car x) (exchange val to_val (cdr x))))))
          (if (equal? val x)
              to_val
              x)
     )
  )
)

(define (different_param x y)
  (cond [(not (equal? (car x) (car y)))
         (let ([bound (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))])
             (list bound (car x) (car y))
           )
         ]
        [null])
  )

(define (bound_lambda x y)
  (if (empty? (car x))
      (list x y)
      (let ([bound (different_param (car x) (car y))])
        (if (empty? bound)
            (let ([new_list (bound_lambda (cons (cdr (car x)) (cdr x)) (cons (cdr (car y)) (cdr y)))])
              (list (cons (cons (caar x) (car (first new_list))) (cdr (first new_list))) (cons (cons (caar y) (car (second new_list))) (cdr (second new_list))))
              )
            (let ([new_list (bound_lambda (cons (exchange (second bound) (first bound) (car x)) (exchange (second bound) (first bound) (cadr x))) (cons (exchange (third bound) (first bound) (car y)) (exchange (third bound) (first bound) (cadr y))))])
              (list (list (caar new_list) (cdr (first new_list))) (list (car (second new_list)) (cdr (second new_list))))
              )
            ) 
        )
      )
  )


(define (expr-lambda-compare x y)
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) 
         (if x '% '(not %))]
        [(or (not (list? x)) 
             (not (list? y)))
         (list 'if '% x y)]
        [(not (equal? (length x) (length y)))
         (list 'if '% x y)]
        [(and (equal? (car x) (car y)) (equal? (car x) 'quote)) (list 'if '% x y)]
        ;[(and (or (equal? (car x) 'if) (equal? (car y) 'if)) (not (equal? (car x) (car y))))
         ;(list 'if '% x y)]
        [(cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
        ))


(define (expr-compare x y)
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) 
         (if x '% '(not %))]
        ; if one of them is not list - which means that not function
        [(or (not (list? x)) 
             (not (list? y)))
         (list 'if '% x y)]
         ; and below here it is your work to figure out how to judge every case
         ; but! please pay attention: this is NOT the only structure you could have for solving this homework
         ;     we actually encourage you to come up with OTHER designs if you can!
         ; please only follow this starting hint when you REALLY don't know where to start!
        [(not (equal? (length x) (length y)))
         (list 'if '% x y)]
        [(and (equal? (car x) (car y)) (equal? (car x) 'quote)) (list 'if '% x y)]
        [(and (or (equal? (car x) 'if) (equal? (car y) 'if)) (not (equal? (car x) (car y))))
         (list 'if '% x y)]
        [(and (equal? (length x) 3) (and (is_lambda (car x)) (is_lambda (car y))))
         (let ([new_list (bound_lambda (cdr x) (cdr y))])
           (if (equal? (car x) (car y))
               (expr-lambda-compare (cons (car x) (first new_list)) (cons (car y) (second new_list)))
               (expr-lambda-compare (cons 'λ (first new_list)) (cons 'λ (second new_list)))))
         ]
        [(cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
        ))

; compare and see if the (expr-compare x y) result is the same with x when % = #t
;                                                 and the same with y when % = #f
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

; WARNING: IT MUST BE A SINGLE TEST CASE
; You need to cover all grammars including:
;     constant literals, variables, procedure calls, quote, lambda, if
(define test-expr-x
  `(cons 12 ((lambda (a) (+ a 1)) 2)))

(define test-expr-y
  `(cons 11 ((lambda (a) (+ a 2)) 3)))


; the following line can be tested from interpreter
;     (eval test-expr-x)
;     (test-expr-compare test-expr-x test-expr-y))
;           test-expr-compare should return #t after you finish its implementation
;     (expr-compare 'a '(cons a b)) 
;     (expr-compare '(cons a b) '(cons a b))
;     (lambda? 'λ)
(equal? (expr-compare 12 12) 12)
(equal? (expr-compare 12 20)  '(if % 12 20))
(equal? (expr-compare #t #t)  #t)
(equal? (expr-compare #f #f)  #f)
(equal? (expr-compare #t #f)  '%)
(equal? (expr-compare #f #t)  '(not %))
(equal? (expr-compare 'a '(cons a b))  '(if % a (cons a b)))
(equal? (expr-compare '(cons a b) '(cons a b))  '(cons a b))
(equal? (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
(equal? (expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c)))
  '(cons (cons a (if % b c)) (cons (if % b a) c)))
(equal? (expr-compare '(cons a b) '(list a b)) '((if % cons list) a b))
(equal? (expr-compare '(list) '(list a)) '(if % (list) (list a)))
(equal? (expr-compare ''(a b) ''(a c))  '(if % '(a b) '(a c)))
(equal? (expr-compare '(quote (a b)) '(quote (a c))) '(if % '(a b) '(a c)))
(equal? (expr-compare '(quoth (a b)) '(quoth (a c))) '(quoth (a (if % b c))))
(equal? (expr-compare '(if x y z) '(if x z z))  '(if x (if % y z) z))
(equal? (expr-compare '(if x y z) '(g x y z))
  '(if % (if x y z) (g x y z)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
  '((lambda (a) ((if % f g) a)) (if % 1 2)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
  '((λ (a) ((if % f g) a)) (if % 1 2)))
(equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
  '((lambda (a!b) a!b) (if % c d)))
(equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
  '(if % '((λ (a) a) c) '((lambda (b) b) d)))
(equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
  '(+
     (not %)
     ((λ (a b!c) (f a b!c)) 1 2)))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
  '((λ (a b) (f (if % a b) (if % b a))) 1 2))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
  '((λ (a b!c) (f (if % a b!c) (if % b!c a)))
     1 2))
(equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))
  '((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3))
(equal? (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))
        '((λ (a)
      ((if % eq? eqv?)
       a
       ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
        a (λ (a!b) (if % a!b a)))))
     (lambda (b!a a!b) (b!a a!b))))