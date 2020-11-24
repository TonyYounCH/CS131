#lang racket
(provide expr-compare)
(define ns (make-base-namespace))

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
(define LAMBDA (string->symbol "\u03BB"))

(define (lambda? x)
  (member x '(lambda 'λ)))

(define (is_lambda x)
  (if (or (equal? x 'lambda) (equal? x 'λ)) #t #f))

(define (is_lambda_fun x)
  (cond
    [(not (pair? x)) #f]
    [(and (equal? (length x) 3) (is_lambda (car x))) #t]
    [else #f]
    )
  )
(define (is_lambda_fun_with_wrong_param x y)
  (cond
    [(and (is_lambda_fun x) (is_lambda_fun y))
     (cond
       [(and (not (pair? (second x))) (not (pair? (second y)))) #f]
       [(and (not (pair? (second x))) (pair? (second y))) #t]
       [(and (not (pair? (second y))) (pair? (second x))) #t]
       [(not (equal? (length (second x)) (length (second y)))) #t]
       [else #f]
       )]
    [else #f]
    )
  )
#|
(define x '((lambda (lambda) (+ lambda if (f lambda))) 3))
(define y '((lambda (if) (+ if if (f λ))) 3))
|#
(define (cleans_lambda_diff1 x y)
  (cond
    [(empty? x) x]
    [(not (pair? x)) x]
    [(is_lambda_fun_with_wrong_param x y) x]
    [(and (is_lambda_fun x) (is_lambda_fun y))
     (let ([new_x (list (first x) (second x) (cleans_lambda_diff1 (third x) (third y)))]
           [new_y (list (first y) (second y) (cleans_lambda_diff2 (third x) (third y)))])
       (first (bound_lambda_fun new_x new_y)))
     ]
    [(or (equal? 'quote (car x)) (equal? 'quote (car y))) x]
    [(cons (cleans_lambda_diff1 (car x) (car y)) (cleans_lambda_diff1 (cdr x) (cdr y)))]
   )
  )

(define (cleans_lambda_diff2 x y)
  (cond
    [(empty? y) y]
    [(not (pair? x)) y]
    [(is_lambda_fun_with_wrong_param x y) y]
    [(and (is_lambda_fun x) (is_lambda_fun y))
     (let ([new_x (list (first x) (second x) (cleans_lambda_diff1 (third x) (third y)))]
           [new_y (list (first y) (second y) (cleans_lambda_diff2 (third x) (third y)))])
       (second (bound_lambda_fun new_x new_y)))
     ]
    [(or (equal? 'quote (car x)) (equal? 'quote (car y))) y]
    [(cons (cleans_lambda_diff2 (car x) (car y)) (cleans_lambda_diff2 (cdr x) (cdr y)))]
   )
  )

(define (bound_var var1 var2)
  (string->symbol (string-append (symbol->string var1) "!" (symbol->string var2)))
  )

(define (is_member x y)
  (cond
    [(not (list? y)) (if (equal? x y) #t #f)]
    [(empty? y) #f]
    [(member x y) #t]
    [else (or (is_member x (car y)) (is_member x (cdr y)))]
    )
  )

(define (params x y)
  (cond
    [(empty? x) #f]
    [else (or (is_member (car x) y) (params (cdr x) y))]
    )
  )

(define (get_new_dict dict x y)
  (cond
    [(empty? x) dict]
    [(is_member (car x) y)
     (get_new_dict (dict-remove dict (car x)) (cdr x) y)]
    [else (get_new_dict dict (cdr x) y)]
    )
  )

(define (helper1_inner dict1 x)
  (cond
    [(equal? dict1 (get_new_dict dict1 (second x) (third x)))
     (list (first x) (second x) (helper1 dict1 (third x)))]
    [else (list (first x) (second x) (helper1 (get_new_dict dict1 (second x) (third x)) (third x)))]
    )
  )

(define (helper1 dict1 x)
  (cond
    [(pair? x)
     (if (not (is_lambda_fun x)) (change_with_dict1 dict1 x) (helper1_inner dict1 x))]
    [(empty? x) null]
    [(if (dict-has-key? dict1 x)
         (bound_var x (dict-ref dict1 x))
         x)]
    )
  )

(define (change_with_dict1 dict1 x)
  (cond
    [(empty? x) null]
    [(not (pair? x))
     (if (dict-has-key? dict1 x)
         (bound_var x (dict-ref dict1 x))
         x)]
    [(equal? (car x) 'quote) x]
    [(cons (helper1 dict1 (car x)) (change_with_dict1 dict1 (cdr x)))]
    )
  )

(define (helper2_inner dict2 x)
  (cond
    [(equal? dict2 (get_new_dict dict2 (second x) (third x)))
     (list (first x) (second x) (helper2 dict2 (third x)))]
    [else (list (first x) (second x) (helper2 (get_new_dict dict2 (second x) (third x)) (third x)))])
  )

(define (helper2 dict2 x)
  (cond
    [(pair? x)
     (if (not (is_lambda_fun x)) (change_with_dict2 dict2 x) (helper2_inner dict2 x))]
    [(empty? x) null]
    [(if (dict-has-key? dict2 x)
         (bound_var (dict-ref dict2 x) x)
         x)]
    )
  )

(define (change_with_dict2 dict2 x)
  (cond
    [(empty? x) null]
    [(not (pair? x))
     (if (dict-has-key? dict2 x)
         (bound_var (dict-ref dict2 x) x)
         x)]
    [(equal? (car x) 'quote) x]
    [(cons (helper2 dict2 (car x)) (change_with_dict2 dict2 (cdr x)))]
    )
  )

(define (bound_lambda_fun x y)
  (define dict (dict-set #hash() '() '()))
  (let ([dict1 (create_dict (cadr x) (cadr y) dict)]
        [dict2 (create_dict (cadr y) (cadr x) dict)])
    (let ([new_dict1 (cons (change_with_dict1 dict1 (cadr x)) (list (change_with_dict1 dict1 (caddr x))))]
          [new_dict2 (cons (change_with_dict2 dict2 (cadr y)) (list (change_with_dict2 dict2 (caddr y))))])
      (if (equal? (car x) (car y)) (list (cons (car x) new_dict1) (cons (car y) new_dict2)) (list (cons 'λ new_dict1) (cons 'λ new_dict2)))
     ))
  )

(define (create_dict param_1 param_2 dict)
  (cond
    [(or (empty? param_1) (empty? param_2)) dict]
    [(and (not (pair? param_1)) (not (pair? param_2)))
     (if (equal? param_1 param_2)
         dict
         (dict-set dict param_1 param_2))]
    [(equal? (car param_1) (car param_2))
     (create_dict (cdr param_1) (cdr param_2) dict)]
    [else
     (let ([new-dict (dict-set dict (car param_1) (car param_2))]) 
       (create_dict (cdr param_1) (cdr param_2) new-dict))])
  )
#|
(define z '(lambda (lambda) (+ lambda if (f lambda))))
(define k '(lambda (if) (+ if if (f
))))
(bound_lambda_fun z k)
(define dict_x (bound_lambda_fun z k))
|#
(define (is_keyword keyword)
  (cond
    [(empty? keyword) #f]
    [(equal? (car keyword) 'if) #t]
    [(equal? (car keyword) 'quote) #t]
    [else #f]
    )
  )
(define (expr-compare-with-faulty-keyword x y)
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
        [(and (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% x y)]
        [(and (or (is_lambda_fun x) (is_lambda_fun y)) (or (not (is_lambda_fun x)) (not (is_lambda_fun y))))
         (list 'if '% x y)]
        [(let ([new_x (cleans_lambda_diff1 x y)]
               [new_y (cleans_lambda_diff2 x y)])
           (if (or (is_keyword (cdr new_x)) (is_keyword (cdr new_y)))
               (cons (expr-compare (car new_x) (car new_y)) (expr-compare-with-faulty-keyword (cdr new_x) (cdr new_y)))
               (cons (expr-compare (car new_x) (car new_y)) (expr-compare (cdr new_x) (cdr new_y))))
          )]
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
        [(and (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% x y)]
        [(and (or (equal? (car x) 'if) (equal? (car y) 'if)) (not (equal? (car x) (car y))))
         (list 'if '% x y)]
        [(and (or (is_lambda_fun x) (is_lambda_fun y)) (or (not (is_lambda_fun x)) (not (is_lambda_fun y))))
         (list 'if '% x y)]
        [(is_lambda_fun_with_wrong_param x y)
         (list 'if '% x y)]
        [(let ([new_x (cleans_lambda_diff1 x y)]
               [new_y (cleans_lambda_diff2 x y)])
           (if (or (is_keyword (cdr new_x)) (is_keyword (cdr new_y)))
               (cons (expr-compare (car new_x) (car new_y)) (expr-compare-with-faulty-keyword (cdr new_x) (cdr new_y)))
               (cons (expr-compare (car new_x) (car new_y)) (expr-compare (cdr new_x) (cdr new_y))))
          )]
        ))

; compare and see if the (expr-compare x y) result is the same with x when % = #t
;                                                 and the same with y when % = #f
(define (test-expr-compare x y) 
  (and (equal? (eval x ns)
               (eval `(let ((% #t)) ,(expr-compare x y)) ns))
       (equal? (eval y ns)
               (eval `(let ((% #f)) ,(expr-compare x y)) ns))))

; WARNING: IT MUST BE A SINGLE TEST CASE
; You need to cover all grammars including:
;     constant literals, variables, procedure calls, quote, lambda, if
(define test-expr-x
  `(cons 12 ((lambda (a) '(+ a 1)) (λ (x y) (+ (if x a) y)))))

(define test-expr-y
  `(cons 11 ((lambda (n) (+ n 2)) (lambda (x k) (+ (+ x k) n)))))


; the following line can be tested from interpreter
;     (eval test-expr-x)
;     (test-expr-compare test-expr-x test-expr-y))
;           test-expr-compare should return #t after you finish its implementation
;     (expr-compare 'a '(cons a b)) 
;     (expr-compare '(cons a b) '(cons a b))
;     (lambda? 'λ)




#|

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

|#