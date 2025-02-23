#lang racket

(require "../chez-init.rkt")

(provide compare-truthiness product-all my-even?-cps halt-cont even-filter-cps all-positive?-cps make-cps valid-even-list?-cps eval-one-exp)

(define stuff #f)


;-----------------------------------------------
; Macros
;-----------------------------------------------

(define-syntax compare-truthiness
  (syntax-rules ()
    [(_ truth ((val check))) (if (eqv? truth (check val))
                                 #t
                                 #f)]
    [(_ truth (first)) (if (eqv? truth first)
                           #t
                           #f)]
    [(_ truth ((val check) checks ...)) (if (eqv? truth (check val))
                                            (compare-truthiness truth (checks ...))
                                            #f)]
    [(_ truth (first checks ...)) (if (eqv? truth first)
                                      (compare-truthiness truth (checks ...))
                                      #f)]))

(define-syntax product-all
  (syntax-rules ()
    [(_ (vals ...))  (* vals ...)]
    [(_ val) val]
    [(_ (vals ...) others ...) (* (* vals ...) (product-all others ...))]
    [(_ val others ...) (* val (product-all others ...))]))

;-----------------------------------------------
; Continuation Passing Style
;-----------------------------------------------

; Here is a list of procedures covert them all to cps

(define scheme-value?
  (lambda (val)
    #t))

(define-datatype continuation continuation?
  [halt-cont]
  [odd-sum-step1 (lst list?) (cont continuation?)]
  [odd-sum-step2 (lst list?) (cont continuation?)]


  
  [my-even?-step1 (cont continuation?)]
  [my-odd?-step1 (cont continuation?)]
  [even-filter-step1 (lst list?) (cont continuation?)]
  [even-filter-step2 (lst list?) (cont continuation?)]
  [valid-even-step1 (lst list?) (cont continuation?)]
  [valid-even-step2 (cont continuation?)]
  [valid-even-step3 (cont continuation?)]
 )

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont () val]

      ;other cases already here
      [odd-sum-step1 (lst cont) (if val
                            (odd-sum-cps (cdr lst) (odd-sum-step2 lst cont))
                            (odd-sum-cps (cdr lst) cont))]
      [odd-sum-step2 (lst cont) (apply-cont cont (+ (car lst) val))]

      
      [my-even?-step1 (cont) (apply-cont cont (not val))]
      [my-odd?-step1 (cont) (apply-cont cont (not val))]
      [even-filter-step1 (lst cont) (if val
                                        (even-filter-cps (cdr lst) (even-filter-step2 lst cont))
                                        (even-filter-cps (cdr lst) cont))]
      [even-filter-step2 (lst cont) (apply-cont cont (cons (car lst) val))]
      [valid-even-step1 (lst cont) (if val
                                       (even-filter-cps lst (valid-even-step2 cont))
                                       (apply-cont cont #f))]
      [valid-even-step2 (cont) ((make-cps length) val (valid-even-step3 cont))]
      [valid-even-step3 (cont) (apply-cont cont (> val 5))]
      )))

(define make-cps
  (lambda (proc)
    (lambda (val cont)
      (apply-cont cont (proc val)))))

(define my-even?-cps
  (lambda (n cont)
  (cond [(= n 0) (apply-cont cont #t)]
        [(> n 0) (my-even?-cps (- n 1) (my-even?-step1 cont))]
        [else (my-even?-cps (+ n 1) (my-even?-step1 cont))]
        )))


(define even-filter-cps
  (lambda (lst cont)
    (if (null? lst)
        (apply-cont cont '())
        (my-even?-cps (car lst) (even-filter-step1 lst cont)))))

(define all-positive?-cps
  (lambda (lst cont)
    (cond
      [(null? lst) (apply-cont cont #t)] 
      [(<= (car lst) 0) (apply-cont cont #f)] 
      [else (all-positive?-cps (cdr lst) cont)])))


(define valid-even-list?-cps
  (lambda (lst cont)
    (if (list? lst)
        (all-positive?-cps lst (valid-even-step1 lst cont))
        (apply-cont cont #f))))


(define my-odd?-cps
  (lambda (n cont)
    (cond [(= n 1) (apply-cont cont #t)]
        [(> n 1) (my-odd?-cps (- n 1) (my-odd?-step1 cont))]
        [else (my-odd?-cps (+ n 1) (my-odd?-step1 cont))]
        )))



(define odd-sum-cps
  (lambda (lst cont)
    (if (null? lst)
        (apply-cont cont 0)
        (my-odd?-cps (car lst) (odd-sum-step1 lst cont)))))

;-----------------------------------------------
; Syntax Expand
;-----------------------------------------------
; add the following base racket procedures interpreter
; only modifying syntax-expand:
; filter
; list-ref
; for-each
; last

(define eval-one-exp
  (lambda (x)
    'nyi))

; here is what I changed

;; ;-------------------+
;; ;                   |
;; ;   sec:DATATYPES   |
;; ;                   |
;; ;-------------------+
;; (define-datatype expression expression?
;; 
;;   [filter-exp
;;    (check expression?)
;;    (lst expression?)]
;;   [list-ref-exp
;;    (index expression?)
;;    (lst expression?)]
;;   [for-each-exp
;;    (proc expression?)
;;    (lst expression?)]
;;   [last-exp
;;    (lst expression?)])
;; 
;;   
;; ;-------------------+
;; ;                   |
;; ;    sec:PARSER     |
;; ;                   |
;; ;-------------------+
;; 
;; (define parse-exp         
;;   (lambda (datum)
;;     (cond
;;          [(eqv? (car datum) 'filter) (filter-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
;;          [(eqv? (car datum) 'list-ref) (list-ref-exp (parse-exp (3rd datum)) (parse-exp (2nd datum)))]
;;          [(eqv? (car datum) 'for-each) (for-each-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
;;          [(eqv? (car datum) 'last) (last-exp (parse-exp (2nd datum)))]
;; 
;; (define parse-let-vals
;;   (lambda (lst)
;;     (cond [(null? lst) (list '() '())]
;;           [else (list (map 1st lst) (map parse-exp (map 2nd lst)))])))
;; 
;; (define check-let-vals?
;;   (lambda (lst)
;;     (cond [(null? lst) #t]
;;           [else (and (check-let-vals? (cdr lst)) (list? (car lst)) (= (length (car lst)) 2) (symbol? (caar lst)))])))
;; 
;; 
;; ;-----------------------+
;; ;                       |
;; ;  sec:SYNTAX EXPANSION |
;; ;                       |
;; ;-----------------------+
;; 
;; ; To be added in assignment 14.
;; (define syntax-expand
;;         (lambda (exp)
;;             (cases expression exp
;;               [filter-exp (check lst)
;;                           (syntax-expand (app-exp (lambda-exp
;;                                                    '(check lst)
;;                                                    (list (let-named-exp
;;                                                     'filtering-proc
;;                                                     '(filt-check filter-list)
;;                                                     (list (var-exp 'check) (var-exp 'lst))
;;                                                     (list (cond-exp
;;                                                            (list (list (app-exp (var-exp 'null?) (list (var-exp 'filter-list))) (lit-exp '()))
;;                                                                  (list (app-exp (var-exp 'filt-check) (list (app-exp (var-exp 'car) (list (var-exp 'filter-list)))))
;;                                                                        (app-exp (var-exp 'cons) (list (app-exp (var-exp 'car) (list (var-exp 'filter-list))) (app-exp (var-exp 'filtering-proc) (list (var-exp 'filt-check) (app-exp (var-exp 'cdr) (list (var-exp 'filter-list))))))))
;;                                                                  (list (var-exp 'else) (app-exp (var-exp 'filtering-proc) (list (var-exp 'filt-check) (app-exp (var-exp 'cdr) (list (var-exp 'filter-list)))))))))))) (list check lst)))
;;                           ]
;;               [list-ref-exp (index exps) (syntax-expand (app-exp (lambda-exp
;;                                                    '(index lst)
;;                                                    (list (let-named-exp
;;                                                           'loop
;;                                                           '(ref-list ref-list-index)
;;                                                           (list (var-exp 'lst) (lit-exp 0))
;;                                                           (list (cond-exp
;;                                                                  (list (list (app-exp (var-exp '=) (list (var-exp 'ref-list-index) (var-exp 'index))) (app-exp (var-exp 'car) (list (var-exp 'ref-list))))
;;                                                                        (list (var-exp 'else) (app-exp (var-exp 'loop) (list (app-exp (var-exp 'cdr) (list (var-exp 'ref-list))) (app-exp (var-exp 'add1) (list (var-exp 'ref-list-index)))))))))))) (list index exps)))]
;;               [for-each-exp (proc lst) (syntax-expand (app-exp (lambda-exp
;;                                         '(lst proc)
;;                                         (list (let-named-exp
;;                                           'loop
;;                                           '(ls)
;;                                           (list (var-exp 'lst))
;;                                           (list (if-exp
;;                                                  (app-exp (var-exp 'null?) (list (var-exp 'ls)))
;;                                                  (app-exp (var-exp 'void) '())
;;                                                  (begin-exp (list (app-exp (var-exp 'proc) (list (app-exp (var-exp 'car) (list (var-exp 'ls))))) (app-exp (var-exp 'loop) (list (app-exp (var-exp' cdr) (list (var-exp 'ls)))))))))))) (list lst proc)))]
;;               [last-exp (lst) (syntax-expand (app-exp
;;                                               (lambda-exp
;;                                                '(lst)
;;                                                (list (let-named-exp
;;                                                  'loop
;;                                                  '(current)
;;                                                  (list (var-exp 'lst))
;;                                                  (list (if-exp (app-exp (var-exp 'null?) (list (app-exp (var-exp 'cdr) (list (var-exp 'current))))) (app-exp (var-exp 'car) (list (var-exp 'current))) (app-exp (var-exp 'loop) (list (app-exp (var-exp 'cdr) (list (var-exp 'current))))))))))
;;                                               (list lst)))]
;;               )))

;-----------------------------------------------
; General Interpreter
;-----------------------------------------------

;; Make your interpreter support a new expression, grab-all. It's
;; called with a symbol, like (grab-all x). It returns the list of
;; values that x is bound to, starting at the most recent binding.
;; For example:
; (let ([x 2])
;   (let ([x 3])
;     (grab-all x)))
; => '(3 2)

;; Additionally, you should be able to call set! on (grab-all x) to
;; set! all the bindings of x to the same value across all environments.
;; For example:
; (let ([x 2])
;   (let ([x 3])
;     (set! (grab-all x)) 4)
;   x)
; => 4 (the value of x in the top-most environment was also set to 4, but the interesting part is that the other one changed)

;; here is what I changed
;-------------------+
;                   |
;   sec:DATATYPES   |
;                   |
;-------------------+

;; (define-datatype expression expression?
;;   [grab-all-exp
;;    (id symbol?)]
;;   [grab-all-set!-exp
;;    (grab-all-exp expression?)
;;    (exp expression?)]
;;   )
;; 
;; (define parse-exp         
;;   (lambda (datum)
;;     (cond
;;       [(list? datum)
;;        (cond
;;          [(eqv? (car datum) 'grab-all) (grab-all-exp (second datum))]
;;          [(eqv? (car datum) 'set!)
;;           (cond [(not (= (length datum) 3)) (error 'parse-exp "parse error set should have two arguments: ~s" datum)]
;;                 [(not (symbol? (car datum))) (error 'parse-exp "parse error set first argument should be an identifier: ~s" datum)]
;;                 [else (with-handlers ([exn:parse?
;;                                        (lambda (x) (error 'parse-exp "parse error set! second arg not a valid expression: ~s" datum))])
;;                         (if [symbol? (2nd datum)]
;;                             (set!-exp (2nd datum) (parse-exp (3rd datum)))
;;                             (grab-all-set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))))])]
;;       [else (error 'parse-exp "bad expression: ~s" datum)])))
;; 
;; (define apply-env-all
;;   (lambda (env sym)
;;     (map unbox (apply-env-all-ref env sym))))
;; 
;; (define apply-env-all-ref
;;   (lambda (env sym)
;;     (cases environment env
;;       [empty-env-record ()
;;                         (apply-global-env-all-ref global-env sym)]
;;       [extended-env-record (syms vals env)
;;                            (let ((pos (list-find-position sym syms)))
;;                              (if (number? pos)
;;                                  (cons (list-ref vals pos) (apply-env-all-ref env sym))
;;                                  (apply-env-all-ref env sym)))])))
;; 
;; (define apply-global-env-all-ref
;;   (lambda (env sym)
;;     (cases environment env
;;       [extended-env-record (syms vals env)
;;                            (let ([pos (list-find-position sym syms)])
;;                              (if (number? pos)
;;                                  (list (list-ref vals pos))
;;                                  (list)))]
;;       [empty-env-record ()
;;                         (error 'global-env "This should never happen")])))
;; 						
;; (define eval-exp
;;   (let ([identity-proc (lambda (x) x)])
;;    (lambda (exp env)
;;     (cases expression exp
;;       [grab-all-exp (id) (apply-env-all env id)]
;;       [grab-all-set!-exp (grab-all-e exp)
;;                          (cases expression grab-all-e
;;                            [grab-all-exp (id)
;;                                          (let ([set-value (eval-exp exp env)])
;;                                            (map (lambda (env-box) (set-box! env-box set-value))
;;                                                 (apply-env-all-ref env id)))]
;;                            [else (raise 'bad-set!-exp)])]
;; 
;; (define syntax-exp
;;    (lambda (exp)
;;     (cases expression exp
;;       [grab-all-exp (id) (grab-all-exp id)]
;;       [grab-all-set!-exp (grab-all-exp exp) (grab-all-set!-exp (syntax-exp grab-all-exp) (syntax-exp exp))]