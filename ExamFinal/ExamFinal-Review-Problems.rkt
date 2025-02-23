#lang racket

(require "../chez-init.rkt")

(provide compare-truthiness product-all my-even?-cps halt-cont even-filter-cps all-positive?-cps make-cps valid-even-list?-cps eval-one-exp)

(define stuff #f)


;-----------------------------------------------
; Macros
;-----------------------------------------------

; This takes in two arguments a #t or #f value that is the value you are looking for
; and a list. This list can either be a direct #t or #f value or a pair.
; The pairs come in (value single-argument-boolean-function) it should run the function
; on the value given in the pair and see if the output is the same as the #t or #f value you are looking for
; ex: (compare-truthiness #t ((5 number?)) returns #t
; but (compare-truthiness #f ((5 number?)) returns #f
; and (compare-truthiness #t (#t) returns #t
; but (compare-truthiness #f (#t) returns #f

(define-syntax compare-truthiness
  (syntax-rules ()
    [(_ ...) 'nyi]))

; This function takes in infinite amount of arguments.
; The aguments are either numbers or lists of numbers
; it should multiply all the numbers that come in together
; and output the result
; ex: (product-all 3 4 5) return 60
; and (product-all (3 4) (2)) returns 24

(define-syntax product-all
  (syntax-rules ()
    [(_ ...) 'nyi]))

;-----------------------------------------------
; Continuation Passing Style
;-----------------------------------------------

; Here is a list of procedures covert them all to cps

(define scheme-value?
  (lambda (val)
    #t))

(define-datatype continuation continuation?
  [halt-cont]
 )

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont () val]
      )))

(define all-positive?
  (lambda (lst)
  (cond
    [(null? lst) #t] 
    [(<= (car lst) 0) #f] 
    [else (all-positive? (cdr lst))])))

(define all-positive?-cps
  (lambda (lst cont)
    'nyi))

(define make-cps
  (lambda (proc)
    (lambda (val cont)
      (apply-cont cont (proc val)))))

(define my-even?
  (lambda (n)
  (cond [(= n 0) 0]
        [(> n 0) (not (my-even? (- n 1)))]
        [else (not (my-even? (+ n 1)))]
        )))

(define my-even?-cps
  (lambda (n cont)
    'nyi))

(define even-filter
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(my-even? (car lst))
       (cons (car lst) (even-filter (cdr lst)))]
      [else (even-filter (cdr lst))])))

(define even-filter-cps
  (lambda (lst cont)
  'nyi))

(define valid-even-list?
  (lambda (lst)
  (and (list? lst)
       (all-positive? lst)
       (> (length (even-filter lst)) 5))))

(define valid-even-list?-cps
  (lambda (lst cont)
  'nyi))


;-----------------------------------------------
; Syntax Expand
;-----------------------------------------------
; add the following base racket procedures interpreter
; only modifying syntax-expand:
; filter
; list-ref
; for-each
; last


;  replace with your interpreter
(define eval-one-exp
  (lambda (x)
    'nyi))

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