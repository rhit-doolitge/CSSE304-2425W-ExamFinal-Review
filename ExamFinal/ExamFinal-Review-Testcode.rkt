#lang racket


(require "../testcode-base.rkt")
(require "ExamFinal-Review-Problems.rkt")
(provide get-weights get-names individual-test test)

(define test
  (make-test
    ; (r)

    (compare-truthiness equal?
                        [(compare-truthiness #t ((5 number?))) #t 1]
                        [(compare-truthiness #t ((5 symbol?))) #f 2]
                        [(compare-truthiness #f (#f)) #t 3]
                        [(compare-truthiness #t ((5 number?) (4 number?) ('works symbol?))) #t 4]
                        [(compare-truthiness #t ((5 number?) #t (4 number?) #t #f)) #f 5]
                        [(compare-truthiness #f ((5 symbol?) (4 symbol?))) #t 6])

    (product-all equal?
                 [(product-all 3 4 5) 60 1]
                 [(product-all 1) 1 2]
                 [(product-all (3 4) (2)) 24 3]
                 [(product-all (5) (2 3)) 30 4]
                 [(product-all 2 2 2 2 (2 3)) 96 4])


    (my-even?-cps equal?
                  [(my-even?-cps 0 (halt-cont)) #t 2]
                  [(my-even?-cps 1 (halt-cont)) #f 2]
                  [(my-even?-cps 2 (halt-cont)) #t 2]
                  [(my-even?-cps 7 (halt-cont)) #f 2]
                  [(my-even?-cps 10 (halt-cont)) #t 2]
                  [(my-even?-cps -4 (halt-cont)) #t 2]
                  [(my-even?-cps -3 (halt-cont)) #f 2])

    (even-filter-cps equal?
                     [(even-filter-cps '(1 2 3 4 5 6) (halt-cont)) '(2 4 6) 3]
                     [(even-filter-cps '(11 13 15) (halt-cont)) '() 3]
                     [(even-filter-cps '(0 -2 -4 3 5) (halt-cont)) '(0 -2 -4) 3]
                     [(even-filter-cps '() (halt-cont)) '() 3])

    (all-positive?-cps equal?
                       [(all-positive?-cps '(1 2 3 4 5) (halt-cont)) #t 3]
                       [(all-positive?-cps '(0 2 3 4 5) (halt-cont)) #f 3]
                       [(all-positive?-cps '(-1 2 3 4 5) (halt-cont)) #f 3]
                       [(all-positive?-cps '() (halt-cont)) #t 3])

    (valid-even-list?-cps equal?
                          [(valid-even-list?-cps '(2 4 6 8 10 12) (halt-cont)) #t 4]
                          [(valid-even-list?-cps '(1 2 3 4 5 6 7 8) (halt-cont)) #f 4]
                          [(valid-even-list?-cps '(0 2 4 6 8 10) (halt-cont)) #f 4]
                          [(valid-even-list?-cps '(2 4 6 -8 10 12) (halt-cont)) #f 4]
                          [(valid-even-list?-cps 42 (halt-cont)) #f 4]
                          [(valid-even-list?-cps '() (halt-cont)) #f 4])

    (filter equal? ; (run-test filter)
            [(eval-one-exp '(filter even? '(1 2 3 4 5 6))) '(2 4 6) 5] ; (run-test filter 1)
            [(eval-one-exp '(filter (lambda (x) (> x 3)) '(1 2 3 4 5))) '(4 5) 5] ; (run-test filter 2)
            [(eval-one-exp '(filter symbol? '(1 hello 3 world))) '(hello world) 6] ; (run-test filter 3)
            [(eval-one-exp '(filter odd? '(1 2 3 4 5))) '(1 3 5) 5] ; (run-test filter 5)
            [(eval-one-exp '(filter (lambda (x) (> x 3)) '(2 4 6 8))) '(4 6 8) 7]
            [(eval-one-exp '((lambda (x) (map add1 x)) (filter number? '(1 2 3 4 not-here def-not-here)))) '(2 3 4 5) 1]
            [(eval-one-exp '((lambda (x) (map add1 x)) (filter odd? (map add1 '(1 2 3 4 5 6))))) '(4 6 8) 1]
            [(eval-one-exp '((lambda (x) (map add1 x)) (filter odd? ((lambda (x) (map sub1 x)) '(1 2 3 4 5))))) '(2 4) 1]
            )

  (list-ref equal? ; (run-test list-ref)
            [(eval-one-exp '(list-ref '(a b c d e) 2)) 'c 5] ; (run-test list-ref 1)
            [(eval-one-exp '(list-ref '(1 2 3 4 5 6) 4)) 5 5] ; (run-test list-ref 2)
            [(eval-one-exp '(list-ref '(a b c d) 0)) 'a 5] ; (run-test list-ref 3)
            [(eval-one-exp '(list-ref '(100 200 300 400) 1)) 200 5] ; (run-test list-ref 4)
            [(eval-one-exp '(list-ref '(apple orange banana) 2)) 'banana 6] ; (run-test list-ref 5)
            [(eval-one-exp '(list-ref (map (lambda (x) (* x 2)) '(1 2 3 4)) 2)) 6 6]
            [(eval-one-exp '(list-ref (filter (lambda (x) (> x 2)) '(1 2 3 4 5)) 1)) 4 6] 
            [(eval-one-exp '(list-ref (list 'a 'b 'c 'd) (+ 1 1))) 'c 6]
  )
  (for-each equal? ; (run-test for-each)
            [(eval-one-exp '(begin (define lst '()) (for-each (lambda (x) (set! lst (cons (+ x 2) lst))) '(4 3 2 1)) lst)) ; Inside: increment each element by 2
             '(3 4 5 6) 8] ; (run-test for-each 1)
            
            [(eval-one-exp '(begin (define sum 0) (for-each (lambda (x) (set! sum (+ sum (* x x)))) '(1 2 3 4)) sum)) ; Inside: sum of squares of each element
             30 8] ; (run-test for-each 2)
            
            [(eval-one-exp '(begin (define result '()) (for-each (lambda (x) (set! result (append result (list (* x 2))))) '(1 2 3))result)) ; Inside: double each element and append it to the list
             '(2 4 6) 7] ; (run-test for-each 3)
            
            [(eval-one-exp '(begin (define special-x 0) (for-each (lambda (x) (set! special-x (+ special-x 10))) '(1 2 3)) special-x)) ; Inside: adds 10 to x for each element
             30 8] ; (run-test for-each 5)
            )
  (last equal? ; (run-test last)
        [(eval-one-exp '(last '(1 2 3 4 5))) 5 5] ; (run-test last 1)
        [(eval-one-exp '(last '(apple orange banana))) 'banana 5] ; (run-test last 2)
        [(eval-one-exp '(last '(100 200 300))) 300 5] ; (run-test last 3)
        [(eval-one-exp '(last '(a b c d))) 'd 5] ; (run-test last 4)
        [(eval-one-exp '(last '(7 8 9 10))) 10 5] ; (run-test last 5)
        [(eval-one-exp '(last (map (lambda (x) (* x 2)) '(1 2 3 4)))) ; Inside: map each number to double, then get the last element
         8 6] ; (run-test last 1)
        
        [(eval-one-exp '(last (filter (lambda (x) (> x 2)) '(1 2 3 4 5)))) ; Inside: filter to keep elements greater than 2, then get the last element
         5 6] ; (run-test last 2)
        
        [(eval-one-exp '(last (list-ref '((a b c) (d e f) (g h i)) 1))) ; Inside: list-ref to get the second sublist, then get the last element
         'f 7] ; (run-test last 4)
        )
  
  (grab-all equal?
      [(eval-one-exp '(grab-all x)) '() 1] ; (run-test grab-all 1)
      [(length (eval-one-exp '(grab-all +))) 1 1] ; (run-test grab-all 2)
      [(eval-one-exp '(let ([x 2]) (grab-all x))) '(2) 1] ; (run-test grab-all 3)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (grab-all x)))) '(3 2) 1] ; (run-test grab-all 4)
      [(eval-one-exp '(let ([x 2]) (let ([y 3]) (grab-all x)))) '(2) 1] ; (run-test grab-all 5)
      [(eval-one-exp '(let ([y 2]) (let ([x 3]) (grab-all x)))) '(3) 1] ; (run-test grab-all 6)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) 4)) x)) 4 1] ; (run-test grab-all 7)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) 4) x))) 4 1] ; (run-test grab-all 8)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) 4) (grab-all x)))) '(4 4) 1] ; (run-test grab-all 9)
      [(eval-one-exp '(let ([x 2]) (let ([y 3]) (set! (grab-all x) 4) y))) 3 1] ; (run-test grab-all 10)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) (add1 x)) (grab-all x)))) '(4 4) 1] ; (run-test grab-all 11)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) (grab-all x)) (grab-all x)))) '((3 2) (3 2)) 1] ; (run-test grab-all 10)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! x 4) x))) 4 1] ; (run-test grab-all 11)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! x 4)) x)) 2 1] ; (run-test grab-all 12)
      [(length (eval-one-exp '(let ([+ 2]) (set! + 3) (grab-all +)))) 2 1] ; (run-test grab-all 13)
   )
 ))

(implicit-run test) ; Run tests as soon as this file is loaded
