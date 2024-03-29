(define test-num 0)
(define section 0)
(define errors 0)
(define total 0)
(define test (lambda (val exp)
                   (set! test-num (+ test-num 1))
                   (set! total (+ total 1))
                   (display "Test ")
                   (display section)
                   (display ".")
                   (display test-num)
                   (if (equal? val exp)
                      (begin
                        (display " passed: ")
                        (display val)
                        (newline))
                      (begin
                        (display " ERROR: expected ")
                        (display exp)
                        (display ", got ")
                        (display val)
                        (newline)
                        (set! errors (+ errors 1))))))

(define new-section (lambda (msg)
      (set! section (+ section 1))
      (set! test-num 0)
      (newline)
      (display "Section ")
      (display section)
      (display ": ")
      (display msg)
      (display  " =================\n")))

(new-section "Literals")
(test 1 1)
(test -1 -1)
(test -1.2345 -1.2345)
(test 2/3 2/3)
(test 10/5 2)
(test 2+3i 2+3i)
(test 2-3i 2-3i)
(test 2.0+3i 2.0+3i)
(test 1@0 1@0)
(test 2@1.5 2@1.5)
(test 2/3@1/3 2/3@1/3)
(test 'abc 'abc)
(test "Hello" "Hello")
(test '(a b c) '(a b c))
(test #(4 3.0 two 2/2) #(4 3.0 two 1))

(new-section "Integers")
(test (+ 1 2) 3)
(test (+ 1 2 3 4 5) 15)
(test (+ 5) 5)
(test (+) 0)
(test (- 1 2) -1)
(test (- 10 1 2 3 4) 0)
(test (- 5) -5)
(test (* 2 3) 6)
(test (* 1 2 3 4 5) 120)
(test (* 5) 5)
(test (*) 1)
(test (/ 10 5) 2)
(test (/ -10 5) -2)
(test (/ -10 -5) 2)
(test (/ 64 2 2 2 2) 4)

(new-section "Lists")
(test '() '())
(test (car '(a b c)) 'a)
(test (cdr '(a b c)) '(b c))
(test (cons 'a '()) '(a))
(test (cons 'a '(b)) '(a b))
(test (cons 'a '(b c)) '(a b c))
(test (cons 'a 'b) '(a . b))
(test (list 'a) '(a))
(test (list 'a 100) '(a 100))
(test (list 'a '()) '(a ()))
(test (length '()) 0)
(test (length '(a b c)) 3)
(test (reverse '(a b c)) '(c b a))
(test (list? '(a b c)) #t)
(test (list? '()) #t)
(test (list? 'a) #f)
(test (list? '(a . b)) #f)
(test (pair? '(a b c)) #t)
(test (pair? '()) #t)
(test (pair? 'a) #f)
(test (pair? '(a . b)) #t)
(test (null? '()) #t)
(test (null? '(())) #f)
(test (null? '(a)) #f)

(new-section "Strings")
(test (string? "hello") #t)
(test (string? 'hello) #f)
(test (string? 123) #f)
(test (string? "") #t)
(test (string-length "") 0)
(test (string-length "hello") 5)

(new-section "Formals")
(test ((lambda () 42)) 42)
(test ((lambda (x) x) 43) 43)
(test ((lambda (x y) (+ x y)) 3 5) 8)
(test ((lambda (x y . z) z) 1 2 3 4 5 6) '(3 4 5 6))
(test ((lambda x x)) '())
(test ((lambda x x) 10) '(10))
(test ((lambda x x) 10 20 30) '(10 20 30))

(new-section "Define")
(define x 1)
(test x 1)
(define x (+ x 1))
(test x 2)
(define x (lambda (x) (* x 2)))
(test (x 21) 42)
(define (f x y) (+ x y))
(test (f 3 5) 8)
(define (f x y . z) z)
(test (f 1 2 3 4 5 6) '(3 4 5 6))
(define (f . x) x)
(test (f) '())
(test (f 10) '(10))
(test (f 10 20 30) '(10 20 30))

(new-section "Assoc")
(define e '((a 1) (b 2) (c 3)))
(test (assq 'a e) '(a 1))
(test (assq 'b e) '(b 2))
(test (assq 'd e) #f)
(test (assq (list 'a) '(((a)) ((b)) ((c)))) #f)
(test (assoc (list 'a) '(((a)) ((b)) ((c)))) '((a)))
(test (assv 5 '((2 3) (5 7) (11 13))) '(5 7))

(new-section "Member")
(test (memq 'a '(a b c)) '(a b c))
(test (memq 'b '(a b c)) '(b c))
(test (memq 'a '(b c d)) #f)
(test (memq (list 'a) '(b (a) c)) #f)
(test (member (list 'a) '(b (a) c)) '((a) c))
(test (memq 101 '(100 101 102)) '(101 102))
(test (memv 101 '(100 101 102)) '(101 102))
(test (member '() '(a b c)) #f)
(test (member '() '()) #f)

(new-section "Case")
(test (case (* 2 3)
        ((2 3 5 7) 'prime)
        ((1 4 6 8 9) 'composite))
      'composite)
(test (case (car '(c d))
            ((a e i o u) 'vowel)
            ((w y) 'semivowel)
            (else 'consonant))
      'consonant)

(newline)
(display "Ran ")
(display total)
(display " tests\n")
(if (zero? errors)
    (display "All tests PASSED!\n")
    (begin
        (display "There were ")
        (display errors)
        (display " errors\n")))
