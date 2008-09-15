;;; these are trivial translations of some numeric tests in the
;;; Clisp test suite.  Presumably they fall under Clisp's license,
;;; which I belive is GPL


(define (ok? tst result expected)
  (if (not (equal? result expected))
      (begin
	(display (object->string tst))
	(display " got ")
	(display (object->string result))
	(display " but expected ")
	(display (object->string expected))
	(newline)
	)))

(defmacro test (tst expected)
  `(let ((result (catch #t (lambda () ,tst) (lambda args 'error))))
     (ok? ',tst result ,expected)))

(test ((lambda (a b) (+ a (* b 3))) 4 5) 19)
(test (symbol? 'car) #t)
(test (pair? 'a) #f)
(test (pair? '()) #f)
(test (pair? '(a b c)) #t)
(test (number? #*101) #f)
(test (number? -5) #t)
(test (integer? 5) #t)
(test (integer? #\+) #f)
(test (char? #\1) #t)
(test (string? "abc") #t)
(test (string? ':+*/-) #f)
;(test (eq? #\a #\a) #t)
(test (eqv? (cons 'a 'b) (cons 'a 'c)) #f)
(test (equal? 3 3) #t)
(test (equal? 3 3.0) #f)
(test (equal? 3.0 3.0) #t)
(test (equal? 3-4i 3-4i) #t)
(test (not 1) #f)
(test (not #f) #t)
(test (char? #\a) #t)
(test (char? #\1) #t)

(test (char-upper-case? #\a) #f)
(test (char-lower-case? #\A) #f)
(test (char-numeric? #\a) #f)
(test (char-numeric? #\5) #t)
(test (char-alphabetic? #\a) #t)
(test (char-alphabetic? #\$) #f)
(test (char=? #\d #\d) #t)
(test (char<? #\z #\0) #f)
(test (char=? #\d #\d) #t)
(test (char<? #\d #\x) #t)
(test (char<? #\d #\d) #f)
(test (vector-length '#(a b c d e f)) 6)
(test (length (list 'a 'b 'c 'd 'e 'f)) 6)
(test (equal? (car (reverse (list 1 2 3 4))) 4) #t)
(test (equal? (car (list 'a 'b 'c 'd 'e 'f 'g)) 'a) #t)
(test (equal? (cdr (list 'a 'b 'c 'd 'e 'f 'g)) '(b c d e f g)) #t)
(test (equal? (cadr (list 'a 'b 'c 'd 'e 'f 'g)) 'b) #t)
(test (equal? (cddr (list 'a 'b 'c 'd 'e 'f 'g)) '(c d e f g)) #t)
(test (equal? (caddr (list 'a 'b 'c 'd 'e 'f 'g)) 'c) #t)
(test (equal? (cdddr (list 'a 'b 'c 'd 'e 'f 'g)) '(d e f g)) #t)
(test (equal? (cadddr (list 'a 'b 'c 'd 'e 'f 'g)) 'd) #t)
(test (equal? (cddddr (list 'a 'b 'c 'd 'e 'f 'g)) '(e f g)) #t)
(test (equal? (caadr (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '((u v w) x)) #t)
(test (equal? (cadar (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '(6 7)) #t)
(test (equal? (cdaar (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '(5)) #t)
(test (equal? (cdadr (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '(y)) #t)
(test (equal? (cddar (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '()) #t)
(test (equal? (caaaar (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '(1 2 3)) #t)
(test (equal? (caadar (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) 6) #t)
(test (equal? (caaddr (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '(q w e)) #t)
(test (equal? (cadaar (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) 5) #t)
(test (equal? (cadadr (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) 'y) #t)
;;(test (equal? (caddar (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '()) 'error)
(test (equal? (caddar (list (list (list (list (list 1 2 3) 4) 5) 1 6 (list 5 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) 6) #t)
(test (equal? (cadddr (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '(a b c)) #t)
(test (equal? (cdaaar (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '(4)) #t)
(test (equal? (cdaadr (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '(x)) #t)
(test (equal? (cdadar (list (list (list (list (list 1 2 3) 4) 5) (list 6 7)) (list (list (list 'u 'v 'w) 'x) 'y) (list (list 'q 'w 'e) 'r) (list 'a 'b 'c) 'e 'f 'g)) '(7)) #t)
(test (equal? (cons 1 2) '(1 . 2)) #t)
(test (equal? (cons 'a (cons 'b (cons 'c '()))) '(a b c)) #t)
(test (equal? (cons 'a (list 'b 'c 'd)) '(a b c d)) #t)
(test (equal? (list 'a 'b 'c 'd) '(a b c d)) #t)
(test (equal? (append (list 'a 'b 'c) (list 'd 'e 'f) '() '(g)) '(a b c d e f g)) #t)

(test (car '(a b c d e f g)) 'a)
(test (cdr '(a b c d e f g)) '(b c d e f g))
(test (caar '((a) b c d e f g)) 'a)
(test (cadr '(a b c d e f g)) 'b)
(test (cdar '((a b) c d e f g)) '(b))
(test (cddr '(a b c d e f g)) '(c d e f g))
(test (caaar '(((a)) b c d e f g)) 'a)
(test (caadr '(a (b) c d e f g)) 'b)
(test (cadar '((a b) c d e f g)) 'b)
(test (caddr '(a b c d e f g)) 'c)
(test (cdaar '(((a b)) c d e f g)) '(b))
(test (cdadr '(a (b c) d e f g)) '(c))
(test (cddar '((a b c) d e f g)) '(c))
(test (cdddr '(a b c d e f g)) '(d e f g))
(test (caaaar '((((a))) b c d e f g)) 'a)
(test (caaadr '(a ((b)) c d e f g)) 'b)
(test (caadar '((a (b)) c d e f g)) 'b)
(test (caaddr '(a b (c) d e f g)) 'c)
(test (cadaar '(((a b)) c d e f g)) 'b)
(test (cadadr '(a (b c) d e f g)) 'c)
(test (caddar '((a b c) d e f g)) 'c)
(test (cadddr '(a b c d e f g)) 'd)
(test (cdaaar '((((a b))) c d e f g)) '(b))
(test (cdaadr '(a ((b c)) d e f g)) '(c))
(test (cdadar '((a (b c)) d e f g)) '(c))
(test (cdaddr '(a b (c d) e f g)) '(d))
(test (cddaar '(((a b c)) d e f g)) '(c))
(test (cddadr '(a (b c d) e f g)) '(d))
(test (cdddar '((a b c d) e f g)) '(d))
(test (cddddr '(a b c d e f g)) '(e f g))
(test (car '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '((((1 2 3) 4) 5) (6 7)))
(test (cdr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '((((u v w) x) y) ((q w e) r) (a b c) e f g))
(test (caar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(((1 2 3) 4) 5))
(test (cadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(((u v w) x) y))
(test (cdar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '((6 7)))
(test (cddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(((q w e) r) (a b c) e f g))
(test (caaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '((1 2 3) 4))
(test (caadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '((u v w) x))
(test (cadar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(6 7))
(test (caddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '((q w e) r))
(test (cdaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(5))
(test (cdadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(y))
(test (cddar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '())
(test (cdddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '((a b c) e f g))
(test (caaaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(1 2 3))
(test (caaadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(u v w))
(test (caadar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) 6)
(test (caaddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(q w e))
(test (cadaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) 5)
(test (cadadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) 'y)
(test (caddar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) 'error)
(test (cadddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(a b c))
(test (cdaaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(4))
(test (cdaadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(x))
(test (cdadar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(7))
(test (cdaddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(r))
(test (cddaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '())
(test (cddadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '())
(test (cdddar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) 'error)
(test (cddddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c) e f g)) '(e f g))
(test (car '()) 'error)
(test (cdr '()) 'error)
(test (caar '()) 'error)
(test (cadr '()) 'error)
(test (cdar '()) 'error)
(test (cddr '()) 'error)
(test (caaar '()) 'error)
(test (caadr '()) 'error)
(test (cadar '()) 'error)
(test (caddr '()) 'error)
(test (cdaar '()) 'error)
(test (cdadr '()) 'error)
(test (cddar '()) 'error)
(test (cdddr '()) 'error)
(test (caaaar '()) 'error)
(test (caaadr '()) 'error)
(test (caadar '()) 'error)
(test (caaddr '()) 'error)
(test (cadaar '()) 'error)
(test (cadadr '()) 'error)
(test (caddar '()) 'error)
(test (cadddr '()) 'error)
(test (cdaaar '()) 'error)
(test (cdaadr '()) 'error)
(test (cdadar '()) 'error)
(test (cdaddr '()) 'error)
(test (cddaar '()) 'error)
(test (cddadr '()) 'error)
(test (cdddar '()) 'error)
(test (cddddr '()) 'error)
(test (car '(a b c d e f g)) 'a)
(test (cdr '(a b c d e f g)) '(b c d e f g))
(test (caar '(a b c d e f g)) 'error)
(test (cadr '(a b c d e f g)) 'b)
(test (cdar '(a b c d e f g)) 'error)
(test (cddr '(a b c d e f g)) '(c d e f g))
(test (caaar '(a b c d e f g)) 'error)
(test (caadr '(a b c d e f g)) 'error)
(test (cadar '(a b c d e f g)) 'error)
(test (caddr '(a b c d e f g)) 'c)
(test (cdaar '(a b c d e f g)) 'error)
(test (cdadr '(a b c d e f g)) 'error)
(test (cddar '(a b c d e f g)) 'error)
(test (cdddr '(a b c d e f g)) '(d e f g))
(test (caaaar '(a b c d e f g)) 'error)
(test (caaadr '(a b c d e f g)) 'error)
(test (caadar '(a b c d e f g)) 'error)
(test (caaddr '(a b c d e f g)) 'error)
(test (cadaar '(a b c d e f g)) 'error)
(test (cadadr '(a b c d e f g)) 'error)
(test (caddar '(a b c d e f g)) 'error)
(test (cadddr '(a b c d e f g)) 'd)
(test (cdaaar '(a b c d e f g)) 'error)
(test (cdaadr '(a b c d e f g)) 'error)
(test (cdadar '(a b c d e f g)) 'error)
(test (cdaddr '(a b c d e f g)) 'error)
(test (cddaar '(a b c d e f g)) 'error)
(test (cddadr '(a b c d e f g)) 'error)
(test (cdddar '(a b c d e f g)) 'error)
(test (cddddr '(a b c d e f g)) '(e f g))
(test (car '(a)) 'a)
(test (cdr '(a)) '())
(test (caar '(a)) 'error)
(test (cadr '(a)) 'error)
(test (cdar '(a)) 'error)
(test (cddr '(a)) 'error)
(test (caaar '(a)) 'error)
(test (caadr '(a)) 'error)
(test (cadar '(a)) 'error)
(test (caddr '(a)) 'error)
(test (cdaar '(a)) 'error)
(test (cdadr '(a)) 'error)
(test (cddar '(a)) 'error)
(test (cdddr '(a)) 'error)
(test (caaaar '(a)) 'error)
(test (caaadr '(a)) 'error)
(test (caadar '(a)) 'error)
(test (caaddr '(a)) 'error)
(test (cadaar '(a)) 'error)
(test (cadadr '(a)) 'error)
(test (caddar '(a)) 'error)
(test (cadddr '(a)) 'error)
(test (cdaaar '(a)) 'error)
(test (cdaadr '(a)) 'error)
(test (cdadar '(a)) 'error)
(test (cdaddr '(a)) 'error)
(test (cddaar '(a)) 'error)
(test (cddadr '(a)) 'error)
(test (cdddar '(a)) 'error)
(test (cddddr '(a)) 'error)
(test (cons 1 2) '(1 . 2))
(test (cons 'a 'b) '(a . b))
(test (cons 'a 'b 'c) 'error)
(test (cons 'a) 'error)
(test (cons) 'error)
(test (cons 'a '()) '(a))
(test (cons '() 'a) '(() . a))
(test (cons 'a (cons 'b (cons 'c '()))) '(a b c))
(test (cons 'a '(b c d)) '(a b c d))
(test (list 'a 'b 'c 'd) '(a b c d))
(test (list 'a) '(a))
(test (list (list 'a 'b) (list 'c 'd)) '((a b) (c d)))
(test (list 'a '()) '(a ()))
(test (list '() 'a) '(() a))
(test (list '() '()) '(() ()))
(test (list 3 4 'a (car (cons 'b 'c)) (+ 6 -2)) '(3 4 a b 4))
(test (append (list 'a 'b 'c) (list 'd 'e 'f) '() (list 'g)) '(a b c d e f g))
(test (append (list 'a 'b 'c) 'd) '(a b c . d))
(test (append 'a 'b) 'error)
(test (append 'a '()) 'error)
(test (append '() '()) '())
(test (append '() 'a) 'a)
(test (append '() (list 'a 'b 'c)) '(a b c))
(test (assoc 'a '((b c) (a u) (a i))) '(a u))
(test (assoc 'a '((b c) ((a) u) (a i))) '(a i))
(test (reverse '(a b c d)) '(d c b a))
(test (symbol? 'elephant) #t)
(test (symbol? 12) #f)
(test (symbol? #f) #f)
(test (symbol? '#f) #f)
(test (symbol? "hello") #f)
(test (number? (cons 1 2)) #f)

(test (char? #\a) #t)
(test (char? #\$) #t)
(test (char? #\.) #t)
(test (char? #\a) #t)
(test (char? #\\) #t)
(test (char? #\5) #t)
(test (char? #\)) #t)
(test (char? #\%) #t)
(test (char? #\space) #t)
(test (char-upper-case? #\a) #f)
(test (char-upper-case? #\$) #f)
(test (char-upper-case? #\.) #f)
(test (char-upper-case? #\A) #t)
(test (char-upper-case? 1) 'error)
(test (char-upper-case? #\\) #f)
(test (char-upper-case? #\5) #f)
(test (char-upper-case? #\)) #f)
(test (char-upper-case? #\%) #f)
(test (char-lower-case? #\a) #t)
(test (char-lower-case? #\$) #f)
(test (char-lower-case? #\.) #f)
(test (char-lower-case? #\A) #f)
(test (char-lower-case? 1) 'error)
(test (char-lower-case? #\\) #f)
(test (char-lower-case? #\5) #f)
(test (char-lower-case? #\)) #f)
(test (char-lower-case? #\%) #f)

(test (char->integer 33) 'error)
(test (char=?) 'error)
(test (char<?) 'error)
(test (char>?) 'error)
(test (char>=?) 'error)
(test (length 'x) 'error)
(test (char? '#\space) #t)
(test (char? '#\newline) #t)
(test (char? '#\a) #t)
(test (char? '#\8) #t)
(test (char? '#\-) #t)
(test (char? '#\n) #t)
(test (char? '#\() #t)
(test (string? "das ist einer der teststrings") #t)
(test (string? '(das ist natuerlich falsch)) #f)
(test (string? "das ist die eine haelfte" "und das die andere") 'error)
(test ((lambda (x) (list x y)) 1 2) 'error)
(test ((lambda (x) (list x)) 1 2) 'error)
(test ((lambda (#\a) (list a)) 1) 'error)
(test ((lambda (#*10) (list 1 2 3))) 'error)
(test ((lambda (x y) ((lambda (a b) (list a b)) 'u 'v)) 5 6) '(u v))
(test ((lambda (x y) (list x y)) 1) 'error)
(test ((lambda (list 1 2 3))) 'error)
(test ((lambda)) 'error)
(test (char=? #\d #\d) #t)
(test (char=? #\A #\a) #f)
(test (char=? #\d #\x) #f)
(test (char=? #\d #\D) #f)
(test (char=? #\d #\d #\d #\d) #t)
(test (char=? #\d #\d #\x #\d) #f)
(test (char=? #\d #\y #\x #\c) #f)
(test (char=? #\d #\c #\d) #f)
(test (char<? #\d #\x) #t)
(test (char<=? #\d #\x) #t)
(test (char<? #\d #\d) #f)
(test (char<=? #\d #\d) #t)
(test (char<? #\a #\e #\y #\z) #t)
(test (char<=? #\a #\e #\y #\z) #t)
(test (char<? #\a #\e #\e #\y) #f)
(test (char<=? #\a #\e #\e #\y) #t)
(test (char>? #\e #\d) #t)
(test (char>=? #\e #\d) #t)
(test (char>? #\d #\c #\b #\a) #t)
(test (char>=? #\d #\c #\b #\a) #t)
(test (char>? #\d #\d #\c #\a) #f)
(test (char>=? #\d #\d #\c #\a) #t)
(test (char>? #\e #\d #\b #\c #\a) #f)
(test (char>=? #\e #\d #\b #\c #\a) #f)
(test (char>? #\z #\a) #t)
(test (char=? #\a #\a) #t)

(test (equal? (cons 1 2) '(1 . 2)) #t)
(test (equal? (cons 1 '()) '(1)) #t)
(test (equal? (cons '() 2) '(() . 2)) #t)
(test (equal? (cons '() '()) '(())) #t)
(test (equal? (cons 1 (cons 2 (cons 3 (cons 4 '())))) '(1 2 3 4)) #t)
(test (equal? (cons 'a 'b) '(a . b)) #t)
(test (equal? (cons 'a (cons 'b (cons 'c '()))) '(a b c)) #t)
(test (equal? (cons 'a (list 'b 'c 'd)) '(a b c d)) #t)
(test (pair? '()) #f)
(test (pair? (cons 1 2)) #t)
(test (pair? ''()) #t)
(test (equal? (let ((tree1 (list 1 (list 1 2) (list (list 1 2 3)) (list (list (list 1 2 3 4)))))) tree1) '(1 (1 2) ((1 2 3)) (((1 2 3 4))))) #t)
(test (equal? (let ((tree2 (list "one" (list "one" "two") (list (list "one" "two" "three"))))) tree2) '("one" ("one" "two") (("one" "two" "three")))) #t)
(test (equal? (let ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4)))) tree1) '(1 (1 2) (1 2 3) (1 2 3 4))) #t)
(test (equal? (let ((tree1 (list 1 (list 1 2))) (tree2 (list 1 (list 1 2)))) tree2) '(1 (1 2))) #t)
(test (equal? (let ((tree1 (list 1 (list 1 2))) (tree2 (list 1 (list 1 2)))) (eqv? tree1 tree2)) #f) #t)
(test (equal? (let ((tree1 (list ''a (list ''b ''c))) (tree2 (list ''a (list ''b ''c)))) tree2) '('a ('b 'c))) #t)
(test (equal? (let ((lst (list 1 (list 2 3)))) lst) '(1 (2 3))) #t)
(test (equal? (let* ((lst (list 1 (list 2 3))) (slst lst)) slst) '(1 (2 3))) #t)
(test (equal? (list 1) '(1)) #t)
(test (equal? (let ((a 1)) a) 1) #t)
(test (equal? (let ((a 1)) (list a 2)) '(1 2)) #t)
(test (equal? (let ((a 1)) (list 'a '2)) '(a 2)) #t)
(test (equal? (let ((a 1)) (list 'a 2)) '(a 2)) #t)
(test (equal? (list) '()) #t)
(test (equal? (let ((a (list 1 2))) a) '(1 2)) #t)
(test (equal? (let ((a (list 1 2))) (list 3 4 'a (car (cons 'b 'c)) (+ 6 -2))) '(3 4 a b 4)) #t)
(test (equal? (cons 'a (cons 'b (cons 'c 'd))) '(a b c . d)) #t)
(test (length (list 'a 'b 'c 'd)) 4)
(test (length (list 'a (list 'b 'c) 'd)) 3)
(test (length '()) 0)

(test (pair? #f) #f)
(test (pair? (cons 1 2)) #t)
(test (pair? (make-vector 6)) #f)
(test (pair? #t) #f)

(test (let ((x (list 'a (list 'b 'c) 'd))) x) '(a (b c) d))
(test (let ((stack (list 'a 'b 'c))) stack) '(a b c))
(test (null? '()) #t)
(test (null? #f) #f)
(test (null? #t) #f)
(test (null? 1) #f)

(test (append (list 'a 'b 'c) (list 'd 'e 'f) '() (list 'g)) '(a b c d e f g))
(test (append (list 'a 'b 'c) 'd) '(a b c . d))
(test (append) '())
(test (append 'a) 'a)

(test (caar 'a) 'error)
(test (caar '(a)) 'error)
(test (cadr 'a) 'error)
(test (cadr '(a . b)) 'error)
(test (cdar 'a) 'error)
(test (cdar '(a . b)) 'error)
(test (cddr 'a) 'error)
(test (cddr '(a . b)) 'error)
(test (caaar 'a) 'error)
(test (caaar '(a)) 'error)
(test (caaar '((a))) 'error)
(test (caadr 'a) 'error)
(test (caadr '(a . b)) 'error)
(test (caadr '(a b)) 'error)
(test (cadar 'a) 'error)
(test (cadar '(a . b)) 'error)
(test (cadar '((a . c) . b)) 'error)
(test (caddr 'a) 'error)
(test (caddr '(a . b)) 'error)
(test (caddr '(a c . b)) 'error)
(test (cdaar 'a) 'error)
(test (cdaar '(a)) 'error)
(test (cdaar '((a . b))) 'error)
(test (cdadr 'a) 'error)
(test (cdadr '(a . b)) 'error)
(test (cdadr '(a b . c)) 'error)
(test (cddar 'a) 'error)
(test (cddar '(a . b)) 'error)
(test (cddar '((a . b) . b)) 'error)
(test (cdddr 'a) 'error)
(test (cdddr '(a . b)) 'error)
(test (cdddr '(a c . b)) 'error)
(test (caaaar 'a) 'error)
(test (caaaar '(a)) 'error)
(test (caaaar '((a))) 'error)
(test (caaaar '(((a)))) 'error)
(test (caaadr 'a) 'error)
(test (caaadr '(a . b)) 'error)
(test (caaadr '(a b)) 'error)
(test (caaadr '(a (b))) 'error)
(test (caadar 'a) 'error)
(test (caadar '(a . b)) 'error)
(test (caadar '((a . c) . b)) 'error)
(test (caadar '((a c) . b)) 'error)
(test (caaddr 'a) 'error)
(test (caaddr '(a . b)) 'error)
(test (caaddr '(a c . b)) 'error)
(test (caaddr '(a c b)) 'error)
(test (cadaar 'a) 'error)
(test (cadaar '(a)) 'error)
(test (cadaar '((a . b))) 'error)
(test (cadaar '((a b))) 'error)
(test (cadadr 'a) 'error)
(test (cadadr '(a . b)) 'error)
(test (cadadr '(a b . c)) 'error)
(test (cadadr '(a (b . e) . c)) 'error)
(test (caddar 'a) 'error)
(test (caddar '(a . b)) 'error)
(test (caddar '((a . b) . b)) 'error)
(test (caddar '((a b . c) . b)) 'error)
(test (cadddr 'a) 'error)
(test (cadddr '(a . b)) 'error)
(test (cadddr '(a c . b)) 'error)
(test (cadddr '(a c e . b)) 'error)
(test (cdaaar 'a) 'error)
(test (cdaaar '(a)) 'error)
(test (cdaaar '((a))) 'error)
(test (cdaaar '(((a . b)))) 'error)
(test (cdaadr 'a) 'error)
(test (cdaadr '(a . b)) 'error)
(test (cdaadr '(a b)) 'error)
(test (cdaadr '(a (b . c))) 'error)
(test (cdadar 'a) 'error)
(test (cdadar '(a . b)) 'error)
(test (cdadar '((a . c) . b)) 'error)
(test (cdadar '((a c . d) . b)) 'error)
(test (cdaddr 'a) 'error)
(test (cdaddr '(a . b)) 'error)
(test (cdaddr '(a c . b)) 'error)
(test (cdaddr '(a c b . d)) 'error)
(test (cddaar 'a) 'error)
(test (cddaar '(a)) 'error)
(test (cddaar '((a . b))) 'error)
(test (cddaar '((a b))) 'error)
(test (cddadr 'a) 'error)
(test (cddadr '(a . b)) 'error)
(test (cddadr '(a b . c)) 'error)
(test (cddadr '(a (b . e) . c)) 'error)
(test (cdddar 'a) 'error)
(test (cdddar '(a . b)) 'error)
(test (cdddar '((a . b) . b)) 'error)
(test (cdddar '((a b . c) . b)) 'error)
(test (cddddr 'a) 'error)
(test (cddddr '(a . b)) 'error)
(test (cddddr '(a c . b)) 'error)
(test (cddddr '(a c e . b)) 'error)

(test (vector? (make-vector 6)) #t)
(test (vector? #*1011) #f)
(test (vector? "hi") #f)
(test (vector? 'hi) #f)
(test (vector? 12) #f)
(test (equal? (string #\c) "c") #t)
(test (string=? "foo" "foo") #t)
(test (string=? "foo" "FOO") #f)
(test (string=? "foo" "bar") #f)
(test (string<? "aaaa" "aaab") #t)
(test (string>=? "aaaaa" "aaaa") #t)
(test (string? "aaaaaa") #t)
(test (string? #\a) #f)
(test (equal? (let ((str "a string")) str) "a string") #t)
(test (string-length "abc") 3)

(test (eq? 'abc 'abc) #t)
(test (length '(this-that)) 1)
(test (length '(this - that)) 3)
(test (length '(a b)) 2)
(test (equal? (cons 'this-one 'that-one) '(this-one . that-one)) #t)
(test (equal? 'foo (quote foo)) #t)
(test (equal? ''foo 'foo) #f)
(test (equal? (car ''foo) 'quote) #t)


(test (let ((l (list 1 2 3))) l) '(1 2 3))
(test (let ((l (list 1 2 3))) (reverse l)) '(3 2 1))
(test (let ((a 1)) a) 1)
(test (let ((a 1)) '(set! a 3)) '(set! a 3))
(test (let ((a 1)) '(set! a 3) a) 1)
(test (let ((a 1)) '(set! a 3) 'a) 'a)
(test (let ((a 1)) '(set! a 3) ''a) ''a)
(test (let ((a 1)) '(set! a 3) '''a) '''a)
(test (let ((a 43)) a) 43)
(test (let ((a 43)) (list a (cons a 3))) '(43 (43 . 3)))
(test (let ((a 43)) (list a (cons a 3)) (list 'a '(cons a 3))) '(a (cons a 3)))
(test 1 1)
(test '1 1)
(test '"foo" "foo")
(test (car '(a b)) 'a)
(test '(car '(a b)) '(car '(a b)))



(test (apply + '(1 2)) 3)
(test (apply - '(1 2)) -1)
(test (apply max 3 5 '(2 7 3)) 7)
(test (apply cons '((+ 2 3) 4)) '((+ 2 3) . 4))
(test (apply + '()) 0)

(test (not #f) #t)
(test (not '()) #f)
(test (not (integer? 'sss)) #t)
(test (not (integer? 1)) #f)
(test (not 3.7) #f)
(test (not 'apple) #f)
(test (eq? 'a 'b) #f)
(test (eq? 'a 'a) #t)
(test (eq? (cons 'a 'b) (cons 'a 'c)) #f)
(test (eq? (cons 'a 'b) (cons 'a 'b)) #f)
(test (eq? '(a . b) '(a . b)) #f)
;(test (eq? #\a #\a) #t)
(test (let ((x "foo")) (eq? x x)) #t)
(test (eq? "foo" "foo") #f)
(test (eq? "foo" "foo") #f)
(test (let ((x 5)) (eq? x x)) #t)
(test (eqv? 'a 'b) #f)
(test (eqv? 'a 'a) #t)
(test (eqv? 3 3) #t)
(test (eqv? 3 3.0) #f)
(test (eqv? 3.0 3.0) #t)
(test (eqv? 3-4i 3-4i) #t)
;(test (eqv? 3-4.0i 3-4i) #f)
(test (eqv? (cons 'a 'b) (cons 'a 'c)) #f)
(test (eqv? (cons 'a 'b) (cons 'a 'b)) #f)
(test (eqv? '(a . b) '(a . b)) #f)
(test (eqv? #\a #\a) #t)
(test (eqv? "foo" "foo") #f)
(test (eqv? "foo" "foo") #f)
(test (equal? 'a 'b) #f)
(test (equal? 'a 'a) #t)
(test (equal? 3 3) #t)
;(test (equal? 3 3.0) #f)
(test (equal? 3.0 3.0) #t)
(test (equal? 3-4i 3-4i) #t)
;(test (equal? 3-4.0i 3-4i) #f)
(test (equal? (cons 'a 'b) (cons 'a 'c)) #f)
(test (equal? (cons 'a 'b) (cons 'a 'b)) #t)
(test (equal? #\a #\a) #t)
(test (equal? #\a #\A) #f)
(test (equal? "foo" "foo") #t)
(test (equal? "foo" "fOo") #f)
(test (equal? "this-string" "this-string") #t)
(test (equal? "this-string" "this-String") #f)
(test (equal? 'a 'b) #f)
(test (equal? 'a 'a) #t)
(test (and) #t)
(test (or) #f)
(test (begin 1 2 3) 3)
(test (begin (+ 2 1) 2) 2)
(test (equal? (let ((a #t) (b #f)) (list (if a 1 2) (if b 1 2) (if a 1) (if b 1 0))) '(1 2 1 0)) #t)
(test (equal? (let ((a #t) (b 10) (c #f)) (list (cond (a 1) (#t 'end)) (cond (b) (#t 'end)) (cond (c 1) (#t 'end)))) '(1 10 end)) #t)
(test (equal? (string-ref "abcdef-dg1ndh" 0) #\a) #t)
(test (equal? (string-ref "abcdef-dg1ndh" 1) #\b) #t)
(test (equal? (string-ref "abcdef-dg1ndh" 6) #\-) #t)
(test (string-ref "abcdef-dg1ndh" 20) 'error)
(test (string-ref "abcdef-dg1ndh") 'error)
(test (string-ref "abcdef-dg1ndh" -3) 'error)
(test (string-ref) 'error)
(test (string-ref 2) 'error)
(test (string-ref "abcde" 2 4) 'error)
(test (string-ref 'a 0) 'error)
(test (string-ref 'anna 0) 'error)
(test (string=? "foo" "foo") #t)
(test (string=? "foo" "Foo") #f)
(test (string=? "foo" "FOO") #f)
(test (string=? "foo" "bar") #f)
(test (string<? "" "abcdefgh") #t)
(test (string<? "a" "abcdefgh") #t)
(test (string<? "abc" "abcdefgh") #t)
(test (string<? "cabc" "abcdefgh") #f)
(test (string<? "abcdefgh" "abcdefgh") #f)
(test (string<? "xyzabc" "abcdefgh") #f)
(test (string<? "abc" "xyzabcdefgh") #t)
(test (string<? "abcdefgh" "") #f)
(test (string<? "abcdefgh" "a") #f)
(test (string<? "abcdefgh" "abc") #f)
(test (string<? "abcdefgh" "cabc") #t)
(test (string<? "abcdefgh" "xyzabc") #t)
(test (string<? "xyzabcdefgh" "abc") #f)
(test (string<? "abcdef" "bcdefgh") #t)
(test (string>? "" "abcdefgh") #f)
(test (string>? "a" "abcdefgh") #f)
(test (string>? "abc" "abcdefgh") #f)
(test (string>? "cabc" "abcdefgh") #t)
(test (string>? "abcdefgh" "abcdefgh") #f)
(test (string>? "xyzabc" "abcdefgh") #t)
(test (string>? "abc" "xyzabcdefgh") #f)
(test (string>? "abcdefgh" "") #t)
(test (string>? "abcdefgh" "a") #t)
(test (string>? "abcdefgh" "abc") #t)
(test (string>? "abcdefgh" "cabc") #f)
(test (string>? "abcdefgh" "xyzabc") #f)
(test (string>? "xyzabcdefgh" "abc") #t)
(test (string>? "abcde" "bc") #f)
(test (string>? "bcdef" "abcde") #t)
(test (string>? "bcdef" "abcdef") #t)
(test (string<? "" "abcdefgh") #t)
(test (string<=? "a" "abcdefgh") #t)
(test (string<=? "abc" "abcdefgh") #t)
(test (string<=? "aaabce" "aaabcdefgh") #f)
(test (string<=? "cabc" "abcdefgh") #f)
(test (string<=? "abcdefgh" "abcdefgh") #t)
(test (string<=? "xyzabc" "abcdefgh") #f)
(test (string<=? "abc" "xyzabcdefgh") #t)
(test (string<=? "abcdefgh" "") #f)
(test (string<=? "abcdefgh" "a") #f)
(test (string<=? "abcdefgh" "abc") #f)
(test (string<=? "abcdefgh" "cabc") #t)
(test (string<=? "abcdefgh" "xyzabc") #t)
(test (string<=? "xyzabcdefgh" "abc") #f)
(test (string<=? "abcdef" "bcdefgh") #t)
(test (string>=? "" "abcdefgh") #f)
(test (string>=? "a" "abcdefgh") #f)
(test (string>=? "abc" "abcdefgh") #f)
(test (string>=? "cabc" "abcdefgh") #t)
(test (string>=? "abcdefgh" "abcdefgh") #t)
(test (string>=? "xyzabc" "abcdefgh") #t)
(test (string>=? "abc" "xyzabcdefgh") #f)
(test (string>=? "abcdefgh" "") #t)
(test (string>=? "abcdefgh" "a") #t)
(test (string>=? "abcdefgh" "abc") #t)
(test (string>=? "abcdefgh" "cabc") #f)
(test (string>=? "abcdefgh" "xyzabc") #f)
(test (string>=? "xyzabcdefgh" "abc") #t)
(test (string>=? "bcdef" "abcdef") #t)
(test (cond) 'error)
(test (cond ('a)) 'a)
(test (cond (#f 'a) ('b)) 'b)
(test (cond (#t 'a) (#t 'b)) 'a)
(test (if #t 1 2) 1)
(test (if #f 1 2) 2)




;;; from r5rs

(test (+ 3 4) 7)
(test ((if #f + *) 3 4) 12)
(test ((lambda (x) (+ x x)) 4)  8)
(define reverse-subtract (lambda (x y) (- y x)))
(test (reverse-subtract 7 10) 3)
(define add4 (let ((x 4)) (lambda (y) (+ x y))))
(test (add4 6) 10)
(test ((lambda x x) 3 4 5 6) '(3 4 5 6))
(test ((lambda (x y . z) z) 3 4 5 6)  '(5 6))
(test (if (> 3 2) 'yes 'no)  'yes)
(test (if (> 2 3) 'yes 'no)  'no)
(test (if (> 3 2) (- 3 2) (+ 3 2))  1)
(test (begin (define x 2) (+ x 1)) 3)
(test (cond ((> 3 2) 'greater)  ((< 3 2) 'less)) 'greater)
(test (cond ((> 3 3) 'greater)  ((< 3 3) 'less)  (else 'equal)) 'equal)
(test (cond ((assv 'b '((a 1) (b 2))) => cadr)  (else #f)) 2)
(test (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))  'composite)
(test (case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant)) 'consonant)
(test (and (= 2 2) (> 2 1))  #t)
(test (and (= 2 2) (< 2 1))  #f)
(test (and 1 2 'c '(f g)) '(f g))
(test (and) #t)
(test  (or (= 2 2) (> 2 1)) #t)
(test  (or (= 2 2) (< 2 1)) #t)
(test  (or #f #f #f) #f)
(test  (or (memq 'b '(a b c))  (/ 3 0))  '(b c))
(test (let ((x 2) (y 3)) (* x y)) 6)
(test (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) 35)
(test (let ((x 2) (y 3)) (let* ((x 7)  (z (+ x y))) (* z x))) 70)
(test (letrec ((even? (lambda (n)  (if (zero? n) #t (odd? (- n 1)))))  (odd? (lambda (n)  (if (zero? n) #f (even? (- n 1)))))) (even? 88))  #t)
(test (begin (define x 0) (begin (set! x 5) (+ x 1)))  6)
(test (do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))  '#(0 1 2 3 4))
(test (let ((x '(1 3 5 7 9))) (do ((x x (cdr x)) (sum 0 (+ sum (car x))))  ((null? x) sum))) 25)
(test (let loop ((numbers '(3 -2 1 6 -5)) (nonneg '()) (neg '())) (cond ((null? numbers) (list nonneg neg)) ((>= (car numbers) 0)  (loop (cdr numbers)  (cons (car numbers) nonneg)  neg)) ((< (car numbers) 0)  (loop (cdr numbers)  nonneg  (cons (car numbers) neg))))) '((6 1 3) (-5 -2)))
(test `(list ,(+ 1 2) 4)  '(list 3 4))
(test `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)  '(a 3 4 5 6 b))
(test `((`foo' ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))  '((foo 7) . cons))
(test `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)  '#(10 5 2.0 4.0 3.0 8))
(test `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)  '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
(test (let ((name1 'x)  (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))  '(a `(b ,x ,'y d) e))
(test (quasiquote (list (unquote (+ 1 2)) 4))  '(list 3 4))
(test '(quasiquote (list (unquote (+ 1 2)) 4)) `(list ,(+ 1 2) 4))
(test (let ((=> #f)) (cond (#t => 'ok)))  'ok)
(test (begin (define add3 (lambda (x) (+ x 3)))(add3 3)) 6)
(test (begin (define first car)(first '(1 2))) 1)
(test (let ((x 5)) (define foo (lambda (y) (bar x y))) (define bar (lambda (a b) (+ (* a b) a))) (foo (+ x 3))) 45)
(test (let ((x 5)) (letrec ((foo (lambda (y) (bar x y))) (bar (lambda (a b) (+ (* a b) a)))) (foo (+ x 3)))) 45)
(test (eqv? 'a 'a)  #t)
(test (eqv? 'a 'b)  #f)
(test (eqv? 2 2) #t)
(test (eqv? '() '()) #t)
(test (eqv? 100000000 100000000) #t)
(test (eqv? (cons 1 2) (cons 1 2)) #f)
(test (eqv? (lambda () 1)  (lambda () 2)) #f)
(test (eqv? #f 'nil) #f)
(test (let ((p (lambda (x) x))) (eqv? p p)) #t)
(define gen-counter (lambda () (let ((n 0))  (lambda () (set! n (+ n 1)) n))))
(test (let ((g (gen-counter))) (eqv? g g)) #t)
(test (eqv? (gen-counter) (gen-counter))  #f)
(define gen-loser (lambda () (let ((n 0))  (lambda () (set! n (+ n 1)) 27))))
(test (let ((g (gen-loser))) (eqv? g g)) #t)
(test (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))  (g (lambda () (if (eqv? f g) 'g 'both)))) (eqv? f g))  #f)
(test (let ((x '(a))) (eqv? x x)) #t)
(test (eq? 'a 'a) #t)
(test (eq? (list 'a) (list 'a)) #f)
(test (eq? '() '()) #t)
(test (eq? car car) #t)
(test (let ((x '(a))) (eq? x x))  #t)
(test (let ((x '#())) (eq? x x))  #t)
(test (let ((p (lambda (x) x))) (eq? p p))  #t)
(test (equal? 'a 'a) #t)
(test (equal? '(a) '(a))  #t)
(test (equal? '(a (b) c) '(a (b) c)) #t)
(test (equal? "abc" "abc") #t)
(test (equal? 2 2)  #t)
(test (equal? (make-vector 5 'a) (make-vector 5 'a))  #t)
(test (complex? 3+4i)  #t)
(test (complex? 3)  #t)
(test (real? 3)  #t)
(test (real? -2.5+0.0i) #t)
(test (real? #e1e10) #t)
(test (rational? 6/10) #t)
(test (rational? 6/3)  #t)
(test (integer? 8/4) #t)
(test (+ 3 4) 7)
(test (+ 3) 3)
(test (+)  0)
(test (* 4) 4)
(test (*)  1)
(test (- 3 4) -1)
(test (- 3 4 5)  -6)
(test (- 3) -3)
(test (/ 3 4 5)  3/20)
(test (/ 3) 1/3)
(test (abs -7) 7)
(test (modulo 13 4) 1)
(test (remainder 13 4) 1)
(test (modulo -13 4) 3)
(test (remainder -13 4) -1)
(test (modulo 13 -4) -3)
(test (remainder 13 -4) 1)
(test (modulo -13 -4)  -1)
(test (remainder -13 -4)  -1)
(test (gcd 32 -36)  4)
(test (gcd) 0)
(test (lcm 32 -36)  288)
(test (lcm) 1)
(test (numerator (/ 6 4)) 3)
(test (denominator (/ 6 4))  2)
(test (string->number "100") 100)
(test (string->number "100" 16) 256)
(test (string->number "1e2") 100.0)
(test (string->number "15##") 1500.0)
(test (not #t) #f)
(test (not 3) #f)
(test (not (list 3)) #f)
(test (not #f) #t)
(test (not '())  #f)
(test (not (list))  #f)
(test (not 'nil) #f)
(test (boolean? #f) #t)
(test (boolean? 0)  #f)
(test (boolean? '()) #f)
(test (pair? '(a . b)) #t)
(test (pair? '(a b c)) #t)
(test (pair? '()) #f)
(test (pair? '#(a b))  #f)
(test (cons 'a '()) '(a))
(test (cons '(a) '(b c d)) '((a) b c d))
(test (cons "a" '(b c)) '("a" b c))
(test (cons 'a 3) '(a . 3))
(test (cons '(a b) 'c) '((a b) . c))
(test  (car '(a b c)) 'a)
(test  (car '((a) b c d))  '(a))
(test  (car '(1 . 2)) 1)
(test  (car '())  'error)
(test (cdr '((a) b c d))  '(b c d))
(test (cdr '(1 . 2)) 2)
(test (cdr '())  'error)
(test (list? '(a b c))  #t)
(test  (list? '()) #t)
(test  (list? '(a . b))  #f)
(test  (let ((x (list 'a))) (set-cdr! x x) (list? x)) #f)
(test  (list 'a (+ 3 4) 'c) '(a 7 c))
(test  (list)  '())
(test  (length '(a b c)) 3)
(test  (length '(a (b) (c d e))) 3)
(test  (length '())  0)
(test (append '(x) '(y))  '(x y))
(test (append '(a) '(b c d)) '(a b c d))
(test (append '(a (b)) '((c)))  '(a (b) (c)))
(test (append '(a b) '(c . d))  '(a b c . d))
(test (append '() 'a)  'a)
(test (reverse '(a b c))  '(c b a))
(test (reverse '(a (b c) d (e (f))))  '((e (f)) d (b c) a))
(test (list-ref '(a b c d) 2) 'c)
(test (list-ref '(a b c d) (inexact->exact (round 1.8))) 'c)
(test (memq 'a '(a b c))  '(a b c))
(test (memq 'b '(a b c))  '(b c))
(test (memq 'a '(b c d))  #f)
(test (memq (list 'a) '(b (a) c))  #f)
(test (member (list 'a) '(b (a) c)) '((a) c))
(test (memv 101 '(100 101 102)) '(101 102))
(let ((e '((a 1) (b 2) (c 3))))
  (test (assq 'a e) '(a 1))
  (test (assq 'b e) '(b 2))
  (test (assq 'd e) #f))
(test (assq (list 'a) '(((a)) ((b)) ((c))))  #f)
(test (assoc (list 'a) '(((a)) ((b)) ((c))))  '((a)))
(test (assv 5 '((2 3) (5 7) (11 13))) '(5 7))
(test (symbol? 'foo) #t)
(test (symbol? (car '(a b))) #t)
(test (symbol? "bar")  #f)
(test (symbol? 'nil) #t)
(test (symbol? '()) #f)
(test (symbol? #f)  #f)
(test (symbol->string 'flying-fish) "flying-fish")
(test (symbol->string  (string->symbol "Malvina")) "Malvina")
(test (eq? 'JollyWog (string->symbol (symbol->string 'JollyWog)))  #t)
(test (string=? "K. Harper, M.D." (symbol->string  (string->symbol "K. Harper, M.D.")))  #t)
(test (vector-ref '#(1 1 2 3 5 8 13 21)  5)  8)
(test (vector-ref '#(1 1 2 3 5 8 13 21)  (let ((i (round (* 2 (acos -1))))) (if (inexact? i)  (inexact->exact i)  i))) 13)
(test (let ((vec (vector 0 '(2 2 2 2) "Anna"))) (vector-set! vec 1 '("Sue" "Sue")) vec) '#(0 ("Sue" "Sue") "Anna"))
(test (vector-set! '#(0 1 2) 1 "doe")  'error)
(test (vector->list '#(dah dah didah))  '(dah dah didah))
(test (list->vector '(dididit dah)) '#(dididit dah))
(test (procedure? car) #t)
(test (procedure? 'car) #f)
(test (procedure? (lambda (x) (* x x)))  #t)
(test (procedure? '(lambda (x) (* x x))) #f)
(test (call-with-current-continuation procedure?)  #t)
(test (apply + (list 3 4)) 7)
(test (begin (define compose (lambda (f g) (lambda args (f (apply g args))))) ((compose sqrt *) 12 75))  30.0)
(test (map cadr '((a b) (d e) (g h))) '(b e h))
(test (map (lambda (n) (expt n n)) '(1 2 3 4 5)) '(1 4 27 256 3125))
(test (map + '(1 2 3) '(4 5 6)) '(5 7 9))
(test (let ((v (make-vector 5))) (for-each (lambda (i) (vector-set! v i (* i i)))  '(0 1 2 3 4)) v) '#(0 1 4 9 16))
(test (force (make-promise (+ 1 2))) 3)
(test (let ((p (make-promise (+ 1 2)))) (list (force p) (force p))) '(3 3))
(test (begin (define a-stream (letrec ((next  (lambda (n) (cons n (make-promise (next (+ n 1))))))) (next 0)))(define head car)(define tail (lambda (stream) (force (cdr stream))))(head (tail (tail a-stream)))) 2)
(test (call-with-current-continuation (lambda (exit) (for-each (lambda (x) (if (negative? x) (exit x))) '(54 0 37 -3 245 19)) #t)) -3)
(define list-length (lambda (obj) (call-with-current-continuation  (lambda (return) (letrec ((r  (lambda (obj) (cond ((null? obj) 0) ((pair? obj)  (+ (r (cdr obj)) 1)) (else (return #f)))))) (r obj))))))
(test (list-length '(1 2 3 4))  4)
(test (list-length '(a b . c))  #f)
(test (call-with-values (lambda () (values 4 5))  (lambda (a b) b))  5)
(test (call-with-values * -) -1)
(test (let ((path '())  (c #f)) (let ((add (lambda (s)  (set! path (cons s path))))) (dynamic-wind  (lambda () (add 'connect))  (lambda () (add (call-with-current-continuation  (lambda (c0) (set! c c0) 'talk1))))  (lambda () (add 'disconnect))) (if (< (length path) 4) (c 'talk2) (reverse path)))) '(connect talk1 disconnect  connect talk2 disconnect))
(test (eval '(* 7 3) (global-environment))  21)



