(set-prompt *g2* "!!")

(define (simple-test expr rtn)
  (clear *g2*)
  (let ((bpos (cursor-position *g2*))
	(len (length expr)))
    (append-text *g2* expr)

    (if (char-whitespace? (expr 0))
	(begin
	  (set-cursor-position *g2* bpos)
	  (evaluate *g2*)))
	  
    (do ((i bpos (+ i 1)))
	((> i (+ bpos len)))
      (set-cursor-position *g2* i)
      (let ((result (evaluate *g2*)))
	(if (not (string=? result rtn))
	    (append-text *g1* (format #f "~%~S, cursor at ~D: ~S~%" expr i result)))))))

(simple-test "(+ 1 2)" "(+ 1 2)\n3")
(simple-test "#\\a" "#\\a\n#\\a")
(simple-test "123" "123\n123")
(simple-test "1.5+i" "1.5+i\n1.5+1i")
(simple-test "\"asdf\"" "\"asdf\"\n\"asdf\"")
(simple-test "\"as df\"" "\"as df\"\n\"as df\"")
(simple-test "1/2" "1/2\n1/2")
(simple-test "'(+ 1 2)" "'(+ 1 2)\n(+ 1 2)")
(simple-test "\"12;34\"" "\"12;34\"\n\"12;34\"")
(simple-test "#(1 2)" "#(1 2)\n#(1 2)")
(simple-test ":hi" ":hi\n:hi")
(simple-test "\"\"" "\"\"\n\"\"")
(simple-test "()" "()\n()")
(simple-test "( )" "( )\n()")
(simple-test "'()" "'()\n()")
(simple-test "*gc-stats*" "*gc-stats*\n#f")
(simple-test "#<eof>" "#<eof>\n#<eof>")
(simple-test "(cons 1 2)" "(cons 1 2)\n(1 . 2)")
(simple-test "(+ 1 (* 2 3))" "(+ 1 (* 2 3))\n7")
(simple-test "(char? #\\a)" "(char? #\\a)\n#t")
(simple-test "(char? #\\))" "(char? #\\))\n#t")
(simple-test "(char? #\\()" "(char? #\\()\n#t")
(simple-test "(char? #\\;)" "(char? #\\;)\n#t")
(simple-test "(char? #\\\")" "(char? #\\\")\n#t")
(simple-test "#2d((1 2) (3 4))" "#2d((1 2) (3 4))\n#2D((1 2) (3 4))")
(simple-test "`(1 2)" "`(1 2)\n(1 2)")
(simple-test "((if (< 3 2) * +) 3 2)" "((if (< 3 2) * +) 3 2)\n5")
(simple-test "(if (char? #\\\") 0 1)" "(if (char? #\\\") 0 1)\n0")
(simple-test "  (+ 1 2)  " "(+ 1 2)\n3")
(simple-test "(+ #| 1 2 3 |# 4 5)" "(+ #| 1 2 3 |# 4 5)\n9")
(simple-test "(char? \"\")" "(char? \"\")\n#f")
(simple-test "(equal? \"\" \"\")" "(equal? \"\" \"\")\n#t")
(simple-test "\"\"" "\"\"\n\"\"")
(simple-test "(#||#)" "(#||#)\n()")
(simple-test "(#|()|#)" "(#|()|#)\n()")
(simple-test "(#|#\\a|#)" "(#|#\\a|#)\n()")
(simple-test "(#|\"\"|#)" "(#|\"\"|#)\n()")
(simple-test "(+ 1 #|\"\"|# 2 3)" "(+ 1 #|\"\"|# 2 3)\n6")
(simple-test "(char?\n    #\\a\n)" "(char?\n    #\\a\n)\n#t")
(simple-test "(+ 1 2 ; a test\n3)" "(+ 1 2 ; a test\n3)\n6")
(simple-test "(+ 1 2 #| a test\n|#3)" "(+ 1 2 #| a test\n|#3)\n6")
(simple-test "\"a;b\"" "\"a;b\"\n\"a;b\"")


;(simple-test "\"\\\"\"" "\"\\\"\n\\\"\"") -- results are correct??
;(simple-test "a\"b" "a\"b\n\"a\"b")

;(simple-test "(+ #| 1 ( 2 3 |# 4 5)" "(+ #| 1 ( 2 3 |# 4 5)\n9") ; TODO: trouble seeing we're in block comment
;(simple-test "(+ #| 1 ; 2 3 |# 4 5)" "(+ #| 1 ; 2 3 |# 4 5)\n9") ; TODO: if cursor between | and ; -- confusion, double-quote has same problem

;TODO: we're being confused by ; comments if parens in comment and we start in the comment
; also "a(n" if cursor if just after the (
; also if  "" "a"  and cursor just before 'a' -> ""
; also  12 "a" -> "a"! no matter where the cursor is


(define (multi-test exprs pos rtn0 rtn1 rtn2)
  (clear *g2*)
  (let ((bpos (cursor-position *g2*))
	(len (length exprs)))
    (append-text *g2* exprs)
	  
    (set-cursor-position *g2* bpos)
    (let ((result (evaluate *g2*)))
      (if (not (string=? result rtn0))
	  (append-text *g1* (format #f "~%~S, cursor at ~D (pos: ~D): ~S~%" exprs i pos result))))

    (do ((i (+ bpos 1) (+ i 1)))
	((> i (+ bpos len)))
      (set-cursor-position *g2* i)
      (let ((result (evaluate *g2*)))
	(if (not (string=? result (if (< (- i bpos) pos) rtn1 rtn2)))
	    (append-text *g1* (format #f "~%~S, cursor at ~D (pos: ~D): ~S~%" exprs i pos result)))))))

(multi-test "123 432" 5 "123 432\n123" "123\n123" "432\n432")
(multi-test "123 #\\a" 5 "123 #\\a\n123" "123\n123" "#\\a\n#\\a")




