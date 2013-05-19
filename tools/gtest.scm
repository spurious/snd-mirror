;;; glistener test suite
;;;    see gcall.c

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
	    (append-text *g1* (format #f "~%~S, cursor at ~D:~%~S~%~S~%" expr i result rtn)))))))

(simple-test "(+ 1 2)" "(+ 1 2)\n3")
(simple-test "#\\a" "#\\a\n#\\a")
(simple-test "#\\x12" "#\\x12\n#\\x12")
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
(simple-test "(eqv? #i3/5 #i3/5)" "(eqv? #i3/5 #i3/5)\n#t")
(simple-test "*gc-stats*" "*gc-stats*\n#f")
(simple-test "#<eof>" "#<eof>\n#<eof>")
(simple-test "(cons 1 2)" "(cons 1 2)\n(1 . 2)")
(simple-test "(+ 1 (* 2 3))" "(+ 1 (* 2 3))\n7")
(simple-test "(char? #\\a)" "(char? #\\a)\n#t")
(simple-test "(char? #\\))" "(char? #\\))\n#t")
(simple-test "(char? #\\()" "(char? #\\()\n#t")
(simple-test "(char? #\\;)" "(char? #\\;)\n#t")
(simple-test "(char? #\\\")" "(char? #\\\")\n#t")
(simple-test "(char? #\\#)" "(char? #\\#)\n#t")
(simple-test "#2d((1 2) (3 4))" "#2d((1 2) (3 4))\n#2D((1 2) (3 4))")
(simple-test "`(1 2)" "`(1 2)\n(1 2)")
(simple-test "((if (< 3 2) * +) 3 2)" "((if (< 3 2) * +) 3 2)\n5")
(simple-test "(if (char? #\\\") 0 1)" "(if (char? #\\\") 0 1)\n0")
;; (simple-test "  (+ 1 2)  " "(+ 1 2)\n3")
;;; for "   #()   " if cursor trails, the preceding whitespace is included -- make test smarter?
;;; and here: (simple-test "#()   " "#()\n#()"), we get trailing space in the first case!
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
(simple-test "\"a)b\"" "\"a)b\"\n\"a)b\"")
(simple-test "\"a(b\"" "\"a(b\"\n\"a(b\"")
(simple-test "'(a #|foo|||||# b)" "'(a #|foo|||||# b)\n(a b)")
(simple-test "(let ((@,@'[1] 1) (\\,| 2)) (+ @,@'[1] \\,|))" "(let ((@,@'[1] 1) (\\,| 2)) (+ @,@'[1] \\,|))\n3")
(simple-test "(length \";\")" "(length \";\")\n1")
(simple-test "(cons \";\" 1)" "(cons \";\" 1)\n(\";\" . 1)")
(simple-test "(length \")\")" "(length \")\")\n1")
(simple-test "\"a\\\"b\"" "\"a\\\"b\"\n\"a\\\"b\"")
(simple-test "(length \"#|\")" "(length \"#|\")\n2")
(simple-test "(length \"(\")" "(length \"(\")\n1") ; works outside this test??
(simple-test "(length '(#xA\"\"#(1)))" "(length '(#xA\"\"#(1)))\n3")
(simple-test "(+ #| 1 ( 2 3 |# 4 5)" "(+ #| 1 ( 2 3 |# 4 5)\n9")
(simple-test "(+ #| 1 ; 2 3 |# 4 5)" "(+ #| 1 ; 2 3 |# 4 5)\n9") 


;(simple-test "\"\\\"\"" "\"\\\"\n\\\"\"")

;; block comments get confused if cursor to left?


; also "a(n" if cursor is just after the (
; (map /""'(123)) if cursor between ' and ( -- trouble


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

(multi-test "123 432" 4 "123 432\n123" "123\n123" "432\n432")
(multi-test "123 #\\a" 4 "123 #\\a\n123" "123\n123" "#\\a\n#\\a")
(multi-test "123 \"a\"" 4 "123 \"a\"\n123" "123\n123" "\"a\"\n\"a\"")
(multi-test "(+ 1 2) 123" 8 "(+ 1 2) 123\n3" "(+ 1 2)\n3" "123\n123")



;;; gtk_text_buffer_backspace -- try at start and make sure pos unchanged


(define (completion-test text rtn)
  (clear *g2*)
  (append-text *g2* text)
  (let ((result (complete *g2*)))
    (if (not (string=? result rtn))
	(append-text *g1* (format #f "~S -> ~S~%" text result)))))

(completion-test "(trunc" "(truncate")
(completion-test "\"card" "\"cardinal.snd\"")

