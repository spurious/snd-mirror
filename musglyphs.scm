;; this file loads the music symbol glyphs in cmn-glyphs.lisp (from the cmn package),
;;; each function in that package becomes a Scheme function of the form:
;;;   name :optional x y size style snd chn ax
;;;   style: #t for lines, #f (default) for filled polygon
;;;
;;; although Snd based here, all this file needs externally are draw-lines, draw-dot, and fill-polygon

(provide 'snd-musglyphs.scm)

(define make-polygon
  (lambda (args)
    (define (total-length vects len)
      (if (null? vects)
	  len
	  (if (vector? (car vects))
	      (total-length (cdr vects) (+ len (vector-length (car vects))))
	      (if (car vects)
		  (total-length (cdr vects) (+ len 1))
		  (total-length (cdr vects) len)))))
    (define (set-vals vects start vals)
      (if (null? vects)
	  vals
	  (if (vector? (car vects))
	      (let* ((vect (car vects))
		     (len (vector-length vect)))
		(do ((i 0 (1+ i)))
		    ((= i len))
		  (vector-set! vals (+ start i) (vector-ref vect i)))
		(set-vals (cdr vects) (+ start len) vals))
	      (if (car vects)
		  (begin
		    (vector-set! vals start (car vects))
		    (set-vals (cdr vects) (+ start 1) vals))
		  (set-vals (cdr vects) start vals)))))
    (set-vals args 0 (make-vector (total-length args 0)))))

(define (make-bezier-1 x0 y0 x1 y1 x2 y2 x3 y3 n)
  ;; creates a line-segment approximation of a bezier curve: n = number of segments
  ;; this is built into Snd as make-bezier, but I wanted an all Scheme version
  (let* ((cx (* 3 (- x1 x0)))
	 (cy (* 3 (- y1 y0)))
	 (bx (- (* 3 (- x2 x1)) cx))
	 (by (- (* 3 (- y2 y1)) cy))
	 (ax (- x3 (+ x0 cx bx)))
	 (ay (- y3 (+ y0 cy by)))
	 (incr (/ 1.0 n))
	 (pts (make-vector (* 2 (+ n 1)))))
    (vector-set! pts 0 x0)
    (vector-set! pts 1 y0)
    (do ((i 0 (1+ i))
	 (val incr (+ val incr)))
	((> i n) pts)
      (vector-set! pts (* i 2) (inexact->exact (floor (+ x0 (* val (+ cx (* val (+ bx (* val ax)))))))))
      (vector-set! pts (+ (* i 2) 1) (inexact->exact (floor (+ y0 (* val (+ cy (* val (+ by (* val ay))))))))))))

;; pass our Snd context into the graphics procedures (there's probably a cleaner way)
(define ps-snd 0)
(define ps-chn 0)
(define ps-ax 0)
(define yoff 0)
(define xoff 0)

;; now make a little Postscript interpreter...
(define curx 0)  ; the Postscript "path" handlers
(define cury 0)
(define pathlist '())
(define ps-size 50)
(define bezier-segments 50)
(define (->x x) 
  (inexact->exact (floor (+ xoff (* ps-size x)))))
(define (->y y) 
  ;; Postscript is right side up, X is upside-down
  (inexact->exact (floor (- yoff (* ps-size y)))))

;; now functions and macros to make it possible to load the original Common Lisp glyphs directly

(define (moveto score x y)
  (set! curx x)
  (set! cury y)
  #f)

(define (rmoveto score x y)
  (set! curx (+ curx x))
  (set! cury (+ cury y))
  #f)

(define (curveto score x0 y0 x1 y1 x2 y2)
  (set! pathlist (cons (make-bezier-1 (->x curx) (->y cury) (->x x0) (->y y0) 
				      (->x x1) (->y y1) (->x x2) (->y y2) 
				      bezier-segments)
			 pathlist))
  (set! curx x2)
  (set! cury y2)
  #f)
  
(define (lineto score x y)
  (let ((v (make-vector 2)))
    (vector-set! v 0 (->x x))
    (vector-set! v 1 (->y y))
    (set! curx x)
    (set! cury y)
    (set! pathlist (cons v pathlist))
    #f))

(define (rlineto score x y)
  (let ((v (make-vector 2)))
    (set! curx (+ curx x))
    (set! cury (+ cury y))
    (vector-set! v 0 (->x curx))
    (vector-set! v 1 (->y cury))
    (set! pathlist (cons v pathlist))
    #f))

(define (fill-in score)
  (if (not (null? pathlist))
      (fill-polygon
       (make-polygon
	(reverse pathlist))
	ps-snd ps-chn ps-ax))
  (set! pathlist '())
  #f)

(define (draw score arg)
  (if (not (null? pathlist))
      (draw-lines
       (make-polygon
	(reverse pathlist))
	ps-snd ps-chn ps-ax))
  (set! pathlist '())
  #f)

(define (circle score x0 y0 rad . rest)
  (draw-dot (->x x0) (->y y0) 
	    (inexact->exact (floor (* ps-size rad 2)))
	    ps-snd ps-chn ps-ax))

(define old-defvar defvar)
(set! defvar define)

(defmacro defun (name ignored-args . body)
  ;; in cmn-glyphs every defun has two args, the "score" and an optional "style"
  `(define ,name (lambda args 
		   ((lambda (score style) ; needed by the procedure body
		      (let ((arglen (length args))
			    (style #f))
			(if (> arglen 0) (set! xoff (list-ref args 0)))
			(if (> arglen 1) (set! yoff (list-ref args 1)))
			(if (> arglen 2) (set! ps-size (list-ref args 2)))
			(if (> arglen 3) (set! style (list-ref args 3)))
			(if (> arglen 4) (set! ps-snd (list-ref args 4)))
			(if (> arglen 5) (set! ps-chn (list-ref args 5)))
			(if (> arglen 6) (set! ps-ax (list-ref args 6)))
			,@body))
		    #f #f))))

(defmacro in-package (name) #f)
(define (music-font score) #f)
(define g-mustext (lambda args #f))
(define (output-type score) #f)
(if (defined? 'declare) (define old-declare declare) (define old-declare #f))
(define (declare args) #f)
(define sound-comment comment)
(define comment
  (make-procedure-with-setter
   (lambda* (:optional (scr #f) (msg 0))
     "(comment :optional (scr #f) (msg 0)) tries to make musglyph.scm safe for comments"
     (if (or (number? msg)
	     (not scr))
	 (sound-comment scr)))
   (lambda* (snd :optional (val #f))
     (if (not val)
	 (apply (setter sound-comment) (list #f snd))
	 (apply (setter sound-comment) (list snd val))))))

;(load "loop.scm") ; Rick's loop implementation (cm/src/loop.scm)
(define progn begin)

(load-from-path "cmn-glyphs.lisp")

(set! defvar old-defvar)
(set! declare old-declare)
