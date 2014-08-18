(provide 'mockery.scm)

(define (outlet-member obj e)
  (or (eq? obj e)
      (and (not (eq? obj (rootlet)))
	   (outlet-member (outlet obj) e))))

(define (make-method f accessor)
  (lambda args
    (if (let? (car args)) 
	(apply f (accessor (car args)) (cdr args))
	(apply f (car args) (accessor (cadr args)) (cddr args)))))

(define (make-object . args)
  (openlet
   (apply inlet args)))


;;; --------------------------------------------------------------------------------

(define *mock-vector*
  (let ((mock-vector-class
	 (openlet  ; this holds the mock-vector methods
	  (inlet 'local-set!         (lambda (obj i val) (#_vector-set! (obj 'v) i val))
		 'vector?            (lambda (obj) #t)
		 'vector-ref         (lambda (obj i) (#_vector-ref (obj 'v) i))
		 'vector-set!        (lambda (obj i val) ((obj 'local-set!) obj i val) val)
		 'let-ref            (lambda (obj i) (#_vector-ref (obj 'v) i))           ; these are the implicit cases
		 'let-set!           (lambda (obj i val) ((obj 'local-set!) obj i val) val)
		 'length             (lambda (obj) (#_vector-length (obj 'v)))
		 'vector-length      (lambda (obj) (#_vector-length (obj 'v)))
		 'map                (lambda (f obj) (map f (obj 'v)))
		 'for-each           (lambda (f obj) (for-each f (obj 'v)))
		 'vector->list       (lambda (obj) (#_vector->list (obj 'v)))
		 'make-shared-vector (lambda* (obj dim (off 0)) (#_make-shared-vector (obj 'v) dim off))
		 'vector-append      (lambda (obj . vs) (apply #_vector-append (obj 'v) vs))
		 'vector-fill!       (lambda* (obj val (start 0) end) (#_vector-fill! (obj 'v) val start (or end (#_vector-length (obj 'v)))))
		 'fill!              (lambda (obj val) (#_fill! (obj 'v) val))
		 'reverse            (lambda (obj) (#_reverse (obj 'v)))
		 'sort!              (lambda (obj f) (#_sort! (obj 'v) f))
		 'arity              (lambda (obj) (#_arity (obj 'v)))
		 'object->string     (lambda* (obj (w #t)) "#<mock-vector-class>")
		 'vector-dimensions  (lambda (obj) (#_vector-dimensions (obj 'v)))
		 'copy               (lambda* (src dest . args)
				       ;; copy by itself does not make a new vector, but if no dest we
				       ;;   need a copy of src, so use coverlet/openlet to make sure
				       ;;   we aren't caught in an infinite recursion.
				       (if (and dest (not (let? dest)))
					   (apply copy (obj 'v) dest args)
					   (let ((nobj (or dest (openlet (copy (coverlet src))))))
					     (openlet src)
					     (set! (nobj 'v) (copy (src 'v)))
					     nobj)))))))

    (define* (make-mock-vector len (init #<unspecified>))
      (openlet (sublet mock-vector-class 
		 'v (#_make-vector len init)
		 'object->string (lambda* (obj (w #t)) (#_object->string (obj 'v) w)))))

    (define (mock-vector . args)
      (let ((v (make-mock-vector 0)))
	(set! (v 'v) (apply #_vector args))
	v))

    (define (mock-vector? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-vector-class)))

    (curlet)))

#|
;; vector that grows to accommodate vector-set!

(define stretchable-vector-class
  (let ((local-ref (lambda (obj index)
		     (if (>= index (length (obj 'v)))
			 (obj 'initial-element)
			 (#_vector-ref (obj 'v) index))))
	(local-set! (lambda (obj index val)
		      (if (>= index (length (obj 'v)))
			  (set! (obj 'v) (copy (obj 'v) (make-vector (+ index 8) (obj 'initial-element)))))
		      (#_vector-set! (obj 'v) index val))))
    (openlet
     (sublet (*mock-vector* 'mock-vector-class)
       'v (vector)
       'object->string (lambda* (obj (w #t)) (format #f "#<stretchable-vector: ~S>" (obj 'v)))
       'initial-element #f
       'vector-ref local-ref
       'let-ref local-ref
       'vector-set! local-set!
       'let-set! local-set!))))
|#


;;; --------------------------------------------------------------------------------

(define *mock-hash-table*
  (let ((mock-hash-table-class
	 (openlet
	  (inlet 'hash-table?        (lambda (obj) #t)
		 'hash-table-ref     (lambda (obj key)      (#_hash-table-ref (obj 'mock-hash-table-table) key))
		 'hash-table-set!    (lambda (obj key val)  (#_hash-table-set! (obj 'mock-hash-table-table) key val))
		 'hash-table-size    (lambda (obj)          (#_hash-table-size (obj 'mock-hash-table-table)))
		 'hash-table-entries (lambda (obj)          (#_hash-table-entries (obj 'mock-hash-table-table)))
		 'make-hash-table-iterator (lambda (obj)    (#_make-hash-table-iterator (obj 'mock-hash-table-table)))
		 'let-ref-fallback   (lambda (obj key)      (#_hash-table-ref (obj 'mock-hash-table-table) key))
		 'let-set!-fallback  (lambda (obj key val)  (#_hash-table-set! (obj 'mock-hash-table-table) key val))

		 ;; the fallbacks are needed because hash-tables and lets use exactly the same syntax in implicit indexing:
		 ;;   (x 'y) but s7 can't tell that in this one case, we actually want the 'y to be a key not a field.
		 ;;   So, to avoid infinite recursion in let-ref (implicit index), if let-ref can't find the let field,
		 ;;   and the let has 'let-ref|set!-fallback, let-ref|set! passes the argument to that function rather than
		 ;;   return #<undefined>.

		 'length             (lambda (obj)          (#_hash-table-size (obj 'mock-hash-table-table)))
		 'map                (lambda (f obj)        (map f (obj 'mock-hash-table-table)))
		 'for-each           (lambda (f obj)        (for-each f (obj 'mock-hash-table-table)))
		 'fill!              (lambda (obj val)      (#_fill! (obj 'mock-hash-table-table) val))
		 'reverse            (lambda (obj)          (#_reverse (obj 'mock-hash-table-table)))
		 'object->string     (lambda* (obj (w #t))  "#<mock-hash-table-class>")
		 'arity              (lambda (obj)          (#_arity (obj 'mock-hash-table-table)))
		 'copy               (lambda* (src dest . args)
				       (if (and dest (not (let? dest)))
					   (apply copy (obj 'mock-hash-table-table) dest args)
					   (let ((nobj (or dest (openlet (copy (coverlet src))))))
					     (openlet src)
					     (set! (nobj 'mock-hash-table-table) (copy (src 'mock-hash-table-table)))
					     nobj)))))))

    (define* (make-mock-hash-table (len 511))
      (openlet (sublet mock-hash-table-class 
		 'mock-hash-table-table (#_make-hash-table len)
		 'object->string (lambda* (obj (w #t)) (#_object->string (obj 'mock-hash-table-table) w)))))

    (define (mock-hash-table . args)
      (let ((v (make-mock-hash-table)))
	(set! (v 'mock-hash-table-table) (apply #_hash-table args))
	v))

    (define (mock-hash-table* . args)
      (let ((v (make-mock-hash-table)))
	(set! (v 'mock-hash-table-table) (apply #_hash-table* args))
	v))

    (define (mock-hash-table? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-hash-table-class)))

    (curlet)))


#|
;; hash-table that returns a special identifier when key is not in the table

(define gloomy-hash-table
  (openlet
   (sublet (*mock-hash-table* 'mock-hash-table-class)
     'mock-hash-table-table #f
     'false (gensym)
     'not-a-key #f
     'hash-table-ref (lambda (obj key)
		       (let ((val (#_hash-table-ref (obj 'mock-hash-table-table) key)))
			 (if (eq? val (obj 'false))
			     #f
			     (or val (obj 'not-a-key)))))
     'hash-table-key? (lambda (obj key)
			(#_hash-table-ref (obj 'mock-hash-table-table) key)))))

(define (hash-table-key? obj key) 
  ((obj 'hash-table-key?) obj key))

(define* (make-gloomy-hash-table (len 511) not-a-key)
  (let ((ht (copy gloomy-hash-table)))
    (set! (ht 'mock-hash-table-table) (make-hash-table len))
    (set! (ht 'not-a-key) not-a-key)
    ht))
|#



;;; --------------------------------------------------------------------------------

(define *mock-string*
  (let ((mock-string-class
	 (openlet
	  (inlet 'string?                (lambda (obj) #t)
		 'string-ref             (lambda (obj i) (#_string-ref (obj 's) i))
		 'string-set!            (lambda (obj i val) (#_string-set! (obj 's) i val))
		 'let-ref                (lambda (obj i) (#_string-ref (obj 's) i))           ; these are the implicit cases
		 'let-set!               (lambda (obj i val) (string-set! (obj 's) i val))
		 'length                 (lambda (obj) (#_string-length (obj 's)))
		 'string-length          (lambda (obj) (#_string-length (obj 's)))
		 'map                    (lambda (f obj) (map f (obj 's)))
		 'for-each               (lambda (f obj) (for-each f (obj 's)))
		 'string->list           (lambda (obj) (#_string->list (obj 's)))
		 'string-append          (make-method #_string-append (lambda (obj) (obj 's)))
		 'string-fill!           (lambda* (obj val (start 0) end) (#_string-fill! (obj 's) val start (or end (#_string-length (obj 's)))))
		 'fill!                  (lambda (obj val) (#_fill! (obj 's) val))
		 'reverse                (lambda (obj) (#_reverse (obj 's)))
		 'object->string         (lambda* (obj (w #t)) "#<mock-string-class>")
		 'arity                  (lambda (obj) (#_arity (obj 's)))
		 'copy                   (lambda* (obj . args) (apply #_copy (obj 's) args))
		 'string-copy            (lambda* (obj) (#_string-copy (obj 's)))
		 'string=?               (make-method #_string=? (lambda (obj) (obj 's)))
		 'string<?               (make-method #_string<? (lambda (obj) (obj 's)))
		 'string>?               (make-method #_string>? (lambda (obj) (obj 's)))
		 'string<=?              (make-method #_string<=? (lambda (obj) (obj 's)))
		 'string>=?              (make-method #_string>=? (lambda (obj) (obj 's)))
		 'string-ci=?            (make-method #_string-ci=? (lambda (obj) (obj 's)))
		 'string-ci<?            (make-method #_string-ci<? (lambda (obj) (obj 's)))
		 'string-ci>?            (make-method #_string-ci>? (lambda (obj) (obj 's)))
		 'string-ci<=?           (make-method #_string-ci<=? (lambda (obj) (obj 's)))
		 'string-ci>=?           (make-method #_string-ci>=? (lambda (obj) (obj 's)))
		 'string-downcase        (lambda (obj) (#_string-downcase (obj 's)))
		 'string-upcase          (lambda (obj) (#_string-upcase (obj 's)))
		 'substring              (lambda (obj . args) (apply #_substring (obj 's) args))
		 'format                 (make-method #_format (lambda (obj) (obj 's)))
		 'string->symbol         (lambda (obj) (#_string->symbol (obj 's)))
		 'string->number         (lambda* (obj (r 10)) (#_string->number (obj 's) r))
		 'gensym                 (lambda (obj) (#_gensym (obj 's)))
		 'make-keyword           (lambda (obj) (#_make-keyword (obj 's)))
		 'write-string           (lambda (obj . args) (apply #_write-string (obj 's) args))
		 'open-input-string      (lambda (obj) (#_open-input-string (obj 's)))
		 'call-with-input-string (lambda (obj f) (#_call-with-input-string (obj 's) f))
		 'with-input-from-string (lambda (obj f) (#_with-input-from-string (obj 's) f))
		 'directory?             (lambda (obj) (#_directory? (obj 's)))
		 'file-exists?           (lambda (obj) (#_file-exists? (obj 's)))
		 'getenv                 (lambda (obj) (#_getenv (obj 's)))
		 'delete-file            (lambda (obj) (#_delete-file (obj 's)))
		 'system                 (lambda* (obj cap) (#_system (obj 's) cap))
		 '->bytevector           (lambda (obj) (#_->bytevector (obj 's))) ; this is in-place! 
		 'load                   (lambda* (obj (e (curlet))) (#_load (obj 's) e))
		 'eval-string            (lambda* (obj (e (curlet))) (#_eval-string (obj 's) e))
		 'string-position        (make-method #_string-position (lambda (obj) (obj 's)))))))

    (define* (make-mock-string len (init #\null))
      (openlet (sublet mock-string-class 
		 's (#_make-string len init)
		 'object->string (lambda* (obj (w #t)) (#_object->string (obj 's) w)))))

    (define (mock-string . args)
      (let ((v (make-mock-string 0)))
	(set! (v 's) (apply #_string args))
	v))

    (define (mock-string? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-string-class)))

    (curlet)))

#|
;; string that is always the current time of day
(require libc.scm)

(define time-string
  (let ((daytime (lambda args
		   (with-let *libc*
		     (let ((timestr (make-string 64))) 
		       (let ((len (strftime timestr 64 "%a %d-%b-%Y %H:%M %Z"
					    (localtime 
					     (time.make (time (c-pointer 0)))))))
			 (substring timestr 0 len)))))))
    (openlet
     (sublet (*mock-string* 'mock-string-class)
       'let-ref-fallback daytime
       'object->string daytime))))

;; similarly ("JIT data"):
(define ? (openlet 
	   (inlet 'object->string (lambda (obj . args) 
				    (apply #_object->string (owlet) args)))))

|#


;;; --------------------------------------------------------------------------------

(define *mock-character*
  (let ((mock-character-class
	 (openlet
	  (inlet 'char?              (lambda (obj) #t)
		 'char-upcase        (lambda (obj) (#_char-upcase (obj 'c)))
		 'char-downcase      (lambda (obj) (#_char-downcase (obj 'c)))
		 'char->integer      (lambda (obj) (#_char->integer (obj 'c)))
		 'char-upper-case?   (lambda (obj) (#_char-upper-case? (obj 'c)))
		 'char-lower-case?   (lambda (obj) (#_char-lower-case? (obj 'c)))
		 'char-alphabetic?   (lambda (obj) (#_char-alphabetic? (obj 'c)))
		 'char-numeric?      (lambda (obj) (#_char-numeric? (obj 'c)))
		 'char-whitespace?   (lambda (obj) (#_char-whitespace? (obj 'c)))
		 'char=?             (make-method #_char=? (lambda (obj) (obj 'c)))
		 'char<?             (make-method #_char<? (lambda (obj) (obj 'c)))
		 'char>?             (make-method #_char>? (lambda (obj) (obj 'c)))
		 'char<=?            (make-method #_char<=? (lambda (obj) (obj 'c)))
		 'char>=?            (make-method #_char>=? (lambda (obj) (obj 'c)))
		 'char-ci=?          (make-method #_char-ci=? (lambda (obj) (obj 'c)))
		 'char-ci<?          (make-method #_char-ci<? (lambda (obj) (obj 'c)))
		 'char-ci>?          (make-method #_char-ci>? (lambda (obj) (obj 'c)))
		 'char-ci<=?         (make-method #_char-ci<=? (lambda (obj) (obj 'c)))
		 'char-ci>=?         (make-method #_char-ci>=? (lambda (obj) (obj 'c)))
		 'char-position      (lambda (obj str) (#_char-position (obj 'c) str))
		 'write-char         (lambda (obj . args) (apply #_write-char (obj 'c) args))
		 'string             (make-method #_string (lambda (obj) (obj 'c)))
		 'object->string     (lambda (obj . args) "#<mock-character-class>")
		 'arity              (lambda (obj) (#_arity (obj 'c)))
		 'copy               (lambda (obj . args) (obj 'c))))))

    (define (mock-char c) 
      (openlet
       (sublet (*mock-character* 'mock-character-class)
	 'c c
	 'object->string (lambda (obj . args) (apply #_object->string (obj 'c) args)))))
    
    (define (mock-char? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-character-class)))

    (curlet)))


;;; --------------------------------------------------------------------------------

(define *mock-number*
  (let ((mock-number-class
	 (openlet
	  (inlet 'number?          (lambda (obj) #t)

		 ;; all the indices and such -- how many of these can be methodized?
		 ;;   if all are, make-method needs indefinite extension
		 ;;   this also affects others above -- char case of string-set! for example
		 ;; these are (nearly) all fake -- need actual code and tests and examples ...
		 
		 'class-name       'mock-number
		 'object->string   (lambda (obj . args) "#<mock-number-class>")
		 'arity            (lambda (obj) (#_arity (obj 'x)))
		 'copy             (lambda (obj) (obj 'x))
		 'real-part        (lambda (obj) (#_real-part (obj 'x)))
		 'imag-part        (lambda (obj) (#_imag-part (obj 'x)))
		 'numerator        (lambda (obj) (#_numerator (obj 'x)))
		 'denominator      (lambda (obj) (#_denominator (obj 'x)))
		 'even?            (lambda (obj) (#_even? (obj 'x)))
		 'odd?             (lambda (obj) (#_odd? (obj 'x)))
		 'zero?            (lambda (obj) (#_zero? (obj 'x)))
		 'positive?        (lambda (obj) (#_positive? (obj 'x)))
		 'negative?        (lambda (obj) (#_negative? (obj 'x)))
		 'infinite?        (lambda (obj) (#_infinite? (obj 'x)))
		 'nan?             (lambda (obj) (#_nan? (obj 'x)))
		 'make-polar       (make-method #_make-polar (lambda (obj) (obj 'x)))
		 'make-rectangular (make-method #_make-rectangular (lambda (obj) (obj 'x)))
		 'magnitude        (lambda (obj) (#_magnitude (obj 'x)))
		 'angle            (lambda (obj) (#_angle (obj 'x)))
		 'rationalize      (lambda (obj . args) (apply #_rationalize (obj 'x) args))
		 'abs              (lambda (obj) (#_abs (obj 'x)))
		 'exp              (lambda (obj) (#_exp (obj 'x)))
		 'log              (lambda (obj . args) (apply #_log (obj 'x) args))
		 'sin              (lambda (obj) (#_sin (obj 'x)))
		 'cos              (lambda (obj) (#_cos (obj 'x)))
		 'tan              (lambda (obj) (#_tan (obj 'x)))
		 'asin             (lambda (obj) (#_asin (obj 'x)))
		 'acos             (lambda (obj) (#_acos (obj 'x)))
		 'atan             (lambda (obj . args) (apply #_atan (obj 'x) args))
		 'sinh             (lambda (obj) (#_sinh (obj 'x)))
		 'cosh             (lambda (obj) (#_cosh (obj 'x)))
		 'tanh             (lambda (obj) (#_tanh (obj 'x)))
		 'asinh            (lambda (obj) (#_asinh (obj 'x)))
		 'acosh            (lambda (obj) (#_acosh (obj 'x)))
		 'atanh            (lambda (obj) (#_atanh (obj 'x)))
		 'sqrt             (lambda (obj) (#_sqrt (obj 'x)))
		 'expt             (make-method #_expt (lambda (obj) (obj 'x)))
		 'floor            (lambda (obj) (#_floor (obj 'x)))
		 'ceiling          (lambda (obj) (#_ceiling (obj 'x)))
		 'truncate         (lambda (obj) (#_truncate (obj 'x)))
		 'round            (lambda (obj) (#_round (obj 'x)))
		 'inexact->exact   (lambda (obj) (#_inexact->exact (obj 'x)))
		 'exact->inexact   (lambda (obj) (#_exact->inexact (obj 'x)))
		 'integer-length   (lambda (obj) (#_integer-length (obj 'x)))
		 'integer-decode-float (lambda (obj) (#_integer-decode-float (obj 'x)))
		 'number?          (lambda (obj) (#_number? (obj 'x)))
		 'integer?         (lambda (obj) (#_integer? (obj 'x)))
		 'real?            (lambda (obj) (#_real? (obj 'x)))
		 'complex?         (lambda (obj) (#_complex? (obj 'x)))
		 'rational?        (lambda (obj) (#_rational? (obj 'x)))
		 'exact?           (lambda (obj) (#_exact? (obj 'x)))
		 'inexact?         (lambda (obj) (#_inexact? (obj 'x)))
		 'ash              (make-method #_ash (lambda (obj) (obj 'x)))
		 'logbit?          (make-method #_logbit? (lambda (obj) (obj 'x)))
		 'number->string   (make-method #_number->string (lambda (obj) (obj 'x)))
		 'random           (make-method #_random (lambda (obj) (obj 'x)))
		 'quotient         (make-method #_quotient (lambda (obj) (obj 'x)))
		 'remainder        (make-method #_remainder (lambda (obj) (obj 'x)))
		 'modulo           (make-method #_modulo (lambda (obj) (obj 'x)))
		 'lognot           (lambda (obj) (#_lognot (obj 'x)))
		 'logior           (make-method #_logior (lambda (obj) (obj 'x)))
		 'logxor           (make-method #_logxor (lambda (obj) (obj 'x)))
		 'logand           (make-method #_logand (lambda (obj) (obj 'x)))
		 'lcm              (make-method #_lcm (lambda (obj) (obj 'x)))
		 'gcd              (make-method #_gcd (lambda (obj) (obj 'x)))
		 '+                (make-method #_+ (lambda (obj) (obj 'x)))
		 '-                (make-method #_- (lambda (obj) (obj 'x)))
		 '*                (make-method #_* (lambda (obj) (obj 'x)))
		 '/                (make-method #_/ (lambda (obj) (obj 'x)))
		 'max              (make-method #_max (lambda (obj) (obj 'x)))
		 'min              (make-method #_min (lambda (obj) (obj 'x)))
		 '=                (make-method #_= (lambda (obj) (obj 'x)))
		 '<                (make-method #_< (lambda (obj) (obj 'x)))
		 '>                (make-method #_> (lambda (obj) (obj 'x)))
		 '<=               (make-method #_<= (lambda (obj) (obj 'x)))
		 '>=               (make-method #_>= (lambda (obj) (obj 'x)))
		 ))))

    (define (mock-number x)
      (openlet
       (sublet (*mock-number* 'mock-number-class)
	 'x x
	 'object->string (lambda (obj . args) (apply #_object->string (obj 'x) args)))))
    
    (define (mock-number? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-number-class)))

    (curlet)))


#|
(define fuzzy-number
  (let ((fuzz (lambda (fx)
		(#_* fx (#_- 1.05 (#_random .1))))))
    (lambda (fx)
      (openlet 
       (sublet 
	   (*mock-number* 'mock-number-class)
	 'let-ref-fallback (lambda (obj sym) (fuzz fx))
	 'object->string (lambda (obj . args) (#_number->string (fuzz fx))))))))
|#



;;; --------------------------------------------------------------------------------

(define *mock-pair*
  (let ((mock-pair-class
	 (openlet
	  (inlet 'pair?            (lambda (obj) #t)
		 'pair-line-number (lambda (obj) (#_pair-line-number (obj 'p)))
		 'list->string     (lambda (obj) (#_list->string (obj 'p)))
		 'object->string   (lambda (obj . arg) "#<mock-pair-class>")
		 'list?            (lambda (obj) (#_list? (obj 'p)))
		 'car              (lambda (obj) (#_car (obj 'p)))
		 'cdr              (lambda (obj) (#_cdr (obj 'p)))
		 'set-car!         (lambda (obj val) (#_set-car! (obj 'p) val))
		 'set-cdr!         (lambda (obj val) (#_set-cdr! (obj 'p) val))
		 'caar             (lambda (obj) (#_caar (obj 'p)))
		 'cadr             (lambda (obj) (#_cadr (obj 'p)))
		 'cdar             (lambda (obj) (#_cdar (obj 'p)))
		 'cddr             (lambda (obj) (#_cddr (obj 'p)))
		 'caaar            (lambda (obj) (#_caaar (obj 'p)))
		 'caadr            (lambda (obj) (#_caadr (obj 'p)))
		 'cadar            (lambda (obj) (#_cadar (obj 'p)))
		 'cdaar            (lambda (obj) (#_cdaar (obj 'p)))
		 'caddr            (lambda (obj) (#_caddr (obj 'p)))
		 'cdddr            (lambda (obj) (#_cdddr (obj 'p)))
		 'cdadr            (lambda (obj) (#_cdadr (obj 'p)))
		 'cddar            (lambda (obj) (#_cddar (obj 'p)))
		 'caaaar           (lambda (obj) (#_caaaar (obj 'p)))
		 'caaadr           (lambda (obj) (#_caaadr (obj 'p)))
		 'caadar           (lambda (obj) (#_caadar (obj 'p)))
		 'cadaar           (lambda (obj) (#_cadaar (obj 'p)))
		 'caaddr           (lambda (obj) (#_caaddr (obj 'p)))
		 'cadddr           (lambda (obj) (#_cadddr (obj 'p)))
		 'cadadr           (lambda (obj) (#_cadadr (obj 'p)))
		 'caddar           (lambda (obj) (#_caddar (obj 'p)))
		 'cdaaar           (lambda (obj) (#_cdaaar (obj 'p)))
		 'cdaadr           (lambda (obj) (#_cdaadr (obj 'p)))
		 'cdadar           (lambda (obj) (#_cdadar (obj 'p)))
		 'cddaar           (lambda (obj) (#_cddaar (obj 'p)))
		 'cdaddr           (lambda (obj) (#_cdaddr (obj 'p)))
		 'cddddr           (lambda (obj) (#_cddddr (obj 'p)))
		 'cddadr           (lambda (obj) (#_cddadr (obj 'p)))
		 'cdddar           (lambda (obj) (#_cdddar (obj 'p)))
		 'assq             (lambda (val obj) (#_assq val (obj 'p)))
		 'assv             (lambda (val obj) (#_assv val (obj 'p)))
		 'assoc            (lambda (val obj . args) (apply #_assoc val (obj 'p) args))
		 'memq             (lambda (val obj) (#_memq val (obj 'p)))
		 'memv             (lambda (val obj) (#_memv val (obj 'p)))
		 'member           (lambda (val obj . args) (apply #_member val (obj 'p) args))
		 'list-ref         (lambda (obj ind) (#_list-ref (obj 'p) ind))
		 'list-set!        (lambda (obj ind val) (#_list-set! (obj 'p) ind val))
		 'list-tail        (lambda (obj . args) (apply #_list-tail (obj 'p) args))
		 'length           (lambda (obj) (#_length (obj 'p)))
		 'arity            (lambda (obj) (#_arity (obj 'p)))
		 'copy             (lambda (obj . args) (apply #_copy (obj 'p) args))
		 'fill!            (lambda (obj val) (#_fill! (obj 'p) val))
		 'reverse          (lambda (obj) (#_reverse (obj 'p)))
		 'reverse!         (lambda (obj) (set! (obj 'p) (#_reverse (obj 'p))))
		 'sort!            (lambda (obj f) (#_sort! (obj 'p) f))
		 'list->vector     (lambda (obj) (#_list->vector (obj 'p)))
		 'for-each         (lambda (f obj) (#_for-each f (obj 'p)))
		 'map              (lambda (f obj) (#_map f (obj 'p)))
		 ))))

    (define (mock-pair p)
      (openlet
       (sublet (*mock-pair* 'mock-pair-class)
	 'p p
	 'object->string (lambda (obj . args) (apply #_object->string (obj 'p) args)))))
    
    (define (mock-pair? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-pair-class)))

    (curlet)))


;;; --------------------------------------------------------------------------------

(define *mock-symbol*
  (let ((mock-symbol-class
	 (openlet
	  (inlet 'symbol?               (lambda (obj) #t)
		 'gensym?               (lambda (obj) (#_gensym? (obj 's)))
		 'symbol->string        (lambda (obj) (#_symbol->string (obj 's)))
		 'symbol->value         (lambda (obj . args) (apply #_symbol->value (obj 's) args))
		 'symbol->dynamic-value (lambda (obj) (#_symbol->dynamic-value (obj 's)))
		 'symbol-access         (lambda (obj . args) (apply #_symbol-access (obj 's) args))
		 'provided?             (lambda (obj) (#_provided? (obj 's)))
		 'provide               (lambda (obj) (#_provide (obj 's)))
		 'defined?              (lambda (obj) (#_defined? (obj 's)))
		 'symbol->keyword       (lambda (obj) (#_symbol->keyword (obj 's)))
		 'keyword?              (lambda (obj) (#_keyword? (obj 's)))
		 'keyword->symbol       (lambda (obj) (#_keyword->symbol (obj 's)))
		 ))))

    (define (mock-symbol s)
      (openlet
       (sublet (*mock-symbol* 'mock-symbol-class)
	 's s
	 'object->string (lambda (obj . args) (apply #_object->string (obj 's) args)))))
    
    (define (mock-symbol? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-symbol-class)))

    (curlet)))


;;; --------------------------------------------------------------------------------
;;; mock-port mock-lambda

