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
	  (inlet 'local-set!         (lambda (obj i val) (#_vector-set! (obj 'value) i val))
		 'class-name         'mock-vector
		 'value              (lambda (obj) (obj 'value))
		 'vector?            (lambda (obj) #t)
		 'vector-ref         (lambda (obj i) (#_vector-ref (obj 'value) i))
		 'vector-set!        (lambda (obj i val) ((obj 'local-set!) obj i val) val)
		 'let-ref            (lambda (obj i) (#_vector-ref (obj 'value) i))           ; these are the implicit cases
		 'let-set!           (lambda (obj i val) ((obj 'local-set!) obj i val) val)
		 'length             (lambda (obj) (#_vector-length (obj 'value)))
		 'vector-length      (lambda (obj) (#_vector-length (obj 'value)))
		 'map                (lambda (f obj) (map f (obj 'value)))
		 'for-each           (lambda (f obj) (for-each f (obj 'value)))
		 'vector->list       (lambda (obj) (#_vector->list (obj 'value)))
		 'make-shared-vector (lambda* (obj dim (off 0)) (#_make-shared-vector (obj 'value) dim off))
		 'vector-append      (make-method #_vector-append (lambda (obj) (obj 'value)))
		 'vector-fill!       (lambda* (obj val (start 0) end) (#_vector-fill! (obj 'value) val start (or end (#_vector-length (obj 'value)))))
		 'fill!              (lambda (obj val) (#_fill! (obj 'value) val))
		 'reverse            (lambda (obj) (#_reverse (obj 'value)))
		 'sort!              (lambda (obj f) (#_sort! (obj 'value) f))
		 'arity              (lambda (obj) (#_arity (obj 'value)))
		 'object->string     (lambda* (obj (w #t)) "#<mock-vector-class>")
		 'vector-dimensions  (lambda (obj) (#_vector-dimensions (obj 'value)))
		 'copy               (lambda* (src dest . args)
				       ;; copy by itself does not make a new vector, but if no dest we
				       ;;   need a copy of src, so use coverlet/openlet to make sure
				       ;;   we aren't caught in an infinite recursion.
				       (if (and dest (not (let? dest)))
					   (apply copy (src 'value) dest args)
					   (let ((nobj (or dest 
							   (dynamic-wind
							       (lambda () (coverlet src))
							       (lambda () (openlet (copy src)))
							       (lambda () (openlet src))))))
					     (if dest
						 (apply copy (src 'value) (nobj 'value) args)
						 (set! (nobj 'value) (copy (src 'value))))
					     nobj)))))))

    (define* (make-mock-vector len (init #<unspecified>))
      (openlet (sublet mock-vector-class 
		 'value (#_make-vector len init)
		 'object->string (lambda* (obj (w #t)) (#_object->string (obj 'value) w)))))

    (define (mock-vector . args)
      (let ((v (make-mock-vector 0)))
	(set! (v 'value) (apply #_vector args))
	v))

    (define (mock-vector? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-vector-class)))

    (curlet)))

#|
;; vector that grows to accommodate vector-set!

(define stretchable-vector-class
  (let ((local-ref (lambda (obj index)
		     (if (>= index (length (obj 'value)))
			 (obj 'initial-element)
			 (#_vector-ref (obj 'value) index))))
	(local-set! (lambda (obj index val)
		      (if (>= index (length (obj 'value)))
			  (set! (obj 'value) (copy (obj 'value) (make-vector (+ index 8) (obj 'initial-element)))))
		      (#_vector-set! (obj 'value) index val))))
    (openlet
     (sublet (*mock-vector* 'mock-vector-class)
       'value (vector)
       'object->string (lambda* (obj (w #t)) (format #f "#<stretchable-vector: ~S>" (obj 'value)))
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
		 'class-name         'mock-hash-table
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
   (sublet (*mock-hash-table* 'mock-hash-table-class) ; ideally this would be a separate (not copied) gloomy-hash-table-class 
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
		 'class-name             'mock-string
		 'value                  (lambda (obj) (obj 'value))
		 'string-ref             (lambda (obj i) (#_string-ref (obj 'value) i))
		 'string-set!            (lambda (obj i val) (#_string-set! (obj 'value) i val))
		 'let-ref                (lambda (obj i) (#_string-ref (obj 'value) i))           ; these are the implicit cases
		 'let-set!               (lambda (obj i val) (string-set! (obj 'value) i val))
		 'length                 (lambda (obj) (#_string-length (obj 'value)))
		 'string-length          (lambda (obj) (#_string-length (obj 'value)))
		 'map                    (lambda (f obj) (map f (obj 'value)))
		 'for-each               (lambda (f obj) (for-each f (obj 'value)))
		 'string->list           (lambda (obj) (#_string->list (obj 'value)))
		 'string-append          (make-method #_string-append (lambda (obj) (obj 'value)))
		 'string-fill!           (lambda* (obj val (start 0) end) (#_string-fill! (obj 'value) val start (or end (#_string-length (obj 'value)))))
		 'fill!                  (lambda (obj val) (#_fill! (obj 'value) val))
		 'reverse                (lambda (obj) (#_reverse (obj 'value)))
		 'object->string         (lambda* (obj (w #t)) "#<mock-string-class>")
		 'arity                  (lambda (obj) (#_arity (obj 'value)))
		 'copy                   (lambda* (obj . args) (apply #_copy (obj 'value) args))
		 'string-copy            (lambda* (obj) (#_string-copy (obj 'value)))
		 'string=?               (make-method #_string=? (lambda (obj) (obj 'value)))
		 'string<?               (make-method #_string<? (lambda (obj) (obj 'value)))
		 'string>?               (make-method #_string>? (lambda (obj) (obj 'value)))
		 'string<=?              (make-method #_string<=? (lambda (obj) (obj 'value)))
		 'string>=?              (make-method #_string>=? (lambda (obj) (obj 'value)))
		 'string-ci=?            (make-method #_string-ci=? (lambda (obj) (obj 'value)))
		 'string-ci<?            (make-method #_string-ci<? (lambda (obj) (obj 'value)))
		 'string-ci>?            (make-method #_string-ci>? (lambda (obj) (obj 'value)))
		 'string-ci<=?           (make-method #_string-ci<=? (lambda (obj) (obj 'value)))
		 'string-ci>=?           (make-method #_string-ci>=? (lambda (obj) (obj 'value)))
		 'string-downcase        (lambda (obj) (#_string-downcase (obj 'value)))
		 'string-upcase          (lambda (obj) (#_string-upcase (obj 'value)))
		 'substring              (lambda (obj . args) (apply #_substring (obj 'value) args))
		 'format                 (make-method #_format (lambda (obj) (obj 'value)))
		 'string->symbol         (lambda (obj) (#_string->symbol (obj 'value)))
		 'string->number         (lambda* (obj (r 10)) (#_string->number (obj 'value) r))
		 'gensym                 (lambda (obj) (#_gensym (obj 'value)))
		 'make-keyword           (lambda (obj) (#_make-keyword (obj 'value)))
		 'write-string           (lambda (obj . args) (apply #_write-string (obj 'value) args))
		 'open-input-string      (lambda (obj) (#_open-input-string (obj 'value)))
		 'call-with-input-string (lambda (obj f) (#_call-with-input-string (obj 'value) f))
		 'with-input-from-string (lambda (obj f) (#_with-input-from-string (obj 'value) f))
		 'directory?             (lambda (obj) (#_directory? (obj 'value)))
		 'file-exists?           (lambda (obj) (#_file-exists? (obj 'value)))
		 'getenv                 (lambda (obj) (#_getenv (obj 'value)))
		 'delete-file            (lambda (obj) (#_delete-file (obj 'value)))
		 'system                 (lambda* (obj cap) (#_system (obj 'value) cap))
		 '->bytevector           (lambda (obj) (#_->bytevector (obj 'value))) ; this is in-place! 
		 'load                   (lambda* (obj (e (curlet))) (#_load (obj 'value) e))
		 'eval-string            (lambda* (obj (e (curlet))) (#_eval-string (obj 'value) e))
		 'string-position        (make-method #_string-position (lambda (obj) (obj 'value)))))))

    (define* (make-mock-string len (init #\null))
      (openlet (sublet mock-string-class 
		 'value (#_make-string len init)
		 'object->string (lambda* (obj (w #t)) (#_object->string (obj 'value) w)))))

    (define (mock-string . args)
      (let ((v (make-mock-string 0)))
	(set! (v 'value) (apply #_string args))
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
		 'class-name         'mock-character
		 'value              (lambda (obj) (obj 'value))
		 'char-upcase        (lambda (obj) (#_char-upcase (obj 'value)))
		 'char-downcase      (lambda (obj) (#_char-downcase (obj 'value)))
		 'char->integer      (lambda (obj) (#_char->integer (obj 'value)))
		 'char-upper-case?   (lambda (obj) (#_char-upper-case? (obj 'value)))
		 'char-lower-case?   (lambda (obj) (#_char-lower-case? (obj 'value)))
		 'char-alphabetic?   (lambda (obj) (#_char-alphabetic? (obj 'value)))
		 'char-numeric?      (lambda (obj) (#_char-numeric? (obj 'value)))
		 'char-whitespace?   (lambda (obj) (#_char-whitespace? (obj 'value)))
		 'char=?             (make-method #_char=? (lambda (obj) (obj 'value)))
		 'char<?             (make-method #_char<? (lambda (obj) (obj 'value)))
		 'char>?             (make-method #_char>? (lambda (obj) (obj 'value)))
		 'char<=?            (make-method #_char<=? (lambda (obj) (obj 'value)))
		 'char>=?            (make-method #_char>=? (lambda (obj) (obj 'value)))
		 'char-ci=?          (make-method #_char-ci=? (lambda (obj) (obj 'value)))
		 'char-ci<?          (make-method #_char-ci<? (lambda (obj) (obj 'value)))
		 'char-ci>?          (make-method #_char-ci>? (lambda (obj) (obj 'value)))
		 'char-ci<=?         (make-method #_char-ci<=? (lambda (obj) (obj 'value)))
		 'char-ci>=?         (make-method #_char-ci>=? (lambda (obj) (obj 'value)))
		 'char-position      (lambda (obj str) (#_char-position (obj 'value) str))
		 'write-char         (lambda (obj . args) (apply #_write-char (obj 'value) args))
		 'string             (make-method #_string (lambda (obj) (obj 'value)))
		 'object->string     (lambda (obj . args) "#<mock-character-class>")
		 'arity              (lambda (obj) (#_arity (obj 'value)))
		 'copy               (lambda (obj . args) (obj 'value))))))

    (define (mock-char c) 
      (openlet
       (sublet (*mock-character* 'mock-character-class)
	 'value c
	 'object->string (lambda (obj . args) (apply #_object->string (obj 'value) args)))))
    
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
		 'value            (lambda (obj) (obj 'value))
		 'object->string   (lambda (obj . args) "#<mock-number-class>")
		 'arity            (lambda (obj) (#_arity (obj 'value)))
		 'copy             (lambda (obj) (obj 'value))
		 'real-part        (lambda (obj) (#_real-part (obj 'value)))
		 'imag-part        (lambda (obj) (#_imag-part (obj 'value)))
		 'numerator        (lambda (obj) (#_numerator (obj 'value)))
		 'denominator      (lambda (obj) (#_denominator (obj 'value)))
		 'even?            (lambda (obj) (#_even? (obj 'value)))
		 'odd?             (lambda (obj) (#_odd? (obj 'value)))
		 'zero?            (lambda (obj) (#_zero? (obj 'value)))
		 'positive?        (lambda (obj) (#_positive? (obj 'value)))
		 'negative?        (lambda (obj) (#_negative? (obj 'value)))
		 'infinite?        (lambda (obj) (#_infinite? (obj 'value)))
		 'nan?             (lambda (obj) (#_nan? (obj 'value)))
		 'make-polar       (make-method #_make-polar (lambda (obj) (obj 'value)))
		 'make-rectangular (make-method #_make-rectangular (lambda (obj) (obj 'value)))
		 'magnitude        (lambda (obj) (#_magnitude (obj 'value)))
		 'angle            (lambda (obj) (#_angle (obj 'value)))
		 'rationalize      (lambda (obj . args) (apply #_rationalize (obj 'value) args))
		 'abs              (lambda (obj) (#_abs (obj 'value)))
		 'exp              (lambda (obj) (#_exp (obj 'value)))
		 'log              (lambda (obj . args) (apply #_log (obj 'value) args))
		 'sin              (lambda (obj) (#_sin (obj 'value)))
		 'cos              (lambda (obj) (#_cos (obj 'value)))
		 'tan              (lambda (obj) (#_tan (obj 'value)))
		 'asin             (lambda (obj) (#_asin (obj 'value)))
		 'acos             (lambda (obj) (#_acos (obj 'value)))
		 'atan             (lambda (obj . args) (apply #_atan (obj 'value) args))
		 'sinh             (lambda (obj) (#_sinh (obj 'value)))
		 'cosh             (lambda (obj) (#_cosh (obj 'value)))
		 'tanh             (lambda (obj) (#_tanh (obj 'value)))
		 'asinh            (lambda (obj) (#_asinh (obj 'value)))
		 'acosh            (lambda (obj) (#_acosh (obj 'value)))
		 'atanh            (lambda (obj) (#_atanh (obj 'value)))
		 'sqrt             (lambda (obj) (#_sqrt (obj 'value)))
		 'expt             (make-method #_expt (lambda (obj) (obj 'value)))
		 'floor            (lambda (obj) (#_floor (obj 'value)))
		 'ceiling          (lambda (obj) (#_ceiling (obj 'value)))
		 'truncate         (lambda (obj) (#_truncate (obj 'value)))
		 'round            (lambda (obj) (#_round (obj 'value)))
		 'inexact->exact   (lambda (obj) (#_inexact->exact (obj 'value)))
		 'exact->inexact   (lambda (obj) (#_exact->inexact (obj 'value)))
		 'integer-length   (lambda (obj) (#_integer-length (obj 'value)))
		 'integer-decode-float (lambda (obj) (#_integer-decode-float (obj 'value)))
		 'number?          (lambda (obj) (#_number? (obj 'value)))
		 'integer?         (lambda (obj) (#_integer? (obj 'value)))
		 'real?            (lambda (obj) (#_real? (obj 'value)))
		 'complex?         (lambda (obj) (#_complex? (obj 'value)))
		 'rational?        (lambda (obj) (#_rational? (obj 'value)))
		 'exact?           (lambda (obj) (#_exact? (obj 'value)))
		 'inexact?         (lambda (obj) (#_inexact? (obj 'value)))
		 'ash              (make-method #_ash (lambda (obj) (obj 'value)))
		 'logbit?          (make-method #_logbit? (lambda (obj) (obj 'value)))
		 'number->string   (make-method #_number->string (lambda (obj) (obj 'value)))
		 'random           (make-method #_random (lambda (obj) (obj 'value)))
		 'quotient         (make-method #_quotient (lambda (obj) (obj 'value)))
		 'remainder        (make-method #_remainder (lambda (obj) (obj 'value)))
		 'modulo           (make-method #_modulo (lambda (obj) (obj 'value)))
		 'lognot           (lambda (obj) (#_lognot (obj 'value)))
		 'logior           (make-method #_logior (lambda (obj) (obj 'value)))
		 'logxor           (make-method #_logxor (lambda (obj) (obj 'value)))
		 'logand           (make-method #_logand (lambda (obj) (obj 'value)))
		 'lcm              (make-method #_lcm (lambda (obj) (obj 'value)))
		 'gcd              (make-method #_gcd (lambda (obj) (obj 'value)))
		 '+                (make-method #_+ (lambda (obj) (obj 'value)))
		 '-                (make-method #_- (lambda (obj) (obj 'value)))
		 '*                (make-method #_* (lambda (obj) (obj 'value)))
		 '/                (make-method #_/ (lambda (obj) (obj 'value)))
		 'max              (make-method #_max (lambda (obj) (obj 'value)))
		 'min              (make-method #_min (lambda (obj) (obj 'value)))
		 '=                (make-method #_= (lambda (obj) (obj 'value)))
		 '<                (make-method #_< (lambda (obj) (obj 'value)))
		 '>                (make-method #_> (lambda (obj) (obj 'value)))
		 '<=               (make-method #_<= (lambda (obj) (obj 'value)))
		 '>=               (make-method #_>= (lambda (obj) (obj 'value)))
		 ))))

    (define (mock-number x)
      (openlet
       (sublet (*mock-number* 'mock-number-class)
	 'value x
	 'object->string (lambda (obj . args) (apply #_object->string (obj 'value) args)))))
    
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
		 'class-name       'mock-pair
		 'value            (lambda (obj) (obj 'value))
		 'pair-line-number (lambda (obj) (#_pair-line-number (obj 'value)))
		 'list->string     (lambda (obj) (#_list->string (obj 'value)))
		 'object->string   (lambda (obj . arg) "#<mock-pair-class>")
		 'list?            (lambda (obj) (#_list? (obj 'value)))
		 'car              (lambda (obj) (#_car (obj 'value)))
		 'cdr              (lambda (obj) (#_cdr (obj 'value)))
		 'set-car!         (lambda (obj val) (#_set-car! (obj 'value) val))
		 'set-cdr!         (lambda (obj val) (#_set-cdr! (obj 'value) val))
		 'caar             (lambda (obj) (#_caar (obj 'value)))
		 'cadr             (lambda (obj) (#_cadr (obj 'value)))
		 'cdar             (lambda (obj) (#_cdar (obj 'value)))
		 'cddr             (lambda (obj) (#_cddr (obj 'value)))
		 'caaar            (lambda (obj) (#_caaar (obj 'value)))
		 'caadr            (lambda (obj) (#_caadr (obj 'value)))
		 'cadar            (lambda (obj) (#_cadar (obj 'value)))
		 'cdaar            (lambda (obj) (#_cdaar (obj 'value)))
		 'caddr            (lambda (obj) (#_caddr (obj 'value)))
		 'cdddr            (lambda (obj) (#_cdddr (obj 'value)))
		 'cdadr            (lambda (obj) (#_cdadr (obj 'value)))
		 'cddar            (lambda (obj) (#_cddar (obj 'value)))
		 'caaaar           (lambda (obj) (#_caaaar (obj 'value)))
		 'caaadr           (lambda (obj) (#_caaadr (obj 'value)))
		 'caadar           (lambda (obj) (#_caadar (obj 'value)))
		 'cadaar           (lambda (obj) (#_cadaar (obj 'value)))
		 'caaddr           (lambda (obj) (#_caaddr (obj 'value)))
		 'cadddr           (lambda (obj) (#_cadddr (obj 'value)))
		 'cadadr           (lambda (obj) (#_cadadr (obj 'value)))
		 'caddar           (lambda (obj) (#_caddar (obj 'value)))
		 'cdaaar           (lambda (obj) (#_cdaaar (obj 'value)))
		 'cdaadr           (lambda (obj) (#_cdaadr (obj 'value)))
		 'cdadar           (lambda (obj) (#_cdadar (obj 'value)))
		 'cddaar           (lambda (obj) (#_cddaar (obj 'value)))
		 'cdaddr           (lambda (obj) (#_cdaddr (obj 'value)))
		 'cddddr           (lambda (obj) (#_cddddr (obj 'value)))
		 'cddadr           (lambda (obj) (#_cddadr (obj 'value)))
		 'cdddar           (lambda (obj) (#_cdddar (obj 'value)))
		 'assq             (lambda (val obj) (#_assq val (obj 'value)))
		 'assv             (lambda (val obj) (#_assv val (obj 'value)))
		 'assoc            (lambda (val obj . args) (apply #_assoc val (obj 'value) args))
		 'memq             (lambda (val obj) (#_memq val (obj 'value)))
		 'memv             (lambda (val obj) (#_memv val (obj 'value)))
		 'member           (lambda (val obj . args) (apply #_member val (obj 'value) args))
		 'list-ref         (lambda (obj ind) (#_list-ref (obj 'value) ind))
		 'list-set!        (lambda (obj ind val) (#_list-set! (obj 'value) ind val))
		 'let-ref          (lambda (obj ind) (#_list-ref (obj 'value) ind))
		 'let-set!         (lambda (obj ind val) (#_list-set! (obj 'value) ind val))
		 'list-tail        (lambda (obj . args) (apply #_list-tail (obj 'value) args))
		 'length           (lambda (obj) (#_length (obj 'value)))
		 'arity            (lambda (obj) (#_arity (obj 'value)))
		 'copy             (lambda (obj . args) (apply #_copy (obj 'value) args))
		 'fill!            (lambda (obj val) (#_fill! (obj 'value) val))
		 'reverse          (lambda (obj) (#_reverse (obj 'value)))
		 'reverse!         (lambda (obj) (set! (obj 'value) (#_reverse (obj 'value))))
		 'sort!            (lambda (obj f) (#_sort! (obj 'value) f))
		 'list->vector     (lambda (obj) (#_list->vector (obj 'value)))
		 'for-each         (lambda (f obj) (#_for-each f (obj 'value)))
		 'map              (lambda (f obj) (#_map f (obj 'value)))
		 ))))

    (define (mock-pair p)
      (openlet
       (sublet (*mock-pair* 'mock-pair-class)
	 'value p
	 'object->string (lambda (obj . args) (apply #_object->string (obj 'value) args)))))
    
    (define (mock-pair? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-pair-class)))

    (curlet)))

;; append is very strange -- not sure how to handle it

#|
(let ((immutable-list-class 
       (sublet (*mock-pair* 'mock-pair-class)
	 'object->string   (lambda (obj . args) 
			     (apply #_object->string (obj 'value) args))
	 'let-set!         (lambda (obj i val) 
			     (set! (obj 'value) (append (copy (obj 'value) (make-list (+ i 1))) (list-tail (obj 'value) (+ i 1))))
			     (list-set! (obj 'value) i val))
	 'list-set!        (lambda (obj i val) 
			     (set! (obj 'value) (append (copy (obj 'value) (make-list (+ i 1))) (list-tail (obj 'value) (+ i 1))))
			     (list-set! (obj 'value) i val))
	 'set-car!         (lambda (obj val) 
			     (set! (obj 'value) (cons val (cdr (obj 'value)))))
	 'set-cdr!         (lambda (obj val) 
			     (set! (obj 'value) (cons (car (obj 'value)) val)))
	 'fill!            (lambda (obj val) 
			     (set! (obj 'value) (fill! (copy (obj 'value)) val)))
	 'reverse!         (lambda (obj) 
			     (set! (obj 'value) (reverse (obj 'value))))
	 'sort!            (lambda (obj func) 
			     (set! (obj 'value) (sort! (copy (obj 'value)) func))))))

    (define (immutable-list lst)
      (openlet 
       (sublet immutable-list-class
	 'value lst))))
|#


;;; --------------------------------------------------------------------------------

(define *mock-symbol*
  (let ((mock-symbol-class
	 (openlet
	  (inlet 'symbol?               (lambda (obj) #t)
		 'class-name            'mock-symbol
		 'value                 (lambda (obj) (obj 'value))
		 'gensym?               (lambda (obj) (#_gensym? (obj 'value)))
		 'symbol->string        (lambda (obj) (#_symbol->string (obj 'value)))
		 'symbol->value         (lambda (obj . args) (apply #_symbol->value (obj 'value) args))
		 'symbol->dynamic-value (lambda (obj) (#_symbol->dynamic-value (obj 'value)))
		 'symbol-access         (lambda (obj . args) (apply #_symbol-access (obj 'value) args))
		 'provided?             (lambda (obj) (#_provided? (obj 'value)))
		 'provide               (lambda (obj) (#_provide (obj 'value)))
		 'defined?              (lambda (obj) (#_defined? (obj 'value)))
		 'symbol->keyword       (lambda (obj) (#_symbol->keyword (obj 'value)))
		 'keyword?              (lambda (obj) (#_keyword? (obj 'value)))
		 'keyword->symbol       (lambda (obj) (#_keyword->symbol (obj 'value)))
		 ))))

    (define (mock-symbol s)
      (openlet
       (sublet (*mock-symbol* 'mock-symbol-class)
	 'value s
	 'object->string (lambda (obj . args) (apply #_object->string (obj 'value) args)))))
    
    (define (mock-symbol? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-symbol-class)))

    (curlet)))


;;; --------------------------------------------------------------------------------

(define *mock-port*
  (let ((mock-port-class
	 (openlet
	  (inlet 'port?               (lambda (obj) #t)
		 'class-name          'mock-port
		 'value               (lambda (obj) (obj 'value))
		 'format              (lambda (obj . args) (apply #_format (obj 'value) args))
		 'object->string      (lambda (obj . args) (apply #_object->string (obj 'value) args))
		 'close-input-port    (lambda (obj) (#_close-input-port (obj 'value)))
		 'close-output-port   (lambda (obj) (#_close-output-port (obj 'value)))
		 'flush-output-port   (lambda (obj) (#_flush-output-port (obj 'value)))
		 'get-output-string   (lambda (obj) (#_get-output-string (obj 'value)))
		 'newline             (lambda (obj) (#_newline (obj 'value)))
		 'write               (lambda (x obj) (#_write x (obj 'value)))
		 'display             (lambda (x obj) (#_display x (obj 'value)))
		 'read-char           (lambda (obj) (#_read-char (obj 'value)))
		 'peek-char           (lambda (obj) (#_peek-char (obj 'value)))
		 'write-char          (lambda (c obj) (#_write-char c (obj 'value)))
		 'write-string        (lambda (s obj . args) (apply #_write-string s (obj 'value) args))
		 'read-byte           (lambda (obj) (#_read-byte (obj 'value)))
		 'write-byte          (lambda (b obj) (#_write-byte b (obj 'value)))
		 'read-line           (lambda (obj . args) (apply #_read-line (obj 'value) args))
		 'read-string         (lambda (k obj) (#_read-string k (obj 'value)))
		 'read                (lambda (obj) (#_read (obj 'value)))
		 'input-port?         (lambda (obj) (#_input-port? (obj 'value)))
		 'output-port?        (lambda (obj) (#_output-port? (obj 'value)))
		 'port-closed?        (lambda (obj) (#_port-closed? (obj 'value)))
		 'char-ready?         (lambda (obj) (#_char-ready? (obj 'value)))
		 'port-line-number    (lambda (obj) (#_port-line-number (obj 'value)))
		 'port-filename       (lambda (obj) (#_port-filename (obj 'value)))
		 ))))

    (define (mock-port port)
      (openlet
       (sublet (*mock-port* 'mock-port-class)
	 'value port
	 'object->string (lambda (obj . args) (apply #_object->string (obj 'value) args)))))
    
    (define (mock-port? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-port-class)))

    (curlet)))


