;;; read/write binary (sound) files
;;;
;;; names are read|write b|l int|float n, 
;;;   so read-bint32 reads the next 4 bytes from the current input port,
;;;   interpreting them as a big-endian 32-bit integer


;;; -------- strings (0-terminated)

(define (read-string)
  (let ((chars '()))
    (do ((c (read-byte) (read-byte)))
	((or (= c 0)
	     (eof-object? c))
	 (apply string (reverse chars)))
      (set! chars (cons (integer->char c) chars)))))

(define (write-string str)
  (for-each write-char str) ; or maybe (lambda (c) (write-byte (char->integer c)))
  (write-byte 0))


;;; -------- 16-bit ints

(define (read-bint16)
  (let ((int (+ (ash (read-byte) 8) (read-byte))))
    (if (> int 32767)
	(- int 65536)
	int)))

(define (read-lint16)
  (let ((int (+ (read-byte) (ash (read-byte) 8))))
    (if (> int 32767)
	(- int 65536)
	int)))

(define (write-bint16 int)
  (write-byte (logand (ash int -8) #xff))
  (write-byte (logand int #xff)))

(define (write-lint16 int)
  (write-byte (logand int #xff))
  (write-byte (logand (ash int -8) #xff)))


;;; -------- 32-bit ints

(define (read-bint32)
  (let ((int (+ (ash (read-byte) 24) (ash (read-byte) 16) (ash (read-byte) 8) (read-byte))))
    (if (> int 2147483647)
	(- int 4294967296)
      int)))

(define (read-lint32)
  (let ((int (+ (read-byte) (ash (read-byte) 8) (ash (read-byte) 16) (ash (read-byte) 24))))
    (if (> int 2147483647)
	(- int 4294967296)
      int)))

(define (write-bint32 int)
  (write-byte (logand (ash int -24) #xff))
  (write-byte (logand (ash int -16) #xff))
  (write-byte (logand (ash int -8) #xff))
  (write-byte (logand int #xff)))

(define (write-lint32 int)
  (write-byte (logand int #xff))
  (write-byte (logand (ash int -8) #xff))
  (write-byte (logand (ash int -16) #xff))
  (write-byte (logand (ash int -24) #xff)))


;;; -------- 64-bit ints

(define (read-bint64)
  (let ((int 0))
    (do ((i 56 (- i 8)))
	((< i 0) int)
      (set! int (logior int (ash (read-byte) i))))))

(define (read-lint64)
  (let ((int 0))
    (do ((i 0 (+ i 8)))
	((= i 64) int)
      (set! int (logior int (ash (read-byte) i))))))
    
(define (write-bint64 int)
  (do ((i 56 (- i 8)))
      ((< i 0))
    (write-byte (logand (ash int (- i)) #xff))))

(define (write-lint64 int)
  (do ((i 0 (+ i 8)))
      ((= i 64))
    (write-byte (logand (ash int (- i)) #xff))))


;;; -------- 32-bit floats (IEEE 754, sign + 23(+1) bits significand + 8 bits exponent)

(define (int_to_float32 int)
  (if (zero? int)
      0.0
      (* (if (zero? (ash int -31)) 1.0 -1.0) 
	 (expt 2 (- (logand (ash int -23) #xff) 127)) 
	 (logior #x800000 (logand int #x7fffff)) 
	 (expt 2 -23))))

(define (read-bfloat32)
  (int_to_float32 (read-bint32)))

(define (read-lfloat32)
  (int_to_float32 (read-lint32)))

(define (float64_to_int32 flt)
  (let* ((data (integer-decode-float flt))
	 (signif (car data))
	 (expon (cadr data))
	 (sign (caddr data)))
    (if (and (= expon 0)
	     (= signif 0))
	0
	;; we're assuming floats are (64-bit) doubles in s7, so this is coercing to a 32-bit float in a sense
	;;   this causes some round-off error
	(logior (if (negative? sign) #x80000000 0)
		(ash (+ expon 52 127) 23)
		(logand (ash signif -29) #x7fffff)))))

(define (write-bfloat32 flt)
  (write-bint32 (float64_to_int32 flt)))

(define (write-lfloat32 flt)
  (write-lint32 (float64_to_int32 flt)))



;;; -------- 64-bit floats (IEEE 754, sign + 52(+1) bits significand + 11 bits exponent)

(define (int_to_float64 int)
  (if (zero? int)
      0.0
      (* (if (zero? (ash int -63)) 1.0 -1.0) 
	 (expt 2 (- (logand (ash int -52) #x7ff) 1023)) 
	 (logior #x10000000000000 (logand int #xfffffffffffff))
	 (expt 2 -52))))

(define (read-bfloat64)
  (int_to_float64 (read-bint64)))

(define (read-lfloat64)
  (int_to_float64 (read-lint64)))

(define (float64_to_int64 flt)
  (let* ((data (integer-decode-float flt))
	 (signif (car data))
	 (expon (cadr data))
	 (sign (caddr data)))
    (if (and (= expon 0)
	     (= signif 0))
	0
	(logior (if (negative? sign) #x8000000000000000 0)
		(ash (+ expon 52 1023) 52)
		(logand signif #xfffffffffffff)))))

(define (write-bfloat64 flt)
  (write-bint64 (float64_to_int64 flt)))

(define (write-lfloat64 flt)
  (write-lint64 (float64_to_int64 flt)))



;;; -------- 80-bit floats (IEEE 754, sign + 63(+1) bits significand + 15 bits exponent, needed for aifc headers)

(define (read-bfloat80->int)
  (let* ((exp 0)
	 (mant1 0)
	 (mant0 0)
	 (sign 0)
	 (buf (make-vector 10)))
    (do ((i 0 (+ i 1)))
	((= i 10))
      (vector-set! buf i (read-byte)))
    (set! exp (logior (ash (vector-ref buf 0) 8) (vector-ref buf 1)))
    (set! sign (if (/= (logand exp #x8000) 0) 1 0))
    (set! exp (logand exp #x7FFF))
    (set! mant1 (+ (ash (vector-ref buf 2) 24) (ash (vector-ref buf 3) 16) (ash (vector-ref buf 4) 8) (vector-ref buf 5)))
    (set! mant0 (+ (ash (vector-ref buf 6) 24) (ash (vector-ref buf 7) 16) (ash (vector-ref buf 8) 8) (vector-ref buf 9)))
    (if (= mant1 mant0 exp sign 0) 
	0
      (round (* (if (= sign 1) -1 1)
		(expt 2.0 (- exp 16383.0))
		(+ (* (expt 2.0 -31.0) mant1)
		   (* (expt 2.0 -63.0) mant0)))))))

(define (write-int->bfloat80 val)
  (let ((exp 0)    
	(sign 0)
	(mant1 0)
	(mant0 0))
    (if (minusp val) 
	(begin
	  (set! sign 1) 
	  (set! val (abs val))))
    (if (not (zero? val))
	(begin
	  (set! exp (round (+ (log val 2.0) 16383.0)))
	  (set! val (* val (expt 2 (- (+ 16383 31) exp))))
	  (set! mant1 (floor val))
	  (set! val (- val mant1))
	  (set! mant0 (floor (* val (expt 2 32))))))
    (write-byte (logior (ash sign 7) (ash exp -8)))
    (write-byte (logand exp #xFF))
    (do ((i 2 (+ i 1))
	 (j 24 (- j 8)))
	((= i 6))
      (write-byte (logand (ash mant1 (- j)) #xFF)))
    (do ((i 6 (+ i 1))
	 (j 24 (- j 8)))
	((= i 10))
      (write-byte (logand (ash mant0 (- j)) #xFF)))))



;;; -------- "au" (NeXT/Sun) header

(define (read-au-header file)
  (with-input-from-file file
    (lambda ()
      (let ((magic (string (integer->char (read-byte)) 
			   (integer->char (read-byte)) 
			   (integer->char (read-byte)) 
			   (integer->char (read-byte)))))
	(if (not (string=? magic ".snd"))
	    (error 'bad-header (format #f "magic word is ~S" magic))
	    (let* ((data-location (read-bint32))
		   (data-size (read-bint32))
		   (data-format (read-bint32))
		   (srate (read-bint32))
		   (chans (read-bint32))
		   (comment (read-string)))
	      (list magic data-location data-size data-format srate chans comment)))))))

(define (write-au-header file chans srate data-size data-format comment)
  (with-output-to-file file
    (lambda ()
      (let* ((comlen (length comment))
	     (data-location (+ 24 (* 4 (floor (+ 1 (/ comlen 4))))))
	     (curloc 24))
	(write-byte (char->integer #\.))
	(write-byte (char->integer #\s))
	(write-byte (char->integer #\n))
	(write-byte (char->integer #\d))
	(write-bint32 data-location)
	(write-bint32 data-size)
	(write-bint32 data-format)
	(write-bint32 srate)
	(write-bint32 chans)
	(if (> comlen 0)
	    (begin
	      (write-string comment)
	      (set! curloc (+ curloc comlen 1)))) ; write-string adds a trailing 0
	(do ((i curloc (+ i 1)))
	    ((>= i data-location))
	  (write-byte 0))))))


;;; -------- "wav" (RIFF) header

