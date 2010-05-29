;;; read/write binary (sound) files

;;; -------- strings (0-terminated)
(define (read-string)
  (let ((chars '()))
    (do ((c (read-byte) (read-byte)))
	((or (= c 0)
	     (eof-object? c))
	 (apply string (reverse chars))))))

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

(define (write-lint16 int)
  (write-byte (logand int #xff))
  (write-byte (logand (ash int -8) #xff))
  (write-byte (logand (ash int -16) #xff))
  (write-byte (logand (ash int -32) #xff)))


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
  (if (zerop int)
      0.0
      (* (if (zero? (ash int -31)) 1.0 -1.0) 
	 (expt 2 (- (logand (ash int -23) #xff) 127)) 
	 (logior #x800000 (logand int #x7fffff)) 
	 (expt 2 -23))))

(define (read-bfloat32)
  (int_to_float32 (read-bint32)))

(define (read-lfloat32)
  (int_to_float32 (read-bint32)))

(define (float32_to_int flt)
  (let* ((data (integer-decode-float flt))
	 (signif (car data))
	 (expon (cadr data))
	 (sign (caddr data)))
    (logior (if (negative? sign) #x80000000 0)
	    (ash (+ expon 23 127) 23)
	    signif)))

(define (write-bfloat32 flt)
  (write-bint32 (float_to_int flt)))

(define (write-lfloat32 flt)
  (write-bint32 (float_to_int flt)))



;;; -------- 64-bit floats (IEEE 754, sign + 52(+1) bits significand + 11 bits exponent)

(define (int_to_float64 int)
  (if (zerop int)
      0.0
      (* (if (zero? (ash int -63)) 1.0 -1.0) 
	 (expt 2 (- (logand (ash int -52) #x7ff) 1023)) 
	 (logior #x10000000000000 (logand int #xfffffffffffff))
	 (expt 2 -52))))

(define (read-bfloat64)
  (int_to_float64 (read-bint64)))

(define (read-lfloat64)
  (int_to_float64 (read-bint64)))

(define (float64_to_int flt)
  (let* ((data (integer-decode-float flt))
	 (signif (car data))
	 (expon (cadr data))
	 (sign (caddr data)))
    (logior (if (negative? sign) #x8000000000000000 0)
	    (ash (+ expon 52 1023) 52)
	    signif)))

(define (write-bfloat64 flt)
  (write-bint64 (float_to_int flt)))

(define (write-lfloat64 flt)
  (write-bint64 (float_to_int flt)))


;;; -------- read "au/snd" header

(define (read-au-header file)
  (with-input-from-file file
    (lambda ()
      (let ((magic (string (integer->char (read-byte)) (integer->char (read-byte)) (integer->char (read-byte)) (integer->char (read-byte)))))
	(if (not (string=? magic ".snd"))
	    (error 'bad-header (format #f "magic word is ~S" magic))
	    (let* ((data-location (read-bint32))
		   (data-size (read-bint32))
		   (data-format (read-bint32))
		   (srate (read-bint32))
		   (chans (read-bint32))
		   (comment (read-string)))
	      (list magic data-location data-size data-format srate chans comment)))))))
