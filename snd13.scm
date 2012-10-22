(define (clm-print . args) 
  "(clm-print . args) applies format to args and prints the result"
  (snd-print (apply format #f args)))

