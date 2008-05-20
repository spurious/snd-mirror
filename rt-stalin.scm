
#!


!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *stalin-stack-size* (* 256 1024))
(define *tar-atomic-heap-size* (* 1024 1024))
(define *tar-nonatomic-heap-size* (* 1024 1024))
(define *tar-roots-size* (* 1024 1024))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; various ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-system-output command cont)
  (let* ((logfilename "/tmp/snd-ls-logtemp")
         (ret (system (<-> command " | tee " logfilename " && exit ${PIPESTATUS[0]}")))
         )
    (let* ((output "")
	   (fd (open-file logfilename "r"))
	   (line (read-line fd)))
      (while (not (eof-object? line))
        (set! output (<-> output line))
	     (set! line (read-line fd)))
      (close fd)
      (system (<-> "rm " logfilename))
      (cont output ret))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Garbage collector ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (defined? '*tar-is-started*))
    (primitive-eval '(define *tar-is-started* #f)))

(if (not *tar-is-started*)
    (eval-c (<-> "-I" snd-header-files-path)
            "#include <rt-various.h>"
            (run-now
             (init_rollendurchmesserzeitsammler ,*tar-atomic-heap-size*
                                                ,*tar-nonatomic-heap-size*
                                                ,*tar-roots-size*))))
(set! *tar-is-started* #t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Stalin functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define stalin-funcs '())

(define (add-stalin-func name body)
  (let ((hit (assq name stalin-funcs)))
    (if hit
        (set-cdr! hit (list body))
        (push! (list name body) stalin-funcs))))

(define (is-stalin-defined? name)
  (memq name stalin-funcs))

(define (get-stalin-func name)
  (cadr (assq name stalin-funcs)))
  
(define-macro (define-stalin name . body)
  (when (pair? name)
    (set! body `((lambda ,(cdr name) ,@body)))
    (set! name (car name)))
  
  (add-stalin-func name `(define ,name ,@body))
  #t)
  
(define (find-stalin-funcs expr-top)
  (define ret '())
  (let loop ((expr expr-top))
    (schemecodeparser expr
                      :symbolfunc (lambda (sym)
                                    (if (assq sym stalin-funcs)
                                        (push! sym ret)))
                      :elsefunc (lambda (expr)
                                  (when (not (memq (car expr) ret))
                                    (let ((hit (assq (car expr) stalin-funcs)))
                                      (if hit
                                          (push! (car hit) ret))))
                                  (for-each loop (cdr expr)))))
  (delete-duplicates ret eq?))

;;(define (add-stalin-bindings name bindings . rest)
;;  #t)

#!
(define-stalin *var* 0)
(define-stalin (add2 a b)
  (+ a b *var* (add2 5 7)))
(define-stalin add (lambda (a b)
                     (+ a (add2 b 9))))
(pretty-print (generate-stalin-code '((+ 2 3 (add 50 (add 90))))))
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Stalin eval-c functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stalin-ec-functions '())

(define (get-stalin-ec-function name)
  (assq name stalin-ec-functions))

;; stalin seems to be missing long long which is needed for time.
(define (get-legal-stalin-type type)
  (fix-defines
   (define s (eval-c-etype->ctype (eval-c-get-known-type type)))
   (define clean (string->symbol s))
   (cond ((string=? s "char *")
          'char*)
         ((memq clean '(void char short int float double void*))
          clean)
         (else
          'void*))))
 
(eval-c-etype->ctype (eval-c-get-known-type '<char-*>))

(define-macro (define-stalin-ec ret-type name body)
  (when (eq? 'lambda (car body))
    (let ()
      (define def (map (lambda (arg)
                         (fix-defines
                          (define type (car arg))
                          (define name (cadr arg))
                          (define rt-type (hashq-ref rt-types type))
                          ;;(c-display "type/rt-type: " type rt-type)
                          (if rt-type
                              `(,(-> rt-type c-type) ,name)
                              arg)))
                       (cadr body)))
      (if (hashq-ref rt-types ret-type)
          (set! ret-type (-> (hashq-ref rt-types ret-type) c-type)))
      
      (set! body `(,(car body) ,def ,@(cddr body)))
      (add-stalin-func name `(define ,name (foreign-procedure ,(map get-legal-stalin-type (map car (cadr body)))
                                                              ,(get-legal-stalin-type ret-type)
                                                              ,(symbol->string name))))))

  (let ((dependents (rt-find-all-funcs body))
	(old (get-stalin-ec-function name)))
    (if old
	`(set-cdr! (assq ',name stalin-ec-functions) (list '(,@dependents)
                                                           '(,ret-type ,name ,body)))
	`(set! stalin-ec-functions (append! stalin-ec-functions
                                            (list (list ',name
                                                        '(,@dependents)
                                                        '(,ret-type ,name ,body))))))))
(define (add-stalin-ec name body)
  (define hit (get-stalin-ec-function name))
  (if hit
      (set-cdr! hit (list '() body))
      (set! stalin-ec-functions (append! stalin-ec-functions
                                         (list (list name
                                                     '()
                                                     body))))))

(define (add-stalin-ec-binding function1 function2)
  (fix-defines 
   (define hit (get-stalin-ec-function function1))
   (define dependents (nth 1 hit))
   (define body (nth 2 hit))
   (set-cdr! hit (list (cons function2 dependents)
                       body))))

#!
!#


(define (get-stalin-ec-funcs program)
  (let ((function-names '())
        (functions '()))
    
    (define (add-func funcname)
      (if (not (memq funcname function-names))
          (let ((func (get-stalin-ec-function funcname)))
            (if func
                (begin
                  (push! funcname function-names)    ;; Add function-name to the list of included function
                  (for-each add-func (nth 1 func))                         ;; Add functions used by the function
                  (push! (nth 2 func) functions))))))     ;; Add function-body to be included.
    
    ;;(c-display "all-funcs:" (rt-find-all-funcs (cdr term)))
    (for-each add-func (rt-find-all-funcs program))
    (reverse! functions)))

#!
(get-stalin-ec-funcs '((rt_write_out_bus 0 2 0.5)))
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Stalin low-level macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define stalin-macros (make-hash-table 219))

(define-macro (define-stalin-macro def . body)
  (if (pair? def)
      `(hashq-set! stalin-macros ',(car def) (lambda ,(cdr def)
                                                   ,@body))
      `(hashq-set! stalin-macros ',def ,@body)))


(define (stalin-macroexpand-1 expr)
  (if (or (not (pair? expr))
	  (null? expr)
	  (not (symbol? (car expr))))
      expr
      (let ((qua (hashq-ref stalin-macros (car expr))))
	(if (not qua)
	    (begin
	      ;;(c-display "Error in expand-a-macro. Macro for " expr " Not found.")
	      expr)
	    (apply qua (cdr expr))))))


(define (stalin-macroexpand expr)
  (schemecodeparser expr
		    :elsefunc (lambda (expr)
                                (when (and (eq? 'set! (car expr))
                                         (not (pair? (cadr expr)))
                                         (is-stalin-defined? (cadr expr)))
                                  (c-display "Bindings defined using define-stalin can not be set!:"
                                             expr)
                                  (throw 'compilation-error))
                                (if (and (eq? 'set! (car expr))
                                         (pair? (cadr expr)))
                                    (stalin-macroexpand
                                     `( ,(<_> 'setter!- (car (cadr expr))) ,@(cdr (cadr expr)) 
                                        ,(caddr expr)))
                                    (let ((topexpand (stalin-macroexpand-1 expr)))
                                      ;;(c-display "expr/topexpand" expr topexpand)
                                      (if (eq? expr topexpand)
                                          `(,(car expr) ,@(map stalin-macroexpand (cdr expr)))
                                          (stalin-macroexpand topexpand)))))))



#!
(define-stalin-macro (dosomething b)
  `(+ 50 ,b 60))
(stalin-macroexpand-1 '(dosomething 55))
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Rt stalin functions and macros ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various

(define-stalin-macro (unquote something)
  (primitive-eval something))

(define-stalin-macro (1+ a) 
  `(+ 1 ,a))
(define-stalin-macro (1- a) 
  `(- ,a 1))
(define-stalin-macro (vref n v)
  `(vector-ref ,v ,n))
(define-stalin-macro (vset! n v val)
  `(vector-set! ,v ,n ,val))
(define-stalin-macro (inc! var how-much)
  `(begin
     (set! ,var (+ ,how-much ,var))
     ,var))
(define-stalin-macro (!= a b)
  `(not (= ,a ,b)))

(define-stalin-macro (while test . body)
  `(do ()
       ((not ,test))
       ,@body))

(define-stalin-macro (when cond . rest)
  `(cond (,cond ,@rest)
	 (else '())))

(define-stalin-macro (call/cc a)
  `(call-with-current-continuation ,a))


;; Code copied from stalin source.
(define (add-stalin-structure s)
  (define (sx-datum d) d)
  (define sx-second cadr)
  (define sx-rest cdr)
  (define (sx-unlist d) d)
  (define sx-every every)
  (define sx-length length)
  (define sx-symbol? symbol?)
  (define (syntax-error s message)
    (c-display message)
    (throw 'compilation-error))
  (define (map-indexed func alist)
    (let ((i -1))
      (map (lambda (s)
             (inc! i 1)
             (func s i))
           alist)))
  (set! s (cons 'define-structure s))
  (when (or (< (sx-length s) 3)
            (not (sx-every sx-symbol? (sx-rest s))))
    (syntax-error s "Improper DEFINE-STRUCTURE"))
  (let ((type (sx-datum (sx-second s)))
        (slots (sx-unlist (sx-rest (sx-rest s)))))
    ;; conventions: TYPE SLOTS
    (primitive-eval
     `(begin
        (define-stalin (,(string->symbol
                          (string-append "make-" (symbol->string type)))
                        ,@(map sx-datum slots))
          ((primitive-procedure make-structure ,type ,(length slots))
           ,@(map sx-datum slots)))
        (define-stalin (,(string->symbol
                          (string-append (symbol->string type) "?"))
                        obj)
          ((primitive-procedure structure? ,type) obj))
        ,@(map-indexed
           (lambda (slot i)
             ;; conventions: SLOT I
             (let ((slot (sx-datum slot)))
               ;; conventions: SLOT
               `(begin
                  (define-stalin (,(string->symbol
                                    (string-append (symbol->string type)
                                                   "-"
                                                   (symbol->string slot)))
                                  s)
                    ((primitive-procedure structure-ref ,type ,i) s))
                  (define-stalin (,(string->symbol
                                    (string-append "SET-"
                                                   (symbol->string type)
                                                   "-"
                                                   (symbol->string slot)
                                                   "!"))
                                  s x)
                    ((primitive-procedure structure-set! ,type ,i) s x))
;;;                 (define (,(string->symbol
;;;                            (string-append "LOCAL-SET-"
;;;                                           (symbol->string type)
;;;                                           "-"
;;;                                           (symbol->string slot)
;;;                                           "!"))
;;;                          s x)
;;;                   (let ((p ((primitive-procedure structure-ref ,type ,i) s)))
;;;		     ;; conventions: P
;;;		     (upon-failure
;;;		      ((primitive-procedure structure-set! ,type ,i) s p)))
;;;                   ((primitive-procedure structure-set! ,type ,i) s x))
                  )
               )
             )
           slots)
        ))))

(define-stalin-ec <void> lowlevel_remove_me (lambda ()
                                              (myexit)))
(define-stalin (remove-me)
  (lowlevel_remove_me))

(define-stalin-ec <void> lowlevel_debug (lambda ((<char*> string))
                                          (rt_debug string)))
(define-stalin-ec <void> lowlevel_debug1 (lambda ((<char*> string)
                                                  (<int> a))
                                           (rt_debug string a)))

(define-stalin-macro (debug string . rest)
  (define something (rt-gensym))
  (if (null? rest)
      `(lowlevel_debug ,string)
      `(lowlevel_debug (apply string-append
                              (cons ,string
                                    (map (lambda (,something)
                                           (cond ((number? ,something)
                                                  (number->string ,something))
                                                 ((symbol? something)
                                                  (symbol->string ,something))
                                                 ((string? ,something)
                                                  string)
                                                 (else
                                                  " <unkown type> ")))
                                         (list ,@rest)))))))

(define-stalin (error message)
  (lowlevel_debug message)
  (remove-me))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sound stuff

(define-stalin _block-time 0)
(define-stalin _time 0)

(define-stalin-ec <int> _get_block_time (lambda ()
                                          (return block_time)))
(define-stalin-ec <int> _get_startframe (lambda ()
                                          (return g_startframe)))
(define-stalin-ec <int> _get_endframe (lambda ()
                                        (return g_endframe)))

(define-stalin-ec <void> _block_ready (lambda ()
                                       (block_ready_internal)))
           
;(add-stalin-ec 'bus-struct bus-struct)

;(add-stalin-ec 'outbus `(<struct-rt_bus*> outbus (cast <struct-rt_bus*> ,(<-> (number->string (cadr (SCM_SMOB_DATA *out-bus*)))
;                                                                              "UL"))))
;(add-stalin-ec 'inbus `(<struct-rt_bus*> inbus (cast <struct-rt_bus*> ,(<-> (number->string (cadr (SCM_SMOB_DATA *in-bus*)))
;                                                                            "UL"))))

;(add-stalin-ec-binding 'outbus 'bus-struct)
;(add-stalin-ec-binding 'inbus 'bus-struct)

(define-stalin-ec <void> rt_write_out_bus-old
  (lambda ((<int> ch)
           (<int> framenum)
           (<float> val))
    (if (>= ch outbus->num_channels)
        return)
    (<struct-rt_bus_data-*> data "&outbus->data[(outbus->num_channels*framenum)+ch]")
    (if (< data->last_written_to block_time)
        (set! data->val val)
        (+= data->val val))
    (set! data->last_written_to block_time)))

(define-stalin-ec <void> rt_write_out_bus
  (lambda ((<int> ch)
           (<int> time)
           (<float> val))
    (if (>= ch ,*rt-num-output-ports*)
        return)
    (let* ((pos <int> (+ (* ch ,*rt-block-size*)
                         (- time
                            block_time))))
      (+= sounddata[pos] val))))

(add-stalin-ec-binding 'rt_write_out_bus 'outbus)

(define-stalin-macro (out . rest)
  (define val (rt-gensym))
  (if (= 2 (length rest))
      `(rt_write_out_bus ,(car rest) _time ,(cadr rest))
      `(let ((,val ,(car rest)))
         (rt_write_out_bus 0 _time ,val)
         (rt_write_out_bus 1 _time ,val))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structures (providing same structure syntax for rt-stalin as guile and snd-rt)

(define-macro (define-stalin-struct name . das-slots)
  (define name-name (rt-gensym))
  (define val-name (rt-gensym))
  (define slots '())
  
  (for-each (lambda (slot)
	      (if (keyword? slot)
		  (push-back! (list (append-various slot) 0) slots)
		  (set-cdr! (last slots) (list slot))))
	    das-slots)

  (add-stalin-structure (cons name (map car slots)))

  (let ((slot-names (map car slots)))
    `(begin

       ;;(add-stalin-func ',name (quote (define-structure ,name ,@slot-names)))
         
       ;;(define-stalin (,(symbol-append 'make- name)); ,@slot-names);:key ,@slots)
       ;;  (make-vector ,(length slot-names)))

       ,@(map (lambda (slot)
                `(define-stalin-macro (,(append-various 'setter!- name ":" slot) ,name-name ,val-name)
                   (quasiquote (,(<_> 'SET- ',name ',(string->symbol "-") ',slot ',(string->symbol "!")) ,,name-name ,,val-name))))
              slot-names)
;;       ,@(map (lambda (slot)
;;                `(define-stalin (,(append-various 'setter!- name ":" slot) ,name-name ,val-name)
;;                   (,(<_> 'SET- name (string->symbol "-") slot (string->symbol "!")) ,name-name ,val-name)))
;;              slot-names)
;;       ,@(map (lambda (slot)
;;                `(define-stalin (,(append-various 'getter- name ":" slot) ,name-name)
;;                   (,(<_> name (string->symbol "-") slot) ,name-name)))
;;              slot-names)
       ,@(map (lambda (slot)
                `(define-stalin-macro (,(append-various 'getter- name ":" slot) ,name-name)
                   (quasiquote (,(<_> ',name ',(string->symbol "-") ',slot) ,,name-name))))
              slot-names)
       )))

(define-stalin-macro (setter!-=> object das-method . rest)
  (cond ((keyword? object)
         (let ((name (rt-gensym))
               (type object)
               (object das-method)
               (das-method (car rest))
               (rest (cdr rest)))
           `(let ((,name ,object))
              (setter!-=> ,(<_> (keyword->symbol type) (string->symbol ":") name) ,das-method ,@rest))))
        (else
         (let ()
           (define method (keyword->symbol das-method))
           (define object-decomposed (map string->symbol (string-split (symbol->string object) #\:)))
           (let ()
             (define struct-name (car object-decomposed))
             (define object-name (if (null? (cdr object-decomposed))
                                     (car object-decomposed)
                                     (cadr object-decomposed)))
             `(,(append-various 'setter!- struct-name ":" method) ,object-name ,@rest))))))

(define-stalin-macro (=> object das-method . rest)
  (cond ((not (null? rest))
         (let ((name (rt-gensym))
               (type object)
               (object das-method)
               (das-method (car rest)))
           `(let ((,name ,object))
              (=> ,(<_> (keyword->symbol type) (string->symbol ":") name) ,das-method))))
        (else
         (if (or (not (symbol? object))
                 (not (keyword? das-method)))
             (begin
               ;;(c-display (symbol? object) (keyword? das-method))
               (c-display "Syntax error" `(=> ,object ,das-method))
               (throw 'compilation-error)))
         (let ()
           (define method (keyword->symbol das-method))
           (define object-decomposed (map string->symbol (string-split (symbol->string object) #\:)))
           (let ()
             (define struct-name (car object-decomposed))
             (define object-name (if (null? (cdr object-decomposed))
                                     (car object-decomposed)
                                     (cadr object-decomposed)))
             `(,(append-various 'getter- struct-name ":" method) ,object-name))))))

#!
(stalin-macroexpand '(=> :coroutine(vref 1 _queue) :time))
(pretty-print (macroexpand '(define-stalin-struct str
                              :a 1
                              :b)))
(define-stalin-struct str
  :a
  :b)
(<rt-stalin>
 (define str (make-str 2 3))
 (set! (=> str :a) 9)
 (lowlevel_debug1 "ai: %d" (=> str :a))
 ;(newline)
 (let loop ()
   (_block_ready)
   (loop)))


(<rt-stalin>
 (spawn 
   (display (format #f "hello"))
   (display (+ 2 3))
   (newline)))

(<rt-stalin>
 (debug "hallo")
 (let loop ()
   (_block_ready)
   (loop)))

!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coroutines

(define-stalin-struct coroutine
  :time
  :continuation)

(define *stalin-queue-max-size* 1024)

(define-stalin _coroutine-dummy (make-coroutine 0 (lambda () #f)))

(define-stalin _current-coroutine
  (make-coroutine 0 
                  (lambda ()
                    (let loop ()
                      (_block_ready)
                      (set! _block-time (_get_block_time))
                      (set! _time (+ _block-time
                                     (_get_startframe)))
                      (insert-coroutine-in-queue! _current-coroutine
                                                  (+ _block-time
                                                     (_get_endframe))
                                                  0)
                      (run-scheduler (lambda ()
                                       (if (and (= _queue-size 0)
                                                (= _block-queue-size 0))
                                           (remove-me))
                                       (loop))))
                    #f)))

(define-stalin _queue-size 0)
(define-stalin _block-queue-size 0)

(define-stalin _next-scheduled-time 0)

(define-stalin _queue (make-vector ,*stalin-queue-max-size* _coroutine-dummy))
(define-stalin _block-queue (make-vector ,*stalin-queue-max-size* _coroutine-dummy))

;(add-stalin-bindings 'queue '(*queue-size* _coroutine-dummy)
;                     'block_queue '(*queue-size* _coroutine-dummy))


(define-stalin (get-first-coroutine-in-queue)
  (define                  which_queue (or (= 0 _block-queue-size)
                                           (and (> _queue-size 0)
                                                (< (=> :coroutine(vref 1 _queue) :time) ;; <= ?
                                                   (=> :coroutine(vref 1 _block-queue) :time)))))
  (define                  queue     (if which_queue
                                         _queue
                                         _block-queue))
  (define                  ret       (vref 1 queue))
  (define                  size      (if which_queue
                                         (1- _queue-size)
                                         (1- _block-queue-size)))
  (define                  last      (vref (1+ size) queue))
  (define                  last-time (=> coroutine:last :time))
  (define                  i         1)
  (define                  child     0)
  (define                  got-it    #f)

  (if which_queue
      (inc! _queue-size -1)
      (inc! _block-queue-size -1))

  ;; Code below is a common binary heap "delete min" operation.
  (while (and (not got-it)
              (<= (<< i 1) size))
    (set! child (<< i 1))
    (if (and (!= child size)
	     (< (=> :coroutine(vref (1+ child) queue) :time)
                (=> :coroutine(vref child      queue) :time)))
        (inc! child 1))
    (if (> last-time (=> :coroutine(vref child queue) :time))
	(begin
	  (vset! i queue (vref child queue))
	  (set! i child))
        (set! got-it #t)))

  (vset! i queue last)

  (if which_queue
      (set! _next-scheduled-time (>> (=> :coroutine(vref 1 queue) :time) 2)))

  (set! (=> coroutine:ret :time)
        (>> (=> coroutine:ret :time)
            2))

  ret)


;;(add-stalin-bindings 'get-first-coroutine-in-queue
;;                     '(block-queue-size queue-size queue block-queue next-scheduled-time))


;; Returns false in case the priority queue is full. (O(log n) efficiency)
;; insert_coroutine_in_queue
;; *************************
;;
;; Returns 0 in case the priority queue is full. (O(log n) efficiency)
(define-stalin (insert-coroutine-in-queue! coroutine time priority)

  (define queue _queue)

  (if (>= _queue-size
          (- ,*stalin-queue-max-size* 2))
      (error "coroutine queue full"))

  (inc! _queue-size 1)

  ;; Not too sure about this one. Maybe it can cover up bugs.
  ;;(if (< time 0)
  ;;    (set! time 0))

  (if (or (= 1 _queue-size)
          (< time _next-scheduled-time))
      (set! _next-scheduled-time time))
  
  ;; Add priority info to the time attribute. ("priority" is a 2 bit integer)
  (set! time (<< time 2))
  (set! time (+ time priority))

  (set! (=> coroutine :time) time)

  ;; Code below is a common binary heap "insert" operation.
  (let* ((i    _queue-size)
	 (newi (>> i 1)))
    (while (> (=> :coroutine(vref newi queue) :time)
              time)
      (vset! i queue (vref newi queue))
      (set! i newi)
      (set! newi (>> newi 1)))

    (vset! i queue coroutine)))



(define-stalin (insert-coroutine-in-block-queue! coroutine time priority)

  (define queue _block-queue)

  (when (>= _block-queue-size
            (- ,*stalin-queue-max-size* 2))
    (debug "block coroutine queue full")
    (remove-me))

  
  (inc! _block-queue-size 1)

  ;; Add priority info to the time attribute. ("priority" is a 2 bit integer)
  (set! time (<< time 2))
  (set! time (+ time priority))

  (set! (=> coroutine :time) time)

  ;; Code below is a common binary heap "insert" operation.
  (let* ((i    _block-queue-size)
	 (newi (>> i 1)))
    (while (> (=> :coroutine(vref newi queue) :time)
              time)
      (vset! i queue (vref newi queue))
      (set! i newi)
      (set! newi (>> newi 1)))

    (vset! i queue coroutine)))


;; How about (get-time) / (set-time! n) ?
(define-stalin (time)
  _time)

(define-stalin (setter!-time new-time)
  (set! _time new-time))

(define-stalin (switch-to-coroutine coroutine cont)
  (set! (=> coroutine:_current-coroutine :continuation) cont)
  (set! _current-coroutine coroutine)
  (set! _time (=> coroutine :time))
  ((=> coroutine :continuation)))

;;  (when (call/cc (lambda (return)
;;                   (set! (=> coroutine:_current-coroutine :continuation)
;;                         (lambda ()
;;                           (return #f)
;;                           #f))
;;                   #t))
;;    (set! _current-coroutine coroutine)
;;    ((=> coroutine :continuation))))

(define-stalin (run-scheduler cont)
  (let ((next (get-first-coroutine-in-queue)))
    (if (not (eq? next _current-coroutine))  ;; Don't want to switch to a coroutine unnecessarily.
	(switch-to-coroutine next cont)
        (cont))))

(define-stalin (yield)
  (insert-coroutine-in-queue! _current-coroutine 
                              _time
                              2) ;; lower priority than wait, but higher than block.
  (call/cc (lambda (return)
             (run-scheduler (lambda ()
                              (return #t))))))

(define-stalin (wait n)
  (inc! _time n)
  (when (> _time ;; Don't want to schedule unnecessarily.
           _next-scheduled-time)
    (insert-coroutine-in-queue! _current-coroutine
                                _time
                                1) ;; higher priority than block, but less than main.
    (call/cc (lambda (return)
               (run-scheduler (lambda ()
                                (return #t)))))))

(define-stalin-macro (spawn . rest)
  (define time (rt-gensym))
  (define coroutine (rt-gensym))
  (define wait 0)
  (define code rest)
  (when (eq? :wait (car rest))
    (set! wait (cadr rest))
    (set! code (cddr rest)))
  `(let* ((,time (+ _time
                    (max 0 (inexact->exact (floor ,wait)))))
          (,coroutine (make-coroutine ,time
                                      (lambda ()
                                        ,@code
                                        (run-scheduler (lambda ()
                                                         #t))))))
     (insert-coroutine-in-queue! ,coroutine
                                 ,time
                                 1)
     ,coroutine))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Block (inner loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; block
;; *****
(define-stalin-macro (block . rest)
  (define next (rt-gensym))
  (define loop (rt-gensym))
  (define duration (rt-gensym))
  (define das-duration #f)
  (define code rest)

  (when (or (equal? (car rest) :iterations)
            (equal? (car rest) :iter))
    (set! das-duration (cadr rest))
    (set! code (cddr rest)))

  (if das-duration
      `(let ((,duration ,das-duration))
         (call/cc (lambda (return)
                    (let ,loop ()
                         (if (> ,duration 0)
                             (let ((,next (min  _next-scheduled-time
                                                (+ _time ,duration))))
                               (while (< _time
                                         ,next)
                                 ,@code
                                 (inc! _time 1))

                               (inc! ,duration (- ,next))
                               
                               ;; Now do an extra-low-priority yield:
                               (insert-coroutine-in-block-queue! _current-coroutine
                                                                 _time
                                                                 3)
                               (run-scheduler ,loop))
                             (return #t))))))
      `(let ,loop ()
            (while (< _time
                      _next-scheduled-time)
              ,@code
              (inc! _time 1))
            
            ;;(debug "im here though")
            ;; Now do an extra-low-priority yield:
            (insert-coroutine-in-block-queue! _current-coroutine
                                              _time
                                              3)
            (run-scheduler ,loop))))


(define-stalin-macro (spawn-block . rest)
  (define startname (rt-gensym))
  (define start 0)
  (define duration #f)
  (define code rest)
  (when (equal? :dur (car rest))
    (set! start (nth 1 rest))
    (set! duration `(- ,(nth 2 rest) ,startname))
    (set! code (nth-cdr 3 rest)))
  `(let ((,startname ,start))
     (spawn :wait ,startname
       (block :iterations ,duration
         ,@code))))


#!

(<rt-stalin>
 (spawn
   (define phase 0.0)
   (block
     (out (* 0.2 (sin phase)))
     (inc! phase 0.062)))
 (let loop ((i 1))
   (when (< i 200)
     (spawn
       (define phase 0.0)
       (define phaseinc (+ 0.020 (/ i 40)))
       (block
         (out (* 0.002 (sin phase)))
         (inc! phase phaseinc)))
     (loop (1+ i)))))
         
:wait 2::s
:wait (^s 2)
:wait s::2
:wait 2:s
:wait 2:s
:wait 2s

(<rt-stalin>
 (spawn
   (debug "hmm?")
   (spawn :wait (* 0.5 44100)
     (debug "jammen")))
 (define phase 0.0)
 (spawn
   (debug "hello")
   ;(wait 88200)
   (debug "hello2")
   (block
     (out (* 0.8 (sin phase)))
     (inc! phase 0.052))))


(pretty-print (generate-stalin-code '((get-first-coroutine-in-queue))))
(pretty-print (generate-stalin-code '((spawn
                                        (debug "hello"))
                                      (yield)
                                      (let loop ()
                                        (_block_ready)
                                        (loop)))))

(pretty-print (generate-stalin-code
               '((define-structure astruct slot1 slot2))))

(pretty-print (macroexpand '(define-stalin-struct astruct :a :b)))
(define-stalin-struct astruct :a :b)

(pretty-print (generate-stalin-code
               `((=> astruct :a)
                 (set! (=> astruct :a) 90))))

(get-stalin-func 'remove-me)
(<rt-stalin>
 (define-structure astruct a b)
 (define obj (make-astruct 2 (lambda ()
                               (debug "a1"))))
 (set! (=> astruct:obj :b) (lambda ()
                             (debug "ai2")))
 ((=> astruct:obj :b))
 (remove-me))

(<rt-stalin>
 (define-structure astruct a b)
 (define obj (make-astruct 2 (lambda ()
                               (debug "a1"))))
 (SET-astruct-b! obj (lambda ()
                      (debug "ai2")))
 ((astruct-b obj))
 (remove-me))

(every symbol? '(astruct slot1 slot2))
(length '(astruct slot1 slot2))

(<rt-stalin>
 (spawn
   (debug "a1")
   ;;(yield)
   (debug "a2"))
 (spawn
   (debug "b1")
   ;;(yield)
   (debug "b2")))

(<rt-stalin>
 (define phase 0.0)
 (spawn
   (debug "hello1"))
 (spawn
   (debug "hello2")) 
 (spawn
   (block :iter (* 2 44100)
     (out (sin phase))
     (inc! phase 0.062))
   (block :iter (* 2 44100)
     (out (sin phase))
     (inc! phase 0.042))))

(if a
    (begin
      (block ...)
      (hello))
    (begin
      ...))
->
(if a
    (begin
      (block :cont (lambda ()
                     (hello))
        ...)
    (begin
      ...))

(let loop ()
  (block ...)
  (loop))
->
(let loop ()
  (block :cont (lambda ()
                 (loop))
    ...))

(begin
  (let loop ()
    (block ...)
    (hello2))
  (hello))
->
(begin
  (let loop ()
    (block :cont (lambda ()
                   (hello2))
      ...))
  (hello)) ;; will not be called.

(begin a b)
->
(a (lambda () b))

(begin a b c)
->
(a (lambda ()
     (b (lambda ()
          c))))
(if a b c)
->
(a (lambda (x) (if x b c)))


(block :code (s 2) (s 4) (lambda ()
                           (oscil* 200))
  (block :code (s 2) (s 4) (lambda ()
                               (oscil* 300))
    (block :code (s 2) (s 4) (lambda ()
                               (oscil* 400))
      (debug "finished"))))
                                 

; (spawn-block :dur (* 3 44100) (* 10 44100)
;   (if (> _framenum 1024)
;       (set! _framenum 0))
;   (out (sin phase))
;   (inc! _framenum 1)
;   (inc! phase 0.032)))



(stalin-macroexpand '(set! (=> coroutine:_current-coroutine :time) new-time))
(stalin-macroexpand '(=> coroutine:_current-coroutine :time))
(stalin-macroexpand '(set! (current-time) 2))

(stalin-macroexpand '(inc! (current-time) 2))
(fix-stalin-set! (stalin-macroexpand (fix-stalin-set! '(inc! (current-time) 2))))
(stalin-macroexpand (fix-stalin-set! '(set! (=> coroutine :time) new-time)))
(stalin-macroexpand (fix-stalin-set! '(set! (=> :coroutine(vref 1 queue) :time) 100)))
!#     




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Syntax check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this list is copied from the stalin source. (without permission)
(define stalin-builtin-funcs
  (append '(set! if)
   (map car
       '( (not not)
          (boolean? boolean?)
	  (eqv? eqv?)
	  (eq? eq?)
	  (equal? equal?)
	  (pair? pair?)
	  (cons cons)
	  (car car)
	  (cdr cdr)
	  (set-car! set-car!)
	  (set-cdr! set-cdr!)
	  (caar caar)
	  (cadr cadr)
	  (cdar cdar)
	  (cddr cddr)
	  (caaar caaar)
	  (caadr caadr)
	  (cadar cadar)
	  (caddr caddr)
	  (cdaar cdaar)
	  (cdadr cdadr)
	  (cddar cddar)
	  (cdddr cdddr)
	  (caaaar caaaar)
	  (caaadr caaadr)
	  (caadar caadar)
	  (caaddr caaddr)
	  (cadaar cadaar)
	  (cadadr cadadr)
	  (caddar caddar)
	  (cadddr cadddr)
	  (cdaaar cdaaar)
	  (cdaadr cdaadr)
	  (cdadar cdadar)
	  (cdaddr cdaddr)
	  (cddaar cddaar)
	  (cddadr cddadr)
	  (cdddar cdddar)
	  (cddddr cddddr)
	  (null? null?)
	  (list? list?)
	  (list list)
	  (list-length list-length)
	  (length length)
	  (sublist sublist)
	  (sub sub)
	  (list-append list-append)
	  (append append)
	  (list-reverse list-reverse)
	  (reverse reverse)
	  (list-tail list-tail)
	  (list-ref list-ref)
	  (ref ref)
	  (memq memq)
	  (memv memv)
	  (member member)
	  (assq assq)
	  (assv assv)
	  (assoc assoc)
	  (list-set! list-set!)
	  (ref! ref!)
	  (list-fill! list-fill!)
	  (fill! fill!)
	  (list-copy list-copy)
	  (copy copy)
	  (symbol? symbol?)
	  (symbol->string symbol->string)
	  (string->uninterned-symbol string->uninterned-symbol)
	  (string->symbol string->symbol)
	  (number? number?)
	  (complex? complex?)
	  (real? real?)
	  (rational? rational?)
	  (integer? integer?)
	  (exact? exact?)
	  (inexact? inexact?)
	  (= =)
	  (< <)
	  (> >)
	  (<= <=)
	  (>= >=)
	  (zero? zero?)
	  (positive? positive?)
	  (negative? negative?)
	  (odd? odd?)
	  (even? even?)
	  (max max)
	  (min min)
	  (+ +)
	  (* *)
	  (- -)
	  (/ /)
	  (abs abs)
	  (quotient quotient)
	  (remainder remainder)
	  (modulo modulo)
	  (gcd gcd)
	  (lcm lcm)
	  (<< <<)
	  (>> >>)
	  (bitwise-not bitwise-not)
	  (bitwise-and bitwise-and)
	  (bitwise-or bitwise-or)
	  (bitwise-xor bitwise-xor)
	  (floor floor)
	  (ceiling ceiling)
	  (truncate truncate)
	  (round round)
	  (exp exp)
	  (log log)
	  (sin sin)
	  (cos cos)
	  (tan tan)
	  (asin asin)
	  (acos acos)
	  (atan atan)
	  (sqrt sqrt)
	  (expt expt)
	  (exact->inexact exact->inexact)
	  (inexact->exact inexact->exact)
	  (number->string number->string)
	  (string->number string->number)
	  (char? char?)
	  (char=? char=?)
	  (char<? char<?)
	  (char>? char>?)
	  (char<=? char<=?)
	  (char>=? char>=?)
	  (char-ci=? char-ci=?)
	  (char-ci<? char-ci<?)
	  (char-ci>? char-ci>?)
	  (char-ci<=? char-ci<=?)
	  (char-ci>=? char-ci>=?)
	  (char-alphabetic? char-alphabetic?)
	  (char-numeric? char-numeric?)
	  (char-whitespace? char-whitespace?)
	  (char-upper-case? char-upper-case?)
	  (char-lower-case? char-lower-case?)
	  (char->integer char->integer)
	  (integer->char integer->char)
	  (char-upcase char-upcase)
	  (char-downcase char-downcase)
	  (string? string?)
	  (make-string make-string)
	  (string string)
	  (string-length string-length)
	  (string-ref string-ref)
	  (string-set! string-set!)
	  (string=? string=?)
	  (string-ci=? string-ci=?)
	  (string<? string<?)
	  (string>? string>?)
	  (string<=? string<=?)
	  (string>=? string>=?)
	  (string-ci<? string-ci<?)
	  (string-ci>? string-ci>?)
	  (string-ci<=? string-ci<=?)
	  (string-ci>=? string-ci>=?)
	  (substring substring)
	  (string-append string-append)
	  (string->list string->list)
	  (list->string list->string)
	  (string-copy string-copy)
	  (string-fill! string-fill!)
	  (string-reverse string-reverse)
	  (vector? vector?)
	  (make-vector make-vector)
	  (make-displaced-vector make-displaced-vector)
	  (vector vector)
	  (vector-length vector-length)
	  (vector-ref vector-ref)
	  (vector-set! vector-set!)
	  (vector->list vector->list)
	  (list->vector list->vector)
	  (vector-fill! vector-fill!)
	  (subvector subvector)
	  (vector-append vector-append)
	  (vector-reverse vector-reverse)
	  (vector-copy vector-copy)
	  (procedure? procedure?)
	  (apply apply)
	  (map map)
	  (for-each for-each)
	  (force force)
	  (call-with-current-continuation call-with-current-continuation)
	  (call-with-input-file call-with-input-file)
	  (call-with-output-file call-with-output-file)
	  (input-port? input-port?)
	  (output-port? output-port?)
	  (current-input-port current-input-port)
	  (current-output-port current-output-port)
	  (with-input-from-file with-input-from-file)
	  (with-output-to-file with-output-to-file)
	  (open-input-file open-input-file)
	  (open-output-file open-output-file)
	  (close-input-port close-input-port)
	  (close-output-port close-output-port)
	  (read read)
	  (read-char read-char)
	  (peek-char peek-char)
	  (eof-object? eof-object?)
	  (char-ready? char-ready?)
	  (write write)
	  (display display)
	  (newline newline)
	  (write-char write-char)
	  (define-write-method define-write-method)
	  (define-display-method define-display-method)
	  (panic panic)
	  (pointer? pointer?)
	  (integer->string integer->string)
	  (integer->input-port integer->input-port)
	  (integer->output-port integer->output-port)
	  (integer->pointer integer->pointer))))
  )


(define* (check-stalin-syntax code :key (varlist '()))
  (c-display "checking" code)
  (schemecodeparser code
                    :symbolfunc (lambda (sym)
                                  (c-display "sym" sym (schemecodeparser-get-varlist))
                                  (when (not (memq sym (schemecodeparser-get-varlist)))
                                    (c-display "Error. Unknown variable " sym ".")
                                    (throw 'compilation-error)))
                    :elsefunc (lambda (expr)
                                (define varlist (schemecodeparser-get-varlist))
                                (c-display expr varlist)
                                (when (and (not (assq (car expr) stalin-funcs))
                                           (not (memq (car expr) varlist))
                                           (not (memq (car expr) stalin-builtin-funcs)))
                                  (c-display (schemecodeparser-get-varlist))
                                  (c-display "Error. Unknown function " (car expr) "in expression" expr)
                                  (throw 'compilation-error))
                                (when (memq (car expr)
                                          '(display newline write-char panic write read 
                                                    with-input-from-file
                                                    with-output-from-file
                                                    open-input-file
                                                    open-output-file
                                                    close-input-port
                                                    close-output-port
                                                    eof-object?))
                                  (c-display "Warning." (car expr) "is not a realtime safe function.\n"
                                             "(the function \"debug\" can be used instead of display. See manual.)"))
                                (if (not (eq? 'foreign-procedure (car expr)))
                                    (for-each (lambda (expr)
                                                (check-stalin-syntax expr :varlist varlist))
                                              (cdr expr))))))


(define (display-stalin-error errormessage)
  (apply (lambda (filename linenumber charnumber error)           
           (set! linenumber (string->number linenumber))
           (set! charnumber (string->number charnumber))
           (let ((i 1)
                 (c 0))
             (call/cc
              (lambda (return)
                (for-each-line-in-file filename
                                       (lambda (line)
                                         (when (and (> i (- linenumber 6))
                                                    (< i (+ linenumber 5)))
                                           (define prestring (<-> (cond ((< i 10) "  ")
                                                                        ((< i 100) " ")
                                                                        (else ""))
                                                                  (number->string i) 
                                                                  ": "))
                                           (c-display (<-> prestring line))
                                           (when (= i linenumber)
                                             (c-display (<-> (make-string (+ (string-length prestring)
                                                                             (- charnumber c))
                                                                          #\space)
                                                             "^"))))
                                         (inc! i 1)
                                         (if (> i (+ linenumber 5))
                                             (return line))
                                         (inc! c (1+ (string-length line)))))))))
         (string-split errormessage #\:)))
#!
(display-stalin-error "/tmp/filePpYCDj.scm:14:323:Unbound variable")
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Generate stalin code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set! (some a) b) -> (setter!-some a b)
#!
had to be put into macroexpand instead.
(define (fix-stalin-set! code)
  (schemecodeparser code
                    :elsefunc (lambda (expr)
                                (if (and (eq? 'set! (car expr))
                                         (pair? (cadr expr)))
                                    `( ,(<_> 'setter!- (car (cadr expr))) ,@(cdr (cadr expr)) 
                                       ,(fix-stalin-set! (caddr expr)))
                                    (map fix-stalin-set! expr)))))
!#


;; Expands macros and include functions and variables which the code depends on,
;; all recursively. (careful with macros since its applied to all included code!)
(define (generate-stalin-code code)

  ;; memoized (lambda (funcname) `(define ,funcname ,(stalin-macroexpand (get-stalin-func funcname))))
  (define get-expanded-code
    (let ((expanded '()))
      (lambda (funcname)
        (let ((expanded (assq funcname expanded)))
        (if expanded
            (cadr expanded)
            (let ((ret (stalin-macroexpand (get-stalin-func funcname))))
              (push! (list funcname ret)
                     expanded)
              ret))))))

  (define (find-dependencies trace code)
    (define funcs (find-stalin-funcs code))
    (if (null? funcs)
        '(#f)
        (flatten (map (lambda (func)
                        (if (memq func trace)
                            '(#f) ;; cyclic dependency
                            (list (find-dependencies (cons func trace)
                                                     (get-expanded-code func))
                                  func)))
                      funcs))))

  (define expanded (stalin-macroexpand code))

  (let ((dependencies
         (delete-duplicates (delete #f (find-dependencies '() expanded) eq?)
                            eq?)))
    ;;(pretty-print dependencies)
    (append (map get-expanded-code dependencies)
            expanded)))


#!
(define-stalin a 0)
(define-stalin b a)
(define-stalin c (+ a b c));b a))
(generate-stalin-code '(c))
(generate-stalin-code '())
!#
  


(define (schemecode->file code)
  (fix-defines
   (define basename (tmpnam))
   ;;(c-display "basename:" basename)
   (define sourcefile (<-> basename ".scm"))
   (define fd (open-file sourcefile "w"))
  
   (for-each (lambda (expr)
               (pretty-print expr fd))
             code)
   
   (close fd)
   (delete-at-exit sourcefile)
   basename))

#!
(schemecode->file (generate-stalin-code '((+ 2 3 (add 50 (add 90))))))
!#

(define (compile-stalin-file basename)
  ;;(define command (<-> "stalin -On -no-clone-size-limit  -split-even-if-no-widening  -Ob -Om -Or -Ot -c " basename ".scm"))
  ;;(define command (<-> "stalin -On -no-clone-size-limit  -split-even-if-no-widening -c " basename ".scm"))
  ;;(define command (<-> "stalin -On -clone-size-limit 0 -no-escaping-continuations -c " basename ".scm"))
  (define command (<-> "stalin -On -clone-size-limit 0 -c " basename ".scm"))
  (delete-at-exit (<-> basename ".c"))
  (c-display command)
  (get-system-output command
                     (lambda (output ret)
                       (when (not (= 0 ret))
                         ;;(c-display "output/ret" output ret)
                         (catch #t
                                (lambda ()
                                  (display-stalin-error output))
                                (lambda x
                                  (c-display "." output)
                                  #f))
                         (throw 'compilation-error))))
  (<-> basename ".c"))

#!
(compile-stalin-file (schemecode->file (generate-stalin-code '((+ 2 3 (add 50 (add 90)))))))
!#


(eval-c (<-> "-I" snd-header-files-path)
        "#include <rt-various.h>"
        (public
         (<void> fix-stalin-c-source (lambda ((<char*> infile)
                                              (<char*> outfile))
                                       (fix_stalin_c_source infile outfile)))))

(define (get-stalin-c-file generated-scheme-file)
  (let* ((basename generated-scheme-file)
         (inname   (compile-stalin-file basename))
         (outname  (<-> (tmpnam) ".c")))
    (delete-at-exit outname)
    (fix-stalin-c-source inname outname)
    (c-display "inname" inname)
    outname))

#!
(get-stalin-c-file (schemecode->file (generate-stalin-code '((display (+ 2 3))(newline)))))
!#

(define (link-stalin-file c-file)
  (define o-file (<-> c-file ".o"))
  (if (not (= 0 (system (<-> "gcc " "-I" snd-header-files-path " " c-file " -shared -o " o-file " -fpic"))))
      (throw 'compilation-error))
  (delete-at-exit o-file)
  o-file)


(eval-c ""
	(<nonstatic-void*> rt_get_stack_address (lambda ()
                                                  (<int> dummy)
                                                  (return &dummy))))

(define (link-stalin-file c-file program)
  (define cleanup (rt-gensym))
  (c-display "c-file:" c-file)
  (apply eval-c-non-macro
         `(,(<-> "-I" snd-header-files-path " -I/home/kjetil/site/include" " -lpcl")
           #f

           "#include <rollendurchmesserzeitsammler.h>"
           "#include <pcl.h>"
           "#include <ucontext.h>"

           "#define fprintf(a,...) rt_debug(__VA_ARGS__)"
           "#define exit(a) myexit()"

           (<int> remove_me 0)

           (<int> g_startframe)
           (<int> g_endframe)           
           (<int> block_time 0)

           (<coroutine_t> dsp_coroutine)
           
           (<void*> heap)
           
           (<char*> start_dyn NULL)
           (<char*> end_dyn NULL)
           (<char*> stack_top NULL)
           (<char*> stack_bot NULL)
           
           (get-proto rt_debug)
           (get-proto rt_get_stack_address)

           ,bus-struct

           (<struct-rt_bus*> outbus (cast <struct-rt_bus*> ,(<-> (number->string (cadr (SCM_SMOB_DATA *out-bus*)))
                                                                 "UL")))
           (<struct-rt_bus*> inbus (cast <struct-rt_bus*> ,(<-> (number->string (cadr (SCM_SMOB_DATA *in-bus*)))
                                                                "UL")))

           (<float*> sounddata)
           
           (run-now
            (set! sounddata (calloc (sizeof <float>) (* ,*rt-block-size*
                                                        ,*rt-num-output-ports*))))

           (<void> clean_sounddata (lambda ()
                                     (memset sounddata 0 (* (sizeof <float>) 
                                                            ,*rt-block-size*
                                                            ,*rt-num-output-ports*))))

                   
           (<void> write_bus (lambda ((<int> ch)
                                      (<int> framenum)
                                      (<float> val))
                               (if (>= ch outbus->num_channels)
                                   return)
                               (<struct-rt_bus_data-*> data "&outbus->data[(outbus->num_channels*framenum)+ch]")
                               (+= data->val val) ;; outbus is nulled out before each block anyway.
                               ;;(if (< data->last_written_to block_time)
                               ;;    (set! data->val val)
                               ;;    (+= data->val val))
                               ;;(set! data->last_written_to block_time)
                               ))
           (<void> sounddata_to_bus (lambda ((<int> startframe)
                                             (<int> endframe))
                                      (for-each 0 ,*rt-num-output-ports*
                                                (lambda (ch)
                                                  (for-each startframe endframe
                                                            (lambda (framenum)
                                                              ,(<-> "write_bus(ch,framenum,"
                                                                    "sounddata[ch*" (number->string *rt-block-size*)
                                                                    "+framenum])")))))))

           (run-now
            (set! heap (tar_new_heap))
            
            (if (== 0 (tar_get_dynamic_roots_for (cast <char*> &heap) &start_dyn &end_dyn))
                (printf (string "Error. Could not find dynamic start and end. Not good.\\n")))
            )

           (<void> block_ready_internal (lambda ()
                                          (set! stack_bot (cast <char*> (rt_get_stack_address)))
                                          ;;(rt_debug (string "stack_bot: %p\\n") stack_bot)
                                          (co_resume)))

           ,@(if (or #t (= 0 (system (<-> "grep exit " c-file))))
                 '((<void> myexit (lambda ()
                                    (set! remove_me 1)
                                    (co_resume))))
                 '())

           ,@(get-stalin-ec-funcs program)
           
           ,(<-> "#include \"" c-file "\"")
           
           ;; public
           (functions->public
            (<int> process_func (lambda ((<void*> something)
                                         (<int> startframe)
                                         (<int> endframe))

                                  (when (== 0 startframe)
                                    (clean_sounddata))

                                  (when (and (== 0 startframe)
                                             (== false (tar_entering_audio_thread heap)))
                                    (fprintf stderr (string "Using too much CPU. Skipping\\n"))
                                    (return 0))

                                  (set! g_startframe startframe)
                                  (set! g_endframe endframe)
                                  (co_call dsp_coroutine)

                                  (when (and (== 0 remove_me)
                                             (== endframe ,*rt-block-size*)
                                             (tar_leave_audio_thread heap))

                                    (rt_debug (string "data: %d, stack: %d %p %p\\n")
                                              (abs (- end_dyn start_dyn))
                                              (abs (- stack_top stack_bot))
                                              stack_bot
                                              stack_top)
                                    
                                    (tar_add_root heap start_dyn end_dyn) ; data
                                    ;;(tar_add_root heap stack_bot stack_top) ; stack
                                    (tar_add_root heap (- stack_top 120000) stack_top) ; stack
                                    (tar_add_root heap dsp_coroutine (+ (cast <char*> dsp_coroutine)
                                                                        (EC_MAX (sizeof <ucontext_t>) ;registers
                                                                                (sizeof <jmp_buf>))))
                                    (tar_run_gc heap)
                                    )

                                  (sounddata_to_bus startframe endframe)

                                  (if (== endframe ,*rt-block-size*)
                                      (+= block_time ,*rt-block-size*))

                                  (return remove_me))))


           (<void> dsp_coroutine_func (lambda ((<void*> arg))
                                        (<int> dummy)
                                        (set! stack_top (cast <char*> &dummy))
                                        (schememain)))
           
           (run-now
            (set! dsp_coroutine (co_create dsp_coroutine_func NULL NULL ,*stalin-stack-size*))
            (co_call dsp_coroutine))
           
           (public
            (<void> ,cleanup (lambda ()
                               (tar_delete heap)
                               (co_delete dsp_coroutine)
                               ))
            (<void> callmain (lambda ()
                               (schememain)))
            )
           
           ))
  (list (process_func)
        cleanup))


#!
(link-stalin-file (get-stalin-c-file (schemecode->file (generate-stalin-code '((display (+ 2 3))(newline))))) '((rt_write_out_bus)))
(link-stalin-file (get-stalin-c-file (schemecode->file (generate-stalin-code '((display (+ 5 3))(newline))))))
(link-stalin-file (get-stalin-c-file (schemecode->file (generate-stalin-code '((display (+ 5 300))(newline))))))
(print_stuff)
(callmain)
!#  


(define (<rt-stalin-do> code)
  (catch 'compilation-error
         (lambda ()
           (fix-defines
            (define generated (generate-stalin-code
                               `( ,@code
                                  ((=> coroutine:_current-coroutine :continuation)))))

            ;;(check-stalin-syntax generated)
            (define funcs (link-stalin-file (get-stalin-c-file (schemecode->file generated))
                                            generated))
            (define realtime (<realtime> (car funcs) #f '()))
            (-> realtime play)
            realtime
            ))
         (lambda x
           #f)))



(define-macro (<rt-stalin> . code)
  (<rt-stalin-do> code))


#!
(define-stalin-macro (block . code)
  `(begin ,@code))
(begin schemecodeparser-varlist)

(define obj (<rt-stalin>
             (display (>> 15 1))
             (newline)
             (define phase 0.0)
             (let toploop ()
               (_block_ready)
               (let loop ((i 0))
                 (out (* 0.04 (sin phase)))
                 (set! phase (+ phase 0.062))
                 (if (< i 1024)
                     (loop (+ i 1))))
               (toploop))))

(<rt-stalin>
 (define-structure astruct b c)
 (make-astruct 2 3)
; (display (astruct-b))
; (display (astruct-c))
 (newline))

(-> obj dir)
(-> obj play)
(-> obj stop)
(rte-info)

(pretty-print (generate-stalin-code '((+ 2 3 (add 50 (add 90))))))
!#

