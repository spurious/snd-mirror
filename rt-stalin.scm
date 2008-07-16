
#!
(let ((time 5960)
      (n 10))
  (- n (remainder time n)))
!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *stalin-stack-size* (* 32 1024)) ;; total stack size (for safety)
(define *stalin-stack-limit* (* 4 1024))  ;; If using more than this, instrument is stopped. (checked every block)
(define *stalin-queue-max-size* 1024) ;; max number of coroutines.
(define *stalin-add-health-checks* #t)
(define *stalin-backtrace-length* 20)

(define *tar-atomic-heap-size* (* 1024 1024))
(define *tar-nonatomic-heap-size* (* 1024 1024))
(define *tar-roots-size* (* 1024 1024))

(define *rt-local-stalin-code-environment* (the-environment))


(define *rt-opt-stack-checks* #t)
(define *rt-opt-cpu-checks* #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; various ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (get-system-output command cont :key (print-output #t))
  (let* ((logfilename "/tmp/snd-ls-logtemp")
         (ret (if print-output
                  (system (<-> command " | tee " logfilename " && exit ${PIPESTATUS[0]}"))
                  (system (<-> command ">" logfilename))))
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
(find-stalin-funcs '(vct a b))
(pretty-print (generate-stalin-code '((vct a b))))
(define-stalin *var* 0)
(define-stalin (add2 a b)
  (+ a b *var* (add2 5 7)))
(define-stalin add (lambda (a b)
                     (+ a (add2 b 9))))
(pretty-print (generate-stalin-code0 '((+ 2 3 (add 50 (add 90))))))
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
 
;;(eval-c-etype->ctype (eval-c-get-known-type '<char-*>))

(define stalin-dont-rename-these '())

(define (define-stalin-ec-do ret-type name body)
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
      (push! name stalin-dont-rename-these)
      (add-stalin-func name `(define ,name (lambda ,(map cadr (cadr body))
                                             ( (foreign-procedure ,(map get-legal-stalin-type (map car (cadr body)))
                                                                  ,(get-legal-stalin-type ret-type)
                                                                  ,(symbol->string name))
                                               ,@(map (lambda (arg)
                                                        (let ((type (car arg))
                                                              (name (cadr arg)))
                                                          (cond ((memq type '(<float> <double>))
                                                                 `(exact->inexact ,name))
                                                                ((memq type '(<int> <short> <char>))
                                                                 `(inexact->exact (floor ,name)))
                                                                (else
                                                                 name))))
                                                      (cadr body))))))

      ;;(add-stalin-func name `(define ,name (foreign-procedure ,(map get-legal-stalin-type (map car (cadr body)))
      ;;                                                        ,(get-legal-stalin-type ret-type)
      ;;                                                        ,(symbol->string name))))
      ))
  ;;(c-display "adding something" name (get-stalin-ec-function name))
  (let ((dependents (rt-find-all-funcs body))
	(old (get-stalin-ec-function name)))
    (if old
	(set-cdr! (assq name stalin-ec-functions) (list dependents
                                                        (list ret-type name body)))
        (set! stalin-ec-functions (append! stalin-ec-functions
                                           (list (list name
                                                       dependents
                                                       (list ret-type name body))))))))
(define-macro (define-stalin-ec ret-type name body)
  `(define-stalin-ec-do ',ret-type ',name ',body))

;; why did I make this one?
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
(pretty-print stalin-ec-functions)
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

(define (define-stalin-macro-do def body)
  (if (pair? def)
      (hashq-set! stalin-macros (car def) (primitive-eval `(labamba-onymous ,(symbol->string (car def))
                                                                            ,(cdr def)
                                                                            ,@body)))
      (hashq-set! stalin-macros def (primitive-eval (car body)))))
(define-macro (define-stalin-macro def . body)
  `(define-stalin-macro-do ',def ',body))

#!
(define-stalin-macro testing (lambda (a) `(+ 2 ,a)))
(define-stalin-macro (testing a)
  `(+ 2 ,a))

(stalin-macroexpand '(testing 50))
(pretty-print (get-stalin-macro 'testing))
(pretty-print  (stalin-macroexpand '(wait-midi
                                      (spawn
                                        (wait-midi :cont #f
                                          (when (and (midi-stop?)
                                                     (= note
                                                        (midi-note)))
                                            (stop oscillator)
                                            #t))))))
(pretty-print  (stalin-macroexpand '(wait-midi
                                      (wait-midi
                                        50))))
                                        
!#

(define (get-stalin-macro name)
  (hashq-ref stalin-macros name))

(define (stalin-macroexpand-1 expr)
  (if (or (not (pair? expr))
	  (null? expr)
	  (not (symbol? (car expr))))
      expr
      (let ((qua (get-stalin-macro (car expr))))
	(if (not qua)
	    (begin
	      ;;(c-display "Error in expand-a-macro. Macro for " expr " Not found.")
	      expr)
            (catch #t
                   (lambda ()
                     (apply qua (cdr expr)))
                   (lambda x
                     (c-display (<-> "\"" (symbol->string (car expr)) "\":"))
                     (pretty-print (procedure-source qua))
                     (c-display (<-> "Error while expanding macro \"" (symbol->string (car expr))
                                     "\" in expression")
                                expr)
                     (c-display x)
                     (throw 'compilation-error)))))))


(define (stalin-macroexpand expr)
  (schemecodeparser expr
		    :elsefunc (lambda (expr)
                                ;;(when (and (eq? 'set! (car expr))
                                ;;           (not (pair? (cadr expr)))
                                ;;           (is-stalin-defined? (cadr expr)))
                                ;;  (c-display "Bindings defined using define-stalin can not be set!:"
                                ;;             expr)
                                ;;  (throw 'compilation-error))
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
(stalin-macroexpand '(quasiquote ((unqoute a) 0)))
(stalin-macroexpand '(quasiquote (((unquote a) 1))))

(define-stalin-macro (dosomething b :key (c 100))
  `(+ 1 ,b 2 ,c))
(stalin-macroexpand-1 '(dosomething 3 (symbol->keyword 'c) 2))
(pretty-print (stalin-macroexpand '(let-keywords* lambda*:G787 #f ((c 100)) (unquote c))))

(pretty-print (fix-stalin-keywords (stalin-macroexpand '(lambda* (:key (c "ai")) c))))
(fix-stalin-keywords (stalin-macroexpand '(debug (a :c "hello"))))

(<rt-stalin>
 (spawn
   (define a (lambda* (:key (c "ai"))
                      c))
   (debug (a :c "hello2"))))


!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; optargs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this needs more thinking. optargs should probably be implemented
;; using macros instead.
;;(load-from-path "stalin-optargs.scm")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Rt stalin functions and macros ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various

(define-stalin-macro (unquote something)
  (local-eval something *rt-local-stalin-code-environment*))

(define-stalin-macro (-> object command)
  `(,object ',command))
  
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
(define-stalin-macro (push! val where)
  (let ((ret (rt-gensym)))
    `(let ((,ret ,val))
       (set! ,where (cons ,ret ,where))
       ,ret)))
(define-stalin-macro (push-back! val where)
  (let ((ret (rt-gensym)))
    `(let ((,ret ,val))
       (set! ,where (append! ,where (list ,ret)))
       ,ret)))
(define-stalin-macro (!= a b)
  `(not (= ,a ,b)))

(define-stalin-macro (while test . body)
  (define loop (rt-gensym))
  (if (eq? #t test)
      `(let ,loop ()
         ,@body
         (,loop))
      `(let ,loop ()
            (if ,test
                (begin
                  ,@body
                  (,loop))))))

(define-stalin-macro (when cond . rest)
  `(cond (,cond ,@rest)
	 (else #f)))

(define-stalin-macro (range name das-start das-end . body)
  (define start (rt-gensym "start"))
  (define end (rt-gensym "end"))
  (define loop (rt-gensym "rangeloop"))
  `(let ((,start ,das-start)
         (,end ,das-end))
     (let ,loop ((,name ,start))
          (when (< ,name ,end)
            ,@body
            (,loop (1+ ,name))))))

(define-stalin-macro (call/cc a)
  `(call-with-current-continuation ,a))

(define-stalin (scale x x1 x2 y1 y2)
  (+ y1
     (/ (* (- x x1)
	   (- y2 y1))
	(- x2 x1))))



(define-stalin-ec <void> lowlevel_remove_me (lambda ()
                                              (myexit)))
(define-stalin (remove-me)
  (lowlevel_remove_me))


(define-stalin-ec <void> lowlevel_debug (lambda ((<char*> string))
                                          (rt_debug string)))
(define-stalin-ec <void> lowlevel_debug1 (lambda ((<char*> string)
                                                  (<int> a))
                                           (rt_debug string a)))
(define-stalin-ec <void> lowlevel_debug2 (lambda ((<char*> string)
                                                  (<int> a)
                                                  (<int> b))
                                           (rt_debug string a b)))

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

;; keywords
(define (make-stalin-keyword sym)
  (cons 'keyword sym))
(define-stalin (symbol->keyword sym)
  (cons 'keyword sym))

(define-stalin (keyword? key)
  (and (pair? key)
       (eq? 'keyword (car key))))
(define-stalin keyword->symbol cdr)

(define-stalin (flatten tree)
  (cond ((null? tree) '())
	((pair? (car tree))
	 (append (flatten (car tree))
		 (flatten (cdr tree))))
	(else
	 (cons (car tree) (flatten (cdr tree))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structures (providing same structure syntax for rt-stalin as guile and snd-rt)


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

  (for-each (lambda (slot)
              (push! (<_> 'SET- name (string->symbol "-") slot (string->symbol "!")) stalin-dont-rename-these)
              (push! (<_> 'name (string->symbol "-") slot) stalin-dont-rename-these))
            (map car slots))

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

(define-stalin-macro setter!-=> (lambda (object das-method . rest)
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
             `(,(append-various 'setter!- struct-name ":" method) ,object-name ,@rest)))))))

(define-stalin-macro => (lambda (object das-method . rest)
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
             `(,(append-various 'getter- struct-name ":" method) ,object-name)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sound stuff

(define-stalin-macro (infix-f t)
  t)
(define-stalin-macro (infix-b t)
  `(* ,*rt-block-size* ,t))
(define-stalin-macro (infix-ms t)
  `(inexact->exact (floor (/ (* ,t ,(rte-samplerate))
                             1000))))
(define-stalin-macro (infix-s t)
  `(* ,(inexact->exact (rte-samplerate)) ,t))
(define-stalin-macro (infix-m t)
  `(* ,(* 60 (inexact->exact (rte-samplerate))) ,t))
(define-stalin-macro (infix-h t)
  `(* ,(* 60 60 (inexact->exact (rte-samplerate))) ,t))

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



;; ADSR envelope

(define-stalin (make-adsr-do a d s r)
  (define in (rt_make_env (vct 0 0 a 1 (+ a d) s) (+ a d))) ;; Small optimization.
  ;;(define in (make-env `((0 0)(,a 1)(,(+ a d) ,s)) :dur (+ a d)))
  (define out (make-env '((0 1)(1 0)) :dur r))
  (define do-out #f)
  (define do-in #t)
  (define vol 0.0)
  (define time 0)
  (lambda cut
    (inc! time 1)
    (cond ((not (null? cut))
           (set! time 0)
           (set! do-out #t))
          (do-out
           (if (= time r)
               #f
               (* vol (env out))))
          (do-in
           (cond ((= time (+ a d))
                  (set! do-in #f)
                  (set! vol s))
                 (else
                  (set! vol (env in))))
           vol)
          (else
           vol))))
(define-stalin-macro (make-adsr :optkey a d s r)
  `(make-adsr-do ,a ,d ,s ,r))




;; CLM

#!
(make-oscil 4 34 23 23 )

(cdr (assq 'documentation (procedure-properties make-oscil)))
(cdr (assq 'documentation (procedure-properties make-env)))
(procedure-properties make-env)

(define *rt-temp-filename* (let ((ret (tmpnam)))
                             (delete-at-exit ret)
                             ret))

(define-macro (get-clm-proto function)
  `(let ()
     (define fd (open-file *rt-temp-filename* "w"))
     (write-line (cdr (assq 'documentation (procedure-properties ,function)))
                 fd)
     (close fd)
     (let* ((fd (open-file *rt-temp-filename* "r"))
            (ret (read fd)))
       (close fd)
       ret)))

(let ()
  (define a (get-clm-proto make-env))
  (cdr a))

(make-env '(2 3))

(define all-clm-generator-names
  (get-system-output "grep S_make clm-strings.h |awk '{print $3}'"
                     (lambda (output ret)
                       (map (lambda (string)
                              (string->symbol (substring string 5 (string-length string))))
                            (remove (lambda (string)
                                      (< (string-length string) 3))
                                    (string-split output (car (string->list "\""))))))
                     :print-output #f))

(begin all-clm-generator-names)

(define all-clm-constructor-names
  (get-system-output "grep S_make clm-strings.h |awk '{print $3}'"
                     (lambda (output ret)
                       (map (lambda (string)
                              (string->symbol string))
                            (remove (lambda (string)
                                      (< (string-length string) 3))
                                    (string-split output (car (string->list "\""))))))
                     :print-output #f))

(begin all-clm-constructor-names)

(pretty-print (map (lambda (func)
                     (get-clm-proto func))
                   (map primitive-eval 
                        (remove (lambda (name)
                                  (not (defined? name)))
                                all-clm-generator-names))))

(pretty-print (map (lambda (func)
                     (get-clm-proto func))
                   (map primitive-eval 
                        (remove (lambda (name)
                                  (not (defined? name)))
                                all-clm-constructor-names))))







(define a (read fd))
(load filename)

(substring "abcd" 1 2)

(begin read-string)

(define-stalin-ec <void*> make_oscil_ (lambda ((<float> a)
                                               (<float> b))
                                        (return (mus_make_oscil a b))))

(define-stalin-macro (make-oscil :key
                                 (frequency *clm-default-frequency*)
                                 (initial-phase 0.0)
                                 freq)
  `(make_oscil_ ,(or freq frequency) ,initial-phase))

(define-stalin-ec <float> oscil_ (lambda ((<void*> a)
                                          (<float> b)
                                          (<float> c))
                                   (return (mus_oscil (cast <mus_any*> a) b c))))

(define-stalin-macro (oscil os :key (fm-input 0.0) (pm-input 0.0))
  `(oscil_ ,os ,fm-input ,pm-input))


(stalin-macroexpand '(make-oscil :frequency 50 :initial-phase 4))
(stalin-macroexpand '(make-oscil :frequency 440))
(generate-stalin-code0 '((make-oscil :frequency 50 :initial-phase 4)))
(<rt-stalin>
 (let loop ((freq 150.0))
   (when (< freq 8200.0)
     (let ((osc (make-oscil :frequency freq)))
       (spawn-block
         (out (* 0.01 (oscil osc)))))
     (loop (+ freq 200.0)))))

(<rt-stalin>
 (let loop ((freq 100.0))
   (when (< freq 1600.0)
     (spawn
       (loop (+ freq 100.0))
       (let ((phase 0.0))
         (block
           (out (* 0.02 (sin phase)))
           (inc! phase (/ (* 3.14159 2.0 freq)
                          44100))))))))

(<rt-stalin>
 (let loop ((freq 100.0))
   (when (< freq 1600.0)
     (spawn
       ;;(debug (number->string freq))
       (let ((phase 0.0))
         (block
           (out (* 0.02 (sin phase)))
           (inc! phase (/ (* 3.14159 2.0 freq)
                          44100.0)))))
     (loop (+ freq 100.0)))))

(<rt-stalin>
 (spawn
   (let ((phase 0.0))
     (block
       (out (* 0.3 (sin phase)))
       (inc! phase (/ (* 3.14159 2.0 800)
                      44100.0))))))


;; Something is wrong. "hello hello3" is printed, but it should not be.
;; Also, if runned twice, it hangs the second time.
(<rt-stalin>
 (spawn
   (debug "hello hello0")
   (wait (* 3 44100))
   (debug "hello hello")
   (wait (* 3 44100)
         );         (lambda ()
  ;         (debug "hello hello2")))
   (debug "hello hello3")))

(<rt-stalin>
 (spawn
   (debug "start0")
   (wait (* 3 44100)
     (debug "start1")
     (wait (* 3 44100)
       (debug "start2")))))

(<rt-stalin>
 (spawn
   (debug "1c")
   (wait 3::s
     (debug "2c")
     )))

(<rt-stalin>
 (debug "1c")
 (wait 3::s
   (debug "2c")
   ))

(<rt-stalin>
 (spawn
   (debug "1c")
   (wait 3::s)
   (debug "2c")))

(<rt-out> (* 0.1 (oscil)))


(define (quick-fib n)
  (<rt-stalin>
   (debug (number->string
           (let fib ((n ,n))
             (if (< n 2)
                 n
                 (+ (fib (- n 1))
                    (fib (- n 2)))))))))

(quick-fib 40)

(define-stalin my-sqrt (foreign-procedure (float) float "sqrtf"))
(pretty-print (generate-stalin-code0 '((display (my-sqrt 50)))))

(<rt-stalin>
 (spawn
   (display (my-sqrt 50))))

!#


;; This is just a quick get-up-and-running implementation. More work is needed.
(for-each (lambda (clm-def)
            ;;(c-display "clm-def" clm-def)
            (let* ((name (car clm-def)) ;; make-oscil
                   (gen-name (string->symbol (substring (symbol->string name) 5 (string-length (symbol->string name))))) ;; oscil
                   (args (cdr clm-def))
                   (argnames (map (lambda x (rt-gensym)) (iota (length args))))
                   (fixed-args-list (map (lambda (arg)
                                           (let ((def 0)
                                                 (n #f))
                                             (if (pair? arg)
                                                 (begin
                                                   (set! n (car arg))
                                                   (if (not (null? (cdr arg)))
                                                       (set! def (primitive-eval (cadr arg)))))
                                                 (set! n arg))
                                             (if (keyword? n)
                                                 (set! n (keyword->symbol n)))
                                             (list n def)))
                                         args))
                   )

              (supereval
               (lambda (out)

                 (out "(define-stalin-ec <void*> make_" gen-name "_ (lambda (")
                 (for-each (lambda (arg)
                             (out `(<float> ,arg)))
                           argnames)
                 (out ")(return (mus_make_" gen-name " ")
                 (for-each (lambda (arg)
                             (out " " arg))
                           argnames)
                 (out "))))\n")
                 
                 (out "(define-stalin-macro (" name " :optkey ")
                 (for-each (lambda (arg)
                             (out arg " "))
                           fixed-args-list)
                 (out ")\n")
                 (out "  `(" 'make_ gen-name "_ ")
                 (for-each (lambda (arg)
                             (out "," (car arg) " "))
                           fixed-args-list)
                 (out "))\n")))))
          clm-constructor-protos)
#!
(pretty-print (get-stalin-macro 'make-waveshape))
(stalin-macroexpand-1 '(make-waveshape))
(stalin-macroexpand '(make-waveshape))
(stalin-macroexpand '(make-oscil :frequency 400))
(stalin-macroexpand '(make-env))
(pretty-print (get-stalin-macro 'make-waveshape))
(pretty-print (get-stalin-ec-function 'make_waveshape_))
(pretty-print (get-stalin-ec-function 'make_oscil_))
(begin stalin-ec-functions)
(get-stalin-func 'make_delay_)

(define-stalin-macro (make-waveshape :optkey
                                     (frequency *clm-default-frequency*)
                                     (partials (quote (1 1)))
                                     (size clm-table-size)
                                     (wave 0) )
  `(make_waveshape_,frequency ,partials ,size ,wave ))

!#


(for-each (lambda (clm-gen)
            ;;(c-display "gen" clm-gen)
            (let* ((name (car clm-gen)) ;; oscil
                   (args (cadr clm-gen))
                   (argnames (map (lambda x (rt-gensym)) (iota (length args))))
                   )
              ;;(c-display (<_> name '_))
              (define-stalin-ec-do '<float> (<_> name '_)
                `(lambda ,(cons '(<void*> generator)
                                (map (lambda (argname)
                                       `(<float> ,argname))
                                     argnames))
                   (return (,(<_> 'mus_ name) (cast <mus_any*> generator) ,@argnames))))
              ))
          rt-clm-generators)

(for-each (lambda (clm-gen)
            (let* ((name (car clm-gen)) ;; oscil
                   (args (cadr clm-gen))                   
                   (argnames (map (lambda x (rt-gensym)) (iota (length args)))))
              (supereval
               (lambda (out)
                 (out "(define-stalin-macro (" name " generator ")
                 (for-each (lambda (must-arg)
                             (out must-arg " "))
                           (remove pair? args))
                 (when (not (equal? args (remove pair? args)))
                   (out ":optkey ")
                   (for-each (lambda (opt-arg)
                               (out opt-arg " "))
                             (%filter pair? args)))
                 (out ")\n")
                 (out "`(" name "_ ,generator ")
                 (for-each (lambda (arg)
                             (if (pair? arg)
                                 (out " ," (car arg))
                                 (out " ," arg)))
                           args)
                 (out "))")))))
          rt-clm-generators)

#!
(pretty-print (get-stalin-ec-function 'oscil_))
(stalin-macroexpand '(oscil gen 3))
(pretty-print (get-stalin-ec-function 'make_oscil_))
!#

;; Add the freq argument.
(define-stalin-macro (make-oscil :optkey
                                 (frequency *clm-default-frequency*)
                                 (initial-phase 0.0)
                                 freq)
  `(make_oscil_ ,(or freq frequency) ,initial-phase))



(define-stalin-ec <int> irandom (lambda ((<int> high))
                                  (return (mus_irandom high))))

;; conflict with stdlib/random
(define-stalin-ec <float> mus_random_ (lambda ((<float> high))
                                        (return (mus_random high))))
(define-stalin (random a)
  (mus_random_ a))

(define-stalin (ibetween a b)
  (+ a (irandom (- b a))))
(define-stalin (between a b)
  (+ a (random (- b a))))



;;;;; vct (quick up-and-running. More work needed)

(define-stalin-ec <void*> rt_alloc_vct (lambda ((<int> length))
                                         (let* ((ret <vct-*> (tar_alloc heap (sizeof <vct>)))
                                                (floats <float-*> (tar_alloc_atomic heap (* (sizeof <float>) length))))
                                           (set! ret->length length)
                                           (set! ret->data floats)
                                           (memset floats 0 (* (sizeof <float>) length))
                                           (return ret))))
(define-stalin (make-vct len)
  (rt_alloc_vct len))

(define-stalin-ec <void> rt_vct_set (lambda ((<void*> vvct)
                                             (<int> pos)
                                             (<float> val))
                                      (<vct-*> das_vct (cast <vct-*> vvct))
                                      (set! das_vct->data[pos] val)))
(define-stalin (vct-set! vct pos val)
  (rt_vct_set vct pos val))

(define-stalin-macro (vct . values)
  (define len (length values))
  (define vct (rt-gensym))
  `(let ((,vct (make-vct ,len)))
     ,@(map (lambda (i val)
              (if (number? val)
                  (set! val (exact->inexact val)))
              `(vct-set! ,vct ,i ,val))
            (iota len)
            values)
     ,vct))

(define-stalin (vct . values)
  (let ((vct (make-vct values))
        (i -1))
    (for-each (lambda (val)
                (vct-set! vct (inc! i 1) val))
              values)
    vct))
  


;;;;; env (quick up-and-running. More work needed)

(define-stalin-ec <void*> rt_make_env (lambda ((<void*> vvct)
                                               (<int> duration))
                                        (<vct*> das_vct (cast <vct-*> vvct))
                                        (<void*> ret (mus_make_env das_vct->data
                                                                   (/ das_vct->length 2)
                                                                   1.0 0 1.0 0
                                                                   duration ;; end
                                                                   NULL))
                                        (when (== NULL ret)
                                          (myerror (string "mus_make_env returned NULL")))
                                        (return ret)))
                                          

(define-stalin (make-env-do data dur)
  (define vct (make-vct (length data)))
  (define i -1)
  (for-each (lambda (val)
              (vct-set! vct (inc! i 1) val))
            data)
  (rt_make_env vct dur))

(define-stalin (make-env-do-pairs data dur)
  (define vct (make-vct (* 2 (length data))))
  (define i -1)
  (for-each (lambda (val)
              (vct-set! vct (inc! i 1) (car val))
              (vct-set! vct (inc! i 1) (cadr val)))
            data)
  (rt_make_env vct dur))

(define-stalin (make-env-parse-data-at-runtime data dur)
  (if (pair? (car data))
      (make-env-do-pairs data dur)
      (make-env-do  data dur)))

(define-stalin-macro (make-env-constant-data data dur)
  `(rt_make_env (vct ,@(flatten data)) ,dur))

(define-stalin-macro (make-env data :optkey duration dur end)
  (define das-dur (or (and duration `(infix-s ,duration))
                      dur
                      end
                      0))
  (let ()
    (define das-data (stalin-macroexpand data))
    (cond ((not (pair? das-data))
           `(make-env-parse-data-at-runtime ,das-data ,das-dur))
          ((eq? 'quote (car das-data))
           `(make-env-constant-data ,(cdr das-data) ,das-dur))
          (else
           `(make-env-parse-data-at-runtime ,das-data ,das-dur)))))

#!
(stalin-macroexpand '(make-env `((0 0)(,a 1)(,(+ a d) ,s)) :dur (+ a d)))
(stalin-macroexpand '(make-env `((,a 1))))

(stalin-macroexpand '(make-env `((0 ,a))))

(make-env '(1 2 3))
(make-env (list a b c d))
(make-env )

(pretty-print (stalin-macroexpand '(make-env '((0 1) (2 4)) :dur 900)))
(pretty-print (stalin-macroexpand '(make-env `(aiai 2 3) :dur 9)))

(<rt-stalin>
 (define das-env (make-env '(0 0.5 1 1) :end 5:-ms))
 (debug (number->string (inexact->exact (floor (* 1000000.0 (env das-env))))))
 (debug (number->string (inexact->exact (floor (* 1000000.0 (env das-env))))))
 (debug (number->string (inexact->exact (floor (* 1000000.0 (env das-env))))))
 ;;(debug (number->string (env das-env))))

 )





!#

;;;;; Alsa midi

;; midi-to-freq made by looking at the pd source
(define-stalin (midi-to-freq freq)
  (cond ((<= freq 0) 0)
        ((> freq 135) 20000)
        (else
         (* 8.17579891564 (exp (* .0577622650 freq))))))

(add-stalin-ec 'midi_eventnum '(<int> midi_eventnum 0))
(add-stalin-ec 'midi_control '(<int> midi_control[500] {0}))
(add-stalin-ec 'midi_data1 '(<int> midi_data1[500] {0}))
(add-stalin-ec 'midi_data2 '(<int> midi_data2[500] {0}))

(define-stalin-ec <void> rt_receive_midi_callback
  (lambda ((<void*> aNULL)
           (<int> control)
           (<int> data1)
           (<int> data2))
    (when (== midi_eventnum 500)
      (rt_debug (string "Error, midi buffer full. Event lost. (this is not supposed to happen)\\n")) ;; should be impossible.
      return)
    (set! midi_control[midi_eventnum] control)
    (set! midi_data1[midi_eventnum] data1)
    (set! midi_data2[midi_eventnum] data2)
    midi_eventnum++))

(add-stalin-ec-binding 'rt_receive_midi_callback 'midi_eventnum)
(add-stalin-ec-binding 'rt_receive_midi_callback 'midi_control)
(add-stalin-ec-binding 'rt_receive_midi_callback 'midi_data1)
(add-stalin-ec-binding 'rt_receive_midi_callback 'midi_data2)

(define-stalin-ec <int> _rt_receive_midi
  (lambda ()
    (rt_receive_midi NULL block_time
                     (cast <snd_seq_t*> ,(<-> (number->string (cadr *rt-midi*)) "UL"))
                     rt_receive_midi_callback)
    (return midi_eventnum)))

(add-stalin-ec-binding '_rt_receive_midi 'rt_receive_midi_callback)
(add-stalin-ec-binding '_rt_receive_midi 'midi_eventnum)
(add-stalin-ec-binding '_rt_receive_midi 'block_time)

(define-stalin-ec <int> _rt_get_midi_control
  (lambda ((<int> num))
    (return midi_control[num])))
(add-stalin-ec-binding '_rt_get_midi_control 'midi_control)

(define-stalin-ec <int> _rt_get_midi_data1
  (lambda ((<int> num))
    (return midi_data1[num])))
(add-stalin-ec-binding '_rt_get_midi_data1 'midi_data1)

(define-stalin-ec <int> _rt_get_midi_data2
  (lambda ((<int> num))
    (return midi_data2[num])))
(add-stalin-ec-binding '_rt_get_midi_data2 'midi_data2)

(define-stalin-ec <void> _rt_reset_midi
  (lambda ()
    (set! midi_eventnum 0)))
(add-stalin-ec-binding '_rt_reset_midi 'midi_eventnum)

(define-stalin-ec <void> _rt_snatch_midi
  (lambda ((<int> num))
    (set! midi_control[num] -1)))
(add-stalin-ec-binding '_rt_snatch_midi 'midi_control)

(define-stalin-ec <void> _rt_unsnatch_midi
  (lambda ((<int> num)
           (<int> control))
    (set! midi_control[num] control)))
(add-stalin-ec-binding '_rt_unsnatch_midi 'midi_control)

(define-stalin (_rt-is-midi-snatched? control)
  (= -1 control))


(define-stalin _curr-midi-control 0)
(define-stalin _curr-midi-data1 0)
(define-stalin _curr-midi-data2 0)


(define-stalin _num-waiting-midi-messages 0)

(define-stalin _last-midi-receive-time -1)


;; wait-midi/wait-midi-do is a bit messy. I had big problems
;; making it produce code which stalin was able to tail-optimize
;; without using -fully-convert-to-CPS
(define-stalin (wait-midi-do check body)

  (when (not (= _time _last-midi-receive-time))
    ;;(lowlevel_debug2 "time: %d %d" _time _last-midi-receive-time)
    (_rt_reset_midi)
    (set! _num-waiting-midi-messages (_rt_receive_midi))
    (set! _last-midi-receive-time _time))
  
  (let loop ((num 0))
    (cond ((< num _num-waiting-midi-messages)
           (let ((control (_rt_get_midi_control num)))
             (cond ((not (_rt-is-midi-snatched? control))
                    (_rt_snatch_midi num)
                    (let ((data1 (_rt_get_midi_data1 num))
                          (data2 (_rt_get_midi_data2 num)))
                      (cond ((check control data1 data2)
                             (set! _curr-midi-control control)
                             (set! _curr-midi-data1   data1)
                             (set! _curr-midi-data2   data2)
                             (body control data1 data2)
                             #t) ;; Return val
                            (else
                             (_rt_unsnatch_midi num control)
                             (loop (1+ num))))))
                   (else
                    (loop (1+ num))))))
          (else
           #f)))) ;; Return val
       

(define-stalin-macro (wait-midi :key 
                                 (command #t)
                                 note
                                 :rest rest)
  (define control (rt-gensym "control"))
  (define data1 (rt-gensym "data1"))
  (define data2 (rt-gensym "data2"))
  (define return (rt-gensym "return-from-midi"))
  (define code rest)
  `(call/cc
    (lambda (,return)
      (let loop ()
        (if (wait-midi-do (lambda (,control ,data1 ,data2)
                            ,(cond ((eq? command 'note-on)
                                    `(_midi-play? ,control ,data2))
                                   ((eq? command 'note-off)
                                    `(and (_midi-stop? ,control ,data2)
                                          (= ,data1 ,note)))))
                          (lambda (_curr-midi-control
                                   _curr-midi-data1
                                   _curr-midi-data2)
                            ,@code))
            (,return #t)
            (wait-synch 1:-b
               (loop)))))))



(define-stalin-macro (_midi-play? :optkey
                                 (control '_curr-midi-control)
                                 (data2 '_curr-midi-data2))
  `(and (>= ,control #x90)
        (<  ,control #xa0)
        (>  ,data2 0)))

(define-stalin-macro (midi-play?)
  `(_midi-play?))

(define-stalin-macro (_midi-stop? :optkey
                                  (control '_curr-midi-control)
                                  (data2 '_curr-midi-data2))
  `(and (>= ,control #x80)
        (< ,control #xa0)
        (or (< ,control #x90)
            (= 0 ,data2))))

;;(stalin-macroexpand '(_midi-stop? 50 60))

(define-stalin-macro (midi-stop?)
  `(_midi-stop?))

(define-stalin-macro (midi-note)
  '_curr-midi-data1)
(define-stalin-macro (_midi-vol :optkey (data2 '_curr-midi-data2))
  `(/ (exact->inexact ,data2) 128.0))
(define-stalin-macro (midi-vol)
  `(_midi-vol))

#!
(<rt-stalin>
 (_rt_receive_midi))

!#


#!
;; stack grows forever. (but not anymore)
(<rt-stalin>
 (let loop ()
   (wait (irandom 50):-ms
     (spawn
       (define osc (make-oscil :frequency (+ 50 (irandom 900))))
       (define duration (+ 0 (+ 400 (irandom 2000)):-ms))
       (define i 0)
       (block :duration duration :cont #f
         (if (< i (>> duration 1))
             (out (* (scale i 0 (>> duration 1) 0.0 0.01)
                     (oscil osc)))
             (out (* (scale i (>> duration 1) duration 0.01 0.0)
                     (oscil osc))))
         (inc! i 1)))
     (loop))))

;; stack does not grow forever.
(<rt-stalin>
 (let loop ()
   (wait (irandom 500):-ms)
   (spawn
     (define osc (make-oscil :frequency (+ 50 (irandom 900))))
     (define duration (+ 0 (+ 400 (irandom 2000)):-ms))
     (define i 0)
     (block :duration duration :cont #f
       (if (< i (>> duration 1))
           (out (* (scale i 0 (>> duration 1) 0.0 0.01)
                   (oscil osc)))
           (out (* (scale i (>> duration 1) duration 0.01 0.0)
                   (oscil osc))))
       (inc! i 1)))
   (loop)))

(<rt-stalin>
 (let loop ()
   (spawn :wait (irandom 50):-ms
     (loop)
     (let ((osc (make-oscil :frequency (+ 50 (irandom 900))))
           (duration (+ 0 (+ 400 (irandom 2000)):-ms))
           (i 0))
       (block :iter duration :cont #f
         (if (< i (>> duration 1))
             (out (* (scale i 0 (>> duration 1) 0.0 0.01)
                     (oscil osc)))
             (out (* (scale i (>> duration 1) duration 0.01 0.0)
                     (oscil osc))))
         (inc! i 1))))))

(<rt-stalin>
 (let loop ()
   (wait (irandom 50):-ms)
   (spawn
     (define osc (make-oscil :frequency (+ 50 (irandom 900))))
     (define duration (+ 0 (+ 400 (irandom 2000)):-ms))
     (define i 0)
     (block :iter duration :cont #f
       (if (< i (>> duration 1))
           (out (* (scale i 0 (>> duration 1) 0.0 0.01)
                   (oscil osc)))
           (out (* (scale i (>> duration 1) duration 0.01 0.0)
                   (oscil osc))))
       (inc! i 1)))
   (loop)))

(<rt-stalin>
 (let loop ()
   (wait 1:-s
     (spawn
       (define osc (make-oscil :frequency (ibetween 50 900)))
       (define duration (+ 0 (+ 400 (irandom 2000)):-ms))
       (define i 0)
       (block :iter duration :cont (lambda ())
         (if (< i (/ duration 2))
             (out (* (scale i 0 (/ duration 2) 0 0.1)
                     (oscil osc)))
             (out (* (scale i (/ duration 2) duration 0.1 0.0)
                     (oscil osc))))
         (inc! i 1)))
     (loop))))

(<rt-stalin>
 (let loop ()
   (wait 1:-s)
   (spawn
     (define osc (make-oscil :frequency (+ 50 (irandom 900))))
     (define duration (+ 0 (+ 400 (irandom 2000)):-ms))
     (define i 0)
     (block :iter duration :cont (lambda ())
       (if (< i (/ duration 2))
           (out (* (scale i 0 (/ duration 2) 0 0.1)
                   (oscil osc)))
           (out (* (scale i (/ duration 2) duration 0.1 0.0)
                   (oscil osc))))
       (inc! i 1)))
   (loop)))

(<rt-stalin>
 (let loop ()
   (spawn :wait (irandom 50):-ms
     (loop)
     (define osc (make-oscil :frequency (between 50 1000)))
     (block :duration (ibetween 400 2000):-ms :cont #f
       (out (* (if (< (block-iterator)
                      (<< (block-duration) 1))
                   (scale (block-iterator) 0                       (<< (block-duration) 1) 0.0 0.01)
                   (scale (block-iterator) (<< (block-duration) 1) (block-duration)        0.01 0.0))
               (oscil osc)))))))


(<rt-stalin>
 (let loop ()
   (wait 2000 ;:-ms;(random 50):-ms
     (lowlevel_debug1 "hepp %d" (inexact->exact (/ _time 44100)))
     (spawn
       (define osc (make-oscil :frequency (between 50 1000)))
       (block :duration (between 200 4000):-ms :cont #f
         (out (* (scale (block-iterator)
                        0 (block-duration)
                        0.0 0.01)
                 (oscil osc))))))
   (debug "hmm?")
   (loop)))


;; BUG. The while loop is not supposed to be called again since wait takes
;; over the continuation.
(<rt-stalin>
 (while #t
   (wait 2000 ;:-ms;(random 50):-ms
     (lowlevel_debug1 "hepp %d" (inexact->exact (/ _time 44100)))
     (spawn
       (define osc (make-oscil :frequency (between 50 1000)))
       (block :duration (between 200 4000):-ms :cont #f
         (out (* (scale (block-iterator)
                        0 (block-duration)
                        0.0 0.01)
                 (oscil osc))))))))
;     (block :duration (between 200 1000):-ms
;       (out (* (scale (block-iterator)
;                      0 (block-duration)
;                      0.01 0.0)
;               (oscil osc)))))))

(<rt-stalin>
 (while #t
   (wait (irandom 50):-ms)
   (spawn
     (define osc (make-oscil :frequency (between 50 1000)))
     (block-out :duration (between 200 1000):-ms
       (* (scale (block-iterator)
                 0 (block-duration)
                 0.0 0.01)
          (oscil osc)))
     (block-out :duration (between 200 1000):-ms
       (* (scale (block-iterator)
                 0 (block-duration)
                 0.01 0.0)
          (oscil osc))))))

(<rt-stalin>
 (while #t
   (wait (irandom 50):-ms)
   (spawn
     (define osc (make-oscil :frequency (between 50 1000)))
     (define duration (ibetween 400 2000))
     
     (block-out :duration (between 200 1000):-ms
       (* (scale (block-iterator)
                 0 (block-duration)
                 0.0 0.01)
          (oscil osc)))
     (block-out :duration (between 200 1000):-ms
       (* (scale (block-iterator)
                 0 (block-duration)
                 0.01 0.0)
          (oscil osc))))))

(<rt-stalin>
 (let loop ()
   (wait (irandom 50):-ms
     (spawn
       (define osc (make-oscil :frequency (ibetween 100 1000)))
       (define duration (ibetween 400 2000):-ms)
       (define e (make-env '(0 0 0.5 0.05 1 0) :end duration))
       (block :duration duration :cont #f
         (out (* (env e)
                 (oscil osc)))))
     (loop))))

(<rt-stalin>
 (let loop ()
   (spawn :wait (irandom 30):-ms
     (loop)
     (define osc (make-oscil :frequency (ibetween 50 2000)))
     (define duration (ibetween 400 2000):-ms)
     (define e (make-env '(0 0 0.5 0.05 1 0) :end duration))
     (block :dur duration :cont #f
       (out (* (env e)
               (oscil osc)))))))


(/ 800.0 15.0)


(<rt-stalin>
 (oscil 2 3 4 5 6 7))

(begin '(a (b c)::ms))
(begin '(a (b c) :ms))

(symbol? (caddr '(wait (irandom 50)::ms)))
(load "/tmp/temp.scm")


(<rt-stalin>
 (spawn
   (define osc (make-oscil :frequency 300))
   (block :iter 1000000 :cont (lambda ())
     (out (* 0.4 (oscil osc)))))
 (spawn
   (define osc (make-oscil :frequency 500))
   (block :iter 1000000 :cont (lambda ())
     (out (* 0.4 (oscil osc))))))


;; crash. Changing :wait in the second spawn to 0, or very low, fixes it.
;; seems like osc dissapears.
(<rt-stalin>
 (let loop ()
   (spawn :wait (between 200 1000):-ms
     (loop)
     (define osc (make-oscil :frequency (between 200 1000)))
     (define duration (between 400 1000):-ms)
     ;(spawn
     ;  (block :dur duration :cont #f
     ;    (out (* (scale (block-iterator) 0 (block-duration) 0.0 0.04)
     ;            (oscil osc)))))
     (spawn :wait duration
       ;;(define osc (make-oscil :frequency 300))
       (block :dur duration :cont #f
         (out (* 0.02;(scale (block-iterator) 0 (block-duration) 0.04 0.0)
                 (oscil osc)))))
     (debug "hmm")
     (neverending-scheduling))))


(<rt-stalin>
 (wait-midi
   (debug (number->string  _curr-midi-control))
   (debug (number->string  _curr-midi-data1))
   (debug (number->string  _curr-midi-data2))))

(<rt-stalin>
 (let loop ()
   (wait-midi
     (when (midi-play?)
       (let ((note (midi-note)))

         ;; Spawn a simple oscillator
         (define oscillator (spawn
                              (let ((osc (make-oscil :frequency (midi-to-freq note))))
                                (block
                                  (out (* 0.2 (oscil osc)))))))
         
         ;; Spawn a job waiting for a stop message for this note.
         (spawn
           (wait-midi :cont #f
             (when (and (midi-stop?)
                        (= note
                           (midi-note)))
               (stop oscillator)
               #t))) ;; Got it. Stop waiting for more midi.
         
         #t))))) ;; Got it.

(pretty-print (get-stalin-macro 'make-oscil))


(lambda ()
  (lowlevel_debug "starting")
  (call-with-current-continuation
   (lambda (return_40)
     (wait-do_-11
      (* 44100 1)
      (lambda ()
        (return_40 #t)))))
  (lowlevel_debug "one second later")
  (neverending-scheduling_-3)))))
=>
(lambda ()
  (lowlevel_debug "starting")
  (let ((return_40 (lambda (_rt_gen980)
                     (lowlevel_debug "one second later")
                     (neverending-scheduling_-3))))
    (wait-do_-11
     (* 44100 1)
     (lambda ()
       (return_40 #t)))))


(let ((a (call/cc (lambda (return)
                    (return "hmm")))))
  (debug a)
  (nevernding-scheduling))
=>
... (not important)



!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coroutines

(define-stalin-struct coroutine
  :time
  :stop-me
  :continuation)


;; Make sure gcc does tail call optimization.
(define-stalin (neverending-scheduling)
  (_run-scheduler neverending-scheduling))

(define-stalin _coroutine-dummy (make-coroutine 0 #f neverending-scheduling))

(define-stalin _current-coroutine
  (make-coroutine 0 
                  #f
                  (lambda ()
                    (let loop ()

                      (when (and (= _queue-size 0)
                                 (= _block-queue-size 0))
                        (remove-me))
                      
                      ;;(debug "loop called")
                      (_block_ready)

                      (set! _block-time (_get_block_time))
                      (set! _time (+ _block-time
                                     (_get_startframe)))
                      (insert-coroutine-in-queue! _current-coroutine
                                                  (+ _block-time
                                                     (_get_endframe))
                                                  0)
                      (_run-scheduler loop))
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


;; insert_coroutine_in_queue  (O(log n) efficiency)
;; ************************************************
;;
;; Returns false in case the priority queue is full.
(define-stalin (insert-coroutine-in-queue! coroutine time priority)

  (define queue _queue)

  (if (>= _queue-size
          (- ,*stalin-queue-max-size* 2))
      (error "coroutine queue full. Increase *stalin-queue-max-size* to increase the queue size."))

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

  (if (>= _block-queue-size
          (- ,*stalin-queue-max-size* 2))
      (error "block coroutine queue full."))

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

(define-stalin (_switch-to-coroutine coroutine _continuation)
  (set! (=> coroutine:_current-coroutine :continuation) _continuation)
  (set! _current-coroutine coroutine)
  (set! _time (=> coroutine :time))
  ((=> coroutine :continuation)))

(define-stalin (_run-scheduler _continuation)
  (let ((next (get-first-coroutine-in-queue)))
    (cond ((=> coroutine:next :stop-me)
           (_run-scheduler _continuation))
          ((not (eq? next _current-coroutine))  ;; Small optimization only.
           (_switch-to-coroutine next _continuation))
          (else
           (set! _time (=> coroutine:next :time))
           (_continuation)))))

(define-stalin (yield-do _continuation)
  (insert-coroutine-in-queue! _current-coroutine 
                              _time
                              2) ;; lower priority than wait, but higher than block.
  (_run-scheduler _continuation))

(define-stalin-macro (yield . code)
  (if (null? code)
      `(call/cc (lambda (return)
                  (yield-do (lambda ()
                              (return #t)))))
      `(yield-do (lambda ()
                   ,@code
                   (neverending-scheduling)
                   ))))



(define-stalin (wait-do n _continuation)
  (inc! _time (max 0 (floor (inexact->exact n))))
  (if (>= _time ;; Don't want to schedule unnecessarily.
          _next-scheduled-time)
      (begin
        (insert-coroutine-in-queue! _current-coroutine
                                    _time
                                    1) ;; higher priority than block, but less than main.
        (_run-scheduler _continuation))
      (_continuation)))

(define-stalin-macro (wait n . code)
  (define return (rt-gensym "return"))
  (if (null? code)
      `(call/cc (lambda (,return)
                  (wait-do ,n (lambda ()
                                (,return #t)))))
      `(wait-do ,n (lambda ()
                     ,@code
                     (neverending-scheduling)
                     ))))

(define-stalin-macro (wait-synch n . code)
  (define das-n (rt-gensym "n"))
  `(let* ((,das-n ,n))
     (wait (- ,das-n (remainder _time ,das-n))
       ,@code)))


(define-stalin (spawn-do time thunk)
  (let ((coroutine (make-coroutine 0 ;; The time value is set in insert-coroutine-in-queue!
                                   #f
                                   thunk)))
    (insert-coroutine-in-queue! coroutine
                                time
                                1)
    coroutine))
  
(define-stalin-macro (spawn :key (wait 0) :rest code)
  `(spawn-do ,(if (and (number? wait)
                       (= 0 wait))
                  '_time
                  `(+ _time
                      (max 0 (inexact->exact (floor ,wait)))))
             (lambda ()
               ,@code
               (neverending-scheduling))))

(define-stalin (stop . coroutine)
  (set! coroutine
        (if (null? coroutine)
            _current-coroutine
            (car coroutine)))
  (set! (=> coroutine :stop-me) #t))

(define-stalin-macro (stop :optkey (coroutine '_current-coroutine))
  `(set! (=> ,(symbol-append 'coroutine: coroutine) :stop-me) #t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Block (inner loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-stalin-macro (block-iterator)
  '_rt-block-iterator)
(define-stalin-macro (block-duration)
  '_rt-block-duration)

;; block
;; *****
(define-stalin-macro (block :key
                            dur
                            duration
                            :rest rest)
  (define the-end (rt-gensym))
  (define next (rt-gensym))
  (define diff (rt-gensym))
  (define loop (rt-gensym))
  (define das-duration #f)
  (define code rest)
  (define cont #f)

  (when (or duration dur)
    (set! das-duration `(inexact->exact (floor ,(or dur duration)))))

  (when (equal? (car code) :cont)
    (set! cont (cadr code))
    (if (not cont)
        (set! cont '(lambda ())))
    (set! code (cddr code)))

  (when (keyword? (car code))
    (c-display "Unknown keyword" (car code) "to block.")
    (throw 'compilation-error))

  (cond ((and das-duration cont)
         `(let* ((_rt-block-duration ,das-duration)
                 (,the-end (+ _time _rt-block-duration))
                 (_rt-block-iterator 0))
            (let ,loop ()
                 (if (< _rt-block-iterator
                        _rt-block-duration)
                     (let* ((,next (min  _next-scheduled-time
                                         ,the-end))
                            (,diff (- ,next _time)))
                       (while (< _time
                                 ,next)
                         ,@code
                         (inc! _rt-block-iterator 1)
                         (inc! _time 1))
                       
                       ;;(inc! _rt-block-iterator ,diff)
                       
                       ;; Now do an extra-low-priority yield:
                       (insert-coroutine-in-block-queue! _current-coroutine
                                                         _time
                                                         3)
                       (_run-scheduler ,loop))
                     (begin
                       (,cont)
                       (neverending-scheduling)
                       )))))
        
        (das-duration
         `(let* ((_rt-block-duration ,das-duration)
                 (,the-end (+ _time _rt-block-duration))
                 (_rt-block-iterator 0))
            (call/cc (lambda (return)
                       (let ,loop ()
                            (if (< _rt-block-iterator
                                   _rt-block-duration)
                                (let* ((,next (min  _next-scheduled-time
                                                    ,the-end))
                                       (,diff (- ,next _time)))
                                  (while (< _time
                                            ,next)
                                    ,@code
                                    (inc! _rt-block-iterator 1)
                                    (inc! _time 1))
                                  
                                  ;;(inc! _rt-block-iterator ,diff)
                                  
                                  ;; Now do an extra-low-priority yield:
                                  (insert-coroutine-in-block-queue! _current-coroutine
                                                                    _time
                                                                    3)
                                  (_run-scheduler ,loop))
                                (return #t)))))))
        (else
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
               (_run-scheduler ,loop)))))


(define-stalin-macro (spawn-block :allow-other-keys :rest rest)
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
       (block :duration ,duration
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



(pretty-print (generate-stalin-code0 '((get-first-coroutine-in-queue))))
(pretty-print (generate-stalin-code0 '((spawn
                                        (debug "hello"))
                                      (yield)
                                      (let loop ()
                                        (_block_ready)
                                        (loop)))))

(pretty-print (generate-stalin-code0
               '((define-structure astruct slot1 slot2))))

(pretty-print (macroexpand '(define-stalin-struct astruct :a :b)))
(define-stalin-struct astruct :a :b)

(pretty-print (generate-stalin-code0
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
   (yield)
   (debug "a2"))
 (spawn
   (debug "b1")
   (yield)
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
        ...))
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

(define stalin-noreturn-funcs '())

(define (find-stalin-expr-outcomes expr)
  (cond ((symbol? expr)
         (list expr))
        ((not (pair? expr))
         (list 'nonsymbol))
        ((eq? 'if (car expr))
         (if (= 3 (length expr))
             (cons 'undefined 
                   (find-stalin-expr-outcomes (nth 2 expr)))
             (append (find-stalin-expr-outcomes (nth 2 expr))
                     (find-stalin-expr-outcomes (nth 3 expr)))))
        ((and (eq? 'let (car expr))
              (symbol? (nth 1 expr)))
         (find-stalin-code-outcomes (cdddr expr)))
        ((memq (car expr) '(let let* letrec))
         (find-stalin-code-outcomes (cddr expr)))
        ((eq? 'lambda (car expr))
         `((_stalin-function ,expr)))
        ((pair? (car expr))
         (find-stalin-expr-outcomes (car expr)))
        ((eq? 'begin (car expr))
         (find-stalin-code-outcomes (cdr expr)))
        (else
         (list (list (car expr))))))
#!
(find-stalin-expr-outcomes '(lambda () 50))
(find-stalin-expr-outcomes '((a)))
(find-stalin-expr-outcomes '(if a b (if 2 (c 2 3))))
(find-stalin-expr-outcomes '(let ((a 9))
                              (if a 
                                  b
                                  (if 2 
                                      (let ((d 100))
                                        (c 2 3))))))
!#

;; code is just a list of expressions.
(define (find-stalin-code-outcomes code)
  (if (null? code)
      '()
      (let ()
        (define last-expr (last code))
        (let loop ((code code))
          (define expr (car code))
          (cond ((eq? expr last-expr)
                 (find-stalin-expr-outcomes expr))
                ((not (pair? expr))
                 (loop (cdr code)))
                ;;((memq (car expr) stalin-noreturn-funcs)
                ;; (car expr))
                (else
                 (loop (cdr code))))))))
  
#!
(find-stalin-code-outcomes (car lotsofcode))
(find-stalin-code-outcomes '((let loop () (loop))))
(find-stalin-code-outcomes '(let ((a 9))
                              (if a 
                                  b
                                  (if 2 
                                      (let ((d 100))
                                        (c 2 3))))))
!#

(define (find-stalin-func-returns code)
  (define ret '())
  (let loop ((code code))
    ;;(c-display "code" code)
    (schemecodeparser code
                      :use-customsymbolhandler?
                      (lambda (expr)
                        (or (and (eq? 'define (nth 0 expr))
                                 (pair? (nth 2 expr))
                                 (eq? 'lambda (car (nth 2 expr))))
                            (memq (car expr) '(let let* letrec))))
                      :customsymbolhandler
                      (lambda (expr)
                        ;;(c-display "got" expr)
                        (cond ((eq? 'define (car expr))
                               (let ((body (nth-cdr 2 (nth 2 expr))))
                                 (loop body)
                                 (push! (list (nth 1 expr) 
                                              (find-stalin-code-outcomes body))
                                        ret)))
                              ((and (eq? 'let (car expr))
                                    (not (pair? (cadr expr))))
                               (for-each loop (map cdr (nth 2 expr))) ;; arguments.
                               (loop (nth-cdr 3 expr))
                               (push! (list (nth 1 expr)
                                            (find-stalin-code-outcomes (nth-cdr 3 expr)))
                                      ret))
                              ;;let, let* and letrec
                              (else
                               (for-each loop (map cdr (nth 1 expr))) ;; arguments.
                               (for-each (lambda (a)
                                           (if (and (pair? (cadr a))
                                                    (eq? 'lambda (car (cadr a))))
                                               (let ((body (cdr (cadr a))))
                                                 (loop body)
                                                 (push! (list (car a)
                                                              (find-stalin-code-outcomes body))
                                                        ret))))
                                         (nth 1 expr))
                               (loop (cddr expr)))))))
  ret)


#!
(find-stalin-func-returns '((let loop () (lambda ()))))
(find-stalin-func-returns '((let loop () (loop) (lowlevel_remove_me))))
(find-stalin-func-returns '((let loop ()
                              (let loop2 ()
                                (loop))
                              (loop2))))

(find-stalin-func-returns '((letrec ((loop (lambda ()
                                             (display gakk)
                                             (loop))))
                              (+ 2 3))))

(let ((code (generate-stalin-code `( (spawn
                                       ,@last-stalin
                                       ((=> coroutine:_current-coroutine :continuation)))))))
  (pretty-print (find-stalin-func-returns
                 (stalin-cond->if code))))

(pretty-print (find-stalin-func-returns
               (stalin-cond->if
                lotsofcode)))


(pretty-print lotsofcode)
(define lotsofcode
  (generate-stalin-code0
   '((let loop ()
       (wait (irandom 500):-ms
         (spawn
           (define osc (make-oscil :frequency (+ 50 (irandom 900))))
           (define duration (+ 0 (+ 400 (irandom 2000)):-ms))
           (define i 0)
           (block :duration duration :cont #f
             (if (< i (>> duration 1))
                 (out (* (scale i 0 (>> duration 1) 0.0 0.01)
                         (oscil osc)))
                 (out (* (scale i (>> duration 1) duration 0.01 0.0)
                         (oscil osc))))
             (inc! i 1)))
         (loop))))))
!#


;; First implementation. Does not handle circular dependencies.
(define (find-stalin-noreturn-funcs-internal-old func-returns known-noreturns)
  (define (find func-returns known-noreturns)
    (define ret known-noreturns)
    (map (lambda (func-return)
           (define name (car func-return))
           (define possible-returns (cadr func-return))
           ;;(c-display "pos" name possible-returns)
           (call/cc (lambda (return)
                      (for-each (lambda (possible-return)
                                  (if (or (not (pair? possible-return))
                                          (not (memq (car possible-return) (cons name ret))))
                                      (return)))
                                possible-returns)
                      (push! name ret))))
         func-returns)
    (delete-duplicates ret eq?))

  (let loop ((old known-noreturns))
    (define new (find func-returns old))
    (if (not (equal? new old))
        (loop new)
        old)))

;; Second implementation. Seems to work fine.
;;
;; Works by:
;; 1. For each function, determine whether the function can return or not.
;;    A function can return if either of its outcomes are
;;       a) Not a function call.
;;    But! A function can never return if all its outcomes are either:
;;       b) In the known-noreturns list.
;;       c) A call to a function which is in the func-noreturns list
;;    If neither b) nor c), for any of its outcomes the function can return.
;; 2. If it can return, put it into the return-funcs list.
;; 3. Continue doing for-each until no function is put into the return-funcs list.
;;
(define (find-stalin-noreturn-funcs-internal func-returns known-noreturns known-returns)
  (define noreturns (remove (lambda (noreturn?)
                              (memq noreturn? known-returns))
                            (map car func-returns)))

  ;; One iteration of reducing the number of noreturns
  (define (reduce-noreturns)
    ;;(c-display "func-returns" func-returns)
    (for-each (curryppla (name possible-returns)
                (define noreturn #t)
                (for-each (lambda (possible-return)
                            (set! noreturn
                                  (and noreturn
                                       (cond ((not (pair? possible-return))                ;; 1a
                                              #f)
                                             ((memq (car possible-return) known-noreturns) ;; 1b
                                              #t)
                                             ((memq (car possible-return) noreturns)       ;; 1c
                                              #t)
                                             (else
                                              #f)))))
                          possible-returns)
                ;;(c-display "name" name noreturns returns noreturn (not noreturn))
                (if (not noreturn)
                    (set! noreturns (delete! name noreturns eq?))))
              func-returns))

  ;; Call reduce noreturns again and again until it can not be reduced anymroe
  (let loop ()
    (define old-noreturns (list-copy noreturns))
    (reduce-noreturns)
    (if (not (equal? old-noreturns noreturns))
        (loop)
        (set! noreturns (delete-duplicates (append known-noreturns noreturns) eq?))))

  
  noreturns)
  

  


(define* (find-stalin-noreturn-funcs code :optional (known-returns '()))
  (define func-returns (find-stalin-func-returns
                        (stalin-cond->if
                         code)))
  ;;(pretty-print func-returns)
  (let ()
    (define noreturns (find-stalin-noreturn-funcs-internal
                       func-returns
                       '(_continuation coroutine-continuation lowlevel_remove_me _switch-to-coroutine)
                       known-returns))
    
    ;; Check if any of the noreturns is ever set! to something not in the noreturns list.
    ;; In case, those noreturns are put into the "known-returns" list.
    ;;  (this block may seem chaotic at first sight, but it's not that bad, although it could be better)
    (let ((res (call/cc
                (lambda (return)
                  (let loop ((code code))
                    (schemecodeparser code
                                      :use-customsymbolhandler?
                                      (lambda (expr)
                                        (eq? 'set! (car expr)))
                                      :customsymbolhandler
                                      (lambda (expr)
                                        (if (memq (nth 1 expr) noreturns)
                                            (let ()
                                              (when (not (call/cc (lambda (return)
                                                                    (define func-checked '())
                                                                    (define (check-func-call func-call)
                                                                      (define func-name (car func-call))
                                                                      (let ((hit (assq func-name func-returns)))
                                                                        ;;(c-display "func-call / hit" func-call hit)
                                                                        (if hit
                                                                            (let ()
                                                                              (if (memq func-name func-checked) ;;avoid circular
                                                                                  (return #f))
                                                                              (push! func-name func-checked)
                                                                              (for-each check-something
                                                                                        (nth 1 hit)))
                                                                            (return #f))))
                                                                    (define (check-something outcome)
                                                                      ;;(c-display "outcome" outcome)
                                                                      (cond ((and (pair? outcome)
                                                                                  (eq? '_stalin-function (car outcome)))
                                                                             (check-expr `(begin
                                                                                            ,@(nth-cdr 2 (nth 1 outcome)))))
                                                                            ((pair? outcome)
                                                                             (check-func-call outcome))
                                                                            ((not (memq outcome noreturns))
                                                                             (return #f))))
                                                                    (define (check-expr expr)
                                                                      ;;(c-display "expr" expr)
                                                                      (for-each check-something
                                                                                (find-stalin-expr-outcomes expr)))
                                                                    (check-expr (nth 2 expr))
                                                                    #t)))
                                                (return (nth 1 expr)))))
                                        `(set! ,(nth 1 expr) ,(loop (nth 2 expr))))))
                  #t))))
      (if (eq? #t res)
          (begin
            ;;(c-display "NORETURNS" noreturns)
            noreturns)
          (find-stalin-noreturn-funcs code (cons res known-returns))))))
        
#!
(find-stalin-noreturn-funcs '((define ai (lambda ()
                                           (ai)))
                              (define get-ai (lambda ()
                                               ai))
                              (define rt_vct_set                                
                                (lambda (vvct_56 pos_57 val_58)
                                  (set! ai (lambda ()
                                             (get-ai)))
                                  (ai)))))

;; cool.
(find-stalin-noreturn-funcs '((let loop ()
                                (let loop2 ()
                                  (if (loop)
                                      (loop)
                                      (loop2)))
                                ;;(set! loop2 loop5)
                                (loop2))))


(define lotsofcode (generate-stalin-code last-stalin))
(find-stalin-noreturn-funcs lotsofcode)

(find-stalin-noreturn-funcs (find-stalin-func-returns
                             (stalin-cond->if
                              lotsofcode))
                            '(wait-do remove-me error myexit _run-scheduler coroutine-continuation 
                                      _switch-to-coroutine yield-do))

(let ((code (generate-stalin-code `( (spawn
                                       ,@last-stalin
                                       ((=> coroutine:_current-coroutine :continuation)))))))
  (find-stalin-noreturn-funcs (find-stalin-func-returns
                               (stalin-cond->if code))
                              '(_continuation coroutine-continuation lowlevel_remove_me _switch-to-coroutine neverending-scheduling__2)))

'(wait-do remove-me error myexit _run-scheduler coroutine-continuation 
          _switch-to-coroutine yield-do)))
                              

!#



(define (stalin-is-expr-noreturn? expr no-returns)
  (define outcomes (find-stalin-expr-outcomes expr))
  ;;(c-display "hepp" expr (pair? (car outcomes)) (memq (car outcomes) no-returns))
  (if (null? outcomes)
      #f
      (call/cc (lambda (return)
                 (for-each (lambda (outcome)
                             (if (or (not (pair? outcome))
                                     (not (memq (car outcome) no-returns)))
                                 (return #f)))
                           outcomes)
                 #t))))

(define (stalin-remove-dead-code code no-returns cont)
  (define somethingisremoved #f)
  (let ((ret (let das-loop ((code code))
               (schemecodeparser code
                                 :blockhandler
                                 (lambda (expr)
                                   (let loop ((expr expr))
                                     (if (null? expr)
                                         '()
                                         (let ()
                                           (define expr0 (car expr))
                                           (if (and (pair? expr0)
                                                    ;;(memq (car expr0) no-returns)
                                                    (not (null? (cdr expr)))
                                                    (begin ;;(c-display "asking" expr no-returns)
                                                           (let ((ret (stalin-is-expr-noreturn? expr0 no-returns)))
                                                             ;;(c-display "ret" ret)
                                                             ret))
                                                    )
                                               (begin
                                                 (set! somethingisremoved #t)
                                                 (list (das-loop expr0)))
                                               (cons (das-loop expr0)
                                                     (loop (cdr expr))))))))))))
    (cont ret somethingisremoved)))

#!
(define lotsofcode
  '((let ((a 9))
      (+ 2 3)
      (lowlevel_remove_me))
    (+ 9 10)))

;; This one is not handled.
(define lotsofcode
  '((let ((a 9))
      (+ 2 3)
      (let ((b (lowlevel_remove_me)))
        (* 100 200)))
    (+ 9 10)))

;; This one is though.
(define lotsofcode
  '((let ((a 9))
      (+ 2 3)
      (lowlevel_remove_me)
      (* 100 200))
    (+ 9 10)))

(stalin-remove-dead-code lotsofcode
                         (find-stalin-noreturn-funcs (find-stalin-func-returns
                                                      (stalin-cond->if
                                                       lotsofcode))
                                                     '(_continuation coroutine-continuation lowlevel_remove_me _switch-to-coroutine))
                         (lambda (code removed?)
                           (if removed?
                               (pretty-print code)
                               "nothing-removed")))
!#


;; Ad-hoc fix for most situations where stalins tco is not sufficiant.
;;
;; Basically, remove-dead-code does this:
;; (begin (remove-me) (+ 2 3)) -> (begin (remove-me))
(define (stalin-remove-dead-code-recursively code)
  (stalin-remove-dead-code code
                           (find-stalin-noreturn-funcs code)
                           (lambda (code removed?)
                             (if removed?
                                 (stalin-remove-dead-code-recursively code)
                                 code))))
#!
;; very very cool
(pretty-print (stalin-remove-dead-code-recursively last-stalin))
(pretty-print (stalin-remove-dead-code-recursively (generate-stalin-code0 last-stalin)))
(define lotsofcode (generate-stalin-code last-stalin))

(pretty-print (stalin-remove-dead-code-recursively 
               (generate-stalin-code `( (spawn
                                          ,@last-stalin
                                          ((=> coroutine:_current-coroutine :continuation)))))))

(<rt-stalin>
 (wait 50))
(begin last-stalin)
!#

(define (stalin-append-continuation code continuation)
  (cond ((eq? 'cond (car code))
         (stalin-append-continuation (stalin-cond->if code) continuation))
        ((and (eq? 'if (car code))
              (= 2 (length code)))
         `(if ,(nth 1 code)
              (begin
                ,(nth 2 code)
                ,continuation)))
        ((and (eq? 'if (car code))
              (= 3 (length code)))
         `(if ,(nth 1 code)
              (begin
                ,(nth 2 code)
                ,continuation)
              (begin
                ,(nth 3 code)
                ,continuation)))
        ((memq (car code) '(let let* letrec))
         `(,(nth 0 code) ,(nth 1 code)
           ,@(nth-cdr 2 code)
           ,continuation))
        (else
         #f)))
         

#!
(let ((a 9))
  (call/cc (lambda (return)
             ...)))
=>
(let ((a 9))
  (call/cc (lambda (return)
             ...))
  (continuation))
!#

;; stalin-monad-do-ify-call/cc is making it easier for stalin-remove-call/cc to recognize
;; call/cc-s which can be tranformed to simple continuations. After this
;; transformation, stalin-remove-call/cc doesn't have to scan blocks to
;; see if they might contain call/cc. Instead stalin-remove-call/cc can just
;; do a simple (and (pair? expr0) (eq? 'call/cc (car expr0))) check.
(define (stalin-monad-do-ify-call/cc code)
  (schemecodeparser code
                    :blockhandler
                    (lambda (expr)
                      (let loop ((expr expr))
                        (if (null? expr)
                            '()
                            (let ()
                              (define expr0 (car expr))
                              (define monad-do (rt-gensym "monad-do"))
                              ;;(c-display "expr0" expr0 (deep-list-search 'call-with-current-continuation
                              ;;                                           expr0)
                              ;;           (cdr expr))
                              (cond ((and (pair? expr0)
                                          (not (null? (cdr expr)))
                                          (not (eq? 'call-with-current-continuation (car expr0)))
                                          (deep-list-search 'call-with-current-continuation
                                                            expr0)
                                          (stalin-append-continuation expr0 `(,monad-do)))
                                     => (lambda (new-expr0)
                                          `((let ((,monad-do ,(stalin-monad-do-ify-call/cc
                                                               `(lambda ()
                                                                  ,@(cdr expr)))))
                                              ,(stalin-monad-do-ify-call/cc new-expr0)))))
                                    (else
                                     (cons (stalin-monad-do-ify-call/cc expr0)
                                           (loop (cdr expr)))))))))))

#!
(pretty-print (let ((code
                     '(let a ()
                        (let ((a 9))
                          (call-with-current-continuation (lambda (ret)
                                                            something))
                          (+ a b))
                        (lowlevel_remove_me))))
                (stalin-monad-do-ify-call/cc code)))
=>
(begin
  (let ((monad-do (lambda ()
                    (lowlevel_remove_me))))
    (let ((a 9))
      (call/cc (lambda (ret)
                 something))
      (+ a b)
      (monad-do))))
!#



#!
Removes some unecessary call/cc-s by recognizing when the code continuing after the call/cc block
"dissapears" in a function which will never return, and tranforming appropriately.

The reason for doing this is that call/cc takes _a lot_ of time to compile with stalin.
!#

(define (stalin-remove-call/cc das-code)
  ;;(pretty-print das-code)
  ;;(c-display "noreturns:" (find-stalin-noreturn-funcs das-code))
  (let das-loop ((code das-code))
    (schemecodeparser code
                      :blockhandler
                      (lambda (expr)
                        (let loop ((expr expr))
                          (if (null? expr)
                              '()
                              (let ()
                                (define expr0 (car expr))
                                ;;(c-display "expr0" expr0 (find-stalin-noreturn-funcs das-code))
                                (if (and (pair? expr0)
                                         (eq? 'call-with-current-continuation (car expr0)) ;; Check if call-wi. is called inisde expr0.
                                         (stalin-is-expr-noreturn? `(begin
                                                                      ,@(cdr expr))
                                                                   (find-stalin-noreturn-funcs das-code)))
                                    (let ()
                                      (define return (car (cadr (nth 1 expr0))))
                                      ;;(pretty-print `(begin ,@(cdr expr)))
                                      ;;(pretty-print expr)
                                      ;;(c-display "found one" (stalin-is-expr-noreturn? `(begin
                                      ;;                                                    ,@(cdr expr))
                                      ;;                                                 (find-stalin-noreturn-funcs das-code)))
                                      (das-loop
                                       `((let ((,return (lambda (,(rt-gensym "call/cc-return"))
                                                          ,@(cdr expr)
                                                          )))
                                           ,@(cddr (nth 1 expr0))))))
                                    (cons (das-loop expr0)
                                          (loop (cdr expr)))))))))))
#!
(pretty-print (stalin-remove-call/cc
               '(lambda ()
                  (lowlevel_debug "starting")
                  (let loop ()
                    (call-with-current-continuation
                     (lambda (return_40)
                       (wait-do_-11
                        (* 44100 1)
                        (lambda ()
                          (return_40 #t)))))
                    (lowlevel_debug "one second later")
                    (loop)))))

(pretty-print (stalin-remove-call/cc
               '(lambda ()
                  (lowlevel_debug "starting")
                  (let loop ()
                    (cond (#t
                           (call-with-current-continuation
                            (lambda (return_40)
                              (wait-do_-11
                               (* 44100 1)
                               (lambda ()
                                 (return_40 #t)))))
                           (lowlevel_debug "one second later")
                           (loop))
                          (else #f))))))



(pretty-print (stalin-remove-call/cc
               '(lambda ()
                  (lowlevel_debug "starting")
                  (call-with-current-continuation
                   (lambda (return_40)
                     (wait-do_-11
                      (* 44100 1)
                      (lambda ()
                        (return_40 #t)))))
                  (lowlevel_debug "one second later")
                  (lowlevel_remove_me))))
=>
(lambda ()
  (lowlevel_debug "starting")
  (let ((return_40 (lambda (_rt_gen980)
                     (lowlevel_debug "one second later")
                     (lowlevel_remove_me))))
    (wait-do_-11
     (* 44100 1)
     (lambda ()
       (return_40 #t)))))


(<rt-stalin>
 (debug "starting")
 (wait 1:-s)
 (debug "one second later"))

(<rt-stalin>
 (spawn (block :dur 500 (out 0))))


;; icmc code
(<rt-stalin>
 (while #t
   (wait (random 30):-ms)
   (define osc (make-oscil :freq (between 50 2000)))
   (define dur (between 400 2000):-ms)
   (define e (make-env '((0 0)(.5 .05)(1 0)) :dur dur))
   (spawn (block :dur dur
            (out (* (env e) (oscil osc)))))))

(<rt-stalin>
 (while #t
   (wait-midi :command note-on
     (define adsr (make-adsr :a 20:-ms :d 20:-ms :s 0.2 :r 50:-ms))
     (define osc
       (make-oscil :freq (midi-to-freq (midi-note))))
     (spawn
       (spawn 
         (block
           (cond ((adsr)
                  => (lambda (vol)
                       (out (* 0.2 vol (midi-vol) (oscil osc)))))
                 (else
                  (stop)))))
       (wait-midi :command note-off :note (midi-note)
         (-> adsr stop-it))))))

(<rt-stalin>
 (while #t
   (wait-midi :command note-on
     (spawn
       (define adsr (make-adsr :a 20:-ms :d 20:-ms :s 0.2 :r 50:-ms))
       (define osc
         (make-oscil :freq (midi-to-freq (midi-note))))
       (spawn 
         (block
           (-> adsr run
               (lambda (vol)
                 (out (* 0.2 vol (midi-vol) (oscil osc))))
               stop)))
       (wait-midi :command note-off :note (midi-note)
         (-> adsr stop-it))))))



(<rt-stalin>
 (while #t
   (wait-midi :command note-on
     (define adsr (make-adsr 20:-ms 20:-ms 0.2 50:-ms))
     (define osc
       (make-oscil :freq (midi-to-freq (midi-note))))
     (spawn
       (spawn 
         (call/cc 
          (lambda (break)
            (block
              (cond ((adsr)
                     => (lambda (vol)
                          (out (* vol 0.2 (midi-vol) (oscil osc)))))
                    (else
                     (break #t)))))))
       (wait-midi :command note-off :note (midi-note)
         (-> adsr stop-it))))))



(<rt-stalin>
 (while #t
   (debug "waiting for midi")
   (wait-midi :command note-on
     (define vol 0.0)
     (spawn
       (define osc
         (make-oscil :freq (midi-to-freq (midi-note))))
       (define player
         (spawn
           (define e (make-env '((0 0)(1 2)(5 1)) :end 50:-ms))
           (block :dur 50:-ms 
             (set! vol (* 0.1 (midi-vol) (env e)))
             (out (* vol (oscil osc))))
           (block (out (* vol (oscil osc))))))
       (wait-midi :command note-off :note (midi-note))
       (stop player)
       (define e (make-env '((0 1)(1 0)) :end 50:-ms))
       (block :dur 50:-ms 
         (out (* vol (env e) (oscil osc))))))))




(<rt-stalin>
 (define decay 50:-ms)
 (while #t
   (wait-midi :command note-on
     (define osc
       (make-oscil :freq (midi-to-freq (midi-note))))
     (spawn
       (define player
         (spawn (block (out (* (midi-vol) (oscil osc))))))
       (let ((a (midi-note)))
         (wait-midi :command note-off :note a
           (stop player))
         (define env (make-env (list 0.0 (midi-vol) 1.0 0.0) :dur decay))
         (block :dur decay
           (out (* (env env) (oscil osc)))))))))


!#



(define (stalin-cond->if code)
  
  (schemecodeparser code
                    :use-customsymbolhandler?
                    (lambda (expr)
                      (eq? 'cond (car expr)))
                    :customsymbolhandler
                    (lambda (expr)
                      (let* ((expr0 (nth 1 expr))
                             (body (if (null? (cddr expr0))
                                       (cadr expr0)
                                       `(begin
                                          ,@(cdr expr0)))))
                        (cond ((eq? 'else (car expr0))
                               (stalin-cond->if body))
                              ((eq? '=> (cadr expr0))
                               (let ((ret (rt-gensym)))
                                 (stalin-cond->if
                                  `(let ((,ret ,(car expr0)))
                                     (if ,ret
                                         (,(caddr expr0) ,ret)
                                         (cond ,@(nth-cdr 2 expr)))))))
                              (else
                               (stalin-cond->if
                                `(if ,(car expr0)
                                     ,body
                                     (cond ,@(nth-cdr 2 expr))))))))))
#!
(stalin-cond->if '(cond (a 1 2)(b 3)(else 4)))
(stalin-cond->if '(cond (a => af)(b 3)(else 4)))
!#


;; (define (a b c) ...) -> (define a (lambda (b c) ...))
(define (stalin-fix-defines code)
  (schemecodeparser code
                    :use-customsymbolhandler?
                    (lambda (expr)
                      (and (eq? 'define (car expr))
                           (pair? (cadr expr))))
                    :customsymbolhandler
                    (lambda (expr)
                      (stalin-fix-defines
                       `(define ,(car (cadr expr)) (lambda ,(cdr (cadr expr))
                                                     ,@(cddr expr)))))))

#!
(stalin-fix-defines '(define (a b c) d e f))
(stalin-fix-defines '(define (((((a b)))) c d) d e f))
!#


;; Only renames the toplevel defines:
;;   (define a (lambda () (a))) -> (define a_-1 (lambda () a))
;; The rest is taken care of in stalin-uniqify-variables
(define (stalin-get-renamed-toplevel-defines code)
  (define get-new-var
    (let ((num 0))
      (lambda (old)
        (if (or (memq old stalin-dont-rename-these)
                (char=? #\_ (car (string->list (symbol->string old))))) ;; don't rename vars starting with "_".
            old
            (<_> old '_ (string->symbol (number->string (inc! num -1))))))))

  (define renameds '())

  (for-each (lambda (expr)
              (if (and (pair? expr)
                       (eq? 'define (car expr)))
                  (let ()
                    (define new-name (get-new-var (cadr expr)))
                    (push! (list (cadr expr) new-name)
                           renameds)
                    `(define ,new-name
                       ,@(cdr expr)))
                  expr))
            code)

  renameds)


#!
(stalin-get-renamed-toplevel-defines
 '((define a (lambda (b) (+ a (gakk))))
   (define gakk (lambda () (let ((a (lambda () 2))) (a))))
   (+ a b (gakk))))
!#



(define (stalin-uniqify-variables code)

  (define get-new-var
    (let ((num -1))
      (lambda (old)
        (if (or (memq old stalin-dont-rename-these)
                (char=? #\_ (car (string->list (symbol->string old)))))
            old
            (<_> old '_ (string->symbol (number->string (inc! num 1))))))))

  (define (broken-list->list old)
    ;;(c-display "old" old (symbol? old))
    (cond ((null? old)
           '())
          ((symbol? old)
           (list old))
          (else
           (cons (car old)
                 (broken-list->list (cdr old))))))
  
  (define (fix-argslist args renameds)
    (define (getit sym)
      (let ((hit (assq sym renameds)))
        (if hit
            (cadr hit)
            sym)))
    (cond ((null? args)
           '())
          ((symbol? args)
           (getit args))
          (else
           (cons (getit (car args))
                 (fix-argslist (cdr args) renameds)))))

  (let loop ((code code)
             (varlist '())
             (renameds (stalin-get-renamed-toplevel-defines code)))
    ;;(c-display "varlist" varlist)
    ;;(c-display "code" code "\n" varlist "\n" renameds)
    (schemecodeparser code
                      :varlist 
                      varlist
                      :symbolfunc
                      (lambda (sym)
                        ;;(c-display "got" sym (assq sym renameds))
                        (let ((hit (assq sym renameds)))
                          (if hit
                              (cadr hit)
                              sym)))
                      :use-customsymbolhandler?
                      (lambda (expr)
                        (memq (car expr) '(define lambda let let* letrec)))
                      :customsymbolhandler
                      (lambda (expr)
                        (cond (#f #f)

                              ;; toplevel-define
                              ((eq? 'define (car expr))
                               (let ((name (loop (cadr expr) varlist renameds)))
                               `(define ,name
                                  ,@(loop (cddr expr)
                                          (cons name varlist)
                                          renameds))))

                               ;; lambda
                              ((eq? 'lambda (car expr))
                               (let* ((arglist (broken-list->list (cadr expr)))
                                      (new-varlist (map get-new-var arglist))
                                      (new-renameds (zip arglist new-varlist)))
                                 `(lambda ,(fix-argslist (cadr expr) new-renameds)
                                    ,@(loop (cddr expr)
                                            (append new-varlist varlist)
                                            (append new-renameds renameds)))))
                              ;; named let
                              ((and (eq? 'let (car expr))
                                    (symbol? (cadr expr)))
                               (let* ((clean-varlist (cons (cadr expr)
                                                           (map car (caddr expr))))
                                      (new-varlist (map get-new-var clean-varlist))
                                      (new-renameds (zip clean-varlist new-varlist))
                                      (vars (map (lambda (newname code)
                                                   `(,newname ,@(loop code varlist renameds)))
                                                 (map cadr (cdr new-renameds))
                                                 (map cdr (caddr expr)))))
                                 `(let ,(car new-varlist) ,vars
                                       ,@(loop (cdddr expr)
                                               (append new-varlist varlist)
                                               (append new-renameds renameds)))))

                              ;; let
                              ((eq? 'let (car expr))
                               (let* ((clean-varlist (map car (nth 1 expr)))
                                      (new-varlist (map get-new-var clean-varlist))
                                      (new-renameds (zip clean-varlist new-varlist))
                                      (vars (map (lambda (newname code)
                                                   `(,newname ,@(loop code varlist renameds)))
                                                 (map cadr new-renameds)
                                                 (map cdr (nth 1 expr)))))
                                 `(let ,vars
                                    ,@(loop (nth-cdr 2 expr)
                                            (append new-varlist varlist)
                                            (append new-renameds renameds)))))

                              ;; let*
                              ((eq? 'let* (car expr))
                               (let ((vars (map (lambda (name code)
                                                  (let* ((new-name (get-new-var name))
                                                         (ret `(,new-name ,@(loop code varlist renameds))))
                                                    (push! new-name varlist)
                                                    (push! (list name new-name) renameds)
                                                    ret))
                                                (map car (nth 1 expr))
                                                (map cdr (nth 1 expr)))))
                                 `(let* ,vars
                                    ,@(loop (nth-cdr 2 expr)
                                            varlist
                                            renameds))))

                              ;; letrec
                              ((eq? 'letrec (car expr))
                               (let* ((clean-varlist (map car (nth 1 expr)))
                                      (new-varlist (map get-new-var clean-varlist))
                                      (new-renameds (zip clean-varlist new-varlist))
                                      (varlist (append new-varlist varlist))
                                      (renameds (append new-renameds renameds))
                                      (vars (map (lambda (newname code)
                                                   `(,newname ,@(loop code varlist renameds))) ;; well...
                                                 (map cadr new-renameds)
                                                 (map cdr (nth 1 expr)))))
                                 `(letrec ,vars
                                    ,@(loop (nth-cdr 2 expr)
                                            varlist
                                            renameds))))

                              )))))

#!
(stalin-uniqify-variables
 '((define time 0)
   `(+ ,time 50)))

(stalin-uniqify-variables
 '((define time 0)
   (lambda (time)
     time)))

(pretty-print (stalin-uniqify-variables
               '((define a (lambda (b) (+ a (gakk))))
                 (define gakk (lambda () (let ((a (lambda () 2))) (a))))
                 (+ a b (gakk)))))

(stalin-uniqify-defines
 (stalin-uniqify-variables '((define time 0)
                             (lambda (time)
                               time))))

(stalin-uniqify-defines '((define time 0)
                          (lambda (time)
                            time)))

(stalin-uniqify-variables '((lambda (a b . c)
                              (+ a b c d))))
(stalin-uniqify-variables '(let loop ((a b))
                             (loop (+ a 2))))
(stalin-uniqify-variables '(let ((loop 90)
                                 (a b))
                             (loop (+ a 2))))
(stalin-uniqify-variables '(let* ((loop 90)
                                  (a loop))
                             (loop (+ a 2))))
(stalin-uniqify-variables '(letrec ((loop 90)
                                    (a loop)) ;; Well, that's wrong.
                             (loop (+ a 2))))
!#





;; (define (a) (+ a b) (define c 2) c) -> (define (a) (+ a b) (letrec ((c 2)) c))
(define (stalin-fix-internal-defines code)
  (map (lambda (code)
         (let das-loop ((code code)
                        (level 0))
           (schemecodeparser  code
                              :blockhandler
                              (lambda (expr)
                                (let loop ((expr expr))
                                  (cond ((null? expr)
                                         '())
                                        ((= 0 level)
                                         (das-loop expr 1))
                                        (else
                                         (let ()
                                           (define expr0 (car expr))
                                           (if (and (pair? expr0)
                                                    (eq? 'define (car expr0)))
                                               (let ()
                                                 (das-loop `((letrec ((,(nth 1 expr0) ,(nth 2 expr0)))
                                                               ,@(cdr expr)))
                                                           (1+ level)))
                                               (cons (das-loop expr0 (1+ level))
                                                     (loop (cdr expr))))))))))))
       code))
#!
(stalin-fix-internal-defines '((define (a) (+ a b) (define c 2) c)))
; -> (define (a) (+ a b) (letrec ((c 2)) c))
(stalin-fix-internal-defines lotsofcode)
!#


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

;; only "(asdasf):-ms" infixes, not "asdf:-ms" infixes.
(define (fix-stalin-infix code)
  (cond ((null? code) '())
        ((not (pair? code)) code)
        (else
         (let ()
           (define first '())
           (let loop ((term code))
             (cond ((null? term)
                    (map fix-stalin-infix code))
                   ((and (not (null? first))
                         (not (null? (cdr first)))
                         (keyword? (car term))
                         (char=? #\- (car (string->list (symbol->string (keyword->symbol (car term)))))))
                    (fix-stalin-infix
                     `(,@(c-butlast first)
                       (,(<_> 'infix (keyword->symbol (car term)))
                        ,(last first))
                       ,@(cdr term))))
                   (else
                    (set! first (append first (list (car term))))
                    (loop (cdr term)))))))))
#!
(fix-stalin-infix '(block :duration (between 200 1000):-:ms aiai more and more))
(fix-stalin-infix '(block :duration (between 200 1000):-m))
!#
;; fixes keywords and the :- infix operator when used in symbols (ie. "asdf:-ms")
(define (fix-stalin-various code)
  (schemecodeparser code
                    :symbolfunc (lambda (sym)
                                  (define first '())
                                  (let loop ((string (string->list (symbol->string sym))))
                                    (cond ((null? string)
                                           sym)
                                          ((and (char=? #\: (car string))
                                                (char=? #\- (cadr string)))
                                           (stalin-super-generate
                                            `(,(string->symbol (<-> "infix-" (list->string (cddr string))))
                                              ,(let ((string (list->string (reverse! first))))
                                                 (or (string->number string)
                                                     (string->symbol string))))))
                                          (else
                                           (push! (car string) first)
                                           (loop (cdr string))))))
                    
                    :keywordfunc (lambda (key)
                                   `',(make-stalin-keyword (keyword->symbol key)))))

         
#!
(fix-stalin-infix '(+ 5 (+ 2 3):-ms))
(fix-stalin-various '(+ 5 esdv :-s))
(fix-stalin-various '(+ 5 6 :-s))
!#

;;; Add a stack check, cpu check and backtrace at all lambda entries.
(define (stalin-add-health-checks code)
  (if (or (not *stalin-add-health-checks*)
          (and (not *rt-opt-cpu-checks*)
               (not *rt-opt-stack-checks*)))
      code
      `(
        (define backtrace (make-vector ,*stalin-backtrace-length* ""))
        (define backtrace-place 0)
        (define (check_health function-name)
          (if (> ((foreign-procedure () int "check_health_internal"))
                 0)
              (let loop ((i 0)
                         (n backtrace-place))
                (if (and (not (string=? "" (vector-ref backtrace n)))
                         (< i ,*stalin-backtrace-length*))
                    (begin
                      ((foreign-procedure (int char*) void "print_backtrace")
                       i (vector-ref backtrace n))
                      (loop (+ i 1)
                            (if (= 0 n) ,(1- *stalin-backtrace-length*) (- n 1))))
                    ((foreign-procedure () void "health_exit")))))
          (vector-set! backtrace backtrace-place function-name)
          (set! backtrace-place (if (= backtrace-place ,(1- *stalin-backtrace-length*))
                                    0
                                    (+ 1 backtrace-place))))
        ,@(let loop ((code code))
            (schemecodeparser code
                              :use-customsymbolhandler?
                              (lambda (expr)
                                (or (memq (car expr) '(let let* letrec))
                                    (and (eq? 'define (car expr))
                                         (pair? (nth 2 expr))
                                         (eq? 'lambda (car (nth 2 expr))))))
                              :customsymbolhandler
                              (lambda (expr)
                                (define expr0 (car expr))
                                (cond ((eq? 'define expr0)
                                       `(define ,(nth 1 expr)
                                          (lambda ,(nth 1 (nth 2 expr))
                                            (check_health ,(symbol->string (nth 1 expr)))
                                            ,@(map loop (nth-cdr 2 (nth 2 expr))))))
                                      ;; named let
                                      ((and (eq? 'let expr0)
                                            (symbol? (nth 1 expr)))
                                       (let ((nonamed-let (loop `(let ,(nth 2 expr)
                                                                   ,@(nth-cdr 3 expr)))))
                                         `(let ,(nth 1 expr) ,(nth 1 nonamed-let)
                                               (check_health ,(symbol->string (nth 1 expr)))
                                               ,@(nth-cdr 2 nonamed-let))))
                                      (else
                                       (let ()
                                         (define names (map car (nth 1 expr)))
                                         (define values (map cadr (nth 1 expr)))
                                         `(,expr0 ,(map (lambda (name value)
                                                          (if (and (pair? value)
                                                                   (eq? 'lambda (nth 0 value)))
                                                              `(,name (lambda ,(nth 1 value)
                                                                        (check_health ,(symbol->string name))
                                                                        ,@(map loop (nth-cdr 2 value))))
                                                              (list name (loop value))))
                                                        names
                                                        values)
                                                  ,@(map loop (nth-cdr 2 expr))))))))))))
#!
;; Running out of stack
(<rt-stalin>
 (let loop ((i 2))
   (+ 2 (loop (1+ i)))))
;; Neverending loop.
(<rt-stalin>
 (let loop ()
   (out 0.0)
   (loop)))
!#


(define (stalin-super-generate code)
  (stalin-fix-defines 
   (fix-stalin-various
    (stalin-macroexpand
     (fix-stalin-infix code)))))


;; Expands macros and include functions and variables which the code depends on,
;; all recursively. (careful with macros since its applied to all included code!)
(define (generate-stalin-code0 code)

  ;; memoized (lambda (funcname) `(define ,funcname ,(stalin-macroexpand (get-stalin-func funcname))))
  (define get-expanded-code
    (let ((expanded '()))
      (lambda (funcname)
        (let ((expanded (assq funcname expanded)))
        (if expanded
            (cadr expanded)
            (let ((ret (stalin-super-generate (get-stalin-func funcname))))
              (push! (list funcname ret)
                     expanded)
              ret))))))

  (define (find-dependencies trace code)
    (define funcs (find-stalin-funcs code))
    ;;(c-display "find" code)
    (if (null? funcs)
        '(#f)
        (flatten (map (lambda (func)
                        (if (memq func trace)
                            '(#f) ;; cyclic dependency
                            (list (find-dependencies (cons func trace)
                                                     (get-expanded-code func))
                                  func)))
                      funcs))))

  (let* ((expanded (stalin-super-generate code))
         (dependencies
          (delete-duplicates (delete #f (find-dependencies '() expanded) eq?)
                             eq?)))
    ;;(pretty-print dependencies)
    (append (map get-expanded-code dependencies)
            expanded)))


#!
(define-stalin a 0)
(define-stalin b a)
(define-stalin c (+ a b c));b a))
(generate-stalin-code0 '(c))
(generate-stalin-code0 '())
!#
  

(define (generate-stalin-code code)
  (define lotsofcode (stalin-uniqify-variables
                      (stalin-fix-internal-defines
                       (generate-stalin-code0 code))))
  (stalin-add-health-checks
   (stalin-remove-call/cc
    (stalin-monad-do-ify-call/cc
     (stalin-remove-dead-code-recursively
      lotsofcode)))))

   

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
(schemecode->file (generate-stalin-code0 '((+ 2 3 (add 50 (add 90))))))
!#

(define (compile-stalin-file basename)
  ;;(define command (<-> "stalin -On -no-clone-size-limit  -split-even-if-no-widening  -Ob -Om -Or -Ot -c " basename ".scm"))
  ;;(define command (<-> "stalin -On -no-clone-size-limit  -Ob -Om -Or -Ot -c " basename ".scm"))
  ;;(define command (<-> "stalin -On -no-clone-size-limit  -split-even-if-no-widening -c " basename ".scm"))
  ;;(define command (<-> "stalin -On -no-clone-size-limit -c " basename ".scm"))
  ;;(define command (<-> "stalin -On -clone-size-limit 0 -no-escaping-continuations -c " basename ".scm"))
  ;;(define command (<-> "stalin -fully-convert-to-CPS -On -clone-size-limit 0 -c " basename ".scm"))
  ;;(define command (<-> "stalin -On -no-clone-size-limit -split-even-if-no-widening  -c " basename ".scm"))
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
(compile-stalin-file (schemecode->file (generate-stalin-code0 '((+ 2 3 (add 50 (add 90)))))))
!#


(eval-c (<-> "-I" snd-header-files-path)
        "#include <rt-various.h>"
        (public
         (<void> fix-stalin-c-source (lambda ((<char*> infile)
                                              (<char*> outfile))
                                       (fix_stalin_c_source infile outfile)))))

(define (get-stalin-c-file generated-scheme-file cont)
  (let* ((basename generated-scheme-file)
         (inname   (compile-stalin-file basename))
         (outname  (<-> (tmpnam) ".c")))
    (delete-at-exit outname)
    (fix-stalin-c-source inname outname)
    (c-display "inname" inname)
    (cont basename
          outname)))

#!
(get-stalin-c-file (schemecode->file (generate-stalin-code0 '((display (+ 2 3))(newline)))))
!#

#!
(define (link-stalin-file c-file)
  (define o-file (<-> c-file ".o"))
  (if (not (= 0 (system (<-> "gcc " "-I" snd-header-files-path " " c-file " -shared -o " o-file " -fpic"))))
      (throw 'compilation-error))
  (delete-at-exit o-file)
  o-file)
!#

(eval-c "-O0 -lpcl"
        "#include <pcl.h>"
;        (<int> use_some_stack (lambda ((<int> counter)
;                                       (<void**> where))
;                                (<int> dummy)
;                                (cond ((== counter 20)
;                                       (set! *where &dummy)
;                                       (return counter))
;                                      (else
;                                       (return (+ 1 
;                                                  (use_some_stack (+ counter 1) where)))))))

;	(<nonstatic-void*> rt_get_stack_address (lambda ()
;                                                  (<int> dummy)
;                                                  (<char*> ret)
;                                                  (use_some_stack 0 &ret)
;                                                  (return ret)))
	(<nonstatic-void*> rt_get_stack_address (lambda ()
                                                  (<int> dummy)
                                                  (return &dummy)))
        ;; hope this works.
        (<nonstatic-int> rt_call_and_use_some_stack (lambda ((<int> counter)
                                                             ((<int> (<void>)) func))
                                                      (if (== counter 2)
                                                          (return (+ (func) counter))
                                                          (return (+ 1 
                                                                     (rt_call_and_use_some_stack (+ counter 1) func))))))
        (<nonstatic-void> rt_no_return_co_resume (lambda ()
                                                   (co_resume))))
        


(define-ec-struct <RT_Stalin>
  <void*> freefunc)


(define (link-stalin-file basename c-file program)
  (c-display "c-file:" c-file)
  (apply eval-c-non-macro
         `(,(<-> "-I" snd-header-files-path " -I/home/kjetil/site/include" " -lpcl")
           #f

           "#include <jack/jack.h>"
           "#include <jack/ringbuffer.h>"
           "#include <rollendurchmesserzeitsammler.h>"
           "#include <pcl.h>"
           "#include <ucontext.h>"

           ,(if *use-alsa-midi*
                "#include  <alsa/asoundlib.h>"
                "/* */")

           "#include <math.h>"
           "#include <_sndlib.h>"
           "#include <clm.h>"
           "#include <xen.h>"
           "#include <vct.h>"
           "#include <clm2xen.h>"

           (shared-struct <RT_Engine>)
           (shared-struct <RT_Stalin>)

           (<int> remove_me 0)

           (<struct-RT_Engine*> rt_engine)

           ;;(<int64> g_time) ;; Not used so far.
           (<int> g_startframe)
           (<int> g_endframe)           
           (<int> block_time 0)

           (<coroutine_t> dsp_coroutine)

           (<tar_heap_t*> heap)
           
           (<char*> start_dyn NULL)
           (<char*> end_dyn NULL)
           (<char*> stack_top NULL)
           (<char*> stack_bot NULL)

           (<jack_time_t> block_enter_time 0)
           
           (get-proto rt_debug)
           (get-proto rt_get_stack_address)
           (get-proto rt_call_and_use_some_stack)
           (get-proto rt_receive_midi)

           ,bus-struct

           (<struct-rt_bus*> outbus (cast <struct-rt_bus*> ,(<-> (number->string (cadr (SCM_SMOB_DATA *out-bus*)))
                                                                 "UL")))
           (<struct-rt_bus*> inbus (cast <struct-rt_bus*> ,(<-> (number->string (cadr (SCM_SMOB_DATA *in-bus*)))
                                                                "UL")))

           (<float*> sounddata)
           
           (run-now
            (fprintf stderr (string "alloc start\\n"))
            (set! sounddata (calloc (sizeof <float>) (* ,*rt-block-size*
                                                        ,*rt-num-output-ports*)))
            (fprintf stderr (string "alloc end\\n")))

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
            (fprintf stderr (string "new heap start\\n"))
            (set! heap (tar_new_heap))
            (fprintf stderr (string "new heap end\\n"))
            
            (if (== 0 (tar_get_dynamic_roots_for (cast <char*> &heap) &start_dyn &end_dyn))
                (printf (string "Error. Could not find dynamic start and end. Not good.\\n")))
            )

           (<void> block_ready_internal (lambda ()
                                          (set! stack_bot (cast <char*> (rt_get_stack_address)))
                                          ;;(rt_debug (string "stack_bot: %p\\n") stack_bot)
                                          (co_resume)))
           
           "void rt_no_return_co_resume(void) __attribute__ ((noreturn))"
           
           "static void myexit(void) __attribute__ ((noreturn))"
           ,@(if (or #t (= 0 (system (<-> "grep exit " c-file))))
                 '((<void> myexit (lambda ()
                                    (set! remove_me 1)
                                    (rt_no_return_co_resume))))
                 '())

           (<void> myerror (lambda ((<char*> string))
                             (rt_debug string)
                             (set! remove_me 1)
                             (rt_no_return_co_resume)))

           "struct linkedlist{struct linkedlist* next;}"
           (<struct-linkedlist*> gc_uncollectable_mem NULL)
           (<void*> tar_alloc_uncollectable (lambda ((<size_t> size))
                                              (<struct-linkedlist*> ret (tar_alloc heap (+ size (sizeof <struct-linkedlist*>))))
                                              (set! ret->next gc_uncollectable_mem)
                                              (set! gc_uncollectable_mem ret)
                                              (return ret+1)))
           
           ,@(get-stalin-ec-funcs program)
           
           (<void> health_exit (lambda ()
                                 (rt_debug (string ,(<-> "Scheme file: \\\"" basename ".scm\\\"")))
                                 (set! remove_me 1)
                                 (rt_no_return_co_resume)))
           
           (<void> print_backtrace
                   (lambda ((<int> num)
                            (<char*> function_name))
                     (rt_debug (string "%d: %s") num function_name)))

           (<int> check_health_internal
                  (lambda ()
                    (<char*> das_stack_bot (cast <char*> (rt_get_stack_address)))
                    
                    (cond ((and ,(if *rt-opt-stack-checks* 1 0)
                                (< das_stack_bot
                                   (- stack_top ,*stalin-stack-limit*)))
                           (set! stack_bot das_stack_bot)
                           (rt_debug (string "Error. Running out of stack. (Endless loop?)\\n\\nLast visisted: (newest->oldest)"))
                           (return 1))
                          ((and ,(if *rt-opt-cpu-checks* 1 0)
                                (> block_enter_time 0)
                                (> (jack_get_time)
                                   (+ block_enter_time ,(c-integer (* 1
                                                                      (/ (* 1000000 *rt-block-size*)
                                                                         (rte-samplerate)))))))
                           (rt_debug (string
                                      ,(<-> "Using too much CPU. Stopping instrument in case this is an endless loop."
                                            "In case not, use the :runtime-checks option to turn off off safety checks,\\n"
                                            "programs usually runs magnitudes faster when doing so.\\n\\nLast visited: (newest->oldest)")))
                           (return 2))
                          (else
                           (return 0)))))
           
           "#define fprintf(a,...) rt_debug(__VA_ARGS__)"
           "#define exit(a) myexit()"
           ;;"static char* dynstart"
           ,(<-> "#include \"" c-file "\"")
           ;;"static char* dynend"
           "#undef fprintf"
           "#undef exit"

           ;; public
           (functions->public
            (<int> process_func (lambda ((<void*> something)
                                         (<int> startframe)
                                         (<int> endframe))

                                  ;;(fprintf stderr (string "remove_me: %d, something: %p\\n") remove_me something)

                                  ;; If remove_me was set during init. (audio thread is not entered in that case)
                                  ;;(if (!= 0 remove_me)
                                  ;;    (return remove_me))

                                  (<static-int> first_run 0)
                                  (when (or (== 0 startframe)
                                            (== 1 first_run))
                                    (set! block_enter_time (- (jack_get_time)
                                                              (/ (* 1000000 startframe)
                                                                 ,(rte-samplerate))))
                                    (clean_sounddata)
                                    (set! first_run 0))

                                  
                                  (when (== false (tar_entering_audio_thread heap))
                                    (fprintf stderr (string "Using too much CPU. Skipping\\n"))
                                    (return 0))

                                  (set! g_startframe startframe)
                                  (set! g_endframe endframe)

                                  ;;(set! g_time (+ rt_engine->block_time startframe))
                                  
                                  (let* ((old_heap <tar_heap_t*> (clm_set_tar_heap heap))
                                         (old_ef  <error_func_t> (clm_set_error_func myerror)))
                                    (co_call dsp_coroutine)
                                    (clm_set_error_func old_ef)
                                    (clm_set_tar_heap old_heap))

                                  (when (== 0 (% block_time (* ,*rt-block-size* 
                                                               (/ (* 2 (cast <int> ,(rte-samplerate)))
                                                                  ,*rt-block-size*))))
                                    (rt_debug (string "data: %d, stack: %d %p %p, num_allocs: %d")
                                              (abs (- end_dyn start_dyn))
                                              (abs (- stack_top stack_bot))
                                              stack_bot
                                              stack_top
                                              heap->num_allocs
                                              ))

                                  (if (tar_leave_audio_thread heap)
                                      (when (== 0 remove_me)
                                        (rt_debug (string "data: %d, stack: %d %p %p, num_allocs: %d")
                                                  (abs (- end_dyn start_dyn))
                                                  (abs (- stack_top stack_bot))
                                                  stack_bot
                                                  stack_top
                                                  heap->num_allocs
                                                  )
                                        
                                        (tar_add_root heap start_dyn end_dyn) ; data
                                        ;;(tar_add_root heap &dynstart &dynend) ;; This might work, but performance-vice it shouldn't matter. Probably better to be safe and just use start_dyn and end_dyn.
                                        (tar_add_root heap stack_bot stack_top) ; stack

                                        ;;(tar_add_root heap (- stack_top 120000) stack_top) ; stack
                                        (tar_add_root heap dsp_coroutine (+ (cast <char*> dsp_coroutine)
                                                                            (EC_MAX (sizeof <ucontext_t>) ;registers
                                                                                    (sizeof <jmp_buf>))))
                                        (tar_run_gc heap)
                                        ;;heap->num_allocs=0
                                        ))
                                  
                                  (sounddata_to_bus startframe endframe)

                                  (if (== endframe ,*rt-block-size*)
                                      (+= block_time ,*rt-block-size*))

                                  (when (< stack_bot
                                           (- stack_top ,*stalin-stack-limit*))
                                    (rt_debug (string "Error. Running out of stack. This might be an error. Stopping instrument. (%d)\\n")
                                              (abs (- stack_top stack_bot)))
                                    (rt_debug (string "If you need more stack, increase *stalin-stack-size* and *stalin-stack-limit*.\\n"))
                                    (set! remove_me 1))

                                  (return remove_me))))


           (<void> dsp_coroutine_func (lambda ((<void*> arg))
                                        (set! stack_top (rt_get_stack_address))
                                        (rt_call_and_use_some_stack 0 schememain)))


           ;; Merk! Veldig lett aa benchmarke gc naa. Bare kjoer clinger's scheme tests med rollend.. og hbgc.

           ;; init (note that (debug) shouldn't be run from guile thread. Must fix)
           (run-now
            (set! dsp_coroutine (co_create dsp_coroutine_func NULL NULL ,*stalin-stack-size*))
            (co_call dsp_coroutine))

;;;           ;; init (note that (debug) shouldn't be run from guile thread. Must fix)
;;;           (run-now
;;;            (set! dsp_coroutine (co_create dsp_coroutine_func NULL NULL ,*stalin-stack-size*))
;;;            (let* ((old_heap <tar_heap_t*> (clm_set_tar_heap heap)))
;;;              (if (!= NULL old_heap)
;;;                  (fprintf stderr (string "Error: old_heap was not NULL (old: %p, new: %p)!\\n") old_heap heap))
;;;              (tar_entering_audio_thread heap)
;;;              (co_call dsp_coroutine)
;;;              (when (tar_leave_audio_thread heap)
;;;                (tar_add_root heap start_dyn end_dyn) ; data
;;;                (tar_add_root heap stack_bot stack_top) ; stack
;;;                ;;(tar_add_root heap (- stack_top 120000) stack_top) ; stack
;;;                (tar_add_root heap dsp_coroutine (+ (cast <char*> dsp_coroutine)
;;;                                                    (EC_MAX (sizeof <ucontext_t>) ;registers
;;;                                                            (sizeof <jmp_buf>))))
;;;                (tar_run_gc heap))
;;;              (clm_set_tar_heap NULL)))

           (<void> free_globals_func (lambda ((<struct-RT_Stalin*> rt_stalin)
                                              (<int> do_I_free_questionmark))
                                       (if (== 0 do_I_free_questionmark)
                                           (fprintf stderr (string "Yea. Hepp, freeing stalin\\n")))
                                       (when (== 1 do_I_free_questionmark)
                                         (fprintf stderr (string "Hepp, freeing stalin\\n"))
                                         (co_delete dsp_coroutine)
                                         (tar_delete heap)
                                         (free sounddata))))
           
           (public
            (<void-*> make-globals-func (lambda ((<struct-RT_Engine-*> engine))
                                          (set! rt_engine engine)

                                          (if (== 1 remove_me)
                                              (return NULL))                                                      
                                          (<struct-RT_Stalin*> rt_stalin (calloc 1 (sizeof (struct <struct-RT_Stalin>))))
                                          (set! rt_stalin->freefunc free_globals_func)
                                          (return rt_stalin))))
           ))
  (let ((globals (make-globals-func (-> *rt-engine* engine-c))))
    (if (not globals)
        #f
        (list (process_func)
              globals))))
          


#!
(link-stalin-file (get-stalin-c-file (schemecode->file (generate-stalin-code0 '((display (+ 2 3))(newline))))) '((rt_write_out_bus)))
(link-stalin-file (get-stalin-c-file (schemecode->file (generate-stalin-code0 '((display (+ 5 3))(newline))))))
(link-stalin-file (get-stalin-c-file (schemecode->file (generate-stalin-code0 '((display (+ 5 300))(newline))))))
(print_stuff)
(callmain)
!#  

(define last-stalin #f)

(define (<rt-stalin-do> code)
  (set! last-stalin code)
  (catch 'compilation-error
         (lambda ()
           (fix-defines
            (define generated (generate-stalin-code
                               `( (spawn
                                    ,@code)
                                  ((=> coroutine:_current-coroutine :continuation)))))
            
            ;;(c-display "generated" generated)
            ;;(check-stalin-syntax generated)
            (get-stalin-c-file (schemecode->file generated)
                               (lambda (basename c-file)
                                 (define funcs (link-stalin-file basename c-file generated))
                                 (if funcs
                                     (let ()
                                       (define realtime (<realtime> (car funcs) (cadr funcs) '()))
                                       (-> realtime play)
                                       realtime)
                                     #f)
                                 ))))
         (lambda x
           #f)))


;; what about (<rt-stalin> (block (out ...))) ?
(define-macro <rt-stalin> 
  (labamba (:key (stack-checks #t)
                 (cpu-checks #t)
                 (runtime-checks 'undefined)
            :rest code)
    (cond ((eq? #t runtime-checks)
           (set! stack-checks #t)
           (set! cpu-checks #t))
          ((eq? #f runtime-checks)
           (set! stack-checks #f)
           (set! cpu-checks #f)))
    `(begin
       (set! *rt-local-stalin-code-environment* (the-environment))
       (set! *rt-opt-stack-checks* ,stack-checks)
       (set! *rt-opt-cpu-checks* ,cpu-checks)
       (<rt-stalin-do> ',code))))


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

(pretty-print (generate-stalin-code0 '((+ 2 3 (add 50 (add 90))))))
!#

