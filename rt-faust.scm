
;; This file is loaded by rt-compiler.scm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Main struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-ec-struct <mus_rt_faust>
  <void*> dsp
  <void*> compute_func
  <float-**> ins
  <float-**> outs
  <int> num_inputs
  <int> num_outputs
  <void*> handle
  <void*> conclude
  <void*> deleteDsp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Caching ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define faust-cache '())
(define (add-faust-cache source handle)
  (push! (list source handle) faust-cache))
(define (find-faust-cache source)
  (call/cc
   (lambda (return)
     (for-each (lambda (cache)
		 (call/cc
		  (lambda (next)
		    (define cache-source (car cache))
		    (if (not (= (length source)
				(length cache-source)))
			(next #f))
		    (for-each (lambda (s1 s2)
				(if (not (or (eq? s1 s2)
					     (string=? s1 s2)))
				    (next #f)))
			      source
			      cache-source)
		    (return (cadr cache)))))
	       faust-cache)
     #f)))
				     
#!
(string=? "avb" "avb")
(eq? "ab" "ab")
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Compiling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define has-compiled-faust-gui #f)

;; Don't reset this one. (Dynamic linking of c++ files seems to be very dynamic.)
;;(define num-compiled-faust-files -1)

(define* (compile-faust source :key (make-gui #t))  
  (define file (generate-faust-source-file source))
  (system (<-> "faust -a " (if make-gui
			       "snd-rt-gtk.cpp "
			       "module.cpp ")
	       "-o " file ".cpp " file ".dsp"))


  ;; Uncomment line below if linking with RTLD_GLOBAL
  ;;(system (<-> "sed -i -e 's/mydsp/mydsp" (number->string (inc! num-compiled-faust-files 1)) "/g' " file ".cpp"))
  (system (<-> "g++ -O3 -ffast-math "
	       (if make-gui
		   (<-> (string-append (string #\`) "pkg-config --cflags --libs gtk+-2.0" (string #\`)) " ")
		   "")
	       " " (or (getenv "CFLAGS") "") " "
	       " " (or (getenv "LDFLAGS") "") " "
	       " " *eval-c-CFLAGS* " "
	       file ".cpp" " -shared -fpic -o " file ".so"))

  (for-each delete-at-exit
	    (list (<-> file ".dsp")
		  (<-> file ".cpp")
		  (<-> file ".so")))

  (let ((handle (c-dynamic-handle (<-> file ".so") :flags (RTLD_LOCAL))))
    (cond ((not handle)
	   (c-display "Unable to dynamically link " (<-> file ".so"))
	   #f)
	  (else
	   (set! has-compiled-faust-gui #t)
	   (add-faust-cache source handle)
	   handle))))


    

#!
(define test (make-faust-object "/home/kjetil/snd-run/osc"))
!#




;; Faust gui

;;(compile-faust "/home/kjetil/snd-run/dummy" :make-gui #t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Eval-C code to connect snd-rt and faust ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Faust dsp

(eval-c ""
	"#include <dlfcn.h>"
	"#include <unistd.h>"

	(shared-struct <mus_rt_faust>)

	(<void> cleanup (lambda ((<struct-mus_rt_faust-*> faust)
				 ((<void> (<void*>)) conclude)
				 ((<void> (<void*>)) deleteDsp))
			  (printf (string "Cleaning up old faust object %p\\n") faust)
			  
			  (conclude faust->dsp)
			  (deleteDsp faust->dsp)
			  
			  (for-each 0 faust->num_inputs
				    (lambda (n)
				      (free faust->ins[n])))
			  (for-each 0 faust->num_outputs
				    (lambda (n)
				      (free faust->outs[n])))
			  (free faust->ins)
			  (free faust->outs)
			  
			  ;; faust->handle is cached and can be reused.
			  ;;(dlclose faust->handle)
			  
			  (free faust)))
	
	(public
	 (<void*> faust-get-gui (lambda ((<struct-mus_rt_faust-*> faust)
					 ((<void*> (<char*> <void*>)) newGTKUI)
					 (<void*> window)
					 ((<void> (<void*> <void*>)) buildUserInterface))
				  (<void*> gtkui (newGTKUI (string "testing") window))
				  (buildUserInterface faust->dsp gtkui)
				  (return gtkui)))
	 
	 (<void> c-cleanup-faust-object (lambda ((<struct-mus_rt_faust-*> faust))
					  (cleanup faust faust->conclude faust->deleteDsp)))

	 (<void> start-faust-gui (lambda (
					  (<struct-mus_rt_faust-*> faust)
					  (<void*> gtkui)
					  ((<void> (<void*>)) runGTKUI)
					  )
				   ;;(printf (string "ui: %p\\n") gtkui)
				   (runGTKUI gtkui)
				   return
				   ))

	 ;; (-> faust init-gui) must have been called before calling faust-contains-ui?
	 (<SCM> faust-contains-ui? (lambda (
					   (<struct-mus_rt_faust-*> faust)
					   ((<int> (<void*>)) containsUI)
					   (<void*> gtkui)
					   )
				    (printf (string "containss? %d\\n" ) (containsUI gtkui))
				    (if (containsUI gtkui)
					(return SCM_BOOL_T)
					(return SCM_BOOL_F))))
	 
	 (<SCM> init-faust-c-object (lambda (
					     (<struct-mus_rt_faust-*> faust)
					     ((<void*> (<void>)) newDsp)
					     ((<int> (<void*>)) getNumInputs)
					     ((<int> (<void*>)) getNumOutputs)
					     ((<void> (<void*> <int>)) init)
					     ;((<void*> (<char*>)) newGTKUI)
					     ;((<void> (<void*> <void*>)) buildUserInterface)
					     ;((<void> (<void*>)) runGTKUI)
					     )
				      ;;(printf (string "ai %p\\n") newDsp);faust)
				      (set! faust->dsp (newDsp))
				      (if (== NULL faust->dsp)
					  (return SCM_BOOL_F))

				      (init faust->dsp ,(mus-srate))

				      (set! faust->num_inputs (getNumInputs faust->dsp))
				      (set! faust->num_outputs (getNumOutputs faust->dsp))

				      (set! faust->ins (calloc (sizeof <float-*>) faust->num_inputs))
				      (set! faust->outs (calloc (sizeof <float-*>) faust->num_outputs))
				      
				      (for-each 0 faust->num_inputs
						(lambda (n)
						  (set! faust->ins[n] (calloc (sizeof <float>) ,rt-max-frame-size))))
				      (for-each 0 faust->num_outputs
						(lambda (n)
						  (set! faust->outs[n] (calloc (sizeof <float>) ,rt-max-frame-size))))
				      
				      (return SCM_BOOL_T)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Create faust object ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (cleanup-faust-object faust)
  (c-display "      TRYING TO CLEANUP")
  )
;  (c-cleanup-faust-object faust))

(define faust-gui-faust #f)

(define (make-faust-object source) ;;file)

  (define handle (find-faust-cache source))
  (define first-one? (not has-compiled-faust-gui))

  (if (not handle)
      (set! handle (compile-faust source first-one?)))

  (let ((faust (<mus_rt_faust>))
	;;(handle (c-dynamic-handle (<-> file ".so") :flags (RTLD_LOCAL)))
	)
    (if first-one?
	(set! faust-gui-faust faust))

    (-> faust handle handle)

    (cond ((not handle)
	   #f)
	  ((not (init-faust-c-object (-> faust get-c-object)
				     (c-dlsym handle "newDsp")
				     (c-dlsym handle "getNumInputs")
				     (c-dlsym handle "getNumOutputs")
				     (c-dlsym handle "init")
				     ))
	   (c-display "Unable to create new Faust Dsp object.")
	   #f)
	  (else
	   (-> faust compute_func (c-dlsym handle "compute"))
	   (-> faust add-method 'init-gui
	       (lambda (dialog)
		 (let ((gtkui (faust-get-gui (-> faust get-c-object)
					     (c-dlsym (-> faust-gui-faust handle) "newGTKUI")
					     (-> dialog dialog)
					     (c-dlsym handle "buildUserInterface"))))
		   (-> faust add-method 'gtkui (lambda ()
						 gtkui)))))

	   (-> faust add-method 'contains-ui? (lambda ()
					       ;;(-> faust init-gui)
					       (faust-contains-ui? (-> faust get-c-object)
								  (c-dlsym (-> faust-gui-faust handle) "containsUI")
								  (-> faust gtkui))))
	   (-> faust add-method 'open-gui
	       (lambda (dialog)
		 ;;(define d (<dialog> "hello" #f))
		 ;(define d (-> dialog dialog))
		 ;(set-car! d "POINTER")
		 ;(c-display "dalog: " d)
		 (start-faust-gui (-> faust get-c-object)
				  (-> faust gtkui)
				  (c-dlsym (-> faust-gui-faust handle) "runGTKUI"))))

	   (-> faust conclude (c-dlsym (-> faust handle) "conclude"))
	   (-> faust deleteDsp (c-dlsym (-> faust handle) "deleteDsp"))
	   (add-finalizer (-> faust get-c-object) cleanup-faust-object)
	   ;;(add-finalizer faust cleanup-faust-object)
	   faust))))



#!
(define test (make-faust-object "/home/kjetil/snd-run/osc"))
(-> test get-c-object)
(-> test dir)
(-> test compute_func)
(-> test dsp)
(-> test num_inputs)
(-> test num_outputs)
(-> test ins)

!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Generate faust code ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-faust-parse term)
  (set! *eval-macro-prefix* eval-faust-macro-prefix)
  (eval-parse term))

(define-faust-macro (= a b)
  (<-> (eval-faust-parse a) "= " (eval-faust-parse b) ";"))

(define-faust-macro (import something)
  (<-> "import(" (eval-parse `(string ,something)) ");"))


;; Guile doesn't like "(: etc. etc.)"
(define-faust-macro (colon . rest)
  (define first #t)
  (apply <-> (map (lambda (res)
		    (if first
			(begin
			  (set! first #f)
			  (eval-parse res) )   		
			(<-> ": " (eval-parse res))))
		  rest)))

;; Automatically add (string) around second argument for the gui objects.
(for-each (lambda (funcname)
	    (primitive-eval `(define-faust-macro (,funcname a . rest)
			       (<-> ,(symbol->string funcname)
				    "(" 
				    (eval-parse `(string ,a))
				    (apply <-> (map (lambda (s)
						      (<-> ", " s))
						    (map eval-parse rest)))
				    ")"))))

	  '(button checkbox hslider vslider nentry vgroup hgroup tgroup vbargraph hbargraph))

;; More faust translation stuff added to eval-c.scm


(define (generate-faust-source-file source)
  (fix-defines
   (define basename (tmpnam))
   (define sourcefile (<-> basename ".dsp"))
   (define fd (open-file sourcefile "w"))
   (for-each (lambda (code)
	       (write-line (<-> code "") fd))
	     source)
   (close fd)
   basename))


(define*2 (generate-faust-source :key 
				(autoimport-libs #t)
				:rest terms)
  (c-display "terms" terms)
  (set! terms (map eval-faust-parse terms))
  (when autoimport-libs
    (set! terms (cons "import(\"music.lib\");" terms))
    ;;(write-line "import(\"effect.lib\");" fd)
    ;;(write-line "import(\"filter.lib\");" fd)
    ;;(write-line "import(\"math.lib\");" fd)
    )
  terms)


#!
(generate-faust-source '(import "ai"))

(generate-faust-source #:autoimport-libs #f
		       "process = osc(500);")
(generate-faust-source "process = osc(500);")
(generate-faust-source "process = osc(500);")
!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; <rt-faust> in Snd-rt ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; (frame by frame processing) ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			   
(<rt-type> '<faust>
	   (lambda (faust)
	     (and (object? faust)
		  (eq? '<mus_rt_faust> (-> faust class-name))))
	   #f
	   :transformfunc
	   (lambda (faust)
	     (-> faust get-c-object))
	   :c-type '<struct-mus_rt_faust-*>)

(rt-ec-function <vct-*> rt_faust_compute
		(lambda (,rt-globalvardecl
			 (<struct-mus_rt_faust-*> faust)
			 (<vct-*> input))
		  (<FaustComputeFunc> compute faust->compute_func)
		  (<vct-*> output (rt_alloc_vct rt_globals faust->num_outputs))
		  ;;(printf (string "faust->compute_func: %p %p\\n") compute faust->dsp)
		  (let* ((minin <int> (MIN faust->num_inputs input->length)))
		    (for-each 0 minin
			      (lambda (n)
				(set! faust->ins[n][0] input->data[n])))
		    (compute faust->dsp 1 faust->ins faust->outs)
		    (for-each 0 faust->num_outputs
			      (lambda (n)
				(set! output->data[n] faust->outs[n][0])))
		    (return output))))

(<rt-func> 'rt_faust_compute '<vct-*> '(<faust> <vct-*>) :needs-rt-globals #t)

(define-rt-macro (faust-compute faust-object input)
  `(rt_faust_compute ,faust-object ,input))


(define-macro (start-faust-gui-if-necessary faust)
  `(let ((dialog (default-rt-dialog (%delay rt-current-rt) :show #f)))
     (-> ,faust init-gui dialog)
     ;;(c-display "contains?" (-> faust contains-ui?))
     (when (-> ,faust contains-ui?)
       (-> ,faust open-gui dialog)
       (-> dialog show))))

(define*2 (<rt-faust-do> :key 
			 (in '(vct))
			 :rest code)
  ;;(c-display "input:" input)
  ;;(set! code (map eval-faust-parse code))
  (set! code (apply generate-faust-source code))
  (c-display "code" code)
  `(faust-compute (extern (let ((faust (make-faust-object ',code)))
			    (start-faust-gui-if-necessary faust)
			    faust))
		  ,in))


(define-rt-macro (<rt-faust-vct> . rest)
  (apply <rt-faust-do> rest))

(define-rt-macro (<rt-faust> . rest)
  `(vct-ref (<rt-faust-vct> ,@rest) 0))


#!
(<rt-out> (vct-scale! (<rt-faust-vct> "process=osc(400),osc(500);")
		      (<slider> "vol" 0.0 0.1 1.0)))

(<rt-out> (* (<rt-faust> "freq = vslider(\"freq\", 600, 0, 2400, 0.1);"
			 "process = vgroup(\"ai\",osc(freq));")
	     (<slider> "vol" 0.0 0.1 1.0)))

(<rt-out> (* (<rt-faust> (= freq    (vslider "freq" 600 0 2400 0.1))
			 (= process (vgroup "ai" (osc freq))))
	     (<slider> "vol" 0.0 0.1 1.0)))

(<rt-out> (<rt-faust> :in (vct (oscil))
		      "process=*(0.3);"))
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; <rt-faust> in Guile ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; (block processing)  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (<rt-faust> . code)
  (define faust (rt-gensym))
  (define rt (rt-gensym))

  (set! code (apply generate-faust-source code))

  `(let* ((,faust (make-faust-object ',code))
	  (,rt (<rt> (lambda () (out (in))))))
     (-> ,rt faust ,faust)
     (-> ,rt play)
     (let ((rt-current-rt ,rt))
       (start-faust-gui-if-necessary ,faust))
     ,rt))


#!
(<rt-faust> "freq    = hslider(\"freq\", 600, 0, 2400, 0.1);"
	    "process = vgroup(\"ai\",osc(freq));")

(<rt-faust> "process = osc(500)*0.2;")

(<rt-faust> "process=osc(400);")
(<rt-faust> (= process (osc 400)))



(let ((in-bus *out-bus*))
  (<rt-faust> "vmeter(x)       = attach(x, envelop(x) : vbargraph(\"dB\", -96, 10));
 	       hmeter(x)       = attach(x, envelop(x) : hbargraph(\"dB\", -96, 10));
	       envelop         = abs : max(db2linear(-96)) : linear2db : min(10)  : max ~ -(96.0/SR);
	       process         = vmeter;")
  (<rt-faust> "freq = vslider(\"freq\", 600, 0, 2400, 0.1);"
	      (= vol (colon (hslider "volume (db)" 0 -96 0 0.1) db2linear))
	      "process = vgroup(\"ai\",osc(freq)*vol);"))


;; To try the next one, go to the faust software catalog at http://faust.grame.fr/catalog.php ,
;; open the code of a program, and select the code. But
;; don't include import("music.lib")!
(<rt-faust> (x11-selection))


(define filename "/home/kjetil/snd-run/osc.dsp")
(<rt-faust> (include ,filename))
(<rt-faust> (include "/home/kjetil/snd-run/osc.dsp"))

(<rt-faust> (import "math.lib")
	    (= (smooth c) (colon "*(1-c)" "+~*(c)"))
	    (= vol        (colon  (hslider "volume (db)" 0 -96 0 0.1)
				  db2linear
				  (smooth 0.99)))
	    (= freq       (hslider "freq" 1000 0 24000 0.1))
	    (= process    (vgroup "Osc" (* (osci freq) vol))))




(let ((abus (make-bus)))
  (<rt-faust> "smooth(c)   = *(1-c) : +~*(c);"
	      "gain        = vslider(\" dB \", 0, -96, 4, 0.1) : db2linear : smooth(0.999);"
	      "process     = vgroup(\"fader\", *(gain));")
  (set! (-> *rt* in-bus) abus) ;; *rt* contains the result of the last evaluated <rt*> expression.
  (<rt-out> (oscil))
  (set! (-> *rt* out-bus) abus))



!#
