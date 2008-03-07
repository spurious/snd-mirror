
(define-ec-struct <mus_rt_faust>
  <void*> dsp
  <void*> compute_func
  <float-**> ins
  <float-**> outs
  <int> num_inputs
  <int> num_outputs)



(define (compile-faust file)
  (system (<-> "FAUST_PATH=/home/kjetil/site/lib/faust/ "
	       "/home/kjetil/site/bin/faust -a module.cpp -o " file ".cpp " file ".dsp"))
  (system (<-> "g++ -O3 " (string-append (string #\`) "pkg-config --cflags --libs gtk+-2.0" (string #\`)) " "
	       file ".cpp" " -c ")))

(define (make-faust-object file)
  (compile-faust file)
  (eval-c "/home/kjetil/snd-run/osc.o"
	  (shared-struct <mus_rt_faust>)
	  "extern void compute(void* self, int len, float** inputs, float** outputs);"
	  (proto->public
	   "dsp* newDsp();"
	   "void deleteDsp(dsp* self);"
	   "int getNumInputs(dsp* self);"
	   "int getNumOutputs(dsp* self);"
	   "void buildUserInterface(dsp* self,UI* interface);"
	   "void init(dsp* self, int freq);"
	   ;;"void compute(dsp* self, int len, float** inputs, float** outputs);"
	   "void conclude(dsp* self);")
	  (variables->public
	   (<void*> compute))
	  (public
	   (<void> init-faust-c-object (lambda ((<struct-mus_rt_faust-*> faust))
					 (printf (string "ai\\n"))
					 (set! faust->ins (calloc (sizeof <float-*>) faust->num_inputs))
					 (set! faust->outs (calloc (sizeof <float-*>) faust->num_outputs))
					 (for-each 0 faust->num_inputs
						   (lambda (n)
						     (set! faust->ins[n] (calloc (sizeof <float>) ,rt-max-frame-size))))
					 (for-each 0 faust->num_outputs
						   (lambda (n)
						     (set! faust->outs[n] (calloc (sizeof <float>) ,rt-max-frame-size))))
					 ))))
  
  (let ((faust (<mus_rt_faust>))
	(dsp (newDsp)))
    (-> faust dsp dsp)
    (init dsp (mus-srate))
    (-> faust compute_func (compute))
    (-> faust add-method 'delete (lambda () (conclude dsp)))
    (-> faust num_inputs (getNumInputs dsp))
    (-> faust num_outputs (getNumOutputs dsp))
    (init-faust-c-object (-> faust get-c-object))
    faust))

#!
(define test (make-faust-object "/home/kjetil/snd-run/osc"))
(-> test get-c-object)
(-> test dir)
(-> test delete)
(-> test compute_func)
(-> test dsp)
(-> test num_inputs)
(-> test num_outputs)
(-> test ins)
!#

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

	   
#!
(define test (make-faust-object "/home/kjetil/snd-run/osc"))
(<rt-play> (lambda ()
	     (out (rt_faust_compute test (vct )))))
!#

#!
(define obj (newDsp))
(init obj (mus-srate))

(<rt-faustfile> filename)

(define guardian (make-guardian))

(let ((obj (list 1 2 3)))
  (guardian obj))

(guardian (list 9 2 3))
(guardian)
(begin guardian)
(gc)

!#


#!

(define (run-faust file)
  #t)

(let ((old-compiler *eval-c-compiler*))
  (set! *eval-c-compiler* "g++")
  (eval-c "/home/kjetil/snd-run/osc.cpp"
	  (run-now
	   (<mydsp*> dsp (new <mydsp>))
	   (dsp->init ,(mus-srate))))
  (set! *eval-c-compiler* old-compiler)))



(define faust-wrapper-compiled #f)
(define (compile-faust-wrapper)
  (when (not faust-wrapper-compiled)
    (system (<-> "g++ -O3 faustwrapper.cpp -shared -o /tmp/tmp.so"))
    (c-display "hmm")
    (c-dynamic-call "faustwrapper_startup" "/tmp/tmp.so")))

!#

#!
(compile-faust-wrapper)
!#
