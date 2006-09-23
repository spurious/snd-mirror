
#!

rt-engine.scm
-Kjetil S. Matheussen/Notam, 2005

rt-engine.scm is developed with support from Notam/Oslo [1]
and the Arts Council Norway [2].

rt-engine creates a realtime engine that should be suitable for hard real
time signal processing.

This file is normally loaded from "rt-compiler.scm".

For documentation, check out
http://www.notam02.no/arkiv/doc/snd-rt/


[1] http://www.notam02.no
[2] http://www.kulturradet.no

!#


(if (not (provided? 'snd-rt-compiler.scm))
    (throw 'rt-compiler-not-loaded))

(if (provided? 'snd-rt-engine.scm)
    (throw 'rt-engine-already-loaded))
(provide 'snd-rt-engine.scm)

(if (not (provided? 'snd-oo.scm)) (load-from-path "oo.scm"))




;; Various general functions and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rt-pointer-size (ec-sizeof-pointer))

(define rt-to-ringbuffer-size (* rt-pointer-size 8192))
(define rt-from-ringbuffer-size (* 1024 1024))

(define rt-allocmem-size (* 1024 1024 8))

(define rt-max-frame-size 4096)

(if (not (defined? '*rt-num-input-ports*))
    (primitive-eval '(define *rt-num-input-ports* 8)))

(if (not (defined? '*rt-num-output-ports*))
    (primitive-eval '(define *rt-num-output-ports* 8)))

(if (not (defined? '*rt-jackname-prefix*))
    (primitive-eval `(define *rt-jackname-prefix* "snd-rt")))

(define rt-max-cpu-usage 80)

(define *out-bus* #f)
(define *in-bus* #f)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; BUS. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#!
Behavior logic for reading and writing:

Reading: bus[n]
Reading one period later[1][2]: bus[n]
Reading two periods later[1]: 0 

writing: bus[n]+=v
writing one period later[1]: bus[n]=v

[1] than the frame was last written to.
[2] This behaviour seems to be different from Supercollider. According to the
    file order-of-execution.rtf, in.ar "zeros any data from the previous
    cycle", which I guess means this behaviour: (ret=bus[n],bus[n]=0,ret).

!#


(define bus-struct (<-> "struct rt_bus_data{"
			"  int last_written_to;"
			"  float val;"
			"};"
			"struct rt_bus{"
			"  int num_channels;"
			"  struct rt_bus_data data[];"
			"}"))

(eval-c ""
	;;;;;;; Das SMOB

	,bus-struct
	
	(<nonstatic-scm_t_bits> rt_bus_tag)
	
	(public
	  (<SCM> rt-bus-p (lambda ((<SCM> rt_bus_smob))
			       (if (SCM_SMOB_PREDICATE rt_bus_tag rt_bus_smob)
				   (return SCM_BOOL_T)
				   (return SCM_BOOL_F))))
	  (<SCM> make-bus2 (lambda ((<int> num_channels))
			     (let* ((ret <struct-rt_bus-*> (calloc 1 (+ (sizeof <struct-rt_bus>)
									(* (sizeof <struct-rt_bus_data>)
									   ,rt-max-frame-size
									   num_channels))))
				    (scmret <SCM>))

			       (set! ret->num_channels num_channels)
			       
			       ;(for-each 0 num_channels
				;	 (lambda (ch)
				;	   (set! ret->data[ch] (calloc (sizeof <float>) ,rt-max-frame-size))))

			       (SCM_NEWSMOB scmret rt_bus_tag ret)
			       (return scmret)))))
	 
	;;(<SCM> mark_rt_bus (lambda ((<SCM> rt_bus_smob))
	;;			 (let* ((rt_bus <struct-mus_rt_bus-*> (cast <void-*> (SCM_SMOB_DATA rt_bus_smob))))
	;;			   (return rt_bus->scm_bus))))
	(<size_t> free_rt_bus (lambda ((<SCM> rt_bus_smob))
				(let* ((rt_bus <struct-rt_bus-*> (cast <void-*> (SCM_SMOB_DATA rt_bus_smob))))
				  ;(for-each 0 rt_bus->num_channels
				;	    (lambda (ch)
				;	      (free rt_bus->data[ch])))
				  (free rt_bus)
				  (return 0))))
	
	(<int> print_rt_bus (lambda ((<SCM> rt_bus_smob) (<SCM> port) (<scm_print_state-*> pstate))
			      (scm_puts (string "#<rt_bus ... > ") port)
			      (return 1)))

	(run-now
	 (set! rt_bus_tag (scm_make_smob_type (string "rt_bus") (sizeof <struct-rt_bus>)))
	 ;;(scm_set_smob_mark rt_bus_tag mark_rt_bus)
	 (scm_set_smob_free rt_bus_tag free_rt_bus)
	 (scm_set_smob_print rt_bus_tag print_rt_bus)))
			    
(define-macro (make-bus . rest)
  (if (null? rest)
      `(make-bus2 1)
      `(make-bus2 ,(car rest))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Jack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin
  (eval-c-add-int-type "jack_nframes_t")
  (eval-c-add-int-type "jack_port_id_t")
  (eval-c-add-float-type "jack_default_audio_sample_t")
  
  (eval-c ""
	  
	  "#include <jack/jack.h>"
	  "#include <jack/ringbuffer.h>"

	  (proto->public
	   "jack_client_t *jack_client_new (const char *client_name);
int jack_client_close (jack_client_t *client);
int jack_client_name_size(void);
int jack_internal_client_new (const char *client_name, const char *so_name,
			      const char *so_data);
void jack_internal_client_close (const char *client_name);
int jack_is_realtime (jack_client_t *client);
void jack_on_shutdown (jack_client_t *client, void (*function)(void *arg), void *arg);
int jack_set_process_callback (jack_client_t *client,
			       JackProcessCallback process_callback,
			       void *arg);
int jack_set_thread_init_callback (jack_client_t *client,
				   JackThreadInitCallback thread_init_callback,
				   void *arg);
int jack_set_freewheel_callback (jack_client_t *client,
				 JackFreewheelCallback freewheel_callback,
				 void *arg);
int jack_set_freewheel(jack_client_t* client, int onoff);
int jack_set_buffer_size (jack_client_t *client, jack_nframes_t nframes);
int jack_set_buffer_size_callback (jack_client_t *client,
				   JackBufferSizeCallback bufsize_callback,
				   void *arg);
int jack_set_sample_rate_callback (jack_client_t *client,
				   JackSampleRateCallback srate_callback,
				   void *arg);
int jack_set_port_registration_callback (jack_client_t *,
					 JackPortRegistrationCallback
					 registration_callback, void *arg);

int jack_set_graph_order_callback (jack_client_t *, JackGraphOrderCallback graph_callback, void *);

int jack_set_xrun_callback (jack_client_t *, JackXRunCallback xrun_callback, void *arg);
int jack_activate (jack_client_t *client);
int jack_deactivate (jack_client_t *client);
jack_port_t *jack_port_register (jack_client_t *client,
                                 const char *port_name,
                                 const char *port_type,
                                 unsigned long flags,
                                 unsigned long buffer_size);

int jack_port_unregister (jack_client_t *, jack_port_t *);
void *jack_port_get_buffer (jack_port_t *, jack_nframes_t);
const char *jack_port_name (const jack_port_t *port);
const char *jack_port_short_name (const jack_port_t *port);
int jack_port_flags (const jack_port_t *port);
const char *jack_port_type (const jack_port_t *port);
int jack_port_is_mine (const jack_client_t *, const jack_port_t *port);
int jack_port_connected (const jack_port_t *port);
int jack_port_connected_to (const jack_port_t *port,
			    const char *port_name);

const char **jack_port_get_connections (const jack_port_t *port);
const char **jack_port_get_all_connections (const jack_client_t *client,
					    const jack_port_t *port);
int  jack_port_tie (jack_port_t *src, jack_port_t *dst);

int  jack_port_untie (jack_port_t *port);
int jack_port_lock (jack_client_t *, jack_port_t *);

int jack_port_unlock (jack_client_t *, jack_port_t *);
jack_nframes_t jack_port_get_latency (jack_port_t *port);
jack_nframes_t jack_port_get_total_latency (jack_client_t *,
					    jack_port_t *port);
void jack_port_set_latency (jack_port_t *, jack_nframes_t);
int jack_port_set_name (jack_port_t *port, const char *port_name);

int jack_port_request_monitor (jack_port_t *port, int onoff);
int jack_port_request_monitor_by_name (jack_client_t *client,
				       const char *port_name, int onoff);
"
;;int jack_port_ensure_monitor (jack_port_t *port, int onoff);
"
int jack_port_monitoring_input (jack_port_t *port);
int jack_connect (jack_client_t *,
		  const char *source_port,
		  const char *destination_port);
int jack_disconnect (jack_client_t *,
		     const char *source_port,
		     const char *destination_port);
"
;;int jack_port_connect (jack_client_t *, jack_port_t *src, jack_port_t *dst);
"
int jack_port_disconnect (jack_client_t *, jack_port_t *);
int jack_port_name_size(void);
int jack_port_type_size(void);
jack_nframes_t jack_get_sample_rate (jack_client_t *);
jack_nframes_t jack_get_buffer_size (jack_client_t *);
const char **jack_get_ports (jack_client_t *, 
			     const char *port_name_pattern, 
			     const char *type_name_pattern, 
			     unsigned long flags);
jack_port_t *jack_port_by_name (jack_client_t *, const char *port_name);
jack_port_t *jack_port_by_id (const jack_client_t *client,
			      jack_port_id_t port_id);
int  jack_engine_takeover_timebase (jack_client_t *);
jack_nframes_t jack_frames_since_cycle_start (const jack_client_t *);
jack_nframes_t jack_frame_time (const jack_client_t *);
float jack_cpu_load (jack_client_t *client);
pthread_t jack_client_thread_id (jack_client_t *);
"
;;extern void (*jack_error_callback)(const char *msg);
"void jack_set_error_function (void (*func)(const char *));")



	  (set!-string-is-pointer-#t)

	  (proto->public
	   "jack_ringbuffer_t *jack_ringbuffer_create(size_t sz);

void jack_ringbuffer_free(jack_ringbuffer_t *rb);
void jack_ringbuffer_get_read_vector(const jack_ringbuffer_t *rb,
				     jack_ringbuffer_data_t *vec);
void jack_ringbuffer_get_write_vector(const jack_ringbuffer_t *rb,
				      jack_ringbuffer_data_t *vec);
size_t jack_ringbuffer_read(jack_ringbuffer_t *rb, char *dest, size_t cnt);
size_t jack_ringbuffer_peek(jack_ringbuffer_t *rb, char *dest, size_t cnt);
void jack_ringbuffer_read_advance(jack_ringbuffer_t *rb, size_t cnt);
size_t jack_ringbuffer_read_space(const jack_ringbuffer_t *rb);
int jack_ringbuffer_mlock(jack_ringbuffer_t *rb);
void jack_ringbuffer_reset(jack_ringbuffer_t *rb);
size_t jack_ringbuffer_write(jack_ringbuffer_t *rb, const char *src,
			     size_t cnt);
void jack_ringbuffer_write_advance(jack_ringbuffer_t *rb, size_t cnt);
size_t jack_ringbuffer_write_space(const jack_ringbuffer_t *rb);

")

	  (set!-string-is-pointer-#f)
	  
	  (variables->public
	   (<int> JackPortIsInput
		  JackPortIsOutput 
		  JackPortIsPhysical
		  JackPortCanMonitor
		  JackPortIsTerminal)
	   (<char*> JACK_DEFAULT_AUDIO_TYPE))
	  
	  ))



(define-ec-struct <Jack_Arg>
  <int> frames
  <int> is_running

  <void-*> rt_callback

  <jack_client_t-*> client
  
  <int> num_inports
  <jack_port_t**> input_ports
  
  <int> num_outports
  <jack_port_t**> output_ports

  <struct-rt_bus-*> out_bus
  <struct-rt_bus-*> in_bus
  
  <void-*> rt_arg

  <float> samplerate
  
  )




(def-class (<jack> name process-func jack-arg num-inports num-outports #:key (autoconnect #t))

  (Super jack-arg)
  
  (define client #f)

  (def-var in-bus #f)
  (def-var out-bus #f)
  
  (def-method (get-client)
    client)

  (def-method (get-arg)
    jack-arg)

  (def-method (get-samplerate)
    (jack_get_sample_rate client))

  (define (clean-up)
    (-> jack-arg destructor))
  
  ;; Destructor (works as a)
  (def-method (close)
    (if client
	(begin
	  (jack_client_close client)
	  (set! client #f)
	  (clean-up))))

  ;; Constructor
  (if (not (call-with-current-continuation
	    (lambda (return)

	      (if (not (ec-pointer? process-func))
		  (begin
		    (c-display "\n\nError. <jack>/constructor: process-func is not an ec-pointer.\n\n")
		    (return #f)))


	      (call-with-current-continuation
	       (lambda (got-it)
		 (for-each (lambda (postfix)
			     (set! client (jack_client_new (<-> name postfix)))
			     (if client
				 (begin
				   (got-it))))
			   '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15"))))
	      
	      (if (not client)
		  (begin
		    (c-display "Could not create jack client with name \"" name "\".")
		    (return #f)))

	      (-> jack-arg client client)

	      ;; Note to myself, why the if???
	      (if num-inports
		  (-> jack-arg num_inports num-inports))
	      (if num-outports
		  (-> jack-arg num_outports num-outports))

	      (set! this->in-bus (make-bus num-inports))
	      (set! this->out-bus (make-bus num-outports))
	      (-> jack-arg in_bus (SCM_SMOB_DATA this->in-bus))
	      (-> jack-arg out_bus (SCM_SMOB_DATA this->out-bus))
	      
	      (jack_set_process_callback client process-func (-> jack-arg get-c-object))
	      
	      (-> jack-arg input_ports (map (lambda (n)
					      (jack_port_register client (format #f "input~A" n) (JACK_DEFAULT_AUDIO_TYPE) (JackPortIsInput) 0))
					    (iota num-inports)))
	      (-> jack-arg output_ports (map (lambda (n)
					       (jack_port_register client (format #f "output~A" n) (JACK_DEFAULT_AUDIO_TYPE) (JackPortIsOutput) 0))
					     (iota num-outports)))
	      (if (not (= 0 (jack_activate client)))
		  (begin
		    (c-display "Cannot activate client")
		    (return #f)))

	      (if autoconnect
		  (let ((portnames (jack_get_ports client #f #f (logior (JackPortIsPhysical) (JackPortIsOutput)))))
		    (if (not portnames)
			(c-display "No Output physical jack ports found")
			(for-each (lambda (portname port)
				    (jack_connect client portname (jack_port_name port)))
				  (ec-get-strings portnames)
				  (-> jack-arg input_ports)))))
	      
	      (if autoconnect
		  (let ((portnames (jack_get_ports client #f #f (logior (JackPortIsPhysical) (JackPortIsInput)))))
		    (if (not portnames)
			(c-display "No Output physical jack ports found")
			(for-each (lambda (portname port)
				    (jack_connect client (jack_port_name port) portname))
				  (ec-get-strings portnames)
				  (-> jack-arg output_ports)))))
	      
	      (return #t)
	      )))
      (begin
	(this->close)
	(set! this #f)))
  
  
  )

(define rt-callback-type "typedef void (*Callback)(void *arg,jack_client_t *client,int is_running,int num_frames,int base_time,float samplerate)")

(eval-c ""
	"#include <jack/jack.h>"

	,bus-struct
	
	(shared-struct <Jack_Arg>)

	,rt-callback-type

	(functions->public
	 (<int> jack_rt_process_dummy (lambda ((<jack_nframes_t> nframes)
					       (<void*> arg))
					(return 0)))
	 
	 (<int> jack_rt_process (lambda ((<jack_nframes_t> nframes)
					 (<void-*> arg))

				  
				  (let* ((jack_arg <struct-Jack_Arg-*>
						   arg)
					 (callback <Callback> jack_arg->rt_callback)
					 
					 (out[jack_arg->num_outports] <float-*>)
					 (in[jack_arg->num_inports] <float-*>)

					 (lokke <int>)
					 )

				    (for-each 0 jack_arg->num_outports
					      (lambda (ch)
					      (set! out[ch] (jack_port_get_buffer jack_arg->output_ports[ch] nframes))))
				    (for-each 0 jack_arg->num_inports
					    (lambda (ch)
					      (set! in[ch] (jack_port_get_buffer jack_arg->input_ports[ch] nframes))))
				    
				    (set! lokke 0)
				    (for-each 0 nframes
					      (lambda (n)
						(for-each 0 jack_arg->num_inports
							  (lambda (ch)
							    (set! jack_arg->in_bus->data[lokke].val
								  in[ch][n])
							    (set! jack_arg->in_bus->data[lokke].last_written_to
								  jack_arg->frames)
							    lokke++))))

				    (for-each 0 (* nframes jack_arg->num_outports)
					      (lambda (n)
						(set! jack_arg->out_bus->data[n].val 0.0f)
						(set! jack_arg->out_bus->data[n].last_written_to
						      jack_arg->frames)))
				    
				    (callback jack_arg->rt_arg
					      jack_arg->client
					      jack_arg->is_running
					      nframes
					      jack_arg->frames
					      jack_arg->samplerate)
				    
				    (set! lokke 0)
				    (for-each 0 nframes
					      (lambda (n)
						(for-each 0 jack_arg->num_outports
							  (lambda (ch)
							    (set! out[ch][n]
								  jack_arg->out_bus->data[lokke].val)
							    lokke++))))
				    
				    (if jack_arg->is_running
					(+= jack_arg->frames nframes))
				    
				    (return 0))))))



(def-class (<jack-rt-driver> num-inputs num-outputs rt_callback rt_arg #:key (autoconnect #t))

  (Super (<jack> *rt-jackname-prefix*
		 (jack_rt_process)
		 (<Jack_Arg> #:rt_callback rt_callback #:rt_arg rt_arg)
		 num-inputs
		 num-outputs
		 autoconnect))

  (-> super samplerate (-> super get-samplerate))
  (set! (mus-srate) (-> super get-samplerate))
  
  (def-var num-inputs num-inputs)
  (def-var num-outputs num-outputs)

  (define jack-arg #f)

  (define jack #f)

  (def-method (get-frame-time time)
    (* time (-> jack-arg samplerate)))
  
  (def-method (get-time)
    (/ (-> jack-arg frames) (-> jack-arg samplerate)))

  (def-method (set-input-args args)
    (-> jack-arg rt_input_args args))

  (def-method (set-output-args args)
    (-> jack-arg rt_output_args args))

  (def-method (start)
    (c-display "<jack-rt-driver>: starting!")
    (-> jack-arg frames 0)
    (-> jack-arg is_running 1))
  
  (def-method (stop)
    (c-display "<jack-rt-driver>: stop.")
    (-> jack-arg frames 0)
    (-> jack-arg is_running 0))

  (def-method (pause)
    (c-display "<jack-rt-driver>: pause.")
    (-> jack-arg is_running 0))

  (def-method (continue)
    (c-display "<jack-rt-driver>: continue.")
    (-> jack-arg is_running 1))

  (def-method (destructor)
    (c-display "<jack-rt-driver> -> destructor")
    (-> jack close))


  (if super
      (begin
	(set! jack super)
	(set! jack-arg (-> jack get-arg)))
      (set! this #f))

  )




#!

(define jack-arg (<Jack_Arg>))
(define jack (<jack> "snd-rt" (jack_rt_process_dummy) jack-arg 2 3))
(-> jack close)


(define obj (Jack_Arg_new))
(begin (jack_rt_process))
(Jack_Arg_set_rt_callback obj (jack_rt_process) 0)

(define jack-arg (<Jack_Arg>))
(-> jack-arg get-c-object)
(Jack_Arg_set_rt_callback (-> jack-arg get-c-object) (jack_rt_process) 0)
(-> jack-arg rt_callback (jack_rt_process))

(define jack-arg (<Jack_Arg> #:rt_callback (jack_rt_process)))

(define jack-driver (<jack-driver> 5 6))
(-> jack-driver destructor)

(begin (jack_rt_process))


(define hobbit-path "/usr/local/share/guile-hobbit")

(set! %load-path (cons hobbit-path %load-path))
(use-modules (slib defmacroexpand) (slib generic-write))
(use-modules (slib generic-write))

(generic-write jack-arg
	       #f
	       200
	       display)
(begin <Jack_Arg>)


(define a (<RT_Event>))
(begin (-> a get-c-object))
(ec-pointer-to-pointer (-> a get-c-object))

!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; PD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-ec-struct <PD_Arg>
;  <void-*> rt_callback
;  )

(if (provided? 'snd-pd-external)
    (eval-c ""
	    
	    "#include <jack/jack.h>"
	    
	    ,rt-callback-type
	    ,bus-struct
	    "extern float sys_getsr(void)"
	    
	    (<int> base_time 0)
	    (<void-*> engine)
	    (<Callback> callback)
	    
	    (public
	     (<void> pd_rt_init (lambda ((<void-*> das_engine)
					 (<Callback> das_callback))
				  (set! engine das_engine)
				  (set! callback das_callback)))
	     (<float> pd_rt_get_samplerate (lambda ()
					     (return (sys_getsr))))
	     (<int> pd_rt_get_time (lambda ()
				     (return base_time))))
	    
	    
	    (functions->public
	     ;; Run the engine.
	     (<void> pd_rt_run (lambda ((<int> nframes))
				 (callback engine
					   NULL
					   1
					   nframes
					   base_time
					   (sys_getsr))
				 (+= base_time nframes)))
	     
	     
	     ;; Copy pd-buffers in and out of "*out-bus*" and "*in-bus*".
	     ;; Theres a unique "*out-bus*" bus and "*in-bus*" bus for each pd-object.
	     (<void> pd_rt_process (lambda ((<int> num_ins)
					    (<float**> ins)
					    (<int> num_outs)
					    (<float**> outs)
					    (<void*> das_inbus)
					    (<void*> das_outbus)
					    (<int> nframes))
				     (<int> lokke 0)
				     (<struct-rt_bus-*> in_bus das_inbus)
				     (<struct-rt_bus-*> out_bus das_outbus)
				     
				     ;; pd-inlets -> "*in-bus*"
				     (for-each 0 nframes
					       (lambda (n)
						 (for-each 0 num_ins
							   (lambda (ch)
							     (set! in_bus->data[lokke].val
								   ins[ch][n])
							     (set! in_bus->data[lokke].last_written_to
								   base_time)
							     lokke++))))

				     ;; "*out-bus*" -> pd-outlets
				     (set! lokke 0)
				     (for-each 0 nframes
					       (lambda (n)
						 (for-each 0 num_outs
							   (lambda (ch)
							     (set! outs[ch][n] out_bus->data[lokke].val)
							     lokke++))))
				     ;; 0 -> "*out-bus*"
				     (for-each 0 (* nframes num_outs)
					       (lambda (n)
						 (set! out_bus->data[n].val 0.0f)
						 (set! out_bus->data[n].last_written_to base_time))))))
	    
	    "typedef void (*PD_RT_RUN)(int nframes)"
	    "typedef void (*PD_RT_PROCESS)(int num_ins,float **ins,int num_outs,float **outs,int nframes)"
	    "extern void snd_pd_set_rt_funcs(PD_RT_RUN r,PD_RT_PROCESS p)"
	    (proto->public
	     "void snd_pd_set_rt_funcs(PD_RT_RUN r,PD_RT_PROCESS p);")))



(def-class (<pd-rt-driver> rt_callback c-engine)
  (def-method (start)
    (c-display "<pd-rt-driver>: starting!"))
  
  (def-method (stop)
    (c-display "<pd-rt-driver>: stop."))

  (def-method (pause)
    (c-display "<pd-rt-driver>: pause."))

  (def-method (continue)
    (c-display "<pd-rt-driver>: continue."))

  (def-method (destructor)
    (c-display "<pd-rt-driver> ending"))

  (def-method (get-frame-time time)
    (* time (pd_rt_get_samplerate)))
  (def-method (get-time)
    (/ (pd_rt_get_time) (pd_rt_get_samplerate)))

  ;;(set! (-> c-engine samplerate) (pd_rt_get_samplerate))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Engine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#!
;;Simple sorting scheme for running procfuncs: (too simple?)

struct procfuncs *sorted=procfuncs.pop(0)
struct procfuncs *unsorted=procfuncs
struct procfuncs *next

while(unsorted):
  next=unsorted.next
  while(sorted):
    when any of the write-buses in unserted = any of the read-buses in sorted:
       unsorted.put_before(sorted)
       goto is_sorted
    sorted=sorted.next
  unsorted.put_at_tail(sorted)
  is_sorted:
  unsorted=next

procfuncs=sorted

;;Can this be done more efficiently? And in case, is it necessary?
!#


(define-ec-struct <RT_Event>
  <int> time
  <struct-RT_Event-*> next
  <void-*> func
  <void-*> arg
  )

;; (The double linked-list operations on procfunc can be optimized. (no point regarding cpu-use, but perhaps for readability...))
(define-ec-struct <RT_Procfunc>
  <struct-RT_Procfunc-*> next
  <struct-RT_Procfunc-*> prev

  ;; <int> num_read_buses
  ;; <struct-rt_bus-**> read_buses
  ;; <int> num_write_buses
  ;; <struct-rt_bus-**> write_buses

  <int> visitors
  <void-*> func
  <void-*> arg
  <SCM> toprotect
  <SCM> smob
  )

(define-ec-struct <RT_Engine>
  <jack_ringbuffer_t-*> ringbuffer_to_rt
  <jack_ringbuffer_t-*> ringbuffer_from_rt

  <int> num_lost_events                     ;; Number of events lost because queue was full.
  <int> num_events                          ;; Number of events waiting to be run
  <int> queue_fullsize
  <int> queue_size
  <struct-RT_Event-**> queue                ;; A priority queue with room for queue_fullsize number of events
  ;;<struct-RT_Event-*> events_rt           ;; Sorted events to be scheduled.
  <int> next_switchtime                     ;; The next time events_noninserted2 becomes events_noninserted1
  <int> next_next_switchtime                ;; The next time after that again that events_noninserted2 becomes events_noninserted1
  <struct-RT_Event-*> events_noninserted1   ;; Events that is scheduled 0.0 or more seconds into the future. (unsorted)
  <struct-RT_Event-*> events_noninserted2   ;; Events that is scheduled 0.1 or more seconds into the future. (unsorted)
  <struct-RT_Event-*> events_non_rt

  <int> num_procfuncs
  <struct-RT_Procfunc-*> procfuncs  

  <char-*> allocplace
  <char-*> allocplace_end

  ;; Time must be unsigned! (wrap-around)
  <int> time
  <int> time_before ;; What is this?
  <float> samplerate
  <float> res
  <char-*> error
  <SCM> errorvariable
  <int> errorvarnum
  <jmp_buf> error_jmp

  <int> max_cpu_usage
  <int> num_max_cpu_interrupts
  <struct-RT_Procfunc-*> skip_this_procfunc_next_cycle   ;; If going past max_cpu_usage after the last instance have run, skip this procfunc on the next cycle.
  <int> accumulated_cpu_usage  ;; Remember cpu usage from last cycle and add it to the next, etc. To avoid hanging if one procfunc use more than 200% cpu.
  ;;<int> temp
  
  )



(eval-c ""
	"#include <stdbool.h>"
	"#include <jack/ringbuffer.h>"
	"#include <jack/jack.h>"

	,bus-struct
	
	(shared-struct <RT_Event>)
	(shared-struct <RT_Procfunc>)
	(shared-struct <RT_Engine>)


	;; Procfunc SMOB
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(<scm_t_bits> procfunc_tag)
	
	(public
	 (<void-*> rt_get_procfunc_data (lambda ((<SCM> procfunc_smob))
					  (let* ((procfunc <struct-RT_Procfunc-*> (cast <void-*> (SCM_SMOB_DATA procfunc_smob))))
					    (return procfunc))))
	 
	 (<SCM> rt_make_procfunc (lambda ((<void-*> func)(<void-*> arg)(<SCM> toprotect))
				   (let* ((procfunc <struct-RT_Procfunc-*> (calloc 1 (sizeof <struct-RT_Procfunc>)))
					  (smob <SCM>))
				     (set! procfunc->func func)
				     (set! procfunc->arg arg)
				     (set! procfunc->toprotect toprotect)
				     (SCM_NEWSMOB smob procfunc_tag procfunc)
				     (set! procfunc->smob smob)
				     (return smob)))))
	
	(<SCM> mark_procfunc (lambda ((<SCM> procfunc_smob))
			       (let* ((procfunc <struct-RT_Procfunc-*> (cast <void-*> (SCM_SMOB_DATA procfunc_smob))))
				 (return procfunc->toprotect))))
	(<size_t> free_procfunc (lambda ((<SCM> procfunc_smob))
				  (let* ((procfunc <struct-RT_Procfunc-*> (cast <void-*> (SCM_SMOB_DATA procfunc_smob))))
				    ;;(fprintf stderr (string "Freeing procfunc smob: %u\\n") procfunc)
				    (free procfunc->arg)
				    (free procfunc)
				    (return 0))))
	(<int> print_procfunc (lambda ((<SCM> procfunc_smob) (<SCM> port) (<scm_print_state-*> pstate))
				;;(<struct-RT_Procfunc-*> procfunc (cast <void-*> (SCM_SMOB_DATA procfunc_smob)))
				(scm_puts (string "#<procfunc ... > ") port)
				(return 1)))
	(run-now
	 (set! procfunc_tag (scm_make_smob_type (string "RT_Procfunc") (sizeof <struct-RT_Procfunc>)))
	 (scm_set_smob_mark procfunc_tag mark_procfunc)
	 (scm_set_smob_free procfunc_tag free_procfunc)
	 (scm_set_smob_print procfunc_tag print_procfunc))


	
	
	;; Code running in hard realtime thread (or can be)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	"typedef int (*Callback)(void *arg,int startframes, int endframe)"
	"typedef void (*Callback2)(struct RT_Engine *,struct RT_Event*)"


	;; The data-type that is sent back to guile thru the ringbuffer_from_rt ringbuffer.

	;; Types:
	"#define RT_DATABACK_EVENT 0"
	"#define RT_DATABACK_UNPROTECT_PROCFUNC 1"
	
	(define-struct <RT_databack>
	  <int> type
	  <void-*> data)
	
	(<void> rt_send_data_back (lambda ((<struct-RT_Engine-*> engine)
					   (<int> type)
					   (<void-*> data))
				    (if (< (jack_ringbuffer_write_space engine->ringbuffer_from_rt) (sizeof <struct-RT_databack>))
					(fprintf stderr
						 (string "Error. from_rt-ringbuffer full. This should be rare, but please report it to me, k.s.matheussen@notam02.no\\n"))
					(let* ((databack <struct-RT_databack>))
					  (set! databack.type type)
					  (set! databack.data data)
					  (jack_ringbuffer_write engine->ringbuffer_from_rt
								 (cast <char-*> &databack)
								 (sizeof <struct-RT_databack>))))))
	
	(<void> rt_unprotect_procfunc (lambda ((<struct-RT_Engine-*> engine)
					       (<struct-RT_Procfunc-*> toremove))
					(rt_send_data_back engine RT_DATABACK_UNPROTECT_PROCFUNC
							   toremove)))
						
	(<int> rt_remove_procfunc_do (lambda ((<struct-RT_Engine-*> engine)
					       (<struct-RT_Procfunc-*> toremove))
					  
				       toremove->visitors--
				       
				       (if (== 0 toremove->visitors)
					   (begin
					     engine->num_procfuncs--
					     
					     (rt_unprotect_procfunc engine toremove)
					     
					     (if (== toremove->prev NULL)
						 (begin
						   (set! engine->procfuncs toremove->next)
						   (if (!= NULL engine->procfuncs)
						       (set! engine->procfuncs->prev NULL)))
						 (begin
						   (set! toremove->prev->next toremove->next)
						   (if (!= toremove->next NULL)
						       (set! toremove->next->prev toremove->prev))))
					     (return 1))
					   (return 0))))
	
	(<void> rt_insert_procfunc_do (lambda ((<struct-RT_Engine-*> engine)
					       (<struct-RT_Procfunc-*> toinsert)
					       (<int> wheretoinsert)) ;; 0=first, 1=last
					
					;;(fprintf stderr (string "inserting: %u, visitors:%d\\n") event toinsert->visitors)
					  
					toinsert->visitors++
					
					(if (== 1 toinsert->visitors)
					    (begin
					      engine->num_procfuncs++

					      (cond ((== NULL engine->procfuncs)
						     (set! toinsert->prev NULL)
						     (set! toinsert->next NULL)
						     (set! engine->procfuncs toinsert))
						    ((== 0 wheretoinsert)
						     (begin
						       (set! toinsert->prev NULL)
						       (set! engine->procfuncs->prev toinsert)
						       (set! toinsert->next engine->procfuncs)
						       (set! engine->procfuncs toinsert)))
						    (else
						     (let* ((procfuncs <struct-RT_Procfunc-*> engine->procfuncs))
						       (while (!= NULL procfuncs->next)
							      (set! procfuncs procfuncs->next))
						       (set! procfuncs->next toinsert)
						       (set! toinsert->prev procfuncs)
						       (set! toinsert->next NULL)))))
						    
					    
					    ;; A procfunc is always protected before being sent here, but since it was already playing, it needs to be unprotected
					    ;; because protection/unprotection are nested functions.
					    (rt_unprotect_procfunc engine toinsert))))


	(functions->public
	 
	 (<void> rt_insert_procfunc (lambda ((<struct-RT_Engine-*> engine)
					     (<struct-RT_Event-*> event))
				      ;;(printf (string "Putting first\\n"))
				      (let* ((toinsert <struct-RT_Procfunc-*> event->arg))
					;;(fprintf stderr (string "inserting: %u, visitors:%d\\n") event toinsert->visitors)
					(rt_insert_procfunc_do engine toinsert 0))))
	 (<void> rt_append_procfunc (lambda ((<struct-RT_Engine-*> engine)
					     (<struct-RT_Event-*> event))
				      ;;(printf (string "Putting last\\n"))
				      (let* ((toinsert <struct-RT_Procfunc-*> event->arg))
					;;(fprintf stderr (string "inserting: %u, visitors:%d\\n") event toinsert->visitors)
					(rt_insert_procfunc_do engine toinsert 1))))
	  
	 (<void> rt_remove_procfunc (lambda ((<struct-RT_Engine-*> engine)
					     (<struct-RT_Event-*> event))
				      (let* ((toremove <struct-RT_Procfunc-*> event->arg))
					(rt_remove_procfunc_do engine toremove))))
	 
	 (<void> rt_remove_all_procfuncs (lambda ((<struct-RT_Engine-*> engine)
						  (<struct-RT_Event-*> event))
					   (let* ((procfunc <struct-RT_Procfunc-*> engine->procfuncs)
						  (next <struct-RT_Procfunc-*>))
					     (while procfunc
						    (set! next procfunc->next)
						    (rt_remove_procfunc_do engine procfunc)
						    (set! procfunc next))))))
	 
	
	;; Priority queue code
	;; rt_insert_event and rt_deletmin_event logic taken from "Data Structures and Agloritm Analysis second edition" by Mark Allen Weiss.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	(<struct-RT_Event> rt_event_dummy)
	

	(<bool> rt_insert_event (lambda ((<struct-RT_Engine-*> engine)
					 (<struct-RT_Event-*> event))
				  
				  (if (>= engine->queue_size (- engine->queue_fullsize 2)) ;; Can 2 be changed to 1 or 0?
				      (begin
					engine->num_events--
					engine->num_lost_events++
					(return false)))

				  engine->queue_size++

				  (if (< event->time 0)
				      (set! event->time 0))

				  (let* ((queue <struct-RT_Event-**> engine->queue)
					 (time <int> event->time)
					 (i <int> engine->queue_size)
					 (newi <int> (>> i 1)))
				    ;;(fprintf stderr (string "insert, i: %d newi: %d\\n") i newi)
				    (while (> queue[newi]->time time)
					   (set! queue[i] queue[newi])
					   (set! i newi)
					   (set! newi (>> newi 1)))
				    ;;(fprintf stderr (string "insert2  %u size: %d, i: %d newi: %d\\n") event->arg engine->queue_size i newi)
				    (set! queue[i] event))
				  (return true)))
	

	(<void> rt_deletemin_event (lambda ((<struct-RT_Engine-*> engine))
				     
				     ;;This check is done elsewhere
				     ;;(if (== engine->queue_size 0)
				     ;;    (return NULL))

				     ;;(<struct-RT_Event-*> event engine->queue[1])
				     engine->num_events--				     
				     engine->queue_size--
				     
				     (let* ((queue <struct-RT_Event-**> engine->queue)					
					    (size <int> engine->queue_size)
					    (last <struct-RT_Event-*> queue[size+1])
					    (last_time <int> last->time)
					    (i <int> 1)
					    (child <int>))
				       ;;(fprintf stderr (string "remove, i: %d child %d, size: %d, last: %u\\n") i child size last)
				       (while (<= (<< i 1) size)
					      (set! child (<< i 1))
					      (if (and (!= child size)
						       (< queue[child+1]->time queue[child]->time))
						  child++)
					      (if (> last_time queue[child]->time)
						  (begin
						    (set! queue[i] queue[child])
						    (set! i child))
						  break))
				       ;;(set! child (<< i 1)))
				       ;;(fprintf stderr (string "remove2, %u size: %d, i: %d child %d\\n") event->arg size i child)
				       ;;(set! queue[size] NULL)
				       (set! queue[i] last))))

	;;(<int> rt_is_queue_not_empty (lambda ((<struct-RT_Engine-*> engine))
	;;			       (return engine->queue_size)))
	
	(<struct-RT_Event-*> rt_findmin_event (lambda ((<struct-RT_Engine-*> engine))
						(return engine->queue_size==0?NULL:engine->queue[1])))
	
						    
			       
	;; Decide whether to insert now or later.
	(<void> rt_queue_event (lambda ((<struct-RT_Engine-*> engine)
					(<struct-RT_Event-*> event))
				 
				 (if (>= event->time engine->next_switchtime)
				     (if (>= event->time engine->next_next_switchtime)
					 (begin
					   (set! event->next engine->events_noninserted2)
					   (set! engine->events_noninserted2 event))
					 (begin
					   (set! event->next engine->events_noninserted1)
					   (set! engine->events_noninserted1 event)))
				     (rt_insert_event engine event))))
				       

	;; Run queued events
	(<void> rt_run_queued_events (lambda ((<struct-RT_Engine-*> engine)
					      (<int> time))
				       (let* ((event <struct-RT_Event-*> (rt_findmin_event engine)))
					 (while (and (!= event NULL)
						     (>= time event->time))
						(let* ((callback <Callback2> event->func))
						  (set! event->next engine->events_non_rt)
						  (set! engine->events_non_rt event)
						  ;;(fprintf stderr (string "while1, is_running: %d\\n") is_running)
						  ;;(fprintf stderr (string "Running callback for event\\n"))
						  (callback engine event)
						  (rt_deletemin_event engine)
						  (set! event (rt_findmin_event engine)))))))

	(<unsigned-int> rt_get_num_cycles (lambda ((<jack_client_t-*> client))
					    (<unsigned-int> ret)
					    (if (== NULL client)
						(return 0))
					    (set! ret (jack_frames_since_cycle_start client))
					    (if (> ret 64000)
						(return 0)
						(return ret))))
						

	(functions->public

	 (<void> rt_callback (lambda ((<struct-RT_Engine-*> engine)
				      (<jack_client_t-*> client)
				      (<int> is_running)
				      (<int> nframes)
				      (<int> base_time)
				      (<float> samplerate))

			       (<int> max_cycle_usage (/ (* nframes engine->max_cpu_usage) 100))
				      
			       (<int> time (+ base_time nframes))

			       (if (> nframes ,rt-max-frame-size)
				   (begin
				     (fprintf stderr (string "Framesize too high.\\n"))
				     return))

			       ;; Check ringbuffer for new events.
			       (if (>= (jack_ringbuffer_read_space engine->ringbuffer_to_rt) (sizeof <void-*>))
				   (while (>= (jack_ringbuffer_read_space engine->ringbuffer_to_rt) (sizeof <void-*>))
					  (let* ((event <struct-RT_Event-*> NULL))
					    (jack_ringbuffer_read engine->ringbuffer_to_rt (cast <char-*> &event) (sizeof <struct-RT_Event-*>))
					    ;;(fprintf stderr (string "got: %u at %f\\n") event event->time)

					    ;; If event->time is less or equal to the current time, we can't just run the event, because there
					    ;; might be events with even less time-value later in the ringbuffer or in the queue.
					    ;; So, queue it:
					    engine->num_events++
					    (rt_queue_event engine event)))

				   ;; one=some_function(numbers_remaining,next_switchtime)/nframes
				   ;; Check for one noninserted events.
				   (if (!= NULL engine->events_noninserted1)
				       (let* ((event <struct-RT_Event-*> engine->events_noninserted1))
					 (set! engine->events_noninserted1 event->next)
					 (rt_insert_event engine event))
				       (if (!= NULL engine->events_noninserted2)
					   (let* ((event <struct-RT_Event-*> engine->events_noninserted2))
					     (set! engine->events_noninserted2 event->next)
					     (rt_insert_event engine event)))))

			       ;; Put events placed in the to-be-queued list into the priority queue and switch the two lists.
			       (if (>= time engine->next_switchtime)
				   (let* ((left <struct-RT_Event-*> engine->events_noninserted1))
				     (set! engine->events_noninserted1 engine->events_noninserted2)
				     (set! engine->events_noninserted2 NULL)
				     (set! engine->next_switchtime engine->next_next_switchtime)
				     (set! engine->next_next_switchtime (+ (* 1024 nframes) engine->next_next_switchtime))
				     ;;(fprintf stderr (string "frames: %d\\n") nframes)
				     ;; queue remaining noninserted1 events.
				     (while (!= NULL left)
					    (let* ((event <struct-RT_Event-*> left))
					      (set! left event->next)
					      (rt_queue_event engine event)))))

			       ;;(fprintf stderr (string "engine: %u\\n") engine)

			       (set! engine->samplerate samplerate)

			       (rt_run_queued_events engine base_time)

			       (set! engine->time_before engine->time) ;; Last time
			       
			       (if (== (setjmp engine->error_jmp) 0)
				   (let* ((time <int> base_time)
					  (next_stop <int>)
					  (event <struct-RT_Event-*>)
					  (end_time <int> (+ base_time nframes)))


				     
				     (while (< time end_time)
					    (set! event (rt_findmin_event engine))
					    (if event
						(set! next_stop (MIN end_time event->time))
						(set! next_stop end_time))

					    ;; Make some noise
					    (if is_running
						(if (> engine->accumulated_cpu_usage max_cycle_usage)
						    (if (== NULL engine->procfuncs)
							(set! engine->accumulated_cpu_usage 0)
							(begin
							  (set! engine->accumulated_cpu_usage
								(- engine->accumulated_cpu_usage (MIN engine->accumulated_cpu_usage
												      max_cycle_usage)))
							  engine->num_max_cpu_interrupts++))
						    (let* ((procfunc <struct-RT_Procfunc-*> engine->procfuncs))
						      (set! engine->time time)
						      (while (!= NULL procfunc)
							     (let* ((callback <Callback> procfunc->func)
								    (next <struct-RT_Procfunc-*> procfunc->next))

							       
							       ;; Run a <realtime> instance function.
							       (if (!= engine->skip_this_procfunc_next_cycle procfunc)
								   (if (== 1 (callback procfunc->arg
										       (- time base_time)
										       (- next_stop base_time)))
								       (rt_remove_procfunc_do engine procfunc)))
							       
							       (if (== NULL next)
								   (set! engine->skip_this_procfunc_next_cycle NULL))
							       
							       ;; Check if too many cpu-cycles are used.
							       (if (> (rt_get_num_cycles client)
								      max_cycle_usage)
								   (begin
								     (set! engine->accumulated_cpu_usage (+ engine->accumulated_cpu_usage
													    (- (jack_frames_since_cycle_start client)
													       max_cycle_usage)))
								     (if (== NULL next)
									 (set! engine->skip_this_procfunc_next_cycle procfunc))
								     engine->num_max_cpu_interrupts++
								     break))

							       (set! procfunc next))))))
					    
					    (set! time next_stop)
					    (rt_run_queued_events engine time))))
			       
			       ;;(set! engine->temp (jack_frames_since_cycle_start client))
			       
			       ;; Put events-to-be-freed into freing-ringbuffer.
			       (while (and (!= engine->events_non_rt NULL)
					   ;;                                                           Be lazy. No rush to fill up the ringbuffer here.
					   (>= (jack_ringbuffer_write_space engine->ringbuffer_from_rt) (/ ,rt-from-ringbuffer-size 2)))
				      (let* ((next <struct-RT_Event-*> engine->events_non_rt->next))
					(rt_send_data_back engine RT_DATABACK_EVENT
							   engine->events_non_rt)
					(set! engine->events_non_rt next))))))
					       

	(public

	 (<void> rt_init_engine (lambda ((<struct-RT_Engine-*> engine))
				  (set! engine->allocplace (malloc ,rt-allocmem-size))
				  (set! engine->allocplace_end (+ engine->allocplace ,rt-allocmem-size))
				  
				  (set! engine->queue (calloc (sizeof <struct-RT_Event-*>) engine->queue_fullsize))
				  (set! rt_event_dummy.time 0.0f)
				  ;;(set! engine->queue_size 1)
				  (for-each 0 engine->queue_fullsize
					    (lambda (i)
					      (set! engine->queue[i] &rt_event_dummy))))))

	
	 
	 ;; Freeing events, protecting and unprotecting procfunc-smobs.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	(public

	 ;; (scm_gc_protect_object/scm_gc_unprotect_object are nested functions.)
	 (<void> rt_protect_var (lambda ((<SCM> var))
				  (scm_gc_protect_object var)
				  "/* */"
				  ))
	 (<void> rt_unprotect_var (lambda ((<SCM> var))
				    (scm_gc_unprotect_object var)
				    "/* */"
				    ))
	 
	 (<void> rt_non_check_non_rt (lambda ((<struct-RT_Engine-*> engine))
				       (while (>= (jack_ringbuffer_read_space engine->ringbuffer_from_rt)
						  (sizeof <struct-RT_databack>))
					      (let* ((databack <struct-RT_databack>))
						(jack_ringbuffer_read engine->ringbuffer_from_rt (cast <char-*> &databack) (sizeof <struct-RT_databack>))
						
						(cond ((== RT_DATABACK_UNPROTECT_PROCFUNC databack.type)
						       (let* ((procfunc <struct-RT_Procfunc-*> databack.data))
							 (rt_unprotect_var procfunc->smob)))
						      
						      ((== RT_DATABACK_EVENT databack.type)
						       (let* ((event <struct-RT_Event-*> databack.data))
							 (free event))))))))

	 ;; Dont use.
	 (<void> rt_reset_engine (lambda ((<struct-RT_Engine-*> engine))
				   
				   (<struct-RT_Event-*> temp)
				   
				   ;; First wait for the ringbuffers to be empty.
				   (while (or (>= (jack_ringbuffer_read_space engine->ringbuffer_to_rt) 0)
					      (>= (jack_ringbuffer_read_space engine->ringbuffer_from_rt) 0))
					  (rt_non_check_non_rt engine)
					  ;;(usleep 50)
					  )
				   
				   (free engine->queue)
				   
				   (set! temp engine->events_noninserted1)
					 
				   )))
	  



	)

(define num-events 0)				      
(def-class (<rt-engine> initdriver)

  (define engine (<RT_Engine> #:ringbuffer_to_rt (jack_ringbuffer_create rt-to-ringbuffer-size)
			      #:ringbuffer_from_rt (jack_ringbuffer_create rt-from-ringbuffer-size)
			      #:max_cpu_usage rt-max-cpu-usage
			      #:queue_fullsize 65536))
									   
  (def-var engine-c (-> engine get-c-object))

  (rt_init_engine this->engine-c)
  
  (define driver (initdriver this->engine-c))
  
  (def-method (add-event frame-time func arg #:key after-run-func)
    (set! num-events (1+ num-events))
    ;; Write to realtime thread. (This function should be written in C with signalling from the rt-thread to avoid usleeping...
    ;; (ec-pointer-to-pointer isn't thread-safe either...))
    (let ((event (<RT_Event> #:time frame-time #:func func #:arg arg)))
      (while (< (jack_ringbuffer_write_space (-> engine ringbuffer_to_rt)) rt-pointer-size)
	     (c-display "sleeping")
	     (usleep 50))
      ;;(c-display "sending" (-> event get-c-object))
      (jack_ringbuffer_write (-> engine ringbuffer_to_rt)
			     (ec-pointer-to-pointer (-> event get-c-object))
			     rt-pointer-size)

      ;; Read from realtime thread
      ;;(c-display "non1")
      (rt_non_check_non_rt this->engine-c)
      ;;(c-display "non2")
      )
    )

  (def-method (start)
    (-> engine next_switchtime 4096)      ;; These numbers doesn't matter much as they'll adjust themself automatically, but next_next needs to be larger than next:
    (-> engine next_next_switchtime 8192)
    (-> driver start))
  

  (add-super! engine)
  
  (if (not driver)
      (set! this #f)
      (add-super! driver))
  
  )


(define *rt-jack-engine* (<rt-engine> (lambda (rt-arg)
				   (<jack-rt-driver> *rt-num-input-ports* *rt-num-output-ports* (rt_callback) rt-arg))))

(define *rt-pd-engine* (if (provided? 'snd-pd-external)
			   (let ((res (<rt-engine> (lambda (c-engine)
						     (pd_rt_init c-engine (rt_callback))
						     (<pd-rt-driver> (rt_callback) c-engine)))))
			     (snd_pd_set_rt_funcs (pd_rt_run) (pd_rt_process))
			     (-> res samplerate (pd_rt_get_samplerate))
			     res)
			   #f))


(define *rt-engine* *rt-jack-engine*)

(add-hook! exit-hook (lambda args
		       (-> *rt-jack-engine* destructor)))
(if (provided? 'snd-pd-external)
    (add-hook! exit-hook (lambda args
			   (-> *rt-pd-engine* destructor))))


(set! *out-bus* (-> *rt-jack-engine* out-bus))
(set! *in-bus* (-> *rt-jack-engine* in-bus))

(-> *rt-engine* start)


#!
(-> *rt-engine* destructor)
!#


#!
(-> rt-jackdriver destructor)

(define RT (<RT_Engine> #:wef 5))
(-> rt-jackdriver num-outputs)
(-> *rt-engine* playback-engines)

(define b (<Jack_Arg>))
(define a (list (<RT_Engine>) (<RT_Engine>)))

(-> b rt_input_args (map (lambda (a) (-> a get-c-object)) a))
(-> b rt_input_args a)
(-> b rt_input_args)

(begin (-> (car a) get-c-object))

(define a (rt_make_procfunc (rt_insert_procfunc) (vector 0 1 2 3)))
(gc)
!#






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define* (rte-restart #:key (num-input-ports *rt-num-input-ports*) (num-output-ports *rt-num-output-ports*))
  (set! *rt-num-input-ports* num-input-ports)
  (set! *rt-num-output-ports* num-output-ports)
  (begin
    (-> *rt-engine* destructor)
    (let ((new-engine (<rt-engine> (lambda (rt-arg)
				     (<jack-rt-driver> num-input-ports num-output-ports (rt_callback) rt-arg)))))
      (set! *out-bus* (-> new-engine out-bus))
      (set! *in-bus* (-> new-engine in-bus))
      (set! *rt-engine* new-engine)
      (set! *rt-jack-engine* new-engine)
      (-> *rt-engine* start))))

(define (rte-silence!)
  (-> *rt-engine* add-event
      (rte-frames)
      (rt_remove_all_procfuncs)
      #f))
  
(define rte-max-cpu-usage
  (make-procedure-with-setter
   (lambda ()
     (-> *rt-engine* max_cpu_usage))
   (lambda (n)
     (-> *rt-engine* max_cpu_usage n))))

(define (rte-pause)
  (-> *rt-engine* pause))
(define (rte-continue)
  (-> *rt-engine* continue))
(define (rte-time)
  (-> *rt-engine* get-time))
(define (rte-frames)
  (-> *rt-engine* frames))
(define (rte-samplerate)
  (-> *rt-engine* get-samplerate))
(define (rte-is-running?)
  (not (= 0 (-> *rt-engine* is_running))))
(define (rte-info)
  (list  (-> *rt-engine* queue_size)
	 (-> *rt-engine* queue_fullsize)
	 (-> *rt-engine* num_lost_events)
	 (-> *rt-engine* num_events)
	 (-> *rt-engine* num_procfuncs)
	 (jack_cpu_load (-> *rt-jack-engine* get-client))
	 (-> *rt-engine* num_max_cpu_interrupts)))

  
  

#!

;;(def-class (<realtime> rt-rt #:key (engine *rt-engine*))
;;  (Super (<realtime-2> (car rt-rt) (cadr rt-rt) engine))
;;  )
   
 
(eval-c ""
	(functions->public
	 ;;(callback procfunc->arg num_outs outs num_ins ins time))
	 (<void> test_generator (lambda ((<SCM> arg)
					 (<int> num_outs) (<float> *outs)
					 (<int> num_ins) (<float> *ins)
					 (<int> time))
				  ;;return)))
				  (let* ((val <float> (/ (cast <float> (random)) RAND_MAX)))
				    (set! outs[0] val)
				    (set! outs[1] val)))))
	
	(public
	 (<float> test_generator2 (lambda ()
				    (return (/ (cast <float> (random)) RAND_MAX))))))






(define osc (make-oscil #:frequency 440))
(define instrument-rt (rt-rt (rt-2 '(lambda ()
				      (out (* 0.4 (oscil osc)))))))

(rt-run 2 2
	(lambda ()
	  (out (* 0.4 (oscil osc)))))

(define instrument (<realtime> instrument-rt))
(-> *rt-engine* start)
(-> instrument play)
(-> instrument stop)
(set! (mus-frequency osc) 200)

(define d (<dialog> "gakk"  #f))
(<slider> d "slider2" 50 100 1200 (lambda (val) 
				    (set! (mus-frequency osc) val))
	  1)
(-> d show)
(-> d hide)
				  
(-> *rt-engine* get-time)
				    
(define instrument (<realtime-2> (test_generator)))
(-> *rt-engine* start)
(-> instrument play-now 2)
(-> instrument play (+ (-> *rt-engine* get-time) 1))
(-> instrument play-now 1 3)
(-> instrument play)
(-> instrument stop)
(-> instrument stop (+ (-> *rt-engine* get-time) 2))

(-> *rt-engine* get-time)


(let* ((amp 0.2)
       (osc (make-oscil :frequency 440))
       (instrument (<realtime> (oscil osc))))
  (-> instrument play-now 10))
~=
(let* ((amp 0.2)
       (osc (make-oscil :frequency 440))
       (instrument (<realtime> (oscil osc))))
  (-> instrument play 0 10)
  (-> instrument start))

(test_generator2)
(> 8 5)

(let ((a 10))
  (while (> a 0)
	 (break)
	 (set! a (1- a)))
  (c-display a))

!#


