<
#!

rt-engine.scm
-Kjetil S. Matheussen/Notam, 2005

rt-engine.scm is developed with support from Notam/Oslo:
http://www.notam02.no


rt-engine creates a realtime engine that should be suitable for hard real
time signal processing.

This file is normally loaded from "rt-compiler.scm".



rt-engine drodle:
-----------------
(-> rt-engine frames)
(-> rt-engine start)
(-> rt-engine get-time)
(-> rt-engine destructor)

(-> rt-engine pause)
(-> rt-engine continue)

(list 
 (-> rt-engine queue_size)
 (-> rt-engine queue_fullsize)
 (-> rt-engine num_procfuncs)
 (-> rt-engine get-time))

(-> rt-engine events_noninserted2)

(list (-> rt-engine frames)
      (-> rt-engine get-time)
      (-> rt-engine is_running)))
      
(-> rt-engine dir)

(let* ((a 5)
       (b (rt (lambda ()
		(set! a 9)
		a))))
  (list a (rt-funcall b)))

(<rt-engine> dir add-event get-method class-name add-method instance?
	     (<jack-rt-driver> dir get-method destructor class-name add-method instance? pause start num-inputs stop num-outputs
			       continue get-time set-input-args set-output-args
			       (<jack> dir get-method class-name add-method
				       instance? close get-client get-arg get-samplerate
				       (<Jack_Arg> output_ports dir get-c-object get-method get-size time
						   is_running destructor rt_callback rt_arg class-name
						   add-method instance? num_inports num_outports input_ports)))
	     (<RT_Engine> next_next_switchtime events_noninserted1 events_noninserted2 dir
			  events_non_rt get-c-object get-method get-size procfuncs destructor
			  class-name add-method instance? ringbuffer_to_rt ringbuffer_from_rt
			  num_events_rt events_rt next_switchtime))

(-> rt-engine num_events_rt)
(list (-> rt-engine get-time)
      (-> rt-engine next_switchtime)
      (-> rt-engine next_next_switchtime))



!#


(if (provided? 'snd-rt-engine.scm)
    (throw 'rt-engine-already-loaded))
(provide 'snd-rt-engine.scm)

(if (not (provided? 'snd-oo.scm)) (load-from-path "oo.scm"))

(c-load-from-path rt-compiler)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rt-pointer-size (ec-sizeof-pointer))

(define rt-ringbuffer-size (* rt-pointer-size 8192))






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
void jack_set_server_dir (const char *path);
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




(def-class (<jack> name process-func jack-arg num-inports num-outports #:key (autoconnect #t))

  (Super jack-arg)
  
  (define client #f)

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
				 (got-it)))
			   '("" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))))
	      
	      (if (not client)
		  (begin
		    (c-display "Could not create jack client with name \"" name "\".")
		    (return #f)))
	      
	      (if num-inports
		  (-> jack-arg num_inports num-inports))
	      (if num-outports
		  (-> jack-arg num_outports num-outports))

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



(define-ec-struct <Jack_Arg>
  <int> frames
  <int> is_running

  <void-*> rt_callback

  <int> num_inports
  <jack_port_t**> input_ports
  
  <int> num_outports
  <jack_port_t**> output_ports

  <void-*> rt_arg

  <float> samplerate
  
  )


(eval-c ""
	"#include <jack/jack.h>"
	(shared-struct <Jack_Arg>)

	"typedef void (*Callback)(void *arg,int is_running,int num_outs, float *outs, int num_ins, float *ins,int time,float samplerate)"

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
					 
					 (out_arg[jack_arg->num_outports] <float>)
					 (in_arg[jack_arg->num_inports] <float>))
				    
				    (for-each 0 jack_arg->num_outports
					      (lambda (ch)
						(set! out[ch] (jack_port_get_buffer jack_arg->output_ports[ch] nframes))))
				    (for-each 0 jack_arg->num_inports
					      (lambda (ch)
						(set! in[ch] (jack_port_get_buffer jack_arg->input_ports[ch] nframes))))
				    (for-each 0 nframes
					      (lambda (frame)
						(for-each 0 jack_arg->num_outports
							  (lambda (ch)
							    (set! out_arg[ch] 0)))
						(for-each 0 jack_arg->num_inports
							  (lambda (ch)
							    (set! in_arg[ch] in[ch][frame])))
						(callback jack_arg->rt_arg
							  jack_arg->is_running
							  jack_arg->num_outports out_arg
							  jack_arg->num_inports in_arg
							  jack_arg->frames
							  jack_arg->samplerate)
						(for-each 0 jack_arg->num_outports
							  (lambda (ch)
							    (set! out[ch][frame] out_arg[ch])))
						(if jack_arg->is_running
						    (+= jack_arg->frames 1)))))
				  (return 0)))))



(def-class (<jack-rt-driver> num-inputs num-outputs rt_callback rt_arg #:key (autoconnect #t))

  (Super (<jack> "snd-rt"
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
;;;;;;;;;;;;;;;;;;;;;;; Engine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-ec-struct <RT_Event>
  <int> time
  <struct-RT_Event-*> next
  <void-*> func
  <void-*> arg
  <int> isremovedorinserted
  )

;; (The double linked-list operations on procfunc can be optimized.)
(define-ec-struct <RT_Procfunc>
  <struct-RT_Procfunc-*> next
  <struct-RT_Procfunc-*> prev
  <int> visitors
  <void-*> func
  <SCM> arg
  <SCM> smob
  )

(define-ec-struct <RT_Engine>
  <jack_ringbuffer_t-*> ringbuffer_to_rt
  <jack_ringbuffer_t-*> ringbuffer_from_rt

  <int> num_lost_events                  ;; Number of events lost because queue was full.
  <int> num_events                     ;; Number of events waiting to be run
  <int> queue_fullsize
  <int> queue_size
  <struct-RT_Event-**> queue             ;; A priority queue with room for queue_fullsize number of eventsd
  ;;<struct-RT_Event-*> events_rt           ;; Sorted events to be scheduled.
  <int> next_switchtime                 ;; The next time events_noninserted2 became events_noninserted1
  <int> next_next_switchtime            ;; The next next time events_noninserted2 became events_noninserted1
  <struct-RT_Event-*> events_noninserted1 ;; Events that is scheduled 0.0 or more seconds into the future. (unsorted)
  <struct-RT_Event-*> events_noninserted2 ;; Events that is scheduled 0.1 or more seconds into the future. (unsorted)
  <struct-RT_Event-*> events_non_rt

  <int> num_procfuncs
  <struct-RT_Procfunc-*> procfuncs  
  )



(eval-c ""
	"#include <stdbool.h>"
	"#include <jack/ringbuffer.h>"

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
	 
	 (<SCM> rt_make_procfunc (lambda ((<void-*> func)(<SCM> arg))
				   (let* ((procfunc <struct-RT_Procfunc-*> (calloc 1 (sizeof <struct-RT_Procfunc>)))
					  (smob <SCM>))
				     ;;(scm_gc_protect_object arg)
				     (set! procfunc->func func)
				     (set! procfunc->arg arg)
				     (SCM_NEWSMOB smob procfunc_tag procfunc)
				     (set! procfunc->smob smob)
				     (return smob)))))

	(<SCM> mark_procfunc (lambda ((<SCM> procfunc_smob))
			       (let* ((procfunc <struct-RT_Procfunc-*> (cast <void-*> (SCM_SMOB_DATA procfunc_smob))))
				 (return procfunc->arg))))
	(<size_t> free_procfunc (lambda ((<SCM> procfunc_smob))
				  (let* ((procfunc <struct-RT_Procfunc-*> (cast <void-*> (SCM_SMOB_DATA procfunc_smob))))
				    ;;(fprintf stderr (string "Freeing procfunc smob: %u\\n") procfunc)
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
	
	"typedef void (*Callback)(SCM arg,int num_outs, float *outs, int num_ins, float *ins,int time,float samplerate)"
	"typedef void (*Callback2)(struct RT_Engine *,struct RT_Event*)"

	

	(functions->public

	 
	 (<void> rt_insert_procfunc (lambda ((<struct-RT_Engine-*> engine)
					     (<struct-RT_Event-*> event))
				      (let* ((toinsert <struct-RT_Procfunc-*> event->arg))

					;;(fprintf stderr (string "inserting: %u, visitors:%d\\n") event toinsert->visitors)
					
					toinsert->visitors++
					
					(if (== 1 toinsert->visitors)
					    (begin
					      engine->num_procfuncs++
					      (set! event->isremovedorinserted 1)
					      (set! toinsert->prev NULL)
					      (if (!= NULL engine->procfuncs)
						  (set! engine->procfuncs->prev toinsert))
					      (set! toinsert->next engine->procfuncs)
					      (set! engine->procfuncs toinsert))))))


	 
	 (<void> rt_remove_procfunc (lambda ((<struct-RT_Engine-*> engine)
					     (<struct-RT_Event-*> event))
				      (let* ((toremove <struct-RT_Procfunc-*> event->arg))
					
					(if (> toremove->visitors 0)
					    (begin

					      ;;(fprintf stderr (string "removing: %u, visitors: %d\\n") event toremove->visitors)
										      
					      toremove->visitors--
					      					      
					      (if (== 0 toremove->visitors)
						  (begin
						    engine->num_procfuncs--
						    (set! event->isremovedorinserted 1)
						    (if (== toremove->prev NULL)
							(begin
							  (set! engine->procfuncs toremove->next)
							  (if (!= NULL engine->procfuncs)
							      (set! engine->procfuncs->prev NULL)))
							(begin
							  (set! toremove->prev->next toremove->next)
							  (if (!= toremove->next NULL)
							      (set! toremove->next->prev toremove->prev))))))))))))


	
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

	(<int> rt_is_queue_not_empty (lambda ((<struct-RT_Engine-*> engine))
				       (return engine->queue_size)))
	
	(<struct-RT_Event-*> rt_findmin_event (lambda ((<struct-RT_Engine-*> engine))
						(return engine->queue_size==0?NULL:engine->queue[1])))
	
						    
			       
	;; Decide whether to insert now or later.
	(<void> rt_queue_event (lambda ((<struct-RT_Engine-*> engine)
					(<struct-RT_Event-*> event))
				 
				 engine->num_events++

				 (if (>= event->time engine->next_switchtime)
				     (if (>= event->time engine->next_next_switchtime)
					 (begin
					   (set! event->next engine->events_noninserted2)
					   (set! engine->events_noninserted2 event))
					 (begin
					   (set! event->next engine->events_noninserted1)
					   (set! engine->events_noninserted1 event)))
				     (rt_insert_event engine event))))
				       
	(functions->public
	 
	 (<void> rt_callback (lambda ((<struct-RT_Engine-*> engine)
				      (<int> is_running)
				      (<int> num_outs) (<float> *outs)
				      (<int> num_ins) (<float> *ins)
				      (<int> time)
				      (<float> samplerate))
			       
			       ;; Check ringbuffer for new events. Put those into engine->events.
			       (if (>= (jack_ringbuffer_read_space engine->ringbuffer_to_rt) (sizeof <void-*>))
				   (while (>= (jack_ringbuffer_read_space engine->ringbuffer_to_rt) (sizeof <void-*>))
					  (let* ((event <struct-RT_Event-*> NULL))
					    (jack_ringbuffer_read engine->ringbuffer_to_rt (cast <char-*> &event) (sizeof <struct-RT_Event-*>))
					    ;;(fprintf stderr (string "got: %u at %f\\n") event event->time)

					    ;; If event->time is less or equal to the current time, we can't just run the event, because there
					    ;; might be events with even less time-value later in the ringbuffer or in the queue.
					    ;; So, queue it:
					    (rt_queue_event engine event)))
				   
				   ;; Check for one noninserted events.
				   (if (!= NULL engine->events_noninserted1)
				       (let* ((event <struct-RT_Event-*> engine->events_noninserted1))
					 (set! engine->events_noninserted1 event->next)
					 (rt_insert_event engine event))
				       (if (!= NULL engine->events_noninserted2)
					   (let* ((event <struct-RT_Event-*> engine->events_noninserted2))
					     (set! engine->events_noninserted2 event->next)
					     (rt_insert_event engine event)))))
			       
			       (if (>= time engine->next_switchtime)
				   (begin
				     ;; Insert remaining noninserted1 events. (Should be very seldom)
				     (while (!= NULL engine->events_noninserted1)
					    (let* ((event <struct-RT_Event-*> engine->events_noninserted1))
					      (set! engine->events_noninserted1 event->next)
					      (rt_insert_event engine event)))
				     (set! engine->events_noninserted1 engine->events_noninserted2)
				     (set! engine->events_noninserted2 NULL)
				     (set! engine->next_switchtime engine->next_next_switchtime)
				     (set! engine->next_next_switchtime (+ 4096 engine->next_next_switchtime))))
			       
			       ;; Run queued events
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
					  (set! event (rt_findmin_event engine)))))
			       
			       ;; Put events-to-be-freed into freing-ringbuffer.
			       (while (and (!= engine->events_non_rt NULL)
					    (>= (jack_ringbuffer_write_space engine->ringbuffer_from_rt) (sizeof <void-*>)))
				       (jack_ringbuffer_write engine->ringbuffer_from_rt
							      (cast <char-*> &engine->events_non_rt)
							      (sizeof <void-*>))
				       (set! engine->events_non_rt engine->events_non_rt->next))
					       
				;; Make some noise
				(if is_running
				    (let* ((procfunc <struct-RT_Procfunc-*> engine->procfuncs))
				      (while (!= NULL procfunc)
					     (let* ((callback <Callback> procfunc->func))
					       (callback procfunc->arg
							 num_outs outs
							 num_ins ins
							 time
							 samplerate))
					     (set! procfunc procfunc->next)))))))


	(public
	 (<void> rt_init_engine (lambda ((<struct-RT_Engine-*> engine))
				  (set! engine->queue (calloc (sizeof <struct-RT_Event-*>) engine->queue_fullsize))
				  (set! rt_event_dummy.time 0.0f)
				  ;;(set! engine->queue_size 1)
				  (for-each 0 engine->queue_fullsize
					    (lambda (i)
					      (set! engine->queue[i] &rt_event_dummy))))))
	
	 
	 ;; Freeing events, protecting and unprotecting procfunc-smobs. Gakk, this is complicated...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	(public

	 ;; (scm_gc_protect_object/scm_gc_unprotect_object are nested functions.)
	 (<void> rt_protect_var (lambda ((<SCM> var))
				  (scm_gc_protect_object var)))
	 (<void> rt_unprotect_var (lambda ((<SCM> var))
				    (scm_gc_unprotect_object var)))
	 
	 (<void> rt_non_check_non_rt (lambda ((<struct-RT_Engine-*> engine))
				       (while (>= (jack_ringbuffer_read_space engine->ringbuffer_from_rt)
						  (sizeof <void-*>))
					      (let* ((event <struct-RT_Event-*> NULL))
						 (jack_ringbuffer_read engine->ringbuffer_from_rt (cast <char-*> &event) (sizeof <struct-RT_Event-*>))
						 ;;(fprintf stderr (string "freeing event: %u \\n") event)
						 
						 ;; Two situations will unprotect a procfunc-smob here:
						 ;;
						 ;; 1. When adding a procfunc in scheme (-> <realtime> play), the smob is always protected.
						 ;;    However, if the procfunc was already playing in the rt-thread (for example by calling the play method two
						 ;;    times on a row), the protection needs to be undone with the help of an unprotection.
						 ;;    (The smob can't be protected inside the realtime thread because rt_protect_var is not realtime safe, and
						 ;;     that the smob can be gc-ed before its handled in the realtime thread.)
						 ;;
						 ;; 2. If a procfunc is stopped in the rt-thread, we unprotect it.
						 ;; 
						 (if (or (and (== 1 event->isremovedorinserted)
							      (== event->func rt_remove_procfunc))
							 (and (== 0 event->isremovedorinserted)
							      (== event->func rt_insert_procfunc)))
						     (let* ((procfunc <struct-RT_Procfunc-*> event->arg))
						       (rt_unprotect_var procfunc->smob)))
						 
						 (free event)
						 ))))))


(define num-events 0)				      
(def-class (<rt-engine> initdriver)

  (define engine (<RT_Engine> #:ringbuffer_to_rt (jack_ringbuffer_create rt-ringbuffer-size)
			      #:ringbuffer_from_rt (jack_ringbuffer_create rt-ringbuffer-size)
			      #:queue_fullsize 65536))
									   
  (define engine-c (-> engine get-c-object))

  (rt_init_engine engine-c)
  
  (define driver (initdriver engine-c))
  
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
      (rt_non_check_non_rt engine-c)
      ;;(c-display "non2")
      )
    )

  (def-method (start)
    (-> engine next_switchtime 4096)
    (-> engine next_next_switchtime 8192)
    (-> driver start))
  

  (add-super! engine)
  
  (if (not driver)
      (set! this #f)
      (add-super! driver))
  
  )


(define rt-engine (<rt-engine> (lambda (rt-arg)
				 (<jack-rt-driver> 5 6 (rt_callback) rt-arg))))

(add-hook! exit-hook (lambda args
		       (-> rt-engine destructor)))


(-> rt-engine start)


#!
(-> rt-engine destructor)
!#


#!
(-> rt-jackdriver destructor)

(define RT (<RT_Engine> #:wef 5))
(-> rt-jackdriver num-outputs)
(-> rt-engine playback-engines)

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


(define (rte-reset)
  (begin
    (-> rt-engine destructor)
    (set! rt-engine (<rt-engine> (lambda (rt-arg)
				   (<jack-rt-driver> 5 6 (rt_callback) rt-arg))))
    (-> rt-engine start)))

(define (rte-pause)
  (-> rt-engine pause))
(define (rte-continue)
  (-> rt-engine pause))
(define (rte-time)
  (-> rt-engine get-time))
(define (rte-frames)
  (-> rt-engine frames))
(define (rte-samplerate)
  (-> rt-engine get-samplerate))
(define (rte-is-running?)
  (not (= 0 (-> rt-engine is_running))))
(define (rte-info)
  (list  (-> rt-engine queue_size)
	 (-> rt-engine queue_fullsize)
	 (-> rt-engine num_lost_events)
	 (-> rt-engine num_events)
	 (-> rt-engine num_procfuncs)))

  
(def-class (<realtime> func arg #:key (engine rt-engine))

  ;;(define procfunc (<RT_Procfunc> #:func func #:arg arg))
  (define procfunc (rt_make_procfunc func arg))
  (define procfunc-data (rt_get_procfunc_data procfunc))

  (def-method (stop #:optional (end (-> engine get-time)))
    ;;(c-display "stop, end: " end)
    (-> engine add-event
	(-> engine get-frame-time end)
	(rt_remove_procfunc)
	procfunc-data))
  
  (def-method (play #:optional (start (-> engine get-time)) end)
    ;;(c-display "play, now/start/end" (-> engine get-time) start end)
    (rt_protect_var procfunc) ;; Unprotection happens in the rt_non_check_non_rt function. (Perhaps I should make a drawing that shows how the protection/unprotection
    (-> engine add-event      ;; madness happens...)
	(-> engine get-frame-time start)
	(rt_insert_procfunc)
	procfunc-data)
    (if end
	(this->stop end)))

  (def-method (stop-now #:optional (end 0))
    (this->stop (+ (-> engine get-time) end)))
		 
  (def-method (play-now #:optional (start 0) end)
    (if (<= end start)
	(c-display "<realtime>->play-now, end<=start: (play-now " start end ")")
	(let ((start-time (-> engine get-time)))
	  (this->play (+ start-time start)
		      (if end
			  (+ start-time end)
			  end))))))

	       
;  (def-method (play-now #:optional length)
;    (if length
;	(this->play (-> engine get-time) (+ (-> engine get-time) length))
;	(this->play (-> engine get-time))))

;  (def-method (start #:optional (time (-> engine get-time)))
;    (rt-start))

  

#!

;;(def-class (<realtime> rt-rt #:key (engine rt-engine))
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
(-> rt-engine start)
(-> instrument play)
(-> instrument stop)
(set! (mus-frequency osc) 200)

(define d (<dialog> "gakk"  #f))
(<slider> d "slider2" 50 100 1200 (lambda (val) 
				    (set! (mus-frequency osc) val))
	  1)
(-> d show)
(-> d hide)
				  
(-> rt-engine get-time)
				    
(define instrument (<realtime-2> (test_generator)))
(-> rt-engine start)
(-> instrument play-now 2)
(-> instrument play (+ (-> rt-engine get-time) 1))
(-> instrument play-now 1 3)
(-> instrument play)
(-> instrument stop)
(-> instrument stop (+ (-> rt-engine get-time) 2))

(-> rt-engine get-time)


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


