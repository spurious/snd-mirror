
(if (not provided? 'snd-eval-c.scm)
    (load-from-path "eval-c.scm"))


(eval-c-add-int-type "int32_t")

(eval-c "-llo"
        "#include <lo/lo.h>"
        "#include <jack/ringbuffer.h>"
        
        (proto->public 
         "lo_blob lo_blob_new(int32_t size, void* data);"
         "lo_address lo_address_new(const char *host, const char *port);"
         "int lo_send_message (lo_address targ, const char *path, lo_message msg);"
         "lo_message lo_message_new (void);"
         "void lo_message_free (lo_message m);"
         "void lo_message_add_int32 (lo_message m, int32_t a);"
         "void lo_message_add_float (lo_message m, float a);"
         "void lo_message_add_string (lo_message m, const char *a);"
         "void lo_address_free (lo_address t);")

        (<jack_ringbuffer_t*> rb NULL)

        (run-now
         (set! rb (jack_ringbuffer_create 1024*64)))

        (<int> generic_handler (lambda ((<const-char*> path)
                                        (<const-char*> types)
                                        (<lo_arg-**> argv)
                                        (<int> argc)
                                        (<void*> data)
                                        (<void*> user_data))
                                 (<int> len (strlen path))
                                 (if (< (jack_ringbuffer_write_space rb) (+ len
                                                                            (sizeof <int>)
                                                                            (sizeof <int>)))
                                     (begin
                                       (fprintf stderr (string "Error. OSC Ringbuffer full.\\n"))
                                       (return 0)))
                                 (jack_ringbuffer_write rb (cast <void*> &len) (sizeof <int>))
                                 (jack_ringbuffer_write rb (cast <void*> path) len)
                                 (jack_ringbuffer_write rb (cast <void*> &argc) (sizeof <int>))
                                 (for-each 0 argc
                                           (lambda (n)
                                             (if (< (jack_ringbuffer_write_space rb) (+ 1
                                                                                        (sizeof <float>)
                                                                                        (sizeof <int>)))
                                                 (begin
                                                   (fprintf stderr (string "Error. OSC Ringbuffer full.\\n"))
                                                   (return 0)))
                                             (jack_ringbuffer_write rb (cast <void*> "&types[n]") 1)
                                             (cond ((== ,(string->symbol "'s'") types[n])
                                                    (<char*> s "&(argv[n]->s)")
                                                    (<int> len (strlen s))
                                                    (jack_ringbuffer_write rb (cast <void*> &len) (sizeof <int>))
                                                    (if (< (jack_ringbuffer_write_space rb) len)
                                                        (begin
                                                          (<char*> error "error")
                                                          (fprintf stderr (string "Error. OSC Ringbuffer full.\\n"))
                                                          (jack_ringbuffer_write rb (cast <void*> error) 6)
                                                          (return 0)))
                                                    (jack_ringbuffer_write rb (cast <void*> argv[n]) len))
                                                   ((== ,(string->symbol "'f'") types[n])
                                                    (jack_ringbuffer_write rb (cast <void*> "&(argv[n]->f)") (sizeof <float>)))
                                                   ((== ,(string->symbol "'i'") types[n])
                                                    (jack_ringbuffer_write rb (cast <void*> "&(argv[n]->i)") (sizeof <int>)))
                                                   (else
                                                    (fprintf stderr (string "Error. Unknown type"))
                                                    (return 0)))))
                                 
                                 ;(<int> i)
                                 ;"printf(\"path: <%s>\\n\", path);"
                                 ;"for (i=0; i<argc; i++) {"
                                 ;"  printf(\"arg %d '%c' \", i, types[i]);"
                                 ;"  lo_arg_pp(types[i], argv[i]);"
                                 ;"  printf(\"\\n\");"
                                 ;"}"
                                 ;(printf (string "\\n"))
                                 (return 0)))


        (<void> error_handler (lambda ((<int> num)
                                       (<const-char*> msg)
                                       (<const-char*> path))
                                (printf (string "liblo server error %d in path %s: %s\\n")
                                        num path msg)))
        
        (<lo_server_thread> st)

        (public
         (<void> osc_internal_start_server (lambda ((<char*> port))
                                             (set! st (lo_server_thread_new port error_handler))
                                             (lo_server_thread_add_method st NULL NULL generic_handler NULL)
                                             (lo_server_thread_start st)))
         
         (<void> osc_internal_stop_server (lambda ()
                                            (lo_server_thread_free st)))

         (<SCM> osc_rb_get_int (lambda ()
                                 (<int> ret)
                                 (if (< (jack_ringbuffer_read_space rb) (sizeof <int>))
                                     (return SCM_BOOL_F))
                                 (jack_ringbuffer_read rb (cast <void*> &ret) (sizeof <int>))
                                 (return (MAKE_INTEGER ret))))
         (<SCM> osc_rb_get_string (lambda ((<int> len))
                                    (<char> dasret[1024])
                                    (while (< (jack_ringbuffer_read_space rb) len)
                                      (usleep 50))
                                    (jack_ringbuffer_read rb (cast <void*> dasret) len)
                                    (set! dasret[len] 0)
                                    (RETURN_STRING dasret)))
         
         (<SCM> osc_rb_get (lambda ()
                             (<char> type)
                             (if (< (jack_ringbuffer_read_space rb) (+ 1 (sizeof <int>)))
                                 (return SCM_BOOL_F))
                             (jack_ringbuffer_read rb &type 1)
                             (cond ((== ,(string->symbol "'s'") type)
                                    (<int> len)
                                    (jack_ringbuffer_read rb (cast <void*> &len) (sizeof <int>))
                                    (return (osc_rb_get_string len)))
                                   ((== ,(string->symbol "'f'") type)
                                    (<float> ret)
                                    (jack_ringbuffer_read rb (cast <void*> &ret) (sizeof <float>))
                                    (return (MAKE_FLOAT ret)))
                                   ((== ,(string->symbol "'i'") type)
                                    (return (osc_rb_get_int)))
                                   (else
                                    (fprintf stderr (string "Error. Unknown type"))
                                    (return SCM_BOOL_F))))))
        )

(define (osc_send host port path . args)
  (define address (lo_address_new host (if (string? port)
                                           port
                                           (number->string port))))
  (define message (lo_message_new))
  (for-each (lambda (arg)
              (cond ((integer? arg)
                     (lo_message_add_int32 message arg))
                    ((number? arg)
                     (lo_message_add_float message arg))
                    ((string? arg)
                     (lo_message_add_string message arg))
                    (else (error "unkown arg" arg))))
            args)
  (let ((res (lo_send_message address path message)))
    (lo_message_free message)
    (lo_address_free address)
    res))

(define (osc_start_server port)
  (if (number? port)
      (set! port (number->string port)))
  (osc_internal_start_server port))

(define (osc_get)
  (let ((strlen (osc_rb_get_int)))
    (if (not strlen)
        #f
        (let ((path (osc_rb_get_string strlen)))
          (define len (osc_rb_get_int))
          (cons path
                (map (lambda (i)
                       (let loop ((ret (osc_rb_get)))
                         (if (not ret)
                             (begin
                               (usleep 50)
                               (loop (osc_rb_get)))
                             ret)))
                     (iota len)))))))

(define (osc_add_callback func)
  (in 100
      (lambda ()
        (catch #t
               (lambda ()
                 (let ((res (osc_get)))
                   (if res
                       (apply func res))))
               (lambda x
                 (c-display "Error: " x)))
        (osc_add_callback func))))


#!
(load-from-path "osc.scm")

(define port 6012)
(osc_start_server port)


(osc_send #f port "/foo/bar" 5.2 9.4 "hello" 5)
(osc_get)

;; osc_add_callback can be used instead of osc_get:

(osc_add_callback (lambda (path . args)
                    (c-display "path: " path ", args:" args)))


!#

