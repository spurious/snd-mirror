\ .snd_forth -- start up file for Snd/Forth -*- snd-forth -*-

\ You can install the *.fs scripts with:
\ 
\   cd ${top_srcdir}/examples/site-lib
\   ./install.fth
\ 
\ or even better
\ 
\   make install-site-fth
\
\ If you have installed *.fs scripts with one of the above mentioned
\ commands, you don't need to add a path to *load-path*.
\ ${prefix}/share/fth/site-fth is already included.  Otherwise you can
\ add a path with e.g.:
\ 
\   "/home/mike/snd" add-load-path

\ A special *SND-HOME* path is ~/.snd.d:
\ 
\ /home/mike/.snd.d
\ /home/mike/.snd.d/sound
\ /home/mike/.snd.d/zap       for set-temp-dir, set-save-dir
\ /home/mike/.snd.d/peaks
\
\ Change these paths to fit your needs!

#t to *fth-verbose*
#f to *fth-debug*

'snd-nogui provided? [unless]
  \ Prints at the same time to Snd's listener and stdout/stderr.
  :port-name "sndout"
  :write-line lambda: <{ line -- }> line snd-print .stdout ;
  make-soft-port set-*stdout* value stdout-io
  :port-name "snderr"
  :write-line lambda: <{ line -- }> line snd-print .stderr ;
  make-soft-port set-*stderr* value stderr-io
[then]

\ clm-print is defined in snd/snd-xen.c
[ifundef] clm-print <'> fth-print alias clm-print [then]
: clm-message { fmt args -- str } ." \ " fmt args format .string cr ;

*fth-verbose* [if] $" loading %S" '( *filename* ) clm-message [then]
"HOME" getenv                             value *home*
*home* "/.snd.d" $+                       value *snd-home*
hostname                                  value *hostname*
*hostname* /\\./ string-split 0 array-ref value *short-hostname*

\ Snd (nogui) starts fth_repl after loading init file, but fth_repl
\ sets *argv*.
*argv* length 0> [if]
  *argv* 0 array-ref undef file-basename value *program-name*
[else]
  : *program-name* ( -- name )
    *argv* 0 array-ref "-" string-split 0 array-ref /\\// string-split -1 array-ref
  ;
[then]

#f value __simple-nogui-prompt__
#f value __lisp-graph__
#t set-show-listener drop

before-load-hook reset-hook!
before-load-hook lambda: <{ fname -- f }>
  *fth-verbose* if $" loading %S" #( fname ) clm-message then
  #t
; add-hook!

\ Set them before loading clm.fs.
44100    	  set-default-output-srate       drop
2                 set-default-output-chans       drop
mus-interp-linear set-locsig-type                drop
mus-next          set-default-output-header-type drop
mus-lfloat        set-default-output-data-format drop
1024 1024 *       set-mus-file-buffer-size       drop
512               set-dac-size                   drop
mus-audio-default set-audio-output-device        drop
#t                set-mus-clipping               drop
#t                set-clipping                   drop
50                set-mus-array-print-length     drop
50                set-object-print-length

dl-load sndlib Init_sndlib
\ dl-load sndins Init_sndins
\ dl-load gdbm   Init_gdbm
require clm
require clm-ins
require examp
require hooks
require marks
require extensions
require env
require mix
require dsp
require rgb

"CLM_SEARCH_PATH" getenv dup [if]
  ":" string-split [each] *clm-search-list* swap array-push to *clm-search-list* [end-each]
[else]
  drop
  *clm-search-list* *snd-home* "/sound" $+ array-push to *clm-search-list*
[then]
#t                                       to *clm-play*
#t                                       to *clm-statistics*
#t                                       to *clm-verbose*
#f                                       to *clm-debug*
*snd-home* "/sound/fth-test.snd" $+      to *clm-file-name*
#t                                       to *clm-delete-reverb*
*snd-home* $" /sound/fth-test.reverb" $+ to *clm-reverb-file-name*
*clm-debug* [if]
  lambda: <{ inst start dur -- }>
    $" %14s[%d]: %5.2f %5.2f" '( inst stack-level start dur ) clm-message
  ;
[else]
  lambda: <{ inst start dur -- }> $" %14s: %5.2f %5.2f" '( inst start dur ) clm-message ;
[then] to *clm-notehook*

'snd-motif provided? 'xm provided? not && [if] dl-load libxm Init_libxm [then]
'snd-gtk   provided? 'xg provided? not && [if] dl-load libxg Init_libxg [then]

#f check-for-unsaved-edits
3 remember-sound-state
*snd-home* add-load-path
*snd-home* "/snd-remember-sound.fs" $+ to remember-sound-filename
*snd-home* "/peaks" $+                 to peak-env-dir

"CLM_SEARCH_PATH" getenv dup [if]
  ":" string-split [each] add-directory-to-view-files-list drop [end-each]
[else]
  drop
  *snd-home* "/sound" add-directory-to-view-files-list drop
[then]

[ifundef] enved
  "enved"    #( 0.0 0.0 25.0 1.0 75.0 1.0 100.0 0.0 )           1.0  define-envelope drop
  "brass"    #( 0.0 0.0 20.0 1.0 40.0 0.6 90.0 0.5 100.0 0.0 )  1.0  define-envelope drop
  "bassoon"  #( 0.0 0.0 10.0 1.0 90.0 1.0 100.0 0.0 )           1.0  define-envelope drop
  "clarinet" #( 0.0 0.0 25.0 1.0 75.0 1.0 100.0 0.0 )          32.0  define-envelope drop
  "woodwind" #( 0.0 0.0 10.0 1.0 90.0 1.0 100.0 0.0 )           0.32 define-envelope drop
[then]

\ Snd hooks
exit-hook lambda: <{ -- f }>
  save-state-file save-state drop
  #t
; add-hook! 

after-save-as-hook lambda: <{ snd fname from-dialog -- f }>
  snd revert-sound drop
  snd close-sound drop
  fname open-sound drop
  #f
; add-hook!

before-save-state-hook lambda: <{ fname -- f }>
  sounds each { snd }
    snd channels 0 ?do 'inset-envelope snd i remove-channel-property drop loop
  end-each
  fname :if-exists :rename io-open-write dup $" \\ -*- snd-forth -*-\n" io-write io-close
  #t
; add-hook!

window-property-changed-hook lambda: <{ cmd -- }>
  $" remote command received: %S" #( cmd ) clm-message
; add-hook!

output-comment-hook lambda: <{ str1 -- str2 }>
  selected-sound marks->string
; add-hook!
0 [if]
  after-open-hook lambda: <{ snd -- }>
    snd comment ( str ) <'> string-eval #t nil fth-catch if stack-reset then
  ; add-hook!
[then]

*snd-home* "/snd-saved.fs" $+  set-save-state-file          drop
*snd-home* "/zap" $+           set-temp-dir                 drop
*snd-home* "/zap" $+           set-save-dir                 drop
"/usr/gnu/cvs/snd"             set-html-dir                 drop
"firefox"                      set-html-program             drop
#f                	       set-trap-segfault            drop
#t                	       set-show-indices             drop
0.0               	       set-auto-update-interval     drop
#f  			       set-verbose-cursor           drop
"rev"           	       add-sound-file-extension     drop
"reverb"        	       add-sound-file-extension     drop
"wave"          	       add-sound-file-extension     drop

'snd-nogui provided? [if]
  \ for loading ~/.snd.d/snd-saved.fs
  <'> noop alias set-doit-button-color	\ ( a -- b )
  <'> noop alias set-doit-again-button-color \ ( a -- b )
  <'> noop alias set-help-button-color	     \ ( a -- b )
  <'> noop alias set-quit-button-color	     \ ( a -- b )
  <'> noop alias set-reset-button-color	     \ ( a -- b )
  : set-mix-color <{ val :optional mix-id #f -- col }> val ;
  #f set-verbose-cursor drop
  \ Fth repl and prompt hooks
  before-repl-hook reset-hook!
  before-repl-hook lambda: <{ -- }>
    *fth-verbose* if
      "" #() clm-message
      $" Starting session on %Ev %Er." #( current-time strftime ) clm-message
      "" #() clm-message
    then
  ; add-hook!

  after-repl-hook lambda: <{ history -- }>
    history readlines array-reverse! array-uniq! array-reverse! { ary }
    history ary writelines
    *fth-verbose* if
      "" #() clm-message
      $" Thank you for using %s!" #( *program-name* string-upcase ) clm-message
      "" #() clm-message
    then
  ; add-hook!

  __simple-nogui-prompt__ [if]
    \ A simple prompt for snd-nogui.
    before-prompt-hook lambda: <{ prompt pos --  new-prompt }>
      $" snd (%d)> " #( pos ) string-format
    ;
  [else]
    \ A more elaborated prompt for fth and snd-forth-nogui.
    before-prompt-hook lambda: <{ prompt pos -- new-prompt }>
      "%EI:%EM%p" current-time strftime string-downcase! { tm }
      "(/usr)?" *home* $+ make-regexp file-pwd "~" regexp-replace { path }
      $" (%s:%s)\n[%s %s] (%d)> " #( *short-hostname* path *program-name* tm pos ) string-format
    ;
  [then] add-hook!
[else]					\ not snd-nogui
  require draw
  'snd-motif provided? [if]
    require effects
    require popup
    edhist-save-hook lambda: <{ prc -- }> "%S" #( prc ) clm-message ; add-hook!
    #t show-smpte-label
    2 set-global-sync
    add-mark-pane
    make-current-window-display
  [then]

  \ C-x C-c
  <char> c 4 lambda: <{ -- f }> 0 snd-exit drop #f ; #t $" terminate Snd [ms]" dup bind-key drop
  \ C-x k
  <char> k 0 lambda: <{ -- f }>
    selected-sound close-sound-extend #f
  ; #t $" close sound and jump to next open [ms]" dup bind-key drop
  \ C-x x
  <char> x 0 lambda: <{ -- val }>
    undef selection? if
      $" selection-eval:" <'> eval-over-selection #f #f prompt-in-minibuffer
    else
      $" no selection" #f #f report-in-minibuffer
    then
  ; #t $" eval over selection" $" eval over selection" bind-key drop

  \ graph-hook <'> display-correlate add-hook!
  \ graph-hook <'> zoom-spectrum     add-hook!
  \ graph-hook <'> superimpose-ffts  add-hook!
  graph-hook lambda: <{ snd chn y0 y1 -- }>
    $" freq: %.3f" #( snd chn left-sample  snd chn spot-freq ) string-format
    snd #f report-in-minibuffer drop
    #f
  ; add-hook!

  mouse-click-hook lambda: <{ snd chn button state x y axis -- }>
    axis time-graph = if
      $" freq: %.3f" #( snd chn #f cursor  snd chn spot-freq ) string-format
      snd #f report-in-minibuffer
    else
      #f
    then
  ; add-hook!

  after-open-hook lambda: <{ snd -- }>
    snd channels 0 ?do snd short-file-name snd i time-graph set-x-axis-label drop loop
    #t snd set-with-tracking-cursor drop \ with-tracking-cursor alias for cursor-follows-play
    channels-combined snd set-channel-style drop
  ; add-hook!

  : snd-set-cursor-style ( snd kind -- f )
    { snd kind }
    snd sound? if
      kind snd #t set-cursor-style
    else
      #f
    then
  ;
  start-playing-hook lambda: <{ snd -- f }> snd cursor-line  snd-set-cursor-style ; add-hook!
  stop-playing-hook  lambda: <{ snd -- f }> snd cursor-cross snd-set-cursor-style ; add-hook!

  enved-hook lambda: <{ en pt x y reason -- en'|#f }>
    reason enved-move-point = if
      x en car f> x en -2 array-ref f< && if
	en en pt 2* array-ref x #f #f stretch-envelope { new-en }
	new-en pt 2* 1+ y array-set!
	new-en
      else
	#f
      then
    else
      #f
    then
  ; add-hook!

  \ from ~/.snd_prefs_forth
  with-buffers-menu
  with-reopen-menu
  0.00 0.10 #t prefs-activate-initial-bounds
  focus-follows-mouse
  save-mark-properties
  defined? use-combo-box-for-fft-size [if] \ effects.fs
    #f to use-combo-box-for-fft-size
  [then]

  rainbow-colormap	    	      set-colormap drop
  #f  			    	      set-show-controls drop
  #f  			    	      set-just-sounds drop
  1.0 			    	      set-enved-base drop
  #t  			    	      set-enved-wave? drop
  #t  			    	      set-show-y-zero drop
  #f  			    	      set-verbose-cursor drop
  beige                               set-selected-graph-color drop
  blue                                set-selected-data-color drop
  #t                     	      set-with-gl drop
  \ graph-once           	      set-transform-graph-type drop
  \ graph-as-sonogram    	      set-transform-graph-type drop
  graph-as-spectrogram   	      set-transform-graph-type drop
  #t                                  set-show-transform-peaks drop
  samaraki-window                     set-fft-window drop
  fourier-transform                   set-transform-type drop
  after-transform-hook <'> fft-peak     add-hook!
  __lisp-graph__ [if]
    lisp-graph-hook <'> display-db      add-hook!
    lisp-graph-hook <'> display-energy  add-hook!
  [then]
  mix-click-hook  <'> mix-click-info    add-hook!
  mark-click-hook <'> mark-click-info   add-hook!
  defined? show-disk-space [if]
    after-open-hook <'> show-disk-space add-hook!
  [then]
  lightsteelblue2 set-help-button-color drop
  lightsalmon2    set-reset-button-color drop
  light-salmon    set-quit-button-color drop
  wheat           set-doit-button-color drop
  burlywood       set-doit-again-button-color drop
  lightsteelblue1 set-pushed-button-color drop
  $" snd> "       set-listener-prompt drop
  #f  		  set-show-controls drop
  160 		  set-window-x drop
  0 		  set-window-y drop
  800 		  set-window-width drop
  600 		  set-window-height drop
  speed-control-as-ratio set-speed-control-style drop
[then]					\ not snd-nogui

*snd-home* "/sound" $+ set-open-file-dialog-directory drop
save-state-file file-exists? [if] require snd-saved [then]
\ find-file searchs in *clm-search-list*
sounds empty? [if] *clm-file-name* find-file dup [if] open-sound [then] drop cr [then]
$" Snd of %s (Fth %s)" #( snd-version fth-version ) clm-message
#t show-listener drop
'snd-nogui provided? [if] stack-reset [else] #f set-show-controls drop [then]

\ .snd_forth ends here
