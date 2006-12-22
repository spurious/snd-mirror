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
\ You must change these paths to your needs!

#t to *fth-verbose*
#f to *fth-debug*

"HOME" getenv                             value *home*
*home* "/.snd.d" $+                       value *snd-home*
hostname                                  value *hostname*
*hostname* /\\./ string-split 0 array-ref value *short-hostname*
"snd"                                     value *program-name*

#f value __simple-nogui-prompt__
#f value __lisp-graph__
#t set-show-listener drop

\ defined in snd/snd-xen.c
[undefined] clm-print [if] ' fth-print alias clm-print [then]

'snd-nogui provided? [if]
  \ Prints to stdout through snd-print.
  : clm-message ( fmt args -- ) "\\ " rot ( fmt ) $+ "\n" $+ swap ( args ) clm-print ;
[else]
  \ Prints to Snd's listener (snd-print) and to stdout if $EMACS is
  \ set or $TERM matches /^xterm/.
  : clm-message ( fmt args -- )
    $" \n\\ " rot ( fmt ) $+ swap ( args ) string-format { msg }
    msg snd-print drop
    "EMACS" getenv
    /^xterm/ "TERM" getenv 1 >string re= || if msg .stdout then
  ;
[then]

*fth-verbose* [if] $" loading %S" '( *filename* ) clm-message [then]

before-load-hook reset-hook!
before-load-hook lambda: <{ fname -- f }>
  *fth-verbose* if $" loading %S" '( fname ) clm-message then
  #t
; add-hook!

\ Set them before loading clm.fs.
22050    	  set-default-output-srate       drop
2                 set-default-output-chans       drop
mus-interp-linear set-locsig-type                drop
mus-next          set-default-output-header-type drop
mus-lfloat        set-default-output-data-format drop
1024 1024 *       set-mus-file-buffer-size       drop
512               set-dac-size                   drop
mus-audio-default set-audio-output-device        drop
#t                set-mus-clipping set-clipping  drop
1024              set-clm-table-size             drop
20                set-mus-array-print-length     drop

dl-load sndlib Init_sndlib
dl-load sndins Init_sndins
dl-load gdbm   Init_gdbm
require clm
require clm-ins
require examp
require hooks
require marks
require extensions
require env
require peak-env
require mix
require dsp
require rgb

*clm-search-list*
"SFDIR" getenv dup [if] array-push [else] drop [then]
*snd-home* "/sound"       $+ array-push
*home* "/Project/Sndtest" $+ array-push to *clm-search-list*
#t                                      to *clm-play*
#t                                      to *clm-statistics*
#t                                      to *clm-verbose*
*snd-home* "/sound/fth-test.snd" $+     to *clm-file-name*
lambda: <{ inst start dur -- }>
  $" %14s: %5.2f %5.2f" '( inst start dur ) clm-message
; to *clm-notehook*
#t                                      to *clm-delete-reverb*
*snd-home* "/sound/fth-test.reverb" $+  to *clm-reverb-file-name*

'snd-motif provided? 'xm provided? not && [if] dl-load libxm Init_libxm [then]
'snd-gtk   provided? 'xg provided? not && [if] dl-load libxg Init_libxg [then]

#f check-for-unsaved-edits
3 remember-sound-state
*snd-home* add-load-path
*snd-home* "/snd-remember-sound.fs" $+ to remember-sound-filename
*snd-home* "/peaks" $+                 to save-peak-env-info-directory

"SFDIR" getenv dup [if] add-directory-to-view-files-list [then] drop
*snd-home* "/sound" $+  add-directory-to-view-files-list drop

[ifundef] enved
  "enved"    '( 0.0 0.0 25.0 1.0 75.0 1.0 100.0 0.0 )           1.0  define-envelope drop
  "brass"    '( 0.0 0.0 20.0 1.0 40.0 0.6 90.0 0.5 100.0 0.0 )  1.0  define-envelope drop
  "bassoon"  '( 0.0 0.0 10.0 1.0 90.0 1.0 100.0 0.0 )           1.0  define-envelope drop
  "clarinet" '( 0.0 0.0 25.0 1.0 75.0 1.0 100.0 0.0 )          32.0  define-envelope drop
  "woodwind" '( 0.0 0.0 10.0 1.0 90.0 1.0 100.0 0.0 )           0.32 define-envelope drop
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
  fname io-open-write dup $" \\ -*- snd-forth -*-\n" io-write io-close
  #t
; add-hook!

window-property-changed-hook lambda: <{ cmd -- }>
  $" remote command received: %S" '( cmd ) clm-message
; add-hook!

output-comment-hook lambda: <{ str1 -- str2 }>
  selected-sound marks->string
; add-hook!
0 [if]
  after-open-hook lambda: <{ snd -- }>
    snd comment ( str ) ['] string-eval #t nil fth-catch if ( str ) drop then
  ; add-hook!
[then]

*snd-home* "/snd-saved.fs" $+  set-save-state-file          drop
*snd-home* "/zap" $+           set-temp-dir                 drop
*snd-home* "/zap" $+           set-save-dir                 drop
"/usr/gnu/cvs/snd"             set-html-dir                 drop
22050             	       set-recorder-srate           drop
2                 	       set-recorder-out-chans       drop
mus-next          	       set-recorder-out-header-type drop
mus-lfloat        	       set-recorder-out-data-format drop
mus-audio-dac-out 	       set-recorder-in-device       drop
2                 	       set-recorder-in-chans        drop
#t                	       set-recorder-autoload        drop
*snd-home* "/sound/rec.snd" $+ set-recorder-file            drop
#t                	       set-trap-segfault            drop
#t                	       set-show-indices             drop
0.0               	       set-auto-update-interval     drop
"rev"           	       add-sound-file-extension     drop
"reverb"        	       add-sound-file-extension     drop
"wave"          	       add-sound-file-extension     drop

'snd-nogui provided? [if]
  #f set-verbose-cursor drop
  \ Fth repl and prompt hooks
  before-repl-hook reset-hook!
  before-repl-hook lambda: <{ -- }>
    *fth-verbose* if
      "" '() clm-message
      $" Starting session on %v %r!" '( current-time strftime ) clm-message
      "" '() clm-message
    then
  ; add-hook!

  after-repl-hook lambda: <{ history -- }>
    history readlines array-reverse! array-uniq! array-reverse! { ary }
    history ary writelines
    *fth-verbose* if
      "" '() clm-message
      $" Thank you for using SND!" '() clm-message
      "" '() clm-message
    then
    1 sleep
  ; add-hook!

  __simple-nogui-prompt__ [if]
    \ A simple prompt for snd-nogui.
    before-prompt-hook lambda: <{ prompt pos --  new-prompt }>
      $" snd (%d)> " '( pos ) string-format
    ;
  [else]
    \ A more elaborated prompt for snd-nogui.
    before-prompt-hook lambda: <{ prompt pos -- new-prompt }>
      "%I:%M%p" current-time strftime string-downcase! { tm }
      file-pwd { path }
      path *home* string-member? if
	path *home* string-split { ary }
	ary length 1 > if
	  ary -1 array-ref "~" string-unshift to path
	else
	  "~" to path
	then
      then
      $" (%s:%s)\n[%s %s] (%d)> " '( *short-hostname* path *program-name* tm pos ) string-format
    ;
  [then] add-hook!
[else]					\ not snd-nogui
  require draw
  'snd-motif provided? [if]
    require effects
    require popup
    edhist-save-hook lambda: <{ prc -- }> "%S" '( prc ) clm-message ; add-hook!
  [then]

  \ C-x C-c
  char c 4 lambda: <{ -- f }> 0 snd-exit #f ; #t $" terminate Snd [ms]" dup bind-key drop
  \ C-x k
  char k 0 lambda: <{ -- f }>
    selected-sound close-sound-extend #f
  ; #t $" close sound and jump to next open [ms]" dup bind-key drop

  graph-hook lambda: <{ snd chn y0 y1 -- }>
    $" freq: %.3f" '( snd chn left-sample  snd chn spot-freq ) string-format
    snd #f report-in-minibuffer drop
    #f
  ; add-hook!

  mouse-click-hook lambda: <{ snd chn button state x y axis -- }>
    axis time-graph = if
      $" freq: %.3f" '( snd chn #f cursor  snd chn spot-freq ) string-format
      snd #f report-in-minibuffer
    else
      #f
    then
  ; add-hook!

  after-open-hook lambda: <{ snd -- val }>
    snd channels 0 ?do snd short-file-name snd i time-graph set-x-axis-label drop loop
    #t snd set-cursor-follows-play drop
    channels-combined snd set-channel-style
  ; add-hook!

  start-playing-hook lambda: <{ snd -- f }>
    #f
    snd sound? if snd cursor-follows-play if drop cursor-line snd #t set-cursor-style then then
  ; add-hook!

  stop-playing-hook lambda: <{ snd -- f }>
    #f
    snd sound? if drop cursor-cross snd #t set-cursor-style then
  ; add-hook!

  enved-hook lambda: <{ en pt x y reason -- en'|#f }>
    reason enved-move-point = if
      x en car f> x en -2 list-ref f< && if
	en en pt 2* list-ref x #f #f stretch-envelope { new-en }
	new-en pt 2* 1+ y list-set!
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
  2 set-global-sync
  'xm provided? [if]
    add-mark-pane
    make-current-window-display
    #t show-smpte-label
  [then]
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
  #f                                  set-with-gl drop
  graph-once                          set-transform-graph-type drop
  #t                                  set-show-transform-peaks drop
  samaraki-window                     set-fft-window drop
  fourier-transform                   set-transform-type drop
  after-transform-hook ' fft-peak     add-hook!
  __lisp-graph__ [if]
    lisp-graph-hook ' display-db      add-hook!
    lisp-graph-hook ' display-energy  add-hook!
  [then]
  mix-click-hook  ' mix-click-info    add-hook!
  mark-click-hook ' mark-click-info   add-hook!
  defined? show-disk-space [if]
    after-open-hook ' show-disk-space add-hook!
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

\ find-file searchs in *clm-search-list*
*clm-file-name*  find-file dup [if] open-sound [then] drop
'snd-nogui provided? [if] cr [then]
$" Snd of %s (Fth %s)" '( snd-version fth-version ) clm-message
128 set-object-print-length

\ .snd_forth ends here
