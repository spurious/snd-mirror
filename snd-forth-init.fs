\ .snd_forth -- start up file for Snd/Forth

\ You can install the *.fs scripts with:
\ 
\   cd ${top_srcdir}/examples/site-lib
\   ./install.fth
\ 
\ or even better
\
\   cd ${top_builddir}
\   make install
\
\ If you have installed *.fs scripts with one of the above mentioned
\ commands, you don't need to add a path to *load-path*.
\ ${prefix}/share/fth/site-fth is already included.  Otherwise you can
\ add a path with e.g.:
\ 
\   "/home/mike/snd" add-load-path

\ A special *SND-HOME* path points here to ~/.snd.d (similar to ~/.emacs.d):
\ 
\ ~/.snd.d            directory for save-state-file
\ ~/.snd.d/sound      directory for *clm-file-name*
\                                   add-directory-to-view-files-list
\                                   set-open-file-dialog-director
\ ~/.snd.d/zap        directory for set-temp-dir
\                                   set-save-dir
\ ~/.snd.d/peaks      directory for set-peak-env-dir
\
\ "HOME" getenv       value *home*
\ *home* "/.snd.d" $+ value *snd-home*
\
\ Change these paths to fit your needs!
\

#t to *fth-verbose*
#f to *fth-debug*

#f value *init-with-peaks*		\ with peak-env support
#f value *init-graph-extra-hooks*	\ with display-correlate, zoom-spectrum, superimpose-ffts
#f value *init-lisp-graph-extra-hooks*	\ with display-energy, display-db

: print-loading-file { fname -- }
  *fth-verbose* if $" \\ loading %s\n" '( fname ) clm-print then
;

*filename* print-loading-file

"HOME" getenv                     value *home*
*home* "/.snd.d"               $+ value *snd-home*
hostname                          value *hostname*
*hostname* /\\./ string-split car value *short-hostname*
*argv* length 0> [if] *argv* car undef file-basename [else] "snd" [then] value *program-name*

before-load-hook lambda: <{ fname -- f }>
  fname print-loading-file
  #t
; add-hook!

\ if configured --with-shared-sndlib
dl-load sndlib Init_sndlib

\ Set them before loading clm.fs.
2                      set-default-output-chans   drop
48000    	       set-default-output-srate   drop
512                    set-dac-size               drop
mus-clipping           set-clipping               drop
1024 1024 *            set-mus-file-buffer-size   drop
24                     set-mus-array-print-length drop
mus-array-print-length set-print-length           drop
128                    set-object-print-length

require clm
require clm-ins

\ Environment variable CLM_SEARCH_PATH
\ Path variable where sound files reside.
\ csh: setenv CLM_SEARCH_PATH /usr/gnu/sound/SFiles:${HOME}/.snd.d/sound
\  sh: CLM_SEARCH_PATH=/usr/gnu/sound/SFiles:${HOME}/.snd.d/sound; export CLM_SEARCH_PATH

"CLM_SEARCH_PATH" getenv dup [if]
  ":" string-split [each] *clm-search-list* swap array-push to *clm-search-list* [end-each]
[else]
  drop
  *clm-search-list* *snd-home* "/sound" $+ array-push to *clm-search-list*
[then]
#t                                     to *clm-play*
#t                                     to *clm-statistics*
#t                                     to *clm-verbose*
#f                                     to *clm-debug*
*snd-home* "/sound/fth-test.snd"    $+ to *clm-file-name*
*snd-home* "/sound/fth-test.reverb" $+ to *clm-reverb-file-name*
#t                                     to *clm-delete-reverb*
lambda: <{ ins beg dur -- }> $" %14s: %5.2f %5.2f" '( ins beg dur ) clm-message ; to *clm-notehook*

'snd-nogui provided? [if]
  \ snd-nogui repl and prompt hooks
  before-repl-hook reset-hook!		\ remove default hook
  before-repl-hook lambda: <{ -- }>
    "" #f clm-message
    $" Starting session on %s." '( $" %Ev %Er" current-time strftime ) clm-message
    "" #f clm-message
  ; add-hook!
  after-repl-hook lambda: <{ history -- }>
    "" #f clm-message
    $" Thank you for using %s!" #( *program-name* string-upcase ) clm-message
    "" #f clm-message
    1 sleep
  ; add-hook!
  
  \ A more elaborated prompt for fth and snd-forth-nogui.
  before-prompt-hook lambda: <{ prompt pos -- new-prompt }>
    "%EI:%EM%p" current-time strftime string-downcase! { tm }
    "(/usr)?" *home* $+ make-regexp file-pwd "~" regexp-replace { path }
    $" (%s:%s)\n[%s %s] (%d)> " #( *short-hostname* path *program-name* tm pos ) string-format
  ; add-hook!
[then]

*snd-home* add-load-path
*init-with-peaks* [if]
  *snd-home* "/peaks"      $+ set-peak-env-dir               drop
[then]
*snd-home* "/snd-saved.fs" $+ set-save-state-file            drop
*snd-home* "/zap"          $+ set-temp-dir                   drop
*snd-home* "/zap"          $+ set-save-dir                   drop
*snd-home* "/sound"        $+ set-open-file-dialog-directory drop
"/usr/gnu/cvs/snd"            set-html-dir                   drop
"BROWSER" getenv "firefox" || set-html-program               drop
#t                	      set-trap-segfault              drop
#t                            set-show-listener              drop
0.0               	      set-auto-update-interval       drop
"rev"           	      add-sound-file-extension       drop
"reverb"        	      add-sound-file-extension       drop
"wave"          	      add-sound-file-extension       drop
*clm-search-list* [each] ( dir ) undef add-directory-to-view-files-list drop [end-each]

before-save-state-hook lambda: <{ fname -- f }>
  $" \\ -*- snd-forth -*-\n" :filename fname with-output-port
  #t				      \ #t --> append mode
; add-hook!

\ make-default-comment from clm.fs
output-comment-hook lambda: <{ str -- s }>
  str empty? if make-default-comment else str then
; add-hook!

'snd-nogui provided? [unless]
  require snd-xm
  after-open-hook <'> show-disk-space add-hook!

  require effects
  #f to use-combo-box-for-fft-size	\ boolean (default #f)

  'snd-motif provided? [if]
    \ snd-xm.fs
    add-mark-pane
  [then]

  'snd-gtk provided? [if]
    $" Serif 10" set-axis-label-font drop
  [then]

  require extensions
  #t set-emacs-style-save-as
  with-reopen-menu
  with-buffers-menu

  require examp
  *init-graph-extra-hooks* [if]
    graph-hook         <'> display-correlate  add-hook!
    graph-hook         <'> zoom-spectrum      add-hook!
    graph-hook         <'> superimpose-ffts   add-hook!
  [then]
  *init-lisp-graph-extra-hooks* [if]
    lisp-graph-hook    <'> display-energy     add-hook!
    lisp-graph-hook    <'> display-db         add-hook!
  [then]
  after-transform-hook <'> fft-peak           add-hook!

  require mix
  mix-click-hook       <'> mix-click-sets-amp add-hook!
  mix-click-hook       <'> mix-click-info     add-hook!

  require marks
  save-mark-properties
  mark-click-hook      <'> mark-click-info    add-hook!

  require dsp
  graph-hook lambda: <{ snd chn y0 y1 -- #f }>
    $" freq: %.3f" #( snd chn left-sample  snd chn spot-freq ) string-format
    snd #f report-in-minibuffer drop
    #f
  ; add-hook!

  mouse-click-hook lambda: <{ snd chn button state x y axis -- a }>
    axis time-graph = if
      $" freq: %.3f" #( snd chn #f cursor  snd chn spot-freq ) string-format
      snd #f report-in-minibuffer
    else
      #f
    then
  ; add-hook!

  require env
  enved-hook lambda: <{ en pt x y reason -- en'|#f }>
    reason enved-move-point = if
      x en 0 array-ref f> x en -2 array-ref f< && if
	en en pt 2* array-ref x #f #f stretch-envelope ( new-en ) dup pt 2* 1+ y array-set!
      else
	#f
      then
    else
      #f
    then
  ; add-hook!

  require rgb
  beige                  set-selected-graph-color    drop
  blue                   set-selected-data-color     drop

  #t           	         set-show-indices            drop
  #t                     set-with-inset-graph        drop
  #t                     set-with-pointer-focus      drop
  #t                     set-ask-about-unsaved-edits drop
  #f                     set-remember-sound-state    drop
  #t                     set-with-smpte-label        drop
  #t                     set-with-toolbar            drop
  #t                     set-show-full-duration      drop
  #t                     set-with-verbose-cursor     drop
  #t                     set-with-tracking-cursor    drop
  #t  			 set-just-sounds             drop
  #t  			 set-enved-wave?             drop
  #t  			 set-show-y-zero             drop
  #t                     set-show-transform-peaks    drop
  speed-control-as-ratio set-speed-control-style     drop
  graph-as-spectrogram   set-transform-graph-type    drop \ graph-once graph-as-sonogram
  rainbow-colormap	 set-colormap                drop
  $" snd> "              set-listener-prompt         drop
  160 		         set-window-x                drop
  0 			 set-window-y                drop
  800 		         set-window-width            drop
  600 		         set-window-height           drop

  exit-hook lambda: <{ -- f }>
    save-state-file save-state drop
    sounds each close-sound drop end-each
    #t
  ; add-hook! 

  after-open-hook lambda: <{ snd -- }>
    snd channels 0 ?do snd short-file-name snd i time-graph set-x-axis-label drop loop
    channels-combined snd set-channel-style
  ; add-hook!

  : snd-set-cursor-style { snd kind -- #f }
    snd sound? if kind snd #t set-cursor-style drop then
    #f
  ;
  start-playing-hook lambda: <{ snd -- f }> snd cursor-line  snd-set-cursor-style ; add-hook!
  stop-playing-hook  lambda: <{ snd -- f }> snd cursor-cross snd-set-cursor-style ; add-hook!

  \ bind-key ( key modifiers func :optional extended=#f origin="" prefs-info="" -- val )
  \ 
  \ modifiers:
  \   0 normal
  \   1 shift
  \   4 control
  \   8 meta
  \ 
  \ extended (prefix key):
  \   #t  C-x
  \   #f  none
  \
  \ func ( -- val )
  \
  \ val should be:
  \   cursor-in-view
  \   cursor-on-left
  \   cursor-on-right
  \   cursor-in-middle
  \   keyboard-no-action
  \ 
  \ C-x C-c terminate Snd
  <char> c 4 lambda: <{ -- val }>
    0 snd-exit drop
    cursor-in-view
  ; #t $" terminate Snd" "terminate-snd" bind-key drop
  \ C-x k close selected sound
  <char> k 0 lambda: <{ -- val }>
    selected-sound close-sound-extend
    cursor-in-view
  ; #t $" close sound and jump to next open" "close-current-sound" bind-key drop
  \ C-x C-k show listener
  <char> k 4 lambda: <{ -- val }>
    #t set-show-listener drop
    cursor-in-view
  ; #t $" show listener" "show-listener" bind-key drop
  \ C-x C-n hide listener
  <char> n 4 lambda: <{ -- val }>
    #f set-show-listener drop
    cursor-in-view
  ; #t $" hide listener" "hide-listener" bind-key drop
  \ C-x C-x play
  <char> x 4 lambda: <{ -- val }>
    #t play drop
    cursor-in-view
  ; #t $" play current sound" "play-current-sound" bind-key drop
  \ C-x C-t play from cursor
  <char> t 4 lambda: <{ -- val }>
    selected-sound :start undef undef undef cursor play drop
    cursor-in-view
  ; #t $" play from cursor" "play-from-cursor" bind-key drop
  \ C-x x eval over selection
  <char> x 0 lambda: <{ -- val }>
    undef selection? if
      $" selection-eval:" <'> eval-over-selection #f #f prompt-in-minibuffer
    else
      $" no selection" #f #f report-in-minibuffer
    then drop
    cursor-in-view
  ; #t $" eval over selection" "eval-over-selection" bind-key drop
[then]					\ not snd-nogui

'snd-nogui provided? [unless]
  save-state-file file-exists? [if] require snd-saved [then]
[then]

\ find-file searchs in *clm-search-list*
sounds empty? [if]
  *clm-file-name* find-file dup [if] open-sound [then] drop cr
[then]

$" Snd of %s (Fth %s)" #( snd-version fth-version ) clm-message

\ .snd_forth ends here
