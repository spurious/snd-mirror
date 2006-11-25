\ .snd_forth -- start up file for Snd/Forth -*- snd-forth -*-

\ You can easily install the *.fs scripts with:
\ 
\   cd fth-0.8.x/examples/site-lib
\   ./install.fth
\
\ If you have installed the *.fs files with install.fth, you don't
\ need to add a path to *load-path*.  ${prefix}/share/fth/site-fth is
\ already included.  Otherwise you can add a path with e.g.
\
\   "/home/mike/snd-8" add-load-path

\ A special SND-HOME path is ~/.snd.d:
\ 
\ /home/mike/.snd.d
\ /home/mike/.snd.d/sound
\ /home/mike/.snd.d/zap       for set-temp-dir, set-save-dir
\ /home/mike/.snd.d/peaks
\
\ You must change these paths to your needs!

$" HOME" getenv $" /.snd.d" $+ value snd-home

#t to *fth-verbose*

before-load-hook lambda: { fname -- f }
  $" \\ loading \"%s\"\n" '( fname ) clm-print
  #t
; 1 make-proc add-hook!

dl-load sndlib Init_sndlib
dl-load sndins Init_sndins
dl-load gdbm Init_gdbm
require clm
require clm-ins
require examp
require hooks
require extensions
require env
require peak-env
require rgb

'snd-motif provided? 'xm provided? not && [if] dl-load libxm Init_libxm [then]
'snd-gtk   provided? 'xg provided? not && [if] dl-load libxg Init_libxg [then]

#f check-for-unsaved-edits
3 remember-sound-state

snd-home add-load-path
snd-home $" /snd-remember-sound.fs" $+ to remember-sound-filename
snd-home $" /peaks" $+ to save-peak-env-info-directory

$" SFDIR" getenv       add-directory-to-view-files-list drop
snd-home $" /sound" $+ add-directory-to-view-files-list drop

*clm-search-list*
$" SFDIR" getenv       array-push
snd-home $" /sound" $+ array-push drop

#t                                    to *clm-verbose*
snd-home $" /sound/rb-test.snd"    $+ to *clm-file-name*
snd-home $" /sound/rb-test.reverb" $+ to *clm-reverb-file-name*
lambda: ( str -- ) ." \ <" .string ." >" cr ; to *clm-notehook*
1024 1024 *                           to *clm-file-buffer-size*
*clm-file-buffer-size* set-mus-file-buffer-size drop

[ifundef] enved
  $" enved"    '( 0.0 0.0 25.0 1.0 75.0 1.0 100.0 0.0 )           1.0  define-envelope drop
  $" brass"    '( 0.0 0.0 20.0 1.0 40.0 0.6 90.0 0.5 100.0 0.0 )  1.0  define-envelope drop
  $" bassoon"  '( 0.0 0.0 10.0 1.0 90.0 1.0 100.0 0.0 )           1.0  define-envelope drop
  $" clarinet" '( 0.0 0.0 25.0 1.0 75.0 1.0 100.0 0.0 )          32.0  define-envelope drop
  $" woodwind" '( 0.0 0.0 10.0 1.0 90.0 1.0 100.0 0.0 )           0.32 define-envelope drop
[then]

\ Snd hooks
exit-hook lambda: ( -- f )
  save-state-file save-state drop
  #t
; 0 make-proc add-hook! 

after-save-as-hook lambda: { snd fname from-dialog -- f }
  snd revert-sound drop
  snd close-sound drop
  fname open-sound drop
  #f
; 3 make-proc add-hook!

before-save-state-hook lambda: { fname -- f }
  fname io-open-write dup $" \\ -*- snd-forth -*-\n" io-write io-close
  #t
; 1 make-proc add-hook!

window-property-changed-hook lambda: { cmd -- }
  $" \\ remote command received: %s" '( cmd ) clm-print
; 1 make-proc add-hook!

snd-home $" /snd-saved.fs" $+  set-save-state-file drop
snd-home $" /zap" $+           set-temp-dir drop
snd-home $" /zap" $+           set-save-dir drop
$" /usr/gnu/cvs/snd"           set-html-dir drop
22050      	  	       set-default-output-srate drop
2          	  	       set-default-output-chans drop
mus-next   	  	       set-default-output-header-type drop
mus-lfloat 	  	       set-default-output-data-format drop
512               	       set-dac-size drop
mus-audio-default 	       set-audio-output-device drop
22050             	       set-recorder-srate drop
2                 	       set-recorder-out-chans drop
mus-next          	       set-recorder-out-header-type drop
mus-lfloat        	       set-recorder-out-data-format drop
mus-audio-dac-out 	       set-recorder-in-device drop
2                 	       set-recorder-in-chans drop
#t                	       set-recorder-autoload drop
snd-home $" /sound/rec.snd" $+ set-recorder-file drop
#t                	       set-trap-segfault drop
#t                	       set-show-indices drop
0.0               	       set-auto-update-interval drop
$" rev"           	       add-sound-file-extension drop
$" reverb"        	       add-sound-file-extension drop
$" wave"          	       add-sound-file-extension drop

'snd-nogui provided? [if]
  \ Fth hooks
  before-repl-hook reset-hook!
  before-repl-hook lambda: ( -- )
    $" \\\n" '() clm-print
    $" \\ Starting session on %s!\n" '( date ) clm-print
    $" \\\n" '() clm-print
  ; 0 make-proc add-hook!

  after-repl-hook lambda: { history -- }
    history readlines array-reverse! array-uniq! array-reverse! { ary }
    history ary writelines
    $" \\\n" '() clm-print
    $" \\ Thank you for using Snd!\n" '() clm-print
    $" \\\n" '() clm-print
    1 sleep
  ; 1 make-proc add-hook!

  before-prompt-hook lambda: { prompt pos --  new-prompt }
    $" snd (%d)> " '( pos ) string-format
  ; 2 make-proc add-hook!
[else]					\ not snd-nogui
  require mix
  require marks
  require draw
  'snd-motif provided? [if]
    require effects
    require popup
    edhist-save-hook lambda: { prc -- } '( prc $" \n" ) .stdout ; 1 make-proc add-hook!
  [then]

  \ C-x C-c
  char c 4 lambda: ( -- f ) 0 snd-exit #f ; 0 make-proc #t $" terminate Snd [ms]" dup bind-key drop
  \ C-x k
  char k 0 lambda: ( -- f )
    selected-sound close-sound-extend #f
  ; 0 make-proc #t $" close sound and jump to next open [ms]" dup bind-key drop

  after-open-hook lambda: { snd -- val }
    snd channels 0 ?do snd short-file-name snd i time-graph set-x-axis-label drop loop
    #t snd set-cursor-follows-play drop
    channels-combined snd set-channel-style
  ; 1 make-proc add-hook!

  start-playing-hook lambda: { snd -- f }
    #f
    snd sound? if snd cursor-follows-play if drop cursor-line snd #t set-cursor-style then then
  ; 1 make-proc add-hook!

  stop-playing-hook lambda: { snd -- f }
    #f
    snd sound? if drop cursor-cross snd #t set-cursor-style then
  ; 1 make-proc add-hook!

  enved-hook lambda: { en pt x y reason -- en'|#f }
    reason enved-move-point = if
      x en car f> x en -2 list-ref f< and if
	en en pt 2* list-ref x #f #f stretch-envelope { new-en }
	new-en pt 2* 1+ y list-set!
	new-en
      else
	#f
      then
    else
      #f
    then
  ; 5 make-proc add-hook!

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
  #f  			    	      set-show-listener drop
  #f  			    	      set-show-controls drop
  #f  			    	      set-just-sounds drop
  1.0 			    	      set-enved-base drop
  #t  			    	      set-enved-wave? drop
  #t  			    	      set-show-y-zero drop
  #t  			    	      set-verbose-cursor drop
  beige                               set-selected-graph-color drop
  blue                                set-selected-data-color drop
  #f                                  set-with-gl drop
  graph-once                          set-transform-graph-type drop
  #t                                  set-show-transform-peaks drop
  dolph-chebyshev-window              set-fft-window drop
  fourier-transform                   set-transform-type drop
  \ lisp-graph-hook ' display-db      2 make-proc add-hook!
  \ lisp-graph-hook ' display-energy  2 make-proc add-hook!
  mix-click-hook  ' mix-click-info  1 make-proc add-hook!
  mark-click-hook ' mark-click-info 1 make-proc add-hook!
  defined? show-disk-space [if]
    after-open-hook ' show-disk-space 1 make-proc add-hook!
  [then]
  $" snd> "              set-listener-prompt drop
  #t  			 set-show-listener drop
  #f  			 set-show-controls drop
  160 			 set-window-x drop
  0 			 set-window-y drop
  800 			 set-window-width drop
  600 			 set-window-height drop
  speed-control-as-ratio set-speed-control-style drop
[then]					\ not snd-nogui

\ find-file searchs in *clm-search-list*
*clm-file-name* find-file dup [if] open-sound [then] drop
$" oboe.snd"    find-file dup [if] open-sound [then] drop

$" fth" textdomain drop

\ .snd_forth ends here
