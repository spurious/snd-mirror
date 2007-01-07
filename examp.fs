\ -*- snd-forth -*-
\ examp.fs -- examples from examp.scm|rb

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Tue Jul 05 13:09:37 CEST 2005
\ Changed: Sat Jan 06 05:56:58 CET 2007

\ Commentary:
\
\ With original comments and doc strings from examp.scm.
\
\ all-chans           	    ( -- array-of-lists )
\ close-sound-extend  	    ( snd -- )
\ snd-snd             	    ( :optional snd -- snd )
\ snd-chn             	    ( :optional chn -- chn )
\
\ from frame.scm
\ insert-vct                ( v :optional beg dur snd chn edpos -- samps )
\
\ examp.(scm|rb)
\ selection-rms       	    ( -- val )
\ region-rms          	    ( :optional n -- val )
\ window-samples      	    ( :optional snd chn -- vct )
\ display-energy      	    ( snd chn -- val )
\ display-db          	    ( snd chn -- val )
\ window-rms                ( -- val )
\ fft-peak            	    ( snd chn scaler -- pk )
\ finfo                     ( file -- str )
\ correlate                 ( snd chn y0 y1 -- val )
\ zoom-spectrum             ( snd chn y0 y1 -- val )
\ superimpose-ffts          ( snd chn y0 y1 -- val )
\ locate-zero               ( limit -- samp )
\ mpg                       ( mpgfile rawfile -- )
\ auto-dot                  ( snd chn y0 y1 -- val )
\ first-mark-in-window-at-left ( -- )
\ flash-selected-data       ( millisecs -- )
\ mark-loops                ( -- )
\
\ do-all-chans              ( func :optional origin -- )
\ update-graphs             ( -- )
\ do-chans                  ( func :optional origin -- )
\ do-sound-chans            ( func :optional origin -- )
\ every-sample?             ( func -- f )
\ sort-samples              ( nbins -- ary )
\ place-sound               ( mono stereo pan -- res )
\ fft-edit            	    ( bottom top :optional snd chn -- vct )
\ fft-squelch         	    ( squelch :optional snd chn -- scl )
\ fft-cancel          	    ( lo-freq hi-freq :optional snd chn -- vct )
\ make-ramp           	    ( :optional size -- gen )
\ ramp                	    ( gen up -- val )
\ squelch-vowels      	    ( :optional snd chn -- val )
\ fft-env-data        	    ( fft-env :optional snd chn -- vct )
\ fft-env-edit        	    ( fft-env :optional snd chn -- vct )
\ fft-env-interp      	    ( env1 env2 interp :optional snd chn -- vct )
\ filter-fft                ( flt :optional normalize snd chn -- val )
\ fft-smoother              ( cutoff start samps :optional snd chn -- val )
\ comb-filter         	    ( scaler size -- proc; x self -- res )
\ comb-chord          	    ( scaler size amp -- proc; x self -- res )
\ zcomb               	    ( scaler size pm -- proc; x self -- val )
\ notch-filter        	    ( scaler size -- proc; x self -- val )
\ formant-filter      	    ( radius frequency -- proc; x self -- val )
\ formants            	    ( r1 f1 r2 f2 r3 f3 -- proc; x self -- val )
\ moving-formant      	    ( radius move -- proc; x self -- val )
\ osc-formants        	    ( radius bases amounts freqs -- proc; x self -- val )
\
\ hello-dentist       	    ( frq amp :optional snd chn -- vct )
\ fp                  	    ( sr osamp osfrq :optional snd chn -- vct )
\ compand             	    ( -- prc; y self -- val )
\ compand-channel     	    ( :optional beg dur snd chn edpos -- val )
\ compand-sound       	    ( :optional beg dur snd -- )
\ expsnd              	    ( gr-env :optional snd chn -- vct )
\ voiced->unvoiced    	    ( amp fftsize r tempo :optional snd chn -- vct )
\ make-sound-interp   	    ( start :optional snd chn -- prc; loc self -- val )
\ sound-interp        	    ( func loc -- val )
\ env-sound-interp    	    ( envelope :optional time-scale snd chn -- file-name )
\ filtered-env        	    ( e :optional snd chn -- val )
\
\ zero+               	    ( -- proc; n self -- val )
\ next-peak           	    ( -- proc; n self -- val )
\ file->vct           	    ( file -- vct )
\ add-notes           	    ( notes :optional snd chn -- #f )
\ smooth-channel-via-ptree  ( :optional beg dur snd chn edpos -- val )
\ ring-modulate-channel     ( freq :optional beg dur snd chn edpos -- val )
\ reverse-by-blocks         ( block-len :optional snd chn -- val )
\ reverse-within-blocks     ( block-len :optional snd chn -- val )
\
\ make-moog-filter    	    ( freq Q -- gen )
\ moog-frequecy@      	    ( gen -- frq )
\ moog-frequecy!      	    ( frq gen -- )
\ moog-filter         	    ( gen sig -- A )

\ Code:

require clm
require env
require rgb

\ #( '( snd0 chn0 ) '( snd0 chn1 ) ... )
: all-chans ( -- array-of-lists )
  #() { ary }
  sounds each { snd }
    snd channels 0 ?do
      ary '( snd i ) array-push drop
    loop
  end-each
  ary
;

\ Also defined in clm.fs.
[undefined] close-sound-extend [if]
  \ 5 == notebook widget
  : close-sound-extend <{ snd -- }>
    main-widgets 5 list-ref false? unless
      0 { idx }
      sounds empty? unless sounds snd list-index to idx then
      snd close-sound drop
      sounds empty? unless
	sounds length 1 = if
	  sounds car
	else 
	  idx sounds length < if
	    sounds idx list-ref
	  else
	    sounds last-pair car
	  then
	then set-selected-sound drop
      then
    else
      snd close-sound drop
    then
  ;
[then]

: snd-snd <{ :optional snd #f -- snd }>
  snd integer? if
    snd
  else
    selected-sound integer? if
      selected-sound
    else
      sounds car
    then
  then
;
: snd-chn <{ :optional chn #f -- chn }>
  chn integer? if
    chn
  else
    #f selected-channel integer? if
      #f selected-channel
    else
      0
    then
  then
;

\ === from frame.scm
\
: insert-vct <{ v :optional beg 0 dur #f snd #f chn #f edpos #f -- samps }>
  doc" Inserts vct V's data into sound SND at BEG."
  v vct? v 1 $" a vct" assert-type
  dur v vct-length || { len }
  beg len v snd chn edpos #f $" %S %s %s %s" '( v beg dur get-func-name ) format insert-samples
;

\ === examp.scm|rb
\ 
\ this mainly involves keeping track of the current sound/channel
: selection-rms ( -- val )
  doc" Returns rms of selection data using sample readers."
  selection? if
    selection-position #f #f 1 #f make-sample-reader { rd }
    selection-frames { len }
    0.0 ( sum ) len 0 ?do rd next-sample dup f* f+ ( sum += ... ) loop
    len f/ fsqrt
  else
    'no-active-selection '( get-func-name ) fth-throw
  then
;

: region-rms <{ :optional reg 0 -- val }>
  doc" Returns rms of region N's data (chan 0)."
  reg region? if
    0 0 reg region->vct { data }
    data dup dot-product data length f/ fsqrt
  else
    'no-such-region '( get-func-name reg ) fth-throw
  then
;

: window-samples <{ :optional snd #f chn #f -- vct }>
  doc" Samples in SND channel CHN in current graph window."

  snd chn left-sample { wl }
  snd chn right-sample { wr }
  wl  wr wl - 1+  snd chn #f channel->vct
;

: display-energy <{ snd chn -- v }>
  doc" A lisp-graph-hook function to display the time domain data as energy (squared).\n\
list-graph-hook ' display-energy add-hook!"
  snd chn undef undef undef make-graph-data dup list? if cadr then { data }
  data if
    snd chn left-sample { ls }
    snd chn right-sample { rs }
    snd srate { sr }
    snd chn y-zoom-slider { y-max }
    data dup vct-multiply!  $" energy"  ls sr f/  rs sr f/  0.0  y-max dup f*  snd chn #t graph
  else
    #f
  then
;
\ lisp-graph-hook ' display-energy add-hook!

hide
: db-calc ( val -- r ) { val } val 0.001 f< if -60.0 else 20.0 val flog10 f* then ;
set-current
: display-db <{ snd chn -- v }>
  doc" A lisp-graph-hook function to display the time domain data in dB.\n\
list-graph-hook ' display-db add-hook!"
  snd chn undef undef undef make-graph-data dup list? if cadr then { data }
  data if
    snd chn left-sample { ls }
    snd chn right-sample { rs }
    snd srate { sr }
    data map
      *key* fabs db-calc 60.0 f+
    end-map  $" dB"  ls sr f/  rs sr f/  0.0  60.0  snd chn #t graph
  else
    #f
  then
;
previous
\ lisp-graph-hook ' display-db add-hook!

: window-rms ( -- val )
  doc" Returns rms of data in currently selected graph window."
  #f #f left-sample  { ls }
  #f #f right-sample { rs }
  ls rs ls - 1+ #f #f #f channel->vct { data }
  data vct-length { len }
  data data len dot-product len f/ fsqrt
;

: fft-peak <{ snd chn scaler -- pk }>
  doc" Returns the peak spectral magnitude"
  snd chn transform-graph?
  snd chn transform-graph-type graph-once = && if
    snd chn #f transform->vct vct-peak f2* snd chn transform-size f/
    object->string snd #f report-in-minibuffer
  else
    #f
  then
;
\ after-transform-hook ' fft-peak add-hook!

\ ;;; -------- 'info' from extsnd.html using format --------

: finfo ( file -- str )
  doc" Returns description (as a string) of file."
  find-file { file }
  file false? if 'no-such-file '( get-func-name file ) fth-throw then
  $" %s: chans: %d, srate: %d, %s, %s, len: %1.3f"
  '( file
     file mus-sound-chans
     file mus-sound-srate
     file mus-sound-header-type mus-header-type-name
     file mus-sound-data-format mus-data-format-name
     file mus-sound-samples file mus-sound-chans file mus-sound-srate f* f/ ) string-format
;

\ ;;; -------- Correlation --------
\ ;;;
\ ;;; correlation of channels in a stereo sound

: correlate <{ snd chn y0 y1 -- val }>
  doc" Returns the correlation of SND's 2 channels (intended for use with graph-hook).  \
y0 and y1 are ignored."
  snd channels 2 = if
    snd 0 #f frames 1 >
    snd 1 #f frames 1 > && if
      snd chn left-sample  { ls }
      snd chn right-sample { rs }
      rs ls - 1+ { ilen }
      ilen flog 2.0 flog f/ fround->s { pow2 }
      2.0 pow2 f** fround->s { fftlen }
      fftlen 2/ { fftlen2 }
      fftlen 1/f { fftscale }
      ls fftlen snd 0 #f channel->vct { rl1 }
      ls fftlen snd 1 #f channel->vct { rl2 }
      fftlen 0.0 make-vct { im1 }
      fftlen 0.0 make-vct { im2 }
      rl1 im1 1 fft drop
      rl2 im2 1 fft drop
      rl1 vct-copy { tmprl }
      im1 vct-copy { tmpim }
      fftlen2 0.0 make-vct { data3 }
      tmprl rl2 vct-multiply! drop
      tmpim im2 vct-multiply! drop
      im2   rl1 vct-multiply! drop
      rl2   im1 vct-multiply! drop
      tmprl tmpim vct-add! drop
      im2 rl2 vct-subtract! drop
      tmprl im2 -1 fft drop
      data3 tmprl vct-add! drop
      data3 fftscale vct-scale! drop
      data3 $" lag time" 0 fftlen2 undef undef snd chn undef undef graph
    then
  else
    $" %s wants stereo input" '( get-func-name ) string-format snd #f report-in-minibuffer
  then
;
\ graph-hook ' correlate add-hook!

\ ;;; -------- set transform-size based on current time domain window size
\ ;;;
\ ;;; also zoom spectrum based on y-axis zoom slider

: zoom-spectrum <{ snd chn y0 y1 -- val }>
  doc" Sets the transform size to correspond to the time-domain window size (use with graph-hook)."
  snd chn transform-graph?
  snd chn transform-graph-type graph-once = && if
    2.0 snd chn right-sample snd chn left-sample f- flog 2.0 flog f/ fceil f** fround->s
    snd chn set-transform-size drop
    snd chn y-zoom-slider snd chn set-spectro-cutoff drop
  then
  #f
;
\ graph-hook ' zoom-spectrum add-hook!

\ ;;; -------- superimpose spectra of sycn'd sounds

: superimpose-ffts <{ snd chn y0 y1 -- val }>
  doc" Superimposes ffts of multiple (syncd) sounds (use with graph-hook)."
  0 sounds each ( snd ) sync max end-each { maxsync }
  0 sounds each { n } n sync snd sync = if n else maxsync 1+ then min end-each { sndsync }
  snd sync 0> snd sndsync = && if
    snd chn left-sample  { ls }
    snd chn right-sample { rs }
    rs ls f- flog 2.0 flog f/ fround->s { pow2 }
    2.0 pow2 f** fround->s { fftlen }
    pow2 2 > if
      nil { ffts }
      sounds each { n }
	n sync snd sync = n channels chn > && if
	  ls fftlen n chn #f channel->vct { fdr }
	  fftlen 0.0 make-vct { fdi }
	  fftlen 2/ 0.0 make-vct { spectr }
	  ffts '( spectr  fdr fdi #f 2 spectrum  0 vct-add! ) 2 list-append to ffts
	then
      end-each
      ffts "spectra" 0.0 0.5 undef undef snd chn undef undef graph drop
    then
  then
  #f
;
\ graph-hook ' superimpose-ffts add-hook!

\ ;;; -------- c-g? example (Anders Vinjar)

: locate-zero ( limit -- samp )
  doc" Looks for successive samples that sum to less than LIMIT, moving the cursor if successful."
  { limit }
  #f #f #f cursor { start }
  start #f #f 1 #f make-sample-reader { sf }
  sf next-sample fabs { val0 }
  sf next-sample fabs { val1 }
  begin
    sf sample-reader-at-end?
    c-g?                     ||
    val0 val1 f+ limit f<    || not
  while
      start 1+ to start
      val1 to val0
      sf next-sample fabs to val1
  repeat
  sf free-sample-reader drop
  start #f #f #f set-cursor
;

\ ;;; -------- translate mpeg input to 16-bit linear and read into Snd
\ ;;;
\ ;;; mpg123 with the -s switch sends the 16-bit (mono or stereo) representation of
\ ;;;   an mpeg file to stdout.  There's also apparently a switch to write 'wave' output.

: mpg ( mpgfile rawfile -- )
  doc" Converts file from MPEG to raw 16-bit samples using mpg123."
  { mpgfile rawfile }
  mpgfile io-open-read { io }
  io io-getc { b0 }
  io io-getc { b1 }
  io io-getc { b2 }
  io io-getc { b3 }
  io io-close
  b0 255 <>
  b1 0b11100000 and 0b11100000 <> || if
    $" %s is not an MPEG file (first 11 bytes: %b %b)" '( mpgfile b0 b1 0b11100000 and ) clm-message
  else
    b1 0b11000    and 3 rshift { id }
    b1 0b110      and 1 rshift { layer }
    b2 0b1100     and 2 rshift { srate-index }
    b3 0b11000000 and 6 rshift { channel-mode }
    id 1 = if
      $" odd: %s is using a reserved Version ID" '( mpgfile ) clm-message
    then
    layer 0= if
      $" odd: %s is using a reserved layer description" '( mpgfile ) clm-message
    then
    channel-mode 3 = if 1 else 2 then { chans }
    id 0= if 4 else id 2 = if 2 else 1 then then { mpegnum }
    layer 3 = if 1 else layer 2 = if 2 else 3 then then { mpeg-layer }
    #( 44100 48000 32000 0 ) srate-index array-ref mpegnum / { srate }
    $" %s: %s Hz, %s, MPEG-%s"
    '( mpgfile srate chans 1 = if "mono" else "stereo" then mpeg-layer ) clm-message
    $" mpg123 -s %s > %s" '( mpgfile rawfile ) string-format file-system { stat }
    stat if
      rawfile chans srate little-endian? if mus-lshort else mus-bshort then open-raw-sound drop
    else
      $" system in %s: %s" '( get-func-name stat ) clm-message
    then
  then
;
\ "mpeg.mpg" "mpeg.raw" mpg

\ ;;; -------- make dot size dependent on number of samples being displayed
\ ;;; 
\ ;;; this could be extended to set time-graph-style to graph-lines
\ ;;; if many samples are displayed, etc

: auto-dot <{ snd chn y0 y1 -- val }>
  doc" Sets the dot size depending on the number of samples being displayed (use with graph-hook)."
  snd chn right-sample snd chn left-sample - { dots }
  dots 100 > if
    1 snd chn set-dot-size drop
  else
    dots 50 > if
      2 snd chn set-dot-size drop
    else
      dots 25 > if
	3 snd chn set-dot-size drop
      else
	5 snd chn set-dot-size drop
      then
    then
  then
  #f
;
\ graph-hook ' auto-dot add-hook!

\ ;;; -------- move window left edge to mark upon 'm'
\ ;;;
\ ;;; in large sounds, it can be pain to get the left edge of the window
\ ;;; aligned with a specific spot in the sound.  In this code, we assume
\ ;;; the desired left edge has a mark, and the 'm' key (without control)
\ ;;; will move the window left edge to that mark.

: first-mark-in-window-at-left ( -- )
  doc" Moves the graph so that the leftmost visible mark is at the left edge."
  #f snd-snd { keysnd }
  #f snd-chn { keychn }
  keysnd keychn left-sample { current-left-sample }
  keysnd keychn #f marks { chan-marks }
  chan-marks length 0= if
    $" no marks" keysnd #f report-in-minibuffer drop
  else
    #f ( flag ) chan-marks map *key* undef mark-sample end-map each { samp }
      samp current-left-sample > if drop ( #f ) samp leave then
    end-each { leftmost }
    leftmost if
      leftmost keysnd keychn set-left-sample drop
      keyboard-no-action
    else
      $" no mark in window" keysnd #f report-in-minibuffer drop
    then
  then
;
\ "m" 0 lambda: <{ -- val }> first-mark-in-window-at-left ;
\ #f "align window left edge with mark" "align window left edge with mark" bind-key drop

\ ;;; -------- flash selected data red and green

defer flash-selected-data ( millisecs -- )
hide
: fsd-cb { millisecs -- prc; self -- val }
  0 proc-create millisecs , ( prc )
 does> { self -- val }
  self @ ( millisecs ) flash-selected-data
  #f
;
user data-red?
#t data-red? !
set-current
lambda: ( millisecs -- )
  doc" Causes the selected data to flash red and green."
  { millisecs }
  selected-sound sound? if
    data-red? @ if green else red then set-selected-data-color drop
    data-red? @ not data-red? !
    millisecs dup fsd-cb in drop
  then
; is flash-selected-data
previous

\ ;;; --------  use loop info (if any) to set marks at loop points

: mark-loops ( -- )
  doc" Places marks at loop points found in the selected sound's header."
  #f snd-snd { snd }
  #f snd-chn { chn }
  snd sound-loop-info
  snd file-name mus-sound-loop-info || { loops }
  loops null? if
    $" %s has no loop info" '( snd short-file-name ) clm-message
  else
    loops car 0<> loops cadr 0<> || if
      loops car  snd chn undef undef add-mark drop
      loops cadr snd chn undef undef add-mark drop
      loops caddr 0<> loops cadddr 0<> || if
	loops caddr  snd chn undef undef add-mark drop
	loops cadddr snd chn undef undef add-mark drop
      then
    then
  then
;

\ ;;; -------- mapping extensions
\ ;;; (map arbitrary single-channel function over various channel collections)

: do-all-chans <{ func :optional origin #f -- }>
  doc" Applies FUNC to all active channels, using ORIGIN as the edit history indication:\n\
lambda: <{ val -- val*2 }> val f2* ; \"double all samples\" do-all-chans"
  all-chans each { lst }
    lst car  { snd }
    lst cadr { chn }
    func 0 #f snd chn #f origin map-channel drop
  end-each
;

: update-graphs ( -- )
  doc" Updates (redraws) all graphs."
  all-chans each { lst }
    lst car  { snd }
    lst cadr { chn }
    snd chn update-time-graph drop
  end-each
;

: do-chans <{ func :optional origin #f -- }>
  doc" Applies FUNC to all sync'd channels using ORIGIN as the edit history indication."
  #f snd-snd sync { snc }
  snc 0> if
    all-chans each { lst }
      lst car  { snd }
      lst cadr { chn }
      snd sync snc = if func 0 #f snd chn #f origin map-channel drop then
    end-each
  else
    $" sync not set" snd-warning drop
  then
;

: do-sound-chans <{ func :optional origin #f -- }>
  doc" applies FUNC to all selected channels using ORIGIN as the edit history indication."
  selected-sound { snd }
  snd sound? if
    snd channels 0 ?do func 0 #f snd i ( chn ) #f origin map-channel drop loop
  else
    $" no selected sound" snd-warning drop
  then
;

hide
: everys-cb { func -- prc; y self -- f }
  1 proc-create func , ( prc )
 does> { y self -- f }
  self @ ( func ) '( y ) run-proc not
;
set-current
: every-sample? ( func -- f )
  doc" Returns #t if FUNC is not #f for all samples in the current channel, \
  otherwise it moves the cursor to the first offending sample."
  { func }
  func everys-cb 0 #f #f #f #f scan-channel { baddy }
  baddy if baddy cadr #f #f #f set-cursor drop then
  baddy not
;
previous

hide
: sorts-cb { bins -- prc; y self -- f }
  1 proc-create bins , ( prc )
 does> { y self -- f }
  self @ { bins }
  y fabs  bins length f* fround->s { bin }
  bins bin 1 object-set+!
  #f
;
set-current
: sort-samples ( nbins -- ary )
  doc" Provides a histogram in BINS bins."
  { nbins }
  nbins 0 make-array { bins }
  bins sorts-cb 0 #f #f #f #f scan-channel drop
  bins
;
previous

\ ;;; -------- mix mono sound into stereo sound panning according to env

hide
: places1-cb { rd pos -- prc; y self -- val }
  1 proc-create rd , pos , ( prc )
 does> { y self -- val }
  self @ { rd }
  self cell+ @ { pos }
  rd read-sample pos f* y f+
;
: places0-cb { rd pos -- prc; y self -- val }
  1 proc-create rd , pos ,  ( prc )
 does> { y self -- val }
  self @ { rd }
  self cell+ @ { pos }
  rd read-sample 1.0 pos f- f* y f+
;
: places3-cb { rd en -- prc; y self -- val }
  1 proc-create rd , en ,  ( prc )
 does> { y self -- val }
  self @ { rd }
  self cell+ @ { en }
  rd read-sample en env f* y f+
;
: places2-cb { rd en -- prc; y self -- val }
  1 proc-create rd , en ,  ( prc )
 does> { y self -- val }
  self @ { rd }
  self cell+ @ { en }
  rd read-sample 1.0 en env f- f* y f+
;
set-current
: place-sound ( mono stereo pan -- res )
  doc" Mixes a mono sound into a stereo sound, \
splitting it into two copies whose amplitudes depend on the envelope PAN-ENV.  \
If PAN-ENV is a number, the sound is split such that 0 is all in channel 0 \
and 90 is all in channel 1."
  { mono stereo pan }
  mono #f #f #f frames { len }
  pan number? if
    pan 90.0 f/ { pos }
    0 mono #f 1 #f make-sample-reader { reader0 }
    0 mono #f 1 #f make-sample-reader { reader1 }
    reader1 pos places1-cb 0 len stereo 1 #f #f map-channel drop
    reader0 pos places0-cb 0 len stereo 0 #f #f map-channel drop
  else
    :envelope pan :end len 1- make-env { e0 }
    :envelope pan :end len 1- make-env { e1 }
    0 mono #f 1 #f make-sample-reader { reader0 }
    0 mono #f 1 #f make-sample-reader { reader1 }
    reader1 e1 places3-cb 0 len stereo 1 #f #f map-channel drop
    reader0 e0 places2-cb 0 len stereo 0 #f #f map-channel drop
  then
;
previous

\ ;;; -------- FFT-based editing

: fft-edit <{ bottom top :optional snd #f chn #f -- vct }>
  doc" Ffts an entire sound, removes all energy below BOTTOM and all above TOP, then inverse ffts."
  snd srate { sr }
  snd chn #f frames { len }
  2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
  0 fsize snd chn #f channel->vct { rdata }
  fsize 0.0 make-vct { idata }
  bottom sr fsize f/ f/ fround->s { lo }
  top    sr fsize f/ f/ fround->s { hi }
  rdata idata 1 fft drop
  lo 0> if
    rdata 0 0.0 vct-set! drop
    idata 0 0.0 vct-set! drop
    fsize 1- { jj }
    lo 1 ?do
      rdata i  0.0 vct-set! drop
      rdata jj 0.0 vct-set! drop
      idata i  0.0 vct-set! drop
      idata jj 0.0 vct-set! drop
      jj 1- to jj
    loop
  then
  hi fsize 2/ < if
    fsize hi - { jj }
    fsize 2/ hi ?do
      rdata i  0.0 vct-set! drop
      rdata jj 0.0 vct-set! drop
      idata i  0.0 vct-set! drop
      idata jj 0.0 vct-set! drop
      jj 1- to jj
    loop
  then
  rdata idata -1 fft drop
  $" %s %s %s" '( bottom top get-func-name ) string-format { origin }
  rdata fsize 1/f vct-scale! ( rdata) 0 len 1- snd chn #f origin vct->channel
;
: fft-squelch <{ squelch :optional snd #f chn #f -- scl }>
  doc" Ffts an entire sound, sets all bins to 0.0 whose energy is below squelch, then inverse ffts."
  snd chn #f frames { len }
  2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
  0 fsize snd chn #f channel->vct { rdata }
  fsize 0.0 make-vct { idata }
  fsize 2/ { fsize2 }
  rdata idata 1 fft drop
  rdata vct-copy { vr }
  idata vct-copy { vi }
  vr vi rectangular->polar drop
  vr vct-peak { scaler }
  squelch scaler f* { scl-squelch }
  rdata 0 vct-ref dup f*  idata 0 vct-ref dup f*  f+  fsqrt   scl-squelch f< if
    rdata 0 0.0 vct-set! drop
    idata 0 0.0 vct-set! drop
  then
  fsize 1- { jj }
  fsize 1 ?do
    rdata i vct-ref dup f*  idata i vct-ref dup f*  f+  fsqrt  scl-squelch f< if
      rdata i  0.0 vct-set! drop
      rdata jj 0.0 vct-set! drop
      idata i  0.0 vct-set! drop
      idata jj 0.0 vct-set! drop
    then
    jj 1- to jj
  loop
  rdata idata -1 fft drop
  $" %s %s" '( squelch get-func-name ) string-format { origin }
  rdata fsize 1/f vct-scale! ( rdata) 0 len 1- snd chn #f origin vct->channel drop
  scaler
;
: fft-cancel <{ lo-freq hi-freq :optional snd #f chn #f -- vct }>
  doc" Ffts an entire sound, sets the bin(s) representing LO-FREQ to HI-FREQ to 0.0, \
then inverse ffts"
  snd srate { sr }
  snd chn #f frames { len }
  2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
  0 fsize snd chn #f channel->vct { rdata }
  fsize 0.0 make-vct { idata }
  rdata idata 1 fft drop
  sr fsize f/ { hz-bin }
  lo-freq hz-bin f/ fround->s { lo-bin }
  hi-freq hz-bin f/ fround->s { hi-bin }
  fsize lo-bin - { jj }
  hi-bin 1+ lo-bin ?do
    rdata i  0.0 vct-set! drop
    rdata jj 0.0 vct-set! drop
    idata i  0.0 vct-set! drop
    idata jj 0.0 vct-set! drop
    jj 1- to jj
  loop
  rdata idata -1 fft drop
  $" %s %s %s" '( lo-freq hi-freq get-func-name ) string-format { origin }
  rdata fsize 1/f vct-scale! ( rdata) 0 len 1- snd chn #f origin vct->channel
;

: make-ramp <{ :optional size 128 -- gen }>
  doc" Returns a ramp generator."
  vct( 0.0 size )
;
: ramp <{ gen up -- val }>
  doc" Is a kind of CLM generator that produces a ramp of a given length, \
then sticks at 0.0 or 1.0 until the UP argument changes."
  gen 0 vct-ref { ctr }
  gen 1 vct-ref { size }
  ctr size f/ { val }
  gen 0  ctr up if 1 else -1 then f+ 0 fmax size fmin  vct-set! drop
  val
;

hide
: squelch-vowels-cb { snd chn -- prc; y self -- val }
  32 { fft-size }
  0 snd chn 1 #f make-sample-reader { read-ahead }
  1 proc-create { prc }
  fft-size 0.0 make-vct map! read-ahead '() apply end-map ( rl )       ,
  fft-size 0.0 make-vct                                   ( im )       ,
  256 make-ramp                                           ( ramper )   ,
  snd chn #f maxamp  fft-size f2/  f/                     ( peak )     ,
  read-ahead                                                           ,
  #f                                                      ( in-vowel ) ,
  prc
 does> { y self -- val }
  self           @ { rl }
  self   cell+   @ { im }
  self 2 cells + @ { ramper }
  self 3 cells + @ { peak }
  self 4 cells + @ { read-ahead }
  self 5 cells + @ { in-vowel }
  rl read-ahead '() apply cycle-set!
  rl cycle-start@ 0= if
    rl im 1 fft drop
    rl rl vct-multiply! drop
    im im vct-multiply! drop
    rl im vct-add! drop
    rl 0 vct-ref rl 1 vct-ref f+ rl 2 vct-ref f+ rl 3 vct-ref f+  peak f> to in-vowel
    in-vowel self 5 cells + !
    im 0.0 vct-fill! drop
  then
  1.0  ramper in-vowel ramp  f- ( rval ) y f*
;
set-current
: squelch-vowels <{ :optional snd #f chn #f -- val }>
  doc" Suppresses portions of a sound that look like steady-state."
  snd chn squelch-vowels-cb 0 #f snd chn #f get-func-name map-channel
;
previous

: fft-env-data <{ fft-env :optional snd #f chn #f -- vct }>
  doc" Applies FFT-ENV as spectral env to current sound, returning vct of new data."
  snd chn #f frames { len }
  2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
  0 fsize snd chn #f channel->vct { rdata }
  fsize 0.0 make-vct { idata }
  fsize 2/ { fsize2 }
  :envelope fft-env :end fsize2 1- make-env { e }
  rdata idata 1 fft drop
  e env { val }
  rdata 0 val object-set*!
  idata 0 val object-set*!
  fsize 1- { jj }
  fsize2 1 ?do
    e env to val
    rdata i  val object-set*!
    rdata jj val object-set*!
    idata i  val object-set*!
    idata jj val object-set*!
    jj 1- to jj
  loop
  rdata idata -1 fft drop
  rdata fsize 1/f vct-scale!
;

: fft-env-edit <{ fft-env :optional snd #f chn #f -- vct }>
  doc" Edits (filters) current chan using FFT-ENV."
  $" %s %s" '( fft-env get-func-name ) string-format { origin }
  fft-env snd chn fft-env-data 0 snd chn #f frames 1- snd chn #f origin vct->channel
;

: fft-env-interp <{ env1 env2 interp :optional snd #f chn #f -- vct }>
  doc" Interpolates between two fft-filtered versions (ENV1 and ENV2 are the spectral envelopes) \
following interp (an env between 0 and 1)."
  \ FIXME: gc problems
  env1 snd chn fft-env-data gc-protect { data1 }
  env2 snd chn fft-env-data gc-protect { data2 }
  snd chn #f frames { len }
  :envelope interp :end len 1- make-env { e }
  $" %s %s %s %s" '( env1 env2 interp get-func-name ) string-format { origin }
  len 0.0 make-vct map!
    e env { pan }
    1.0 pan f-  data1 i vct-ref f*  data2 i vct-ref pan f*  f+
  end-map ( new-data ) 0 len 1- snd chn #f origin vct->channel
  data1 gc-unprotect drop
  data2 gc-unprotect drop
;

: filter-fft <{ flt :optional normalize #t snd #f chn #f -- val }>
  doc" Gets the spectrum of all the data in the given channel, \
applies the function FLT to it, then inverse ffts.  \
FLT should take one argument, the current spectrum value.\n\
lambda: <{ y -- val }> y 0.01 f< if 0.0 else y then ; filter-fft\n\
is like fft-squelch."
  snd chn #f frames { len }
  snd chn #f maxamp { mx }
  2.0  len flog 2.0 flog f/ fceil  f** fround->s { fsize }
  fsize 2/ { fsize2 }
  0 fsize snd chn #f channel->vct { rdata }
  fsize 0.0 make-vct { idata }
  rdata rectangular-window fsize #t 1.0 #f ( in-place == #f ) normalize snd-spectrum { spect }
  rdata idata 1 fft drop
  flt '( spect 0 vct-ref ) run-proc drop
  fsize 1- { jj }
  fsize2 1 ?do
    spect i vct-ref { orig }
    flt '( orig ) run-proc { cur }
    orig fabs 0.000001 f> if
      cur orig f/ { scl }
      rdata i  scl object-set*!
      idata i  scl object-set*!
      rdata jj scl object-set*!
      idata jj scl object-set*!
    else
      cur fabs 0.000001 f> if
	cur 2.0 fsqrt f/ { scl }
	rdata i  scl vct-set! drop
	idata i  scl vct-set! drop
	rdata jj scl vct-set! drop
	idata jj scl fnegate vct-set! drop
      then
    then
    jj 1- to jj
  loop
  rdata idata -1 fft drop
  $" %S %s" '( flt get-func-name ) string-format { origin }
  mx f0<> if
    rdata vct-peak { pk }
    rdata mx pk f/ vct-scale!
  else
    rdata
  then 0 len 1- snd chn #f origin vct->channel
;
\ 0.5  0.5  make-one-zero filter-fft
\ 0.05 0.05 make-one-pole filter-fft
\ lambda: <{ y -- val }> y 0.1 f< if 0.0 else y then ; filter-fft
\ 0 0 0 1 0 make-sample-reader filter-fft
\ ' contrast-enhancement filter-fft
\ lambda: <{ y -- val }> y y f* y f* ; filter-fft

: fft-smoother <{ cutoff start samps :optional snd #f chn #f -- val }>
  doc" Uses fft-filtering to smooth a section:\n\
#f #f #f cursor value curs
0.1 curs 400 #f #f fft-smoother  curs 400 #f #f #f undef vct->channel"
  2.0  samps 1+ flog 2.0 flog f/ fceil  f** fround->s { fftpts }
  start fftpts snd chn #f channel->vct { rl }
  fftpts 0.0 make-vct { im }
  fftpts cutoff f* fround->s { top }
  rl 0 vct-ref { old0 }
  rl samps 1- vct-ref { old1 }
  rl vct-peak { oldmax }
  rl im 1 fft drop
  fftpts top ?do
    rl i 0.0 vct-set! drop
    im i 0.0 vct-set! drop
  loop
  rl im -1 fft drop
  rl fftpts 1/f vct-scale! drop
  rl vct-peak { newmax }
  newmax f0<> if
    oldmax newmax f/ 1.5 f> if rl oldmax newmax f/ vct-scale! drop then
    rl 0 vct-ref { new0 }
    rl samps 1- vct-ref { new1 }
    old0 new0 f- { offset0 }
    old1 new1 f- { offset1 }
    offset1 offset0 f= if 0.0 else offset1 offset0 f- samps f/ then { incr }
    offset0 { trend }
    samps 0 ?do
      rl i trend object-set+!
      trend incr f+ to trend
    loop
  then
  rl
;

\ ;;; -------- comb-filter

: comb-filter ( scaler size -- proc; x self -- res )
  doc" Returns a comb-filter ready for map-channel etc: 0.8 32 comb-filter map-channel.  \
If you're in a hurry use: 0.8 32 make-comb clm-channel instead."
  { scaler size }
  scaler size make-comb { gen }
  1 proc-create gen ,
 does> ( x self -- res )
  { x self }
  self @ ( cmb ) x 0.0 comb
;

\ by using filters at harmonically related sizes, we can get chords:

: comb-chord ( scaler size amp -- proc; x self -- res )
  doc" Returns a set of harmonically-related comb filters: 0.95 100 0.3 comb-chord map-channel"
  { scaler size amp }
  scaler size make-comb { c1 }
  scaler size 0.75 f* f>s make-comb { c2 }
  scaler size 1.2  f* f>s make-comb { c3 }
  1 proc-create amp , c1 , c2 , c3 ,
 does> ( x self -- res )
  { x self }
  self @ { amp }
  self 1 cells + @ { c1 }
  self 2 cells + @ { c2 }
  self 3 cells + @ { c3 }
  c1 x 0.0 comb c2 x 0.0 comb c3 x 0.0 comb f+ f+ amp f*
;

\ ;;; or change the comb length via an envelope:

: zcomb ( scaler size pm -- proc; x self -- val )
  doc" Returns a comb filter whose length varies according to an envelope:\n\
0.8 32 '( 0 0 1 10 ) zcomb map-channel "
  { scaler size pm }
  :size size :max-size pm 0.0 max-envelope 1.0 f+ size f+ fround->s make-comb { cmb }
  :envelope pm :end #f #f #f frames make-env { penv }
  1 proc-create cmb , penv ,
 does> { x self -- val }
  self @ ( cmb ) x self cell+ @ ( penv ) env comb
;

: notch-filter ( scaler size -- proc; x self -- val )
  doc" Returns a notch-filter: 0.8 32 notch-filter map-channel"
  make-notch { gen }
  1 proc-create gen ,
 does> { x self -- val }
  self @ ( cmd ) x 0.0 notch
;

: formant-filter ( radius frequency -- proc; x self -- val )
  doc" Returns a formant generator: 0.99 2400 formant-filter map-channel.  \
Faster is:  0.99 2400 make-formant filter-sound"
  make-formant { gen }
  1 proc-create gen ,
 does> { x self -- val }
  self @ ( frm ) x formant
;

: formants ( r1 f1 r2 f2 r3 f3 -- proc; x self -- val )
  doc" Returns 3 formant filters in parallel: 0.99 900 0.98 1800 0.99 2700 formants map-channel"
  { r1 f1 r2 f2 r3 f3 }
  r1 f1 make-formant { fr1 }
  r2 f2 make-formant { fr2 }
  r3 f3 make-formant { fr3 }
  1 proc-create fr1 , fr2 , fr3 ,
 does> { x self -- val }
  self           @ x formant
  self 1 cells + @ x formant f+
  self 2 cells + @ x formant f+
;

: moving-formant ( radius move -- proc; x self -- val )
  doc" Returns a time-varying (in frequency) formant filter:\n\
0.99 '( 0 1200 1 2400 ) moving-formant map-channel"
  { radius move }
  radius move cadr make-formant { frm }
  :envelope move :end #f #f #f frames make-env { menv }
  1 proc-create frm , menv ,
 does> { x self -- val }
  self @ ( frm ) x formant ( ret )
  self @ ( frm ) self cell+ @ ( menv ) env set-mus-frequency drop
  ( ret )
;

: osc-formants ( radius bases amounts freqs -- proc; x self -- val )
  doc" Returns a time-varying (in frequency) formant filter:\n\
0.99 '( 0 1200 1 2400 ) moving-formant map-channel"
  { radius bases amounts freqs }
  bases vct-length { len }
  len make-array map! radius bases i vct-ref make-formant end-map { frms }
  len make-array map!        freqs i vct-ref make-oscil   end-map { oscs }
  1 proc-create frms , amounts , oscs , bases ,
 does> { x self -- val }
  self           @ { frms }
  self 1 cells + @ { amounts }
  self 2 cells + @ { oscs }
  self 3 cells + @ { bases }
  0.0 ( val )
  frms each { frm }
    frm x formant f+ ( val += ... )
    frm bases i vct-ref
    amounts i vct-ref  oscs i array-ref 0.0 0.0 oscil  f*  f+
    set-mus-frequency drop
  end-each
  ( val )
;

\ ;;; -------- hello-dentist
\ ;;;
\ ;;; CLM instrument version is in clm.html

hide
: hd-input-cb { in-data -- prc; dir self -- val }
  1 proc-create { prc }
  in-data   ,
  0 ( idx ) ,
  prc
 does> { dir self -- val }
  self       @ { in-data }
  self cell+ @ { idx }
  in-data idx object-range? if in-data idx vct-ref else 0.0 then ( val )
  idx dir + self cell+ !
  ( val )
;
set-current
: hello-dentist <{ frq amp :optional snd #f chn #f -- vct }>
  doc" Varies the sampling rate randomly, making a voice sound quavery:\n\
40.0 0.1 #f #f hello-dentist drop"
  :frequency frq :amplitude amp make-rand-interp { rn }
  snd chn #f frames { len }
  0 len snd chn #f channel->vct { in-data }
  :srate 1.0 :input in-data hd-input-cb make-src { rd }
  $" %s %s %s" '( frq amp get-func-name ) string-format { origin }
  amp f2* 1.0 f+ len f* fround->s 0.0 make-vct map!
    rd  rn 0.0 rand-interp  #f src
  end-map ( out-data ) 0 len snd chn #f origin vct->channel
;
previous

\ ;;; a very similar function uses oscil instead of rand-interp, giving
\ ;;; various "Forbidden Planet" sound effects:

hide
: fp-input-cb { sf -- prc; dir self -- val }
  1 proc-create sf ,
 does> { dir self -- val }
  self @ ( sf ) dir 0> if next-sample else previous-sample then
;
set-current
: fp <{ sr osamp osfrq :optional snd #f chn #f -- vct }>
  doc" Varies the sampling rate via an oscil: 1.0 0.3 20 #f #f fp"
  osfrq make-oscil { os }
  0 snd chn 1 #f make-sample-reader { sf }
  :srate sr :input sf fp-input-cb make-src { s }
  snd chn #f frames { len }
  $" %s %s %s %s" '( sr osamp osfrq get-func-name ) string-format { origin }
  len 0.0 make-vct map!
    s  os 0.0 0.0 oscil osamp f* #f src
  end-map ( out-data ) 0 len snd chn #f origin vct->channel
  sf free-sample-reader drop
;
previous

\ ;;; -------- compand, compand-channel

: compand <{ -- prc; y self -- val }>
  doc" Returns a compander: compand map-channel"
  1 proc-create { prc }
  vct( -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250 
     0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000 ) ,
  prc
 does> { inval self -- val }
  self @ ( tbl ) inval 8.0 f* 8.0 f+ ( index ) 17 array-interp
;

: compand-channel <{ :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Applies a standard compander to sound."
  compand beg dur snd chn edpos #t #f $" %s %s %s" '( beg dur get-func-name ) format ptree-channel
;

: compand-sound <{ :optional beg 0 dur #f snd #f -- }>
  doc" Applies companding to every channel of SND."
  snd snd-snd to snd
  snd sound? if
    snd channels 0 ?do
      beg dur snd i ( chn ) #f compand-channel drop
    loop
  else
    'no-such-sound '( get-func-name snd ) fth-throw
  then
;

\ ;;; the next (expsnd) changes the tempo according to an envelope; the new duration
\ ;;; will depend on the expansion envelope -- we integrate it to get
\ ;;; the overall expansion, then use that to decide the new length.

hide
: es-input-cb { sf -- prc; dir self -- val }
  1 proc-create sf ,
 does> { dir self -- val }
  self @ ( sf ) next-sample
;
set-current
: expsnd <{ gr-env :optional snd #f chn #f -- vct }>
  doc" Uses the granulate generator to change tempo according to an envelope:\n\
'( 0 0.5 2 2.0 ) #f #f expsnd"
  snd chn #f frames { len }
  len snd srate f/ gr-env integrate-envelope f* gr-env envelope-last-x f/ { dur }
  0 snd chn 1 #f make-sample-reader { sf }
  :expansion gr-env cadr :jitter 0 :input sf es-input-cb make-granulate { gr }
  :envelope gr-env :duration dur make-env { ge }
  snd srate dur f* fround->s { sound-len }
  sound-len len max to len
  $" %s %s" '( gr-env get-func-name ) string-format { origin }
  len 0.0 make-vct map!
    gr #f granulate ( val )
    gr ge env set-mus-increment drop
  end-map ( out-data ) 0 len snd chn #f origin vct->channel
  sf free-sample-reader drop
;
previous

: voiced->unvoiced <{ amp fftsize r tempo :optional snd #f chn #f -- vct }>
  doc" Turns a vocal sound into whispering: 1.0 256 2.0 2.0 #f #f voiced->unvoiced"
  fftsize 2/ { freq-inc }
  fftsize 0.0 make-vct { fdr }
  fftsize 0.0 make-vct { fdi }
  freq-inc 0.0 make-vct { spectr }
  snd srate 3.0 f/ make-rand { noi }
  0 { inctr }
  1.0 r fftsize f/ f- { radius }
  snd srate fftsize / { bin }
  snd chn #f frames { len }
  len tempo f/ fround->s len max { out-len }
  freq-inc tempo f* fround->s { hop }
  0.0 0.0 { old-peak-amp new-peak-amp }
  $" %s %s %s %s %s" '( amp fftsize r tempo get-func-name ) string-format { origin }
  freq-inc nil make-array map! radius i bin * make-formant end-map { formants }
  out-len 0.0 make-vct map!
    i freq-inc mod 0= if
      c-g? if "interrupted" leave then	\ ;; if C-g exit the loop returning the string "interrupted"
      inctr fftsize snd chn #f channel->vct to fdr
      fdr vct-peak old-peak-amp fmax to old-peak-amp
      fdr fdi #f 2 spectrum ( fdr ) spectr vct-subtract! ( fdr ) freq-inc 1/f vct-scale! drop
      hop +to inctr
    then
    spectr fdr vct-add! ( spectr ) formants noi 0.0 rand formant-bank ( outval )
    dup fabs new-peak-amp fmax to new-peak-amp
    ( outval )
  end-map old-peak-amp new-peak-amp f/ amp f* vct-scale! 0 out-len snd chn #f origin vct->channel
;

\ ;;; -------- sound interp
\ ;;;
\ ;;; make-sound-interp sets up a sound reader that reads a channel at an arbitary location,
\ ;;;   interpolating between samples if necessary, the corresponding "generator" is sound-interp

: make-sound-interp ( start :optional snd chn -- prc; loc self -- val )
  doc" Return an interpolating reader for SND's channel CHN."
  '( #f #f ) 1 get-optargs { start snd chn }
  2048 { bufsize }
  1 proc-create { prc }
  start bufsize snd chn #f channel->vct ( data )   ,
  start                                 ( curbeg ) ,
  start bufsize +                       ( curend ) ,
  snd                                   ( snd )    ,
  chn                                   ( chn )    ,
  prc
 does> { loc self -- val }
  self           @ { data }
  self   cell+   @ { curbeg }
  self 2 cells + @ { curend }
  self 3 cells + @ { snd }
  self 4 cells + @ { chn }
  2048 { bufsize }
  128  { buf4size }
  loc fround->s to loc
  loc curbeg < if
    \ get previous buffer
    loc bufsize - buf4size + 0 max to curbeg
    curbeg bufsize + to curend
    curbeg bufsize snd chn #f channel->vct to data
  else
    loc curend > if
      \ get next buffer
      loc buf4size - 0 max to curbeg
      curbeg bufsize + to curend
      curbeg bufsize snd chn #f channel->vct to data
    then
  then
  data   self           !
  curbeg self   cell+   !
  curend self 2 cells + !
  data loc curbeg - bufsize array-interp
;

: sound-interp ( func loc -- val )
  doc" Return sample at LOC (interpolated if necessary) from FUNC created by make-sound-interp."
  ( func loc ) run-proc
;

\ ;; env-sound-interp takes an envelope that goes between 0 and 1 (y-axis), and a time-scaler
\ ;;   (1.0 = original length) and returns a new version of the data in the specified channel
\ ;;   that follows that envelope (that is, when the envelope is 0 we get sample 0, when the
\ ;;   envelope is 1 we get the last sample, envelope = .5 we get the middle sample of the 
\ ;;   sound and so on. (env-sound-interp '(0 0 1 1)) will return a copy of the
\ ;;   current sound; (env-sound-interp '(0 0 1 1 2 0) 2.0) will return a new sound 
\ ;;   with the sound copied first in normal order, then reversed.  src-sound with an
\ ;;   envelope could be used for this effect, but it is much more direct to apply the
\ ;;   envelope to sound sample positions.

: env-sound-interp <{ envelope :optional time-scale 1.0 snd #f chn #f -- file-name }>
  doc" Reads SND's channel CHN according to ENVELOPE and TIME-SCALE."
  snd chn #f frames { len }
  time-scale len f* fround->s { newlen }
  0 snd chn make-sound-interp { reader }
  :envelope envelope :end newlen :scaler len make-env { read-env }
  snd-tempnam { tempfilename }
  tempfilename snd srate 1 #f mus-next $" env-sound-interp temp file" mus-sound-open-output { fil }
  8192 { bufsize }
  1 bufsize make-sound-data { data }
  newlen 0 ?do
    bufsize 0 do data 0 i  reader read-env env sound-interp  sound-data-set! drop loop
    fil 0 bufsize 1- 1 data mus-sound-write drop
  bufsize +loop
  newlen bufsize mod ?dup-if fil 0 rot 1- 1 data mus-sound-write drop then
  fil newlen 4 * mus-sound-close-output drop
  $" %s %s %s" '( envelope time-scale get-func-name ) string-format { origin }
  0 newlen tempfilename snd chn #t ( truncate ) origin set-samples
  tempfilename file-delete
;

\ ;;; -------- filtered-env 

hide
: fe-cb { flt amp-env -- prc; y self -- val }
  1 proc-create flt , amp-env ,
 does> { y self -- val }
  self @ { flt }
  self cell+ @ ( amp-env ) env { env-val }
  flt 0 env-val        set-mus-xcoeff drop
  flt 1 env-val 1.0 f- set-mus-xcoeff drop
  flt env-val y f* one-pole
;
set-current
: filtered-env <{ e :optional snd #f chn #f -- val }>
  doc" It's a time-varying one-pole filter: \
when env is at 1.0, no filtering, as env moves to 0.0, low-pass gets more intense; \
amplitude and low-pass amount move together."
  1.0 0.0 make-one-pole { flt }
  :envelope e :end snd chn #f frames 1- make-env { amp-env }
  flt amp-env fe-cb  0 #f snd chn #f $" %s %s" '( e get-func-name ) string-format  map-channel
;
previous

: zero+ ( -- proc; n self -- val )
  doc" Finds the next positive-going zero crossing (if searching forward) (for use with C-s)"
  1 proc-create 0.0 ( lastn ) ,
 does> { n self -- val }
  self @ ( lastn ) f0<  n f0>= && -1 && ( rtn )
  n self ! ( lastn = n )
  ( rtn )
;

: next-peak ( -- proc; n self -- val )
  doc" Finds the next max or min point in the time-domain waveform (for use with C-s)"
  1 proc-create ( last0 ) #f , ( last1 ) #f ,
 does> { n self -- val }
  self       @ { last0 }
  self cell+ @ { last1 }
  last0 number?
  last0 last1 f< last1 n f> &&
  last0 last1 f> last1 n f< && || &&
  -1 && ( rtn )
  last1 self !   ( last0 = last1 )
  n self cell+ ! ( last1 = n )
  ( rtn )
;

\ ;;; -------- file->vct and a sort of cue-list, I think

[undefined] file->vct [if]
  : file->vct ( file -- vct )
    doc" Returns a vct with FILE's data."
    { file }
    file find-file to file
    file false? if 'no-such-file '( get-func-name file ) fth-throw then
    0 file undef 1 #f make-sample-reader { reader }
    file mus-sound-frames 0.0 make-vct map! reader next-sample end-map ( data )
    reader free-sample-reader drop
  ;
[then]

hide
: an-cb ( notes snd chn -- prc; self -- #f )
  { notes snd chn }
  0 proc-create { prc }
  notes ,
  snd   ,
  chn   ,
  prc
 does> { self -- #f }
  self           @ { notes }
  self   cell+   @ { snd }
  self 2 cells + @ { chn }
  snd chn #f cursor { start }
  notes each { note }
    note car find-file { file }
    file false? if 'no-such-file '( "add-notes" file ) fth-throw then
    note cadr  ?dup-if else 0.0 then { offset }
    note caddr ?dup-if else 1.0 then { amp }
    snd srate offset f* fround->s start + { beg }
    amp 1.0 f<> if
      file file->vct amp vct-scale! beg snd chn #f "add-notes" 0 mix-vct drop
    else
      file beg 0 snd chn #f undef 0 mix drop
    then
  end-each
  #f
;
set-current
: add-notes <{ notes :optional snd #f chn #f -- #f }>
  doc" Adds (mixes) NOTES which is a list of lists of the form:\n\
file :optional offset 0.0 amp 1.0\n\
starting at the cursor in the currently selected channel:\n\
'( '( \"oboe.snd\" ) '( \"pistol.snd\" 1.0 2.0 ) ) add-notes"
  notes snd chn an-cb  $" %s %s" '( notes get-func-name ) string-format  as-one-edit
;
previous

\ ;;; -------- smooth-channel as virtual op

hide
: scvp3-cb <{ y data forward -- val }>
  data 0 vct-ref { angle }
  data 1 vct-ref { incr }
  data 3 vct-ref  data 4 vct-ref  data 2 vct-ref angle f+ fcos  f*  f+ ( val )
  data 0 angle incr forward if f+ else f- then vct-set! drop ( val )
;
: scvp1-cb { data -- prc1; frag-beg frag-dur self -- vct }
  2 proc-create { prc } data , prc
 does> { frag-beg frag-dur self -- vct }
  self @ { data }
  pi frag-dur f/ { incr }
  data 1          incr    vct-set! drop
  data 0 frag-beg incr f* vct-set! drop
  data
;
set-current
: smooth-channel-via-ptree <{ :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  beg snd chn edpos sample { y0 }
  beg dur snd chn #f frames 1- || snd chn edpos sample { y1 }
  y1 y0 f> if pi else 0.0 then { init-angle }
  y0 y1 f+ f2/ { off }
  y1 y0 f- fabs f2/ { scale }
  vct( 0.0 0.0 init-angle off scale ) { data }
  $" %s %s %s" '( beg dur get-func-name ) string-format { origin }
  ['] scvp3-cb beg dur snd chn edpos #t data scvp1-cb origin ptree-channel
;
previous

\ ;;; -------- ring-modulate-channel (ring-mod as virtual op)

hide
: rmc-cb3 <{ y data forward -- val }>
  data 0 vct-ref { angle }
  data 1 vct-ref { incr }
  angle fsin y f* ( val )
  data 0 angle incr forward if + else - then vct-set! drop ( val )
;
: rmc-cb2 { freq snd -- prc; frag-beg frag-dur self -- vct }
  2 proc-create { prc } freq , snd , prc
 does> { frag-beg frag-dur self -- vct }
  two-pi self @ ( freq ) f* self cell+ @ ( snd ) srate f/ { incr }
  vct( frag-beg incr f* two-pi fmod incr )
;
set-current
: ring-modulate-channel <{ freq :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
  $" %s %s %s %s" '( freq beg dur get-func-name ) string-format { origin }
  ['] rmc-cb3  beg dur snd chn edpos #f  freq snd rmc-cb2  origin ptree-channel
;
previous

\ ;; -------- reorder blocks within channel

hide
: rbb-cb { rd beg ctr actual-block-len len snd chn -- prc; y self -- val }
  1 proc-create rd , beg , ctr , actual-block-len , len , snd , chn , ( prc )
 does> { y self -- val }
  self @ { rd }
  self cell+ @ { beg }
  self 2 cells + @ { ctr }
  self 3 cells + @ { actual-block-len }
  self 4 cells + @ { len }
  self 5 cells + @ { snd }
  self 6 cells + @ { chn }
  rd read-sample { val }
  beg 10 < if
    val beg f* 0.1 f* to val
  else
    beg actual-block-len 10 - > if
      val actual-block-len beg - f* 0.1 f* to val
    then
  then
  1 +to beg
  beg actual-block-len = if
    1 self 2 cells + +! ( ctr++ )
    0 self 1 cells + !  ( beg = 0 )
    len  self 2 cells + @ ( ctr ) actual-block-len *  - 0 max snd chn 1 #f make-sample-reader self !
  then
  val
;
set-current
: reverse-by-blocks <{ block-len :optional snd #f chn #f -- val }>
  doc" Divide sound into block-len blocks, recombine blocks in reverse order."
  snd chn #f frames { len }
  len snd srate block-len f* f/ fround->s { num-blocks }
  num-blocks 1 > if
    len num-blocks f/ fceil f>s { actual-block-len }
    len actual-block-len - snd chn 1 #f make-sample-reader { rd }
    0 { beg }
    1 { ctr }
    $" %s %s" '( block-len get-func-name ) string-format { origin }
    rd beg ctr actual-block-len len snd chn rbb-cb  0 #f snd chn #f origin map-channel
  else
    #f
  then
;
previous

hide
: rwb-cb { len actual-block-len no-clicks-env snd chn -- prc; self -- val }
  0 proc-create len , actual-block-len , no-clicks-env , snd , chn , ( prc )
 does> { self -- val }
  self           @ { len }
  self   cell+   @ { actual-block-len }
  self 2 cells + @ { no-clicks-env }
  self 3 cells + @ { snd }
  self 4 cells + @ { chn }
  len 0 ?do
    i actual-block-len snd chn #f reverse-channel drop
    no-clicks-env i actual-block-len snd chn #f env-channel drop
  actual-block-len +loop
  #t
;
set-current
: reverse-within-blocks <{ block-len :optional snd #f chn #f -- val }>
  doc" Divide sound into blocks, recombine in order, but each block internally reversed."
  snd chn #f frames { len }
  len snd srate block-len f* f/ fround->s { num-blocks }
  num-blocks 1 > if
    len num-blocks f/ fceil f>s { actual-block-len }
    '( 0.0 0.0  0.01 1.0  0.99 1.0  1.0 0.0 ) { no-clicks-env }
    $" %s %s" '( block-len get-func-name ) string-format { origin }
    len actual-block-len no-clicks-env snd chn rwb-cb  origin as-one-edit
  else
    0 #f snd chn #f reverse-channel
  then
;
previous

\ === Moog Filter ===

hide
vct( 0.999969 0.990082 0.980347 0.970764 0.961304 0.951996 0.94281 0.933777 0.924866 0.916077 
   0.90741 0.898865 0.890442 0.882141  0.873962 0.865906 0.857941 0.850067 0.842346 0.834686
   0.827148 0.819733 0.812378 0.805145 0.798004 0.790955 0.783997 0.77713 0.770355 0.763672
   0.75708  0.75058 0.744141 0.737793 0.731537 0.725342 0.719238 0.713196 0.707245 0.701355
   0.695557 0.689819 0.684174 0.678558 0.673035 0.667572 0.66217 0.65686 0.651581 0.646393
   0.641235 0.636169 0.631134 0.62619 0.621277 0.616425 0.611633 0.606903 0.602234 0.597626
   0.593048 0.588531 0.584045 0.579651 0.575287  0.570953 0.566681 0.562469 0.558289 0.554169
   0.550079 0.546051 0.542053 0.538116 0.53421 0.530334 0.52652 0.522736 0.518982 0.515289
   0.511627 0.507996  0.504425 0.500885 0.497375 0.493896 0.490448 0.487061 0.483704 0.480377
   0.477081 0.473816 0.470581 0.467377 0.464203 0.46109 0.457977 0.454926 0.451874 0.448883
   0.445892 0.442932 0.440033 0.437134 0.434265 0.431427 0.428619 0.425842 0.423096 0.42038
   0.417664 0.415009 0.412354 0.409729 0.407135 0.404572 0.402008 0.399506 0.397003 0.394501
   0.392059 0.389618 0.387207 0.384827 0.382477 0.380127 0.377808 0.375488 0.37323 0.370972
   0.368713 0.366516 0.364319 0.362122 0.359985 0.357849 0.355713 0.353607 0.351532 0.349457
   0.347412 0.345398 0.343384 0.34137 0.339417 0.337463 0.33551 0.333588 0.331665 0.329773
   0.327911 0.32605 0.324188 0.322357 0.320557 0.318756 0.316986 0.315216 0.313446 0.311707
   0.309998 0.308289 0.30658 0.304901 0.303223 0.301575 0.299927 0.298309 0.296692 0.295074
   0.293488 0.291931 0.290375 0.288818 0.287262 0.285736 0.284241 0.282715 0.28125 0.279755
   0.27829 0.276825 0.275391 0.273956 0.272552 0.271118 0.269745 0.268341 0.266968 0.265594
   0.264252 0.262909 0.261566 0.260223 0.258911 0.257599 0.256317
   0.255035 0.25375 ) constant moog-gaintable

#( 0.0        -1.0
   0.03311111 -0.9
   0.06457143 -0.8
   0.0960272  -0.7
   0.127483   -0.6
   0.1605941  -0.5
   0.1920544  -0.4
   0.22682086 -0.3
   0.2615873  -0.2
   0.29801363 -0.1
   0.33278003 -0.0
   0.37086168  0.1
   0.40893877  0.2
   0.4536417   0.3
   0.5         0.4
   0.5463583   0.5
   0.5943719   0.6
   0.6556281   0.7
   0.72185487  0.8
   0.8096009   0.9
   0.87913835  0.95
   0.9933787   1.0
   1.0         1.0 ) constant moog-freqtable

struct
  cell% field moog-freq
  cell% field moog-Q
  cell% field moog-s
  cell% field moog-y
  cell% field moog-fc
end-struct moog-filter%
set-current

: moog-frequecy@ ( gen -- frq ) moog-freq @ ;
: moog-frequecy! ( frq gen -- )
  { frq gen }
  frq gen moog-freq !
  frq mus-srate f2/ f/ moog-freqtable 1.0 envelope-interp gen moog-fc !
;

: make-moog-filter ( freq Q -- gen )
  doc" Makes a new moog-filter generator.  \
FREQ is the cutoff in Hz, Q sets the resonance: 0 = no resonance, 1: oscillates at FREQUENCY."
  { freq Q }
  moog-filter% %alloc { gen }
  freq           gen moog-freq !
  Q              gen moog-Q !
  4 0.0 make-vct gen moog-s !
  0.0            gen moog-y !
  freq mus-srate f2/ f/ moog-freqtable 1.0 envelope-interp gen moog-fc !
  gen
;

: moog-filter ( gen sig -- A )
  { gen sig }
  0.25 sig gen moog-y @ f- f* { A }
  gen moog-s @ each { st }
    gen moog-fc @ A st f- f* A f+ -0.95 fmax 0.95 fmin to A
    gen moog-s  @ i A vct-set! drop
    A st f+ -0.95 fmax 0.95 fmin to A
  end-each
  gen moog-fc @ 99.0 f* { ix }
  ix fround->s { ixint }
  ix ixint f- { ixfrac }
  A gen moog-Q @ f*
  1.0 ixfrac f- moog-gaintable ixint  99 + vct-ref f*
      ixfrac    moog-gaintable ixint 100 + vct-ref f* f+ f* gen moog-y !
  A
;
previous

\ 500.0 0.1 make-moog-filter value gen
\ lambda: <{ y }> gen swap moog-filter ; map-channel
\ gen 1.0 moog-filter

\ examp.fs ends here
