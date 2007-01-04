\ -*- snd-forth -*-
\ examp.fs -- examples from examp.rb

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Tue Jul 05 13:09:37 CEST 2005
\ Changed: Wed Jan 03 00:09:05 CET 2007

\ Commentary:

\ all-chans           ( -- array-of-lists )
\ close-sound-extend  ( snd -- )
\ snd-snd             ( snd|#f -- snd )
\ snd-chn             ( chn|#f -- chn )
\
\ examp.(scm|rb)
\ 
\ selection-rms       ( -- val )
\ region-rms          ( :optional n -- val )
\ window-samples      ( :optional snd chn -- vct )
\ display-energy      ( -- proc; snd chn self -- val )
\ display-db          ( -- proc; snd chn self -- val )
\ fft-peak            ( snd chn -- pk )
\ 
\ fft-edit            ( bottom top :optional snd chn -- vct )
\ fft-squelch         ( squelch :optional snd chn -- scl )
\ fft-cancel          ( lo-freq hi-freq :optional snd chn -- vct )
\ make-ramp           ( :optional size -- gen )
\ ramp                ( gen up -- val )
\ squelch-vowels      ( :optional snd chn -- val )
\
\ comb-filter         ( scaler size -- proc; x self -- res )
\ comb-chord          ( scaler size amp -- proc; x self -- res )
\ zcomb               ( scaler size pm -- proc; x self -- val )
\ notch-filter        ( scaler size -- proc; x self -- val )
\ formant-filter      ( radius frequency -- proc; x self -- val )
\ formants            ( r1 f1 r2 f2 r3 f3 -- proc; x self -- val )
\ moving-formant      ( radius move -- proc; x self -- val )
\ osc-formants         ( radius bases amounts freqs -- proc; x self -- val )
\
\ zero+               ( -- proc; n self -- val )
\ next-peak           ( -- proc; n self -- val )
\
\ make-moog-filter    ( freq Q -- gen )
\ moog-frequecy@      ( gen -- frq )
\ moog-frequecy!      ( frq gen -- )
\ moog-filter         ( gen sig -- A )

\ Code:

require clm
require env

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

: snd-snd ( snd|#f -- snd )
  { snd }
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

: snd-chn ( chn|#f -- chn )
  { chn }
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
  0                                     ( curbeg ) ,
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
    bufsize +to curend
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
\ FIXME: buggy
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
  loop
  newlen bufsize mod ?dup-if fil 0 rot 1- 1 data mus-sound-write drop then
  fil newlen 4 * mus-sound-close-output drop
  $" %s %s %s" '( envelope time-scale get-func-name ) string-format { origin }
  0 newlen tempfilename snd chn #t ( truncate ) origin set-samples
  tempfilename file-delete
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
    note car { file }
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
\ FIXME: origin is buggy
: add-notes <{ notes :optional snd #f chn #f -- #f }>
  doc" Adds (mixes) NOTES which is a list of lists of the form:\n\
file :optional offset 0.0 amp 1.0\n\
starting at the cursor in the currently selected channel:\n\
'( '( \"oboe.snd\" ) '( \"pistol.snd\" 1.0 2.0 ) ) add-notes"
  notes snd chn an-cb { prc }
  $" %m %s" '( notes get-func-name ) string-format { origin }
  prc origin as-one-edit
;
previous

\ --- comb-filter ---

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
