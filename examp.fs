\ -*- snd-forth -*-
\ examp.fs -- examples from examp.rb

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Tue Jul 05 13:09:37 CEST 2005
\ Changed: Thu Dec 21 18:22:48 CET 2006

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
\ comb-filter         ( scaler size -- proc; x self -- res )
\ comb-chord          ( scaler size amp -- proc; x self -- res )
\ zcomb               ( scaler size pm -- proc; x self -- val )
\ notch-filter        ( scaler size -- proc; x self -- val )
\ formant-filter      ( radius frequency -- proc; x self -- val )
\ formants            ( r1 f1 r2 f2 r3 f3 -- proc; x self -- val )
\ moving-formant      ( radius move -- proc; x self -- val )
\ osc-formant         ( radius bases amounts freqs -- proc; x self -- val )
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
  : close-sound-extend ( snd -- )
    { snd }
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

\ --- comb-filter ---

: comb-filter ( scaler size -- proc; x self -- res )
  doc" Returns a comb-filter ready for map-channel etc: 0.8 32 comb-filter map-channel.  \
If you're in a hurry use: 0.8 32 make-comb clm-channel instead."
  { scaler size }
  lambda-create scaler size make-comb , latestxt 1 make-proc
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
  lambda-create amp , c1 , c2 , c3 , latestxt 1 make-proc
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
  :size size :max-size pm 0.0 max-envelope 1.0 f+ size f+ floor f>s make-comb { cmb }
  :envelope pm :end #f #f #f frames make-env { penv }
  lambda-create cmb , penv , latestxt 1 make-proc
 does> { x self -- val }
  self @ ( cmb ) x self cell+ @ ( penv ) env comb
;

: notch-filter ( scaler size -- proc; x self -- val )
  doc" Returns a notch-filter: 0.8 32 notch-filter map-channel"
  make-notch lambda-create , latestxt 1 make-proc
 does> { x self -- val }
  self @ ( cmd ) x 0.0 notch
;

: formant-filter ( radius frequency -- proc; x self -- val )
  doc" Returns a formant generator: 0.99 2400 formant-filter map-channel.  \
Faster is:  0.99 2400 make-formant filter-sound"
  make-formant lambda-create , latestxt 1 make-proc
 does> { x self -- val }
  self @ ( frm ) x formant
;

: formants ( r1 f1 r2 f2 r3 f3 -- proc; x self -- val )
  doc" Returns 3 formant filters in parallel: 0.99 900 0.98 1800 0.99 2700 formants map-channel"
  { r1 f1 r2 f2 r3 f3 }
  r1 f1 make-formant { fr1 }
  r2 f2 make-formant { fr2 }
  r3 f3 make-formant { fr3 }
  lambda-create fr1 , fr2 , fr3 , latestxt 1 make-proc
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
  lambda-create frm , menv , latestxt 1 make-proc
 does> { x self -- val }
  self @ ( frm ) x formant ( ret )
  self @ ( frm ) self cell+ @ ( menv ) env set-mus-frequency drop
  ( ret )
;

: osc-formant ( radius bases amounts freqs -- proc; x self -- val )
  doc" Returns a time-varying (in frequency) formant filter:\n\
0.99 '( 0 1200 1 2400 ) moving-formant map-channel"
  { radius bases amounts freqs }
  bases vct-length { len }
  len make-array map! radius bases i vct-ref make-formant end-map { frms }
  len make-array map!        freqs i vct-ref make-oscil   end-map { oscs }
  lambda-create frms , amounts , oscs , bases , latestxt 1 make-proc
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
  lambda-create 0.0 ( lastn ) , latestxt 1 make-proc
 does> { n self -- val }
  self @ ( lastn ) f0<  n f0>= && -1 && ( rtn )
  n self ! ( lastn = n )
  ( rtn )
;

: next-peak ( -- proc; n self -- val )
  doc" Finds the next max or min point in the time-domain waveform (for use with C-s)"
  lambda-create ( last0 ) #f , ( last1 ) #f , latestxt 1 make-proc
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
  ix floor f>s { ixint }
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
