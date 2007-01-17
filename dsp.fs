\ dsp.fs -- dsp.scm|rb --> dsp.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Dec 30 04:52:13 CET 2005
\ Changed: Wed Jan 17 00:48:48 CET 2007

\ src-duration             ( en -- dur )
\ dolph                    ( n gamma -- im )
\ dolph-1                  ( n gamma -- im )
\ down-oct                 ( n :optional snd chn -- vct )
\ stretch-sound-via-dft    ( factor :optional snd chn -- )
\ compute-uniform-circular-string ( size x0 x1 x2 mass xspring damp -- )
\ compute-string           ( size x0 x1 x2 masses xsprings esprings damps haptics -- )
\ freqdiv                  ( n :optional snd chn -- )
\ adsat                    ( size :optional beg dur snd chn -- res )
\ spike                    ( :optional snd chn -- res )
\ spot-freq                ( samp :optional snd chn -- )
\ chorus                   ( -- proc; inval self -- val )
\ chordalize               ( -- proc; x self -- val )
\ zero-phase               ( :optional snd chn -- vct )
\ rotate-phase             ( func :optional snd chn -- vct)
\ make-asyfm               ( :key frequency initial-phase ratio r index -- gen )
\ asyfm-J                  ( gen input -- val )
\ asyfm-I                  ( gen input -- val )
\ make-cosine-summation    ( :key frequency initial-phase -- gen )
\ cosine-summation         ( gen r -- val )
\ make-kosine-summation    ( :key frequency initial-phase -- gen )
\ kosine-summation         ( gen r k -- val )
\ fejer-sum                ( angle n -- val )
\ legendre-sum             ( angle n -- val )
\ sum-of-n-sines           ( angle n -- val )
\ sum-of-n-odd-sines       ( angle n -- val )
\ sum-of-n-odd-cosines     ( angle n -- val )
\ band-limited-sawtooth    ( x a N fi -- val )
\ band-limited-square-wave ( theta n -- val )
\ brighten-slightly        ( amount :optional snd chn -- )
\ brighten-slightly-1      ( coeffs :optional snd chn -- )
\ spectrum->coeffs         ( order spectr -- vct )
\ fltit-1                  ( order spectr -- proc;  y self -- val )
\ 
\ make-hilbert-transform   ( :optional len -- gen )
\ make-highpass            ( fc :optional len -- gen )
\ make-lowpass             ( fc :optional len -- gen )
\ make-bandpass            ( flo fhi :optional len -- gen )
\ make-bandstop            ( flo fhi :optional len -- gen )
\ make-differentiator      ( :optional len -- gen )
\ make-butter-high-pass    ( freq -- flt )
\ make-butter-low-pass     ( freq -- flt )
\ make-butter-band-pass    ( freq band -- flt )
\ make-butter-band-reject  ( freq band -- flt )
\ make-biquad              ( a0 a1 a2 b1 b2 -- gen )
\ make-iir-low-pass-2      ( fc :optional d -- gen )
\ make-iir-high-pass-2     ( fc :optional d -- gen )
\ make-iir-band-pass-2     ( f1 f2 -- gen )
\ make-iir-band-stop-2     ( f1 f2 -- gen )
\ make-eliminate-hum       ( :optional hum-freq hum-harmonics bandwith  -- )
\
\ make-peaking-2           ( f1 f2 m -- prc; y self -- val )
\ cascade->canonical       ( A -- A' )
\ make-butter-lp 	   ( M fc -- flt )
\ make-butter-hp 	   ( M fc -- flt )
\ make-butter-bp 	   ( M f1 f2 -- flt )
\ make-butter-bs 	   ( M f1 f2 -- flt )
\
\ make-notch-frequency-response ( cur-srate freqs :optional notch-width -- fresp )
\ notch-channel            ( freqs :optional ... )
\ notch-sound              ( freqs :optional filter-order snd chn notch-width -- f )
\ notch-selection          ( freqs :optional filter-order notch-width -- f )
\ fractional-fourier-transform ( real imaginary n angle -- hr hi )
\ z-transform              ( data n z -- )
\ dht                      ( data -- vct )
\ find-sine                ( freq beg dur -- amp ph )
\ goertzel                 ( freq :optional beg dur -- amp )
\ 
\ make-spencer-filter      ( -- gen )
\ any-random               ( amount :optional en -- r )
\ gaussian-distribution    ( s -- en )
\ pareto-distribution      ( a -- en )
\ inverse-integrate        ( dist :optional data-size e-size -- vct )
\ gaussian-envelope        ( s -- en )
\ 
\ channel-mean             ( :optional snd chn -- val )
\ channel-total-energy     ( :optional snd chn -- val )
\ channel-average-power    ( :optional snd chn -- val )
\ channel-rms              ( :optional snd chn -- val )
\ channel-variance         ( :optional snd chn -- val )
\ channel-norm             ( :optional snd chn -- val )
\ channel-lp               ( p :optional snd chn -- val )
\ channel-lp-inf           ( :optional snd chn -- val )
\ channel2-inner-product   ( s1 c1 s2 c2 -- val )
\ channel2-angle           ( s1 c1 s2 c2 -- val )
\ channel2-orthogonal?     ( s1 c1 s2 c2 -- f )
\ channel2-coefficient-of-projection ( s1 c1 s2 c2 -- val )
\ channel-distance         ( s1 c1 s2 c2 -- val )
\ periodogram              ( N -- )
\
\ shift-channel-pitch      ( freq :optional order beg dur snd chn edpos -- val )
\ ssb-bank                 ( old-freq new-freq pairs :optional ... )
\ ssb-bank-env             ( old-freq new-freq freq-env pairs :optional ... )
\ make-transposer          ( old-freq new-freq pairs :optional order bw -- gen )
\ transpose                ( gen input -- val )
\ make-fdelay              ( len pitch scaler -- prc; y self -- val )
\ fdelay                   ( gen input -- val )
\ transposed-echo          ( pitch scaler secs -- val )
\ 
\ vct-polynomial           ( v coeffs -- vct )
\ channel-polynomial       ( coeffs :optional snd chn -- vct )
\ spectral-polynomial      ( coeffs :optional snd chn -- vct )
\
\ scentroid                ( file :key beg dur db-floor rfreq fftsize -- vals )
\ invert-filter            ( fcoeffs -- res )
\ make-volterra-filter     ( acoeffs bcoeffs -- gen )
\ volterra-filter          ( flt x -- val )
\
\ make-moving-max          ( :optional size -- gen )
\ moving-max               ( gen y -- scl )
\ make-moving-sum          ( :optional size -- gen )
\ moving-sum               ( gen y -- val )
\ make-moving-rms          ( :optional size -- gen )
\ moving-rms               ( gen y -- val )
\ make-moving-length       ( :optional size -- gen )
\ moving-length            ( gen y -- val )
\ harmonicizer             ( freq coeffs pairs :optional ... )
\ linear-src-channel       ( sr :optional snd chn -- file )
\ make-mfilter             ( :key decay frequency -- gen )
\ mfilter                  ( m :optional x-input y-input -- val )
\ display-bark-fft         ( :optional off -- )
\ undisplay-bark-fft       ( -- )
\ lpc-coeffs               ( data n m -- val )
\ lpc-predict              ( data n coeffs m nf :optional clipped -- val )
\ unclip-channel           ( :optional snd chn -- hash )
\ unclip-sound             ( :optional snd -- ary-of-hashs )

require clm
require env
require examp

\ ;;; -------- src-duration (see src-channel in extsnd.html)

: src-duration ( en -- dur )
  doc" Returns the new duration of a sound after using ENVELOPE \
for time-varying sampling-rate conversion."
  { en }
  en  0 object-ref { ex0 }
  en -2 object-ref { ex1 }
  ex1 ex0 f- { all-x }
  0.0 ( dur )
  en object-length 3 - 0 ?do
    en i     object-ref { x0 }
    en i 1 + object-ref { xy0 }
    en i 2 + object-ref { x1 }
    en i 3 + object-ref { xy1 }
    xy0 f0= if 1.0 else xy0 1/f then { y0 }
    xy1 f0= if 1.0 else xy1 1/f then { y1 }
    xy0 xy1 f- fabs 0.0001 f< if
      x1 x0 f- all-x f/ y0 f*
    else
      y1 flog y0 flog f-  xy0 xy1 f-  f/  x1 x0 f-  all-x  f/  f*
    then ( area ) fabs ( dur ) f+
  2 +loop
  ( dur )
;

\ ;;; -------- Dolph-Chebyshev window
\ ;;; 
\ ;;; formula taken from Richard Lyons, "Understanding DSP"
\ ;;; see clm.c for C version (using either GSL's or GCC's complex trig functions)

: dolph ( n gamma -- im )
  doc" Produces a Dolph-Chebyshev FFT data window of N points using GAMMA as the window parameter."
  { n gamma }
  10.0 gamma f** cacosh n c/ ccosh { alpha }
  alpha cacosh n c* ccosh 1/c { den }
  n 0.0 make-vct { rl }
  n 0.0 make-vct { im }
  pi n f/ { freq }
  0.0 { phase }
  n 0 ?do
    phase ccos alpha c* cacos n c* ccos den c* { val }
    rl i  val real-ref vct-set! drop
    im i  val imag-ref vct-set! drop
    phase freq f+ to phase
  loop
  rl im -1 fft ( rl ) dup vct-peak 1/f vct-scale! ( rl ) n 2/ cycle-start!
  n 0 ?do im i  rl cycle-ref  vct-set! drop loop
  im
;

\ ;;; this version taken from Julius Smith's "Spectral Audio..." with three changes
\ ;;;   it does the DFT by hand, and is independent of anything from Snd (fft, vcts etc)

: dolph-1 ( n gamma -- im )
  { n gamma }
  10.0 gamma f** cacosh n c/ ccosh { alpha }
  alpha cacosh n c* ccosh 1/c { den }
  pi n f/ { freq }
  half-pi fnegate { phase }
  -1.0 { mult }
  n undef make-array map!
    phase ccos alpha c* cacos n c* ccos den c* mult c* ( val )
    mult fnegate to mult
    phase freq f+ to phase
  end-map { vals }
  \ now take the DFT
  0.0 { pk }
  n undef make-array map!
    0.0 ( sum )
    vals each ( val ) 2.0 0+1.0i c*  pi j i * f*  c*  n c/ cexp c* c+ end-each
    ( sum ) cabs dup pk fmax to pk ( sum )
  end-map ( w ) map! *key* pk f/ end-map ( w )
;

\ ;;; ------- move sound down by n (a power of 2) ---

: down-oct <{ n :optional snd #f chn #f -- vct }>
  doc" Moves a sound down by power of 2 n."
  snd chn #f frames { len }
  2.0  len flog 2.0 flog f/ fceil ( pow2 )  f** f>s { fftlen }
  fftlen 1/f { fftscale }
  0 fftlen snd chn #f channel->vct { rl1 }
  fftlen 0.0 make-vct { im1 }
  rl1 im1 1 fft drop
  rl1 fftscale vct-scale! drop
  im1 fftscale vct-scale! drop
  fftlen n * 0.0 make-vct { rl2 }
  fftlen n * 0.0 make-vct { im2 }
  fftlen 1- { kdx }
  fftlen n * 1- { jdx }
  fftlen 2/ 0 ?do
    rl2 i    rl1 i   vct-ref  vct-set! drop
    rl2 jdx  rl1 kdx vct-ref  vct-set! drop
    im2 i    im1 i   vct-ref  vct-set! drop
    im2 jdx  im1 kdx vct-ref  vct-set! drop
    jdx 1- to jdx
    kdx 1- to kdx
  loop
  rl2 im2 -1 fft
  rl2 0 n len * snd chn #f $" %s %s" '( n get-func-name ) string-format vct->channel
;

: stretch-sound-via-dft <{ factor :optional snd #f chn #f -- }>
  doc" Makes the given channel longer (FACTOR should be > 1.0) \
by squeezing in the frequency domain, then using the inverse DFT to get the time domain result."
  snd chn #f frames { n }
  n f2/ floor f>s { n2 }
  n factor f* fround->s { out-n }
  0 n snd chn #f channel->vct { in-data }
  out-n 0.0 make-array { fr }
  two-pi n f/ { freq }
  n 0 ?do
    i n2 < if
      fr i                 freq 0.0-1.0i c* i c* in-data edot-product  array-set!
    else
      fr out-n n - 1- i +  freq 0.0-1.0i c* i c* in-data edot-product  array-set!
    then
  loop
  two-pi out-n f/ { freq }
  out-n 0.0 make-vct map! freq 0.0+1.0i c* i c* fr edot-product n c/ real-ref end-map ( out-data )
  0 out-n snd chn #f $" %s %s" '( factor get-func-name ) string-format vct->channel drop
;

\ ;;; -------- compute-uniform-circular-string
\ ;;;
\ ;;; this is a simplification of the underlying table-filling routine for "scanned synthesis".
\ ;;; To watch the wave, open some sound (so Snd has some place to put the graph), turn off
\ ;;; the time domain display (to give our graph all the window -- to do this in a much more
\ ;;; elegant manner, see snd-motif.scm under scanned-synthesis).

: compute-uniform-circular-string ( size x0 x1 x2 mass xspring damp -- )
  { size x0 x1 x2 mass xspring damp }
  damp mass f/ { dm }
  xspring mass f/ { km }
  1.0 dm f+ { denom }
  dm km f2* f- 2.0 f+  denom f/ { p1 }
  km denom f/ { p2 }
  -1.0 denom f/ { p3 }
  x0 map!
    x1 i vct-ref p1 f*
    x1 i 1- object-ref  x1 i 1+ size = if 0 else i 1+ then object-ref f+ p2 f*  f+
    x2 i vct-ref p3 f*  f+
  end-map to x0
  x2 0.0 vct-fill! drop
  x2 x1  vct-add!  drop
  x1 0.0 vct-fill! drop
  x1 x2  vct-add!  drop
;

: compute-string ( size x0 x1 x2 masses xsprings esprings damps haptics -- )
  { size x0 x1 x2 masses xsprings esprings damps haptics }
  x0 map!
    damps    i vct-ref masses i vct-ref f/ { dm }
    xsprings i vct-ref masses i vct-ref f/ { km }
    esprings i vct-ref masses i vct-ref f/ { cm }
    1.0 dm cm f+ f+ { denom }
    dm km f2* f- 2.0 f+ denom f/ { p1 }
    km denom f/ { p2 }
    -1.0 denom f/ { p3 }
    haptics i vct-ref masses i vct-ref denom f* f/ { p4 }
    x1 i vct-ref p1 f*
    x1 i 1- object-ref x1 i 1+ size = if 0 else i 1+ then vct-ref f+ p2 f*  f+
    x2 i vct-ref p3 f*  f+
    p4  f+
  end-map to x0
  size 0 ?do
    x2 i  x1 i vct-ref  vct-set! drop
    x1 i  x0 i vct-ref  vct-set! drop
  loop
;

\ ;;; -------- "frequency division" -- an effect from sed_sed@my-dejanews.com

hide
: freqdiv-cb { div n curval -- proc; y self -- val }
  1 proc-create div , n , curval ,
 does> ( y self -- val )
  { y self }
  self           @ { div }
  self   cell+   @ { n }
  div 0= if y self 2 cells + ! ( curval ) then
  1 self +! ( div++ )
  div n = if 0 self ! then
  self 2 cells + @ ( curval )
;
set-current
: freqdiv   <{ n :optional snd #f chn #f -- }>
  doc" Repeats each nth sample N times (clobbering the intermediate samples): 8 freqdiv"
  0 n 0.0 freqdiv-cb 0 #f snd chn #f $" %s %s" '( n get-func-name ) format map-channel drop
;
previous

\ ;;; -------- "adaptive saturation" -- an effect from sed_sed@my-dejanews.com ---

hide
: adsat-cb { mn mx n vals -- proc; inval self -- res }
  1 proc-create { prc } vals , n , mx , mn , prc
 does> ( inval self -- res )
  { inval self }
  self           @ { vals }
  self 1 cells + @ { n }
  self 2 cells + @ { mx }
  self 3 cells + @ { mn }
  vals length n = if
    vals each { x }
      vals i  x f0>= if mx else mn then  vct-set! drop
    end-each
    0   self 1 cells + ! ( n )
    0.0 self 2 cells + ! ( mx )
    0.0 self 3 cells + ! ( mn )
    vals
  else
    vals n inval vct-set! drop
    1 self 1 cells + +! ( n++ )
    inval mx fmax self 2 cells + ! ( mx )
    inval mn fmin self 3 cells + ! ( mn )
    #f
  then
;
set-current
: adsat <{ size :optional beg 0 dur #f snd #f chn #f -- val }>
  doc" An 'adaptive saturation' sound effect."
  $" %s %s %s %s" '( size beg dur get-func-name ) string-format { origin }
  0.0 0.0 0 size 0.0 make-vct adsat-cb beg dur snd chn #f origin map-channel
;
previous

\ ;;; -------- spike ---
\ ;;;
\ ;;; makes sound more spikey -- sometimes a nice effect

hide
: spike-cb ( snd chn -- proc; x0 self -- res )
  { snd chn }
  snd chn #f maxamp { amp }
  1 proc-create 0.0 ( x1 ) , 0.0 ( x2 ) , amp ,
 does> ( x0 self -- res )
  { x0 self }
  self @ { x1 }
  self 1 cells + @ { x2 }
  self 2 cells + @ { amp }
  x0 amp amp f* f/ x2 fabs f* x1 fabs f* { res }
  x1 self cell+ ! ( x2 )
  x0 self ! ( x1 )
  res
;
set-current
: spike <{ :optional snd #f chn #f -- val }>
  doc" Multiplies successive samples together to make a sound more spikey."
  snd chn spike-cb 0 #f snd chn #f get-func-name map-channel
;
previous

\ ;;; -------- easily-fooled autocorrelation-based pitch tracker

: spot-freq <{ samp :optional snd #f chn #f -- }>
  doc" Tries to determine the current pitch: left-sample spot-freq"
  snd srate s>f { sr }
  2.0   sr 20.0 f/ flog  2.0 flog  f/  fceil  f** f>s { fftlen }
  samp fftlen snd chn #f channel->vct autocorrelate { data }
  data vct-peak { cor-peak }
  cor-peak f2*  { cor-peak2 }
  0.0 ( ret )
  fftlen 2 - 1 ?do
    data i     vct-ref data i 1 + vct-ref f<
    data i 1 + vct-ref data i 2 + vct-ref f> && if
      drop ( old ret )
      data i     vct-ref cor-peak f+ cor-peak2 f/ flog10 { logla }
      data i 1 + vct-ref cor-peak f+ cor-peak2 f/ flog10 { logca }
      data i 2 + vct-ref cor-peak f+ cor-peak2 f/ flog10 { logra }
      logla logra f- f2/  logla logra f+ logca -2.0 f* f+  f/ { offset }
      sr  offset i f+ 1.0 f+ f2*  f/ ( new ret )
      leave
    then
  loop
;
0 [if]
\ Left sample:
graph-hook lambda: <{ snd chn y0 y1 -- }>
  $" freq: %.3f" '( snd chn LEFT-SAMPLE  snd chn spot-freq ) string-format
  snd #f report-in-minibuffer drop
  #f
; add-hook!
\ At cursor position:
mouse-click-hook lambda: <{ snd chn button state x y axis -- }>
  axis time-graph = if
    $" freq: %.3f" '( snd chn #f CURSOR  snd chn spot-freq ) string-format
    snd #f report-in-minibuffer
  else
    #f
  then
; add-hook!
[then]

\ ;;; -------- chorus (doesn't always work and needs speedup)

5    value chorus-size
0.05 value chorus-time
20.0 value chorus-amount
10.0 value chorus-speed

hide
: make-flanger ( -- ri gen )
  :frequency chorus-speed :amplitude chorus-amount make-rand-interp { ri }
  chorus-time 3.0 f* #f srate f* random floor f>s { len }
  len :max-size len chorus-amount f>s + 1+ make-delay { gen }
  #( gen ri )
;
: flanger ( dly inval -- val )
  { dly inval }
  dly 0 array-ref  inval  dly 1 array-ref  0.0 rand-interp  delay inval f+
;
set-current
: chorus ( -- proc; inval self -- val )
  doc" Tries to produce the chorus sound effect."
  chorus-size nil make-array map! make-flanger end-map { dlys }
  1 proc-create dlys ,
 does> ( inval self -- val )
  { inval self }
  self @ { dlys }
  0.0 ( sum ) dlys each ( dly ) inval flanger f+ ( sum++ ) end-each
;
previous

\ ;;; -------- chordalize (comb filters to make a chord using chordalize-amount and chordalize-base)

0.95           value chordalize-amount
100            value chordalize-base
'( 1 3/4 5/4 ) value chordalize-chord

: chordalize ( -- proc; x self -- val )
  doc" Uses harmonically-related comb-filters to bring out a chord in a sound.  \
Global variable CHORDALIZE-CHORD is a list of members of chord such as '( 1 5/4 3/2 )."
  chordalize-chord map
    :scaler chordalize-amount :size chordalize-base *key* r* r>s make-comb
  end-map { combs }
  chordalize-chord length 0.5 f/ { scaler }
  1 proc-create combs , scaler ,
 does> ( x self -- val )
  { x self }
  self       @ { combs }
  self cell+ @ { scaler }
  0.0 ( val ) combs each ( gen ) x 0.0 comb f+ ( val += ... ) end-each scaler f*
;

\ ;;; -------- zero-phase, rotate-phase
\ ;;; fft games (from the "phazor" package of Scott McNab)

: zero-phase <{ :optional snd #f chn #f -- vct }>
  doc" Calls fft, sets all phases to 0, and un-ffts."
  snd chn #f frames { len }
  2.0  len flog 2.0 flog f/ fceil ( pow2 )  f** fround->s { fftlen }
  fftlen 1/f { fftscale }
  0 fftlen snd chn #f channel->vct { rl }
  rl vct-peak { old-pk }
  fftlen 0.0 make-vct { im }
  rl im 1 fft drop
  rl im rectangular->polar drop
  rl fftscale vct-scale! drop
  im 0.0 vct-scale! drop
  rl im -1 fft drop
  rl vct-peak { pk }
  rl old-pk pk f/ vct-scale! 0 len snd chn #f get-func-name vct->channel
;

: rotate-phase <{ func :optional snd #f chn #f -- vct }>
  doc" Calls fft, applies FUNC, a proc or xt, to each phase, then un-ffts."
  func proc? func xt? || func 1 $" a proc or xt" _ assert-type
  snd chn #f frames { len }
  2.0  len flog 2.0 flog f/ fceil ( pow2 )  f** fround->s { fftlen }
  fftlen 2/ { fftlen2 }
  fftlen 1/f { fftscale }
  0 fftlen snd chn #f channel->vct { rl }
  rl vct-peak { old-pk }
  fftlen 0.0 make-vct { im }
  rl im 1 fft drop
  rl im rectangular->polar drop
  rl fftscale vct-scale! drop
  im 0 0.0 vct-set! drop
  func xt? if func 1 make-proc to func then
  fftlen2 1 ?do
    im i  func  '( im i  object-ref )  run-proc  object-set!
    im i negate im i negate object-ref fnegate   object-set!	\ handles negative index
  loop
  rl im -1 fft drop
  rl vct-peak { pk }
  $" <'> %s %s" '( func proc-name get-func-name ) string-format { origin }
  rl old-pk pk f/ vct-scale! 0 len snd chn #f origin vct->channel
;
\ See note above for a good origin for reuse.
\ lambda: <{ x }> 0.0       ; rotate-phase \ is the same as zero-phase
\ lambda: <{ x }> pi random ; rotate-phase \ randomizes phases
\ lambda: <{ x }> x         ; rotate-phase \ returns original
\ lambda: <{ x }> x fnegate ; rotate-phase \ reverses original
\ lambda: <{ x }> x f2*     ; rotate-phase \ reverb-effect (best with voice)
\ lambda: <{ x }> x 12.0 f* ; rotate-phase \ "bruise blood" effect

\ ;;; -------- asymmetric FM (bes-i0 case)

: make-asyfm <{ :key frequency 440.0 initial-phase 0.0 ratio 1.0 r 1.0 index 1.0 -- gen }>
  #{ :freq  frequency hz->radians
     :phase initial-phase
     :ratio ratio
     :r     r
     :index index }
;
: asyfm-freq-ref   ( gen -- val ) :freq  hash-ref radians->hz ;
: asyfm-freq-set!  ( gen val -- ) :freq  swap hz->radians hash-set! ;
: asyfm-phase-ref  ( gen -- val ) :phase hash-ref ; 
: asyfm-phase-set! ( gen val -- ) :phase swap hash-set! ;
: asyfm-ratio-ref  ( gen -- val ) :ratio hash-ref ; 
: asyfm-ratio-set! ( gen val -- ) :ratio swap hash-set! ;
: asyfm-r-ref      ( gen -- val ) :r     hash-ref ; 
: asyfm-r-set!     ( gen val -- ) :r     swap hash-set! ;
: asyfm-index-ref  ( gen -- val ) :index hash-ref ; 
: asyfm-index-set! ( gen val -- ) :index swap hash-set! ;
: asyfm-J ( gen input -- val )
  doc" ;; this is the same as the CLM asymmetric-fm generator, \
set r != 1.0 to get the asymmetric spectra.\n\
:frequency 2000 :ratio 0.1 make-asyfm value gen\n\
lambda: <{ n }> gen 0.0 asyfm-J ; map-channel."
  { gen input }
  gen :freq  hash-ref { freq }
  gen :phase hash-ref { phase }
  gen :ratio hash-ref { ratio }
  gen :r     hash-ref { r }
  gen :index hash-ref { index }
  r 1/f { r1 }
  ratio phase f* { modphase }
  modphase fcos r r1 f- f* index f* 0.5 f* fexp
  modphase fsin r r1 f+ f* index f* 0.5 f* phase f+ fsin  f* ( val )
  gen :phase phase input freq f+ f+ hash-set!
;
\ :frequency 2000 :ratio 0.1 value gen
\ lambda: <{ n -- val }> gen 0.0 asyfm-J ; map-channel
: asyfm-I ( gen input -- val )
  { gen input }
  gen :freq  hash-ref { freq }
  gen :phase hash-ref { phase }
  gen :ratio hash-ref { ratio }
  gen :r     hash-ref { r }
  gen :index hash-ref { index }
  r 1/f { r1 }
  ratio phase f* { modphase }
  modphase fcos r r1 f+ f* index f* 0.5 f*
  r r1 f+ index f* bes-i0 flog 0.5 f*  f-  fexp
  modphase fsin r r1 f- f* index f* 0.5 f* phase f+ fsin  f* ( val )
  gen :phase phase input freq f+ f+ hash-set!
;

\ ;;; -------- cosine-summation (a simpler version of sine-summation)
\ ;;;
\ ;;; from Andrews, Askey, Roy "Special Functions" 5.1.16

: cosine-summation ( gen r -- val )
  doc" A variant of the CLM sine-summation generator; R controls successive sinusoid amplitudes."
  { gen r }
  1.0  r r f*  f-
  1.0  r r f*  f+  r f2*  gen 0.0 0.0 oscil f*  f-  f/  1.0 f-
  1.0  r r f*  f-
  1.0  r r f*  f+  r f2*  f*  f/  f*
;
<'> make-oscil alias make-cosine-summation 
\ 100.0 make-cosine-summation value gen
\ lambda: <{ y }> gen 0.5 cosine-summation 0.2 f* ; map-channel

\ ;;; -------- kosine-summation
\ ;;;
\ ;;; from Askey "Ramanujan and Hypergeometric Series" in Berndt and
\ ;;;   Rankin "Ramanujan: Essays and Surveys"

\ ;;;
\ ;;; this gives a sum of cosines of decreasing amp where the "k" parameter determines
\ ;;;   the "index" (in FM nomenclature) -- higher k = more cosines; the actual amount
\ ;;;   of the nth cos involves hypergeometric series (looks like r^n/n!
\ ;;;   (~=e^n?) with a million other terms).

: kosine-summation ( gen r k -- val )
  doc" It's a variant of sum-of-cosines; \
R controls successive sinusoid amplitude; \
K controls how many sinusoids are produced."
  { gen r k }
  1.0 r r f* f+  r f2* gen 0.0 0.0 oscil f*  f-  k fnegate  f**
  1.0 r r f* f+  r f2*  f-  k  f**  f*
;
<'> make-oscil alias make-kosine-summation 
\ 100.0 make-kosine-summation value gen
\ lambda: <{ y }> gen 0.5 5.0 kosine-summation 0.2 f* ; map-channel

\ ;;; -------- legendre, fejer

: fejer-sum ( angle n -- val )
  doc" Produces a band-limited pulse train."
  \ ;; from "Trigonometric Series" Zygmund p88
  { angle n }
  angle f0= if
    1.0
  else
    n 1.0 f+ angle f* f2/ fsin  angle f2/ fsin f2*  f/ ( val )  dup f* n 1.0 f+ f/ f2* 
  then
;
\ 0.0 value angle
\ lambda: <{ y }> angle 3.0 fejer-sum 0.1 f* ( val ) 0.1 +to angle ; map-channel

: legendre-sum ( angle n -- val )
  doc" Produces a band-limited pulse train."
  \ ;; from Andrews, Askey, Roy "Special Functions" p 314
  { angle n }
  angle f0= if
    1.0
  else
    n 0.5 f+ angle f* fsin  angle f2/ fsin  f/ dup f*
  then
;
\ 0.0 value angle
\ lambda: <{ y }> angle 3.0 legendre-sum 0.1 f* ( val ) 0.1 +to angle ; map-channel

\ ;;; -------- variations on sum-of-cosines
\ ;;; from "Trigonometric Delights" by Eli Maor

: sum-of-n-sines ( angle n -- val )
  doc" Produces the sum of N sines."
  { angle n }
  angle f2/ { a2 }
  a2 fsin { den }
  den f0= if 0.0 else n a2 f* fsin  n 1.0 f+ a2 f* fsin f* den f/ then
;
\ 0.0 value angle
\ lambda: <{ y }> angle 3.0 sum-of-n-sines 0.1 f*  0.1 +to angle ; map-channel

: sum-of-n-odd-sines ( angle n -- val )
  doc" Produces the sum of N odd-numbered sines."
  { angle n }
  angle fsin { den }
  angle n f* fsin dup { na2 }
  den f0= if 0.0 else na2 den f/ then
;
: sum-of-n-odd-cosines ( angle n -- val )
  doc" Produces the sum of N odd-numbered cosines."
  { angle n }
  angle fsin f2* { den }
  den f0= if n else angle n f* f2*  den f/ then
;
: band-limited-sawtooth ( x a N fi -- val )
  doc" Produces a band-limited sawtooth; \
X is the current phase, \
A is the amp (more or less), \
N is 1..10 or thereabouts, \
FI is the phase increment."
  { x a N fi }
  a dup f*  x fcos a f* -2.0 f*  f+ 1.0 f+ { s4 }
  s4 f0= if
    0.0
  else
    a N 1.0 f- f**  N 1.0 f- x f* fi f+ fsin  f* { s1 }
    a N f**  N x f* fi f+ fsin  f*               { s2 }
    x fi f+ fsin a f+                            { s3 }
    fi fsin s3 fnegate f+ s2 fnegate f+ s1 f+  s4 f/
  then
;
\ 0.0 value angle
\ lambda: <{ y }> angle 0.5 8.0 0.2 band-limited-sawtooth  0.2 +to angle ; map-channel

: band-limited-square-wave ( theta n -- val )
  doc" Produces a square-wave; N sets how squared-off it is, THETA is instantaneous phase."
  swap fsin f* ftanh
;
\ 0.0 value angle
\ lambda: <{ y }> angle 10.0 band-limited-square-wave  0.2 +to angle ; map-channel

\ ;;; -------- brighten-slightly

hide
: bs-cb { brt mx -- proc; y self -- val }
  1 proc-create mx , brt ,
 does> { y self -- val }
  self       @ { mx }
  self cell+ @ { brt }
  brt y f* fsin mx f*
;
set-current
: brighten-slightly <{ amount :optional snd #f chn #f -- val }>
  doc" It's a form of contrast-enhancement (AMOUNT between ca 0.1 and 1)."
  snd chn #f maxamp { mx }
  two-pi amount f*  mx f/ { brt }
  brt mx bs-cb 0 #f snd chn #f $" %s %s" '( amount get-func-name ) string-format map-channel
;
previous

hide
: brighten-slightly-1-cb { pcoeffs mx -- proc; y self -- val }
  1 proc-create mx , pcoeffs ,
 does> { y self -- val }
  self       @ { mx }
  self cell+ @ { pcoeffs }
  pcoeffs y mx f/ polynomial mx f*
;
set-current
: brighten-slightly-1 <{ coeffs :optional snd #f chn #f -- }>
  doc" It's a form of contrast-enhancement (AMOUNT between ca 0.1 and 1)."
  snd chn #f maxamp { mx }
  coeffs mus-chebyshev-first-kind partials->polynomial { pcoeffs }
  pcoeffs mx brighten-slightly-1-cb 0 #f snd chn #f
  $" %s %s" '( coeffs get-func-name ) string-format map-channel drop
;
\ '( 1 0.5 3 1 ) brighten-slightly-1
previous

\ ;;; -------- FIR filters

: spectrum->coeffs ( order spectr -- vct )
  doc" Returns FIR filter coefficients given the filter order and desired spectral envelope"
  { order spectr }
  order 0.0 make-vct { coeffs }
  order { n }
  n 1+ f2/ { am }
  am floor f>s { m }
  two-pi n f/ { q }
  n 1- { jj }
  m 0 ?do
    spectr 0 vct-ref f2/ ( xt )
    m 1 ?do spectr i vct-ref   q i  am j 1 f- f-  f* f*  fcos  f*  f+ ( xt += ... ) loop
    ( xt ) n f/ f2* { coeff }
    coeffs i  coeff vct-set! drop
    coeffs jj coeff vct-set! drop
    -1 +to jj
  loop
  coeffs
;
: fltit-1 ( order spectr -- proc;  y self -- val )
  doc" Creates an FIR filter from spectrum and order and returns a closure that calls it:\n\
10 vct( 0 1.0 0 0 0 0 0 0 1.0 0 ) fltit-1 map-channel"
  { order spectr }
  :order order :xcoeffs order spectr spectrum->coeffs make-fir-filter
  1 proc-create swap ,
 does> ( y self -- val )
  @ swap fir-filter
;
\ 10 vct( 0 1.0 0 0 0 0 0 0 1.0 0 ) fltit-1 map-channel

\ ;;; -------- Hilbert transform

: make-hilbert-transform <{ :optional len 30 -- gen }>
  doc" Makes a Hilbert transform filter."
  len 2* 1+ { arrlen }
  arrlen 0.0 make-vct { arr }
  len even? if len else len 1+ then { lim }
  lim len negate ?do
    i len + { kk }
    i pi f* { denom }
    1.0  denom fcos  f- { num }
    num f0<> i 0<> || if
      arr kk  num denom f/  denom len f/ fcos 0.46 f* 0.54 f+  f*  vct-set! drop
    then
  loop
  :order arrlen :xcoeffs arr make-fir-filter
;
<'> fir-filter alias hilbert-transform
<'> hilbert-transform $" ( gen :optional input 0.0 -- val )  \
The generator corresponding to make-hilbert-transform." help-set!
\ 15 make-hilbert-transform value h
\ lambda: <{ y }> h y hilbert-transform ; map-channel

\ ;;; -------- highpass filter 

: make-highpass <{ fc :optional len 30 -- gen }>
  doc" Makes an FIR highpass filter."
  len 2* 1+ { arrlen }
  arrlen 0.0 make-vct { arr }
  len len negate ?do
    pi i f* { denom }
    fc i f* fsin fnegate { num }
    arr i len +
    i 0= if
      1.0 fc pi f/ f-
    else
      num denom f/
      pi i f* len f/ fcos 0.46 f* 0.54 f+  f*
    then vct-set! drop
  loop
  :order arrlen :xcoeffs arr make-fir-filter
;
<'> fir-filter alias highpass
<'> highpass $" ( gen :optional input 0.0 -- val )  \
The generator corresponding to make-highpass." help-set!
\ pi 0.1 f* make-highpass value hp
\ lambda: <{ y }> hp y highpass ; map-channel

\ ;;; -------- lowpass filter 

: make-lowpass <{ fc :optional len 30 -- gen }>
  doc" Makes an FIR lowpass filter."
  len 2* 1+ { arrlen }
  arrlen 0.0 make-vct { arr }
  len len negate ?do
    pi i f* { denom }
    fc i f* fsin { num }
    arr i len +
    i 0= if
      fc pi f/
    else
      num denom f/
      pi i f* len f/ fcos 0.46 f* 0.54 f+  f*
    then vct-set! drop
  loop
  :order arrlen :xcoeffs arr make-fir-filter
;
<'> fir-filter alias lowpass
<'> lowpass $" ( gen :optional input 0.0 -- val )  \
The generator corresponding to make-lowpass." help-set!
\ pi 0.2 f* make-lowpass value lp
\ lambda: <{ y }> lp y lowpass ; map-channel

\ ;;; -------- bandpass filter 

: make-bandpass <{ flo fhi :optional len 30 -- gen }>
  doc" Makes an FIR bandpass filter."
  len 2* 1+ { arrlen }
  arrlen 0.0 make-vct { arr }
  len len negate ?do
    pi i f* { denom }
    fhi i f* fsin flo i f* fsin f- { num }
    arr i len +
    i 0= if
      fhi flo f- pi f/
    else
      num denom f/
      pi i f* len f/ fcos 0.46 f* 0.54 f+  f*
    then vct-set! drop
  loop
  :order arrlen :xcoeffs arr make-fir-filter
;
<'> fir-filter alias bandpass
<'> bandpass $" ( gen :optional input 0.0 -- val )  \
The generator corresponding to make-bandpass." help-set!
\ pi 0.1 f* pi 0.2 f* make-bandpass value bp
\ lambda: <{ y }> bp y bandpass ; map-channel

\ ;;; -------- bandstop filter 

: make-bandstop <{ flo fhi :optional len 30 -- gen }>
  doc" Makes an FIR bandstop (notch) filter."
  len 2* 1+ { arrlen }
  arrlen 0.0 make-vct { arr }
  len len negate ?do
    pi i f* { denom }
    fhi i f* fsin flo i f* fsin f- { num }
    arr i len +
    i 0= if
      1.0  fhi flo f- pi f/  f-
    else
      num denom f/
      pi i f* len f/ fcos 0.46 f* 0.54 f+  f*
    then vct-set! drop
  loop
  :order arrlen :xcoeffs arr make-fir-filter
;
<'> fir-filter alias bandstop
<'> bandstop $" ( gen :optional input 0.0 -- val )  \
The generator corresponding to make-bandstop." help-set!
\ pi 0.1 f* pi 0.3 f* make-bandstop value bs
\ lambda: <{ y }> bs y bandstop ; map-channel

\ ;;; -------- differentiator

: make-differentiator <{ :optional len 30 -- gen }>
  doc" Makes an FIR differentiator (highpass) filter."
  len 2* 1+ { arrlen }
  arrlen 0.0 make-vct { arr }
  len len negate ?do
    i 0<> if
      pi i f* { pi*i }
      arr i len +
      pi*i fcos i f/  pi*i fsin  pi*i i f* f/  f-
      pi*i len f/ fcos 0.46 f* 0.54 f+  f*
      vct-set! drop
    then
  loop
  :order arrlen :xcoeffs arr make-fir-filter
;
<'> fir-filter alias differentiator
<'> differentiator $" ( gen :optional input 0.0 -- val )  \
The generator corresponding to make-differentiator." help-set!
\ make-differentiator value dt
\ lambda: <{ y }> dt y differentiator ; map-channel

\ ;;; -------- IIR filters
\ ;;; see analog-filter.scm for the usual suspects

\ ;;; -------- Butterworth filters (see also further below -- make-butter-lp et al)
\ ;;;
\ ;; translated from CLM butterworth.cl:
\ ;;
\ ;;   Sam Heisz, January 1998
\ ;;   inspired by some unit generators written for Csound by Paris Smaragdis
\ ;;   who based his work on formulas from 
\ ;;   Charles Dodge, Computer music: synthesis, composition, and performance.

: make-butter-high-pass ( freq -- flt )
  doc" Makes a Butterworth filter with high pass cutoff at FREQ."
  { freq }
  pi freq f* #f srate f/ ftan { r }
  r r f* { r2 }
  r 2.0 fsqrt r2 f* f* 1/f { c1 }
  -2.0 c1 f* { c2 }
  c1 { c3 }
  r2 1.0 f- c1 f* f2* { c4 }
  1.0 2.0 fsqrt r f* f- r2 f+ c1 f* { c5 }
  3 vct( c1 c2 c3 ) vct( 0.0 c4 c5 ) make-filter
;
: make-butter-low-pass ( freq -- flt )
  doc" Makes a Butterworth filter with low pass cutoff at FREQ.  \
The result can be used directly: 500.0 make-butter-low-pass filter-sound, \
or via the 'butter' generator."
  { freq }
  pi freq f* #f srate f/ ftan 1/f { r }
  r r f* { r2 }
  r 2.0 fsqrt f* 1.0 r2 f* f* 1/f { c1 }
  c1 f2* { c2 }
  c1 { c3 }
  1.0 r2 f- c1 f* f2* { c4 }
  1.0 2.0 fsqrt r f* f- r2 f+ c1 f* { c5 }
  3 vct( c1 c2 c3 ) vct( 0.0 c4 c5 ) make-filter
;
: make-butter-band-pass ( freq bw -- flt )
  doc" Makes a bandpass Butterworth filter with low edge at FREQ and width BAND."
  { freq bw }
  #f srate { sr }
  2.0 pi freq f* f* sr f/ fcos f2* { d }
  pi bw f* sr f/ ftan 1/f { c }
  1.0 c f+ 1/f { c1 }
  0.0 { c2 }
  c1 fnegate { c3 }
  c fnegate d c1 f* f* { c4 }
  c 1.0 f- c1 f* { c5 }
  3 vct( c1 c2 c3 ) vct( 0.0 c4 c5 ) make-filter
;
: make-butter-band-reject ( freq bw -- flt )
  doc" Makes a band-reject Butterworth filter with low edge at FREQ and width BAND."
  { freq bw }
  #f srate { sr }
  2.0 pi freq f* f* sr f/ fcos f2* { d }
  pi bw f* sr f/ ftan { c }
  1.0 c f+ 1/f { c1 }
  d fnegate c1 f* { c2 }
  c1 { c3 }
  c2 { c4 }
  1.0 c f- c1 f* { c5 }
  3 vct( c1 c2 c3 ) vct( 0.0 c4 c5 ) make-filter
;
<'> filter alias butter
<'> butter $" ( gen :optional input 0.0 -- val )  \
The generator side for the various make-butter procedures." help-set!
\ 500.0 make-butter-low-pass filter-sound

\ ;;; from "DSP Filter Cookbook" by Lane et al, Prompt Pubs, 2001
\ ;;; 
\ ;;; use with the filter generator
\ ;;;   1000 make-iir-high-pass-2 value gen
\ ;;;   gen 1.0 filter
\ ;;;   etc

:  make-biquad ( a0 a1 a2 b1 b2 -- gen )
  doc" Returns a biquad filter (use with the CLM filter gen)"
  { a0 a1 a2 b1 b2 }
  3 vct( a0 a1 a2 ) vct( 0.0 b1 b2 ) make-filter
;
: make-iir-low-pass-2 ( fc :optional d -- gen )
  '( 2.0 fsqrt ) 1 get-optargs { fc d }
  two-pi fc f* mus-srate f/ { theta }
  1.0 d f2/ theta fsin f* f-  1.0 d f2/ theta fsin f* f+  f/ f2/ { beta }
  beta 0.5 f+ theta fcos f* { gamma }
  beta 0.5 f+ gamma fnegate f+ f2/ { alpha }
  3 vct( alpha  alpha f2*  alpha ) vct( 0.0  gamma -2.0 f*  beta f2* ) make-filter
;
: make-iir-high-pass-2 ( fc :optional d -- gen )
  '( 2.0 fsqrt ) 1 get-optargs { fc d }
  two-pi fc f* mus-srate f/ { theta }
  1.0 d f2/ theta fsin f* f-  1.0 d f2/ theta fsin f* f+  f/ f2/ { beta }
  beta 0.5 f+ theta fcos f* { gamma }
  beta 0.5 f+ gamma f+ f2/ { alpha }
  3 vct( alpha  alpha -2.0 f*  alpha ) vct( 0.0  gamma -2.0 f*  beta f2* ) make-filter
;
: make-iir-band-pass-2 ( f1 f2 -- gen )
  { f1 f2 }
  two-pi f1 f2 f* fsqrt mus-srate f/ { theta }
  f1 f2 f* fsqrt f2 f1 f- f/ { Q }
  theta Q f2* f/ ftan { t2 }
  1.0 t2 f-  1.0 t2 f+  f/ f2/ { beta }
  beta 0.5 f+ theta fcos f* { gamma }
  0.5 beta f- { alpha }
  3 vct( alpha  0.0  alpha fnegate ) vct( 0.0  gamma -2.0 f*  beta f2* ) make-filter
;
: make-iir-band-stop-2 ( f1 f2 -- gen )
  { f1 f2 }
  two-pi f1 f2 f* fsqrt mus-srate f/ { theta }
  f1 f2 f* fsqrt f2 f1 f- f/ { Q }
  theta Q f2* f/ ftan { t2 }
  1.0 t2 f-  1.0 t2 f+  f/ f2/ { beta }
  beta 0.5 f+ theta fcos f* { gamma }
  0.5 beta f+ { alpha }
  3 vct( alpha  gamma -2.0 f*  alpha fnegate ) vct( 0.0  gamma -2.0 f*  beta f2* ) make-filter
;
: make-eliminate-hum <{ :optional hum-freq 60.0 hum-harmonics 5 bandwith 10 -- }>
  hum-harmonics nil make-array map!
    hum-freq i 1.0 f+ f* { center }
    bandwith f2/ { b2 }
    center b2 f- center b2 f+ make-iir-band-stop-2
  end-map ( gen )
;
: eliminate-hum ( gens x0 -- val )
  { gens x0 }
  gens each ( gen ) x0 filter to x0 end-each
  x0
;
\ make-eliminate-hum value hummer
\ lambda: <{ x }> hummer x eliminate-hum ; map-channel

\ ;; bandpass, m is gain at center of peak
\ ;; use map-channel with this one (not clm-channel or filter)
: make-peaking-2 ( f1 f2 m -- prc; y self -- val )
  { f1 f2 m }
  f1 f2 f* fsqrt two-pi f* mus-srate f/ { theta }
  f1 f2 f* fsqrt f2 f1 f- f/ { Q }
  4.0 m 1.0 f+ f/ theta Q f2* f/ ftan f* { t2 }
  1.0 t2 f- 1.0 t2 f+ f/ f2/ { beta }
  0.5 beta f+ theta fcos f* { gamma }
  0.5 beta f- { alpha }
  3
  vct( alpha 0.0 alpha fnegate )
  vct( 0.0 -2.0 gamma f* beta f2* ) make-filter { flt }
  1 proc-create flt , m 1.0 f- , ( prc )
 does> { y self -- val }
  self @ ( flt ) y filter self cell+ @ ( m ) f* y f+
;

: cascade->canonical ( A -- A' )
  doc" Converts a list of cascade coeffs (vcts with 3 entries) to canonical form."
  { A }
  A length { K }
  K 2* 1+ 0.0 make-vct { d }
  K 2* 1+ 0.0 make-vct { a1 }
  a1 0 1.0 vct-set! drop
  A each { h }
    i 2* 3 + 0 ?do
      d i 0.0 vct-set! drop
      2 i min 0 ?do d j  h i vct-ref a1 j i - vct-ref f*  object-set+! loop
    loop
    i 2* 3 + 0 ?do a1 j  d j vct-ref  vct-set! drop loop
  end-each
  a1
;

: make-butter-lp ( M fc -- flt )
  doc" Returns a butterworth low-pass filter; \
its order is M * 2, FC is the cutoff frequency in Hz."
  { M fc }
  #() { xcoeffs }
  #() { ycoeffs }
  two-pi fc f* mus-srate f/ { theta }
  theta fsin { st }
  theta fcos { ct }
  M 0 ?do
    i 1+ 2* 1- pi f*  4 M * f/ fsin f2* { d }
    1.0 d st f* f2/ f-  1.0 d st f* f2/  f/ f2/ { beta }
    beta 0.5 f+ ct f* { gamma }
    gamma fnegate beta f+ 0.5 f+ 0.25 f* { alpha }
    xcoeffs vct( alpha f2*  alpha 4.0 f*  alpha f2* ) array-push drop
    ycoeffs vct( 1.0  -2.0 gamma f*  beta f2* )       array-push drop
  loop
  M 2* 1+
  xcoeffs cascade->canonical
  ycoeffs cascade->canonical make-filter
;

: make-butter-hp ( M fc -- flt )
  doc" Returns a butterworth high-pass filter; \
its order is M * 2, FC is the cutoff frequency in Hz."
  { M fc }
  #() { xcoeffs }
  #() { ycoeffs }
  two-pi fc f* mus-srate f/ { theta }
  theta fsin { st }
  theta fcos { ct }
  M 0 ?do
    i 1+ 2* 1- pi f*  4 M * f/ fsin f2* { d }
    1.0 d st f* f2/ f-  1.0 d st f* f2/  f/ f2/ { beta }
    beta 0.5 f+ ct f* { gamma }
    gamma beta f+ 0.5 f+ 0.25 f* { alpha }
    xcoeffs vct( alpha f2*  alpha -4.0 f*  alpha f2* ) array-push drop
    ycoeffs vct( 1.0  -2.0 gamma f*  beta f2* )        array-push drop
  loop
  M 2* 1+
  xcoeffs cascade->canonical
  ycoeffs cascade->canonical make-filter
;

: make-butter-bp ( M f1 f2 -- flt )
  doc" Returns a butterworth band-pass filter; \
its order is M * 2, F1 and F2 are the band edge frequencies in Hz."
  { M f1 f2 }
  #() { xcoeffs }
  #() { ycoeffs }
  f1 f2 f* fsqrt { f0 }
  f0 f2 f1 f- f/ { Q }
  two-pi f0 f* mus-srate f/ { theta0 }
  theta0 Q f2* f/ theta0 fsin f* f2* { de }
  de f2/ { de2 }
  theta0 f2/ ftan { tn0 }
  1 1 { jj kk }
  M 0 ?do
    kk 2* 1- pi f* M 2* f/ fsin f2* { Dk }
    de2 de2 f* 1.0 f+ Dk de2 f* f/ { Ak }
    de Dk f* Ak Ak f* 1.0 f- fsqrt Ak f+ f/ fsqrt { dk1 }
    Dk dk1 f/ de2 f* { Bk }
    Bk Bk f* 1.0 f- fsqrt Bk f+ { Wk }
    jj 1 = if tn0 Wk f/ else tn0 Wk f* then fatan f2* { thetajk }
    1.0 0.5 dk1 f* thetajk fsin f* f-
    1.0 0.5 dk1 f* thetajk fsin f* f+ f/ f2/ { betajk }
    0.5 betajk f+ thetajk fcos f* { gammajk }
    Wk Wk 1/f f- dk1 f/ { wk2 }
    0.5 betajk f- wk2 wk2 f* 1.0 f+ fsqrt f* { alphajk }
    xcoeffs vct( alphajk f2*  0.0  -2.0 alphajk f* ) array-push drop
    ycoeffs vct( 1.0  -2.0 gammajk f*  betajk f2* )  array-push drop
    jj 1 = if
      2 to jj
    else
      1 +to kk
      1 to jj
    then
  loop
  M 2* 1+
  xcoeffs cascade->canonical
  ycoeffs cascade->canonical make-filter
;

: make-butter-bs ( M f1 f2 -- flt )
  doc" Returns a butterworth band-stop filter; \
its order is M * 2, F1 and F2 are the band edge frequencies in Hz."
  { M f1 f2 }
  #() { xcoeffs }
  #() { ycoeffs }
  f1 f2 f* fsqrt { f0 }
  f0 f2 f1 f- f/ { Q }
  two-pi f0 f* mus-srate f/ { theta0 }
  theta0 Q f2* f/ theta0 fsin f* f2* { de }
  de f2/ { de2 }
  theta0 fcos { ct }
  theta0 f2/ ftan { tn0 }
  1 1 { jj kk }
  M 0 ?do
    kk 2* 1- pi f* M 2* f/ fsin f2* { Dk }
    de2 de2 f* 1.0 f+ Dk de2 f* f/ { Ak }
    de Dk f* Ak Ak f* 1.0 f- fsqrt Ak f+ f/ fsqrt { dk1 }
    Dk dk1 f/ de2 f* { Bk }
    Bk Bk f* 1.0 f- fsqrt Bk f+ { Wk }
    jj 1 = if tn0 Wk f/ else tn0 Wk f* then fatan f2* { thetajk }
    1.0 0.5 dk1 f* thetajk fsin f* f-
    1.0 0.5 dk1 f* thetajk fsin f* f+ f/ f2/ { betajk }
    0.5 betajk f+ thetajk fcos f* { gammajk }
    0.5 betajk f+ 1.0 thetajk fcos f- 1.0 ct f- f/ f* f2/ { alphajk }
    xcoeffs vct( alphajk f2*  ct alphajk f* -4.0 f*  alphajk f2* ) array-push drop
    ycoeffs vct( 1.0  -2.0 gammajk f*  betajk f2* )                array-push drop
    jj 1 = if
      2 to jj
    else
      1 +to kk
      1 to jj
    then
  loop
  M 2* 1+
  xcoeffs cascade->canonical
  ycoeffs cascade->canonical make-filter
;
  

\ ;;; -------- notch filters

: make-notch-frequency-response <{ cur-srate freqs :optional notch-width 2 -- fresp }>
  '( 1.0 0.0 ) { freq-response }
  freqs each { f }
    f notch-width f- f2* cur-srate f/ freq-response cons to freq-response \ ; left upper y hz
    1.0 freq-response cons to freq-response \ ; left upper y resp
    f notch-width f2/ f- f2* cur-srate f/ freq-response cons to freq-response \ ; left bottom y hz
    0.0 freq-response cons to freq-response \ ; left bottom y resp
    f notch-width f2/ f+ f2* cur-srate f/ freq-response cons to freq-response \ ; right bottom y hz
    0.0 freq-response cons to freq-response \ ; right bottom y resp
    f notch-width f+ f2* cur-srate f/ freq-response cons to freq-response \ ; right upper y hz
    1.0 freq-response cons to freq-response \ ; right upper y resp
  end-each
  1.0 1.0 freq-response cons cons list-reverse
;

: notch-channel <{ freqs
     :optional filter-order #f beg 0 dur #f snd #f chn #f edpos #f truncate #t notch-width 2 -- f }>
  doc" Returns a notch filter removing freqs."
  snd srate s>f freqs notch-width make-notch-frequency-response { nf }
  filter-order 2.0 snd srate notch-width f/ flog 2.0 flog f/ fceil f** fround->s || { order }
  $" %s %s %s %s %s" '( freqs filter-order beg dur get-func-name ) string-format { origin }
  nf order beg dur snd chn edpos truncate origin filter-channel
;

: notch-sound <{ freqs :optional filter-order #f snd #f chn #f notch-width 2 -- f }>
  doc" Returns a notch filter removing freqs."
  snd srate s>f freqs notch-width make-notch-frequency-response { nf }
  filter-order 2.0 snd srate notch-width f/ flog 2.0 flog f/ fceil f** fround->s || { order }
  $" %s %s 0 #f notch-channel " '( freqs filter-order ) string-format { origin }
  nf order snd chn #f origin filter-sound
;

: notch-selection <{ freqs :optional filter-order #f notch-width 2 -- f }>
  doc" Returns a notch filter removing freqs."
  selection? if
    selection-srate s>f freqs notch-width make-notch-frequency-response ( nf )
    filter-order
    2.0 selection-srate notch-width f/ flog 2.0 flog f/ fceil f** fround->s || ( order )
    #t ( truncate ) filter-selection
  then
;

\ ;;; -------- fractional Fourier Transform, z transform
\ ;;;
\ ;;; translated from the fxt package of Joerg Arndt

: fractional-fourier-transform ( real imaginary n angle -- hr hi )
  doc" performs a fractional Fourier transform on data; \
if angle=1.0, you get a normal Fourier transform."
  { fr fi n v }
  v two-pi f* n f/ { ph0 }
  n 0.0 make-vct { hi }
  n 0.0 make-vct map!
    0.0 0.0 { sr si }
    fr each { x }
      ph0 i f* j f* { phase }
      phase fcos { c }
      phase fsin { s }
      phase fcos { c }
      fi i vct-ref { y }
      x c f* y s f* f- { r }
      y c f* x s f* f+ { i }
      sr r f+ to sr
      si i f+ to si
    end-each
    hi i si vct-set! drop
    sr
  end-map ( hr ) hi
;

: z-transform ( data n z -- )
  doc" Performs a Z transform on DATA; \
if z=e^2*pi*j/n you get a Fourier transform; complex results in returned vector."
  { f n z }
  n nil make-array map!
    0.0 ( sum )
    1.0 { t }
    z i f** { m }
    n 0 ?do
      f i vct-ref t c* c+ ( sum += ... )
      t m c* to t
    loop ( sum )
  end-map
;
\ data n  0.0  2.0 n f/ pi f*  make-rectangular cexp z-transform

\ ;;; -------- slow Hartley transform

: dht ( data -- vct )
  doc" Returns the Hartley transform of DATA."
  { data }
  data length { len }
  two-pi len f/ { w }
  len 0.0 make-vct { arr }
  len 0 do
    len 0 do
      arr j
      data i vct-ref
      i j w f* f* fcos
      i j w f* f* fsin f+ f* object-set+!
    loop
  loop
  arr
;

: find-sine ( freq beg dur -- amp ph )
  doc" Returns the amplitude and initial-phase (for sin) at FREQ."
  { freq beg dur }
  freq hz->radians { incr }
  0.0 0.0 { sw cw }
  beg #f #f 1 #f make-sample-reader { reader }
  dur 0 do
    reader next-sample { samp }
    i incr c* csin samp c* sw c+ to sw
    i incr c* ccos samp c* cw c+ to sw
  loop
  sw sw c* cw cw c* c+ csqrt dur c/ 2.0 c* ( amp )
  cw sw catan2                             ( ph )
;

hide
: goert-cb { y0 y1 y2 cs -- prc; y self -- f }
  1 proc-create y0 , y1 , y2 , cs , ( prc )
 does> { y self -- f }
  self 1 cells + @ self 2 cells + ! ( y2 = y1 )
  self @ self 1 cells + !           ( y1 = y0 )
  self 1 cells + @ { y1 }
  self 2 cells + @ { y2 }
  self 3 cells + @ { cs }
  y1 cs c* y2 c- y c+ self ! ( y0 = ... )
  #f
;
set-current
: goertzel <{ freq :optional beg 0 dur #f -- amp }>
  doc" Returns the amplitude of the FREQ spectral component."
  #f srate { sr }
  0.0 0.0 0.0 { y0 y1 y2 }
  two-pi freq f* sr f/ { rfreq }
  rfreq fcos f2* { cs }
  dur unless #f #f #f frames to dur then
  y0 y1 y2 cs goert-cb beg dur #f #f #f scan-channel drop
  0.0  rfreq fnegate make-rectangular cexp y1 c* y0 c+ magnitude
;
previous

: make-spencer-filter ( -- gen )
  doc" It's a version of make-fir-filter; \
it returns one of the standard smoothing filters from the era when computers were human beings."
  #( -3 -6 -5 3 21 46 67 74 67 46 21 3 -5 -6 -3 ) { lst }
  :xcoeffs lst 0.0 make-vct map! *key* 320.0 f/ end-map :order 15 make-fir-filter
;

\ ;;; -------- any-random
\ ;;;
\ ;;; arbitrary random number distributions via the "rejection method"

: any-random <{ amount :optional en #f -- r }>
  amount f0= if
    0.0
  else
    en if
      en envelope-last-x { x1 }
      0.0 0.0 { x y }
      begin
	x1 random to x
	1.0 random to y
	c-g?  y  x en 1.0 envelope-interp f<= ||
      until
      x
    else
      amount random
    then
  then
;
: gaussian-distribution ( s -- en )
  { s }
  '() { en }
  2.0 s s f* f* { den }
  0.0 -4.0 { x y }
  21 0 do
    x en cons to en
    y y f* den f/ fexp en cons to en
    x 0.05 f+ to x
    y 0.40 f* to y
  loop
  en list-reverse
;
: pareto-distribution ( a -- en )
  { a }
  '() { en }
  1.0 a 1.0 f+ f** a f/ { scl }
  0.0 1.0 { x y }
  21 0 do
    x en cons to en
    a  y a 1.0 f+ f**  f/ scl f* en cons to en
    x 0.05 f+ to x
    y 0.20 f* to y
  loop
  en list-reverse
;
\ lambda: <{ y -- val }> 1.0 '( 0 1 1 1 )          any-random ; map-channel \ uniform distribution
\ lambda: <{ y -- val }> 1.0 '( 0 0 0.95 0.1 1 1 ) any-random ; map-channel \ mostly toward 1.0
\ 1.0 gaussian-distribution value g lambda: <{ y -- val }> 1.0 g any-random ; map-channel
\ 1.0 pareto-distribution   value g lambda: <{ y -- val }> 1.0 g any-random ; map-channel

\ ;;; this is the inverse integration function used by CLM to turn a
\ ;;; distribution function into a weighting function

: inverse-integrate <{ dist :optional data-size 512 e-size 50 -- vct }>
  '() { en }
  dist cadr exact->inexact dup { sum first-sum }
  dist car { x0 }
  dist envelope-last-x { x1 }
  x1 x0 f- e-size f/ { xincr }
  x0 { x }
  e-size 0 ?do
    sum en cons to en
    x en cons to en
    x dist 1.0 envelope-interp sum f+ to sum
    x xincr f+ to x
  loop
  en cadr first-sum f- data-size 1- f/ { incr }
  en list-reverse to en
  first-sum to x
  data-size 0.0 make-vct map!
    x en 1.0 envelope-interp
    x incr f+ to x
  end-map ( data )
;
: gaussian-envelope ( s -- en )
  { s }
  '() { en }
  2.0 s s f* f* { den }
  -1.0 -4.0 { x y }
  21 0 do
    x en cons to en
    y y f* den f/ fexp en cons to en
    x 0.1 f+ to x
    y 0.4 f* to y
  loop
  en list-reverse
;
\ :envelope 1.0 gaussian-envelope make-rand

\ ;;; ---------------- Julius Smith stuff ----------------
\ ;;;
\ ;;; these are from "Mathematics of the DFT", W3K Pubs

hide
: chm-cb { sum -- prc; y self -- f }
  1 proc-create sum , ( prc )
 does> { y self -- f }
  y self +! ( sum += y )
  #f
;
set-current
: channel-mean <{ :optional snd #f chn #f -- val }>   \ <f, 1> / n
  doc" Returns the average of the samples in the given channel: <f,1>/n."
  0.0 { sum }
  snd chn #f frames { len }
  sum chm-cb 0 len snd chn #f scan-channel drop
  sum len f/
;
previous

hide
: chte-cb { sum -- prc; y self -- f }
  1 proc-create sum , ( prc )
 does> { y self -- f }
  y y f* self +! ( sum += y )
  #f
;
set-current
: channel-total-energy <{ :optional snd #f chn #f -- val }>   \ <f, f>
  doc" Returns the sum of the squares of all the samples in the given channel: <f,f>."
  0.0 { sum }
  snd chn #f frames { len }
  sum chte-cb 0 len snd chn #f scan-channel drop
  sum
;
previous

: channel-average-power <{ :optional snd #f chn #f -- val }> \ <f, f> / n
  doc" Returns the average power in the given channel: <f,f>/n."
  snd chn channel-total-energy snd chn #f frames f/
;
: channel-rms <{ :optional snd #f chn #f -- val }> \ sqrt(<f, f> / n)
  doc" Returns the RMS value of the samples in the given channel: sqrt(<f,f>/n)."
  snd chn channel-average-power fsqrt
;
: channel-variance <{ :optional snd #f chn #f -- val }> \ <f, f> - (<f, 1> / n) ^ 2 with quibbles
  doc" Returns the sample variance in the given channel: <f,f>-((<f,1>/ n)^2."
  snd chn #f frames { len }
  len len 1- f/ snd chn channel-mean f* { mu }
  snd chn channel-total-energy { P }
  P mu mu f* f-
;
: channel-norm <{ :optional snd #f chn #f -- val }> \ sqrt(<f, f>)
  doc" Returns the norm of the samples in the given channel: sqrt(<f,f>)."
  snd chn channel-total-energy fsqrt
;

hide
: chlp-cb { sum p -- prc; y self -- f }
  1 proc-create sum , p , ( prc )
 does> { y self -- f }
  y fabs self cell+ @ ( p ) f** self +! ( sum += y ** p )
;
set-current
: channel-lp <{ p :optional snd #f chn #f -- val }>
  doc" Returns the Lp norm of the samples in the given channel."
  0.0 { sum }
  snd chn #f frames { len }
  sum p chlp-cb 0 len snd chn #f scan-channel drop
  sum p 1/f f**
;
previous

hide
: chlpinf-cb { mx -- prc; y self -- f }
  1 proc-create mx , ( prc )
 does> { y self -- f }
  y fabs self @ fmax self !
;
set-current
: channel-lp-inf <{ :optional snd #f chn #f -- val }>
  doc" Returns the maxamp in the given channel (the name is just math jargon for maxamp)."
  0.0 { mx }
  snd chn #f frames { len }
  mx chlpinf-cb 0 len snd chn #f scan-channel drop
  mx
;
previous

: channel2-inner-product ( s1 c1 s2 c2 -- val )	  \ <f, g>
  doc" Returns the inner-product of the two channels: <f,g>."
  { s1 c1 s2 c2 }
  0.0 { sum }
  0 s1 c1 1 #f make-sample-reader { r1 }
  0 s2 c2 1 #f make-sample-reader { r2 }
  0.0 ( sum )
  s1 c1 #f frames 0 ?do r1 next-sample r2 next-sample f* f+ ( sum += ... ) loop
  ( sum )
;
: channel2-angle ( s1 c1 s2 c2 -- val )	  \ acos(<f, g> / (sqrt(<f, f>) * sqrt(<g, g>)))
  doc" Treats the two channels as vectors, \
returning the 'angle' between them: acos(<f,g>/(sqrt(<f,f>)*sqrt(<g,g>)))"
  { s1 c1 s2 c2 }
  s1 c1 s2 c2 channel2-inner-product { inprod }
  s1 c1 channel-norm { norm1 }
  s2 c2 channel-norm { norm2 }
  inprod norm1 norm2 f* f/ facos
;
: channel2-orthogonal? ( s1 c1 s2 c2 -- f )	  \ <f, g> == 0
  doc" Returns #t if the two channels' inner-product is 0: <f,g>==0."
  { s1 c1 s2 c2 }
  s1 c1 s2 c2 channel2-inner-product fzero?
;
: channel2-coefficient-of-projection ( s1 c1 s2 c2 -- val ) \ s1,c1 = x, s2,c2 = y, <f, g> / <f, f>
  doc" Returns <f,g>/<f,f>."
  { s1 c1 s2 c2 }
  s1 c1 s2 c2 channel2-inner-product s1 c1 channel-total-energy f/
;

\ ;;; -------- end of JOS stuff --------

: channel-distance ( s1 c1 s2 c2 -- val )
  doc" Returns the euclidean distance between the two channels: sqrt(<f-g,f-g>)."
  { s1 c1 s2 c2 }
  0 s1 c1 1 #f make-sample-reader { r1 }
  0 s2 c2 1 #f make-sample-reader { r2 }
  0.0 ( sum )
  s1 c1 #f frames s2 c2 #f frames min 0 ?do
    r1 next-sample r2 next-sample f- dup f* f+ ( sum += diff^2 )
  loop
  ( sum ) fsqrt
;

: periodogram ( N -- )
  doc" Displays an 'N' point Bartlett periodogram of the samples in the current channel."
  { N }
  #f #f #f frames { len }
  0 #f #f 1 #f make-sample-reader { rd }
  N 2* { N2 }
  N2 0.0 make-vct { rl }
  N 0 ?do rl i  rd next-sample  vct-set! drop loop
  N2 0.0 make-vct { im }
  rl im N2 1 mus-fft drop
  N 0.0 make-vct map!
    rl i vct-ref dup f*
    im i vct-ref dup f* f+
  end-map ( average-data ) len N f/ fceil 1/f vct-scale! "average-data"
  0.0 1.0 #f #f #f #f #t #t graph
;

\ ;;; -------- ssb-am friends

hide
: scp-cb { gen -- prc; y self -- val }
  1 proc-create gen , ( prc )
 does> { y self -- val }
  self @ y 0.0 ssb-am
;
set-current
: shift-channel-pitch <{ freq :optional order 40 beg 0 dur #f snd #f chn #f edpos #f -- val }>
  :frequency freq :order order make-ssb-am { gen }
  $" %s %s %s %s %s" '( freq order beg dur get-func-name ) string-format { origin }
  gen scp-cb beg dur snd chn edpos origin map-channel
;
previous

: hz->2pi ( freq -- r ) two-pi f*  #f srate f/ ;

hide
: ssbmc-cb { nmx ssbs bands -- prc; y self -- val }
  1 proc-create nmx , ssbs , bands ,
 does> { y self -- val }
  self           @ { nmx }
  self 1 cells + @ { ssbs }
  self 2 cells + @ { bands }
  0.0 ssbs each ( gen )  bands i array-ref y bandpass  0.0 ssb-am f+ ( sum+=... ) end-each
  ( sum ) dup fabs nmx fmax self ! ( to nmx )
;
: ssbaoe-cb { mx ssbs bands beg dur snd chn edpos -- prc; self -- val }
  0.0 { nmx }
  nmx ssbs bands ssbmc-cb { proc }
  0 proc-create proc , mx , nmx , beg , snd , chn , edpos , ( prc )
 does> { self -- val }
  self           @ { proc }
  self 1 cells + @ { mx }
  self 2 cells + @ { nmx }
  self 3 cells + @ { beg }
  self 4 cells + @ { dur }
  self 5 cells + @ { snd }
  self 6 cells + @ { chn }
  self 7 cells + @ { edpos }
  proc beg dur snd chn edpos #f map-channel drop
  mx nmx f/ beg dur snd chn #f scale-channel
;
set-current
: ssb-bank <{ old-freq new-freq pairs
     :optional order 40 bw 50.0 beg 0 dur #f snd #f chn #f edpos #f -- val }>
  pairs nil make-array { ssbs }
  pairs nil make-array { bands }
  new-freq old-freq f- old-freq f/ { factor }
  snd chn #f maxamp { mx }
  pairs 0 ?do
    i 1.0 f+ { idx }
    old-freq idx f* { aff }
    idx pairs f2* f/ 1.0 f+ bw f* { bwf }
    ssbs  i  :frequency idx factor f* old-freq f* :order 40 make-ssb-am   array-set!
    bands i  aff bwf f- hz->2pi  aff bwf f+ hz->2pi  order make-bandpass  array-set!
  loop
  $" %s %s %s %s %s %s %s %s"
  '( old-freq new-freq pairs order bw beg dur get-func-name ) string-format { origin }
  mx ssbs bands beg dur snd chn edpos ssbaoe-cb  origin  as-one-edit
;
previous

hide
: ssbemc-cb { nmx ssbs bands frenvs -- prc; y self -- val }
  1 proc-create nmx , ssbs , bands , frenvs ,
 does> { y self -- val }
  self           @ { nmx }
  self 1 cells + @ { ssbs }
  self 2 cells + @ { bands }
  self 3 cells + @ { frenvs }
  0.0 ssbs each ( gen )
    bands i array-ref y bandpass  frenvs i array-ref env  ssb-am f+ ( sum+=... )
  end-each
  ( sum ) dup fabs nmx fmax self ! ( to nmx )
;
: ssbeaoe-cb { mx ssbs bands frenvs beg dur snd chn edpos -- prc; self -- val }
  0.0 { nmx }
  nmx ssbs bands frenvs ssbemc-cb { proc }
  0 proc-create proc , mx , nmx , beg , snd , chn , edpos , ( prc )
 does> { self -- val }
  self           @ { proc }
  self 1 cells + @ { mx }
  self 2 cells + @ { nmx }
  self 3 cells + @ { beg }
  self 4 cells + @ { dur }
  self 5 cells + @ { snd }
  self 6 cells + @ { chn }
  self 7 cells + @ { edpos }
  proc beg dur snd chn edpos #f map-channel drop
  mx nmx f/ beg dur snd chn #f scale-channel
;
set-current
: ssb-bank-env <{ old-freq new-freq freq-env pairs
     :optional order 40 bw 50.0 beg 0 dur #f snd #f chn #f edpos #f -- val }>
  pairs nil make-array { ssbs }
  pairs nil make-array { bands }
  pairs nil make-array { frenvs }
  new-freq old-freq f- old-freq f/ { factor }
  snd chn #f maxamp { mx }
  snd chn #f frames 1- { len }
  pairs 0 ?do
    i 1.0 f+ { idx }
    old-freq idx f* { aff }
    idx pairs f2* f/ 1.0 f+ bw f* { bwf }
    ssbs  i  :frequency idx factor f* old-freq f* :order 40 make-ssb-am   array-set!
    bands i  aff bwf f- hz->2pi  aff bwf f+ hz->2pi  order make-bandpass  array-set!
    :envelope freq-env :scaler idx hz->radians :end len make-env
    frenvs i  rot array-set!
  loop
  $" %s %s %s %s %s %s %s %s %s"
  '( old-freq new-freq freq-env pairs order bw beg dur get-func-name ) string-format { origin }
  mx ssbs bands frenvs beg dur snd chn edpos ssbeaoe-cb  origin  as-one-edit
;
previous

\ ;; echoes with each echo at a new pitch via ssb-am etc

: make-transposer <{ old-freq new-freq pairs :optional order 40 bw 50.0 -- gen }>
  new-freq old-freq f- old-freq f/ { factor }
  pairs nil make-array map! i 1+ factor f* old-freq f* make-ssb-am end-map { ssbs }
  pairs nil make-array map!
    i 1.0 f+ { idx }
    idx old-freq f* { aff }
    idx pairs f2* f/ 1.0 f+ bw f* { bwf }
    aff bwf f- hz->radians  aff bwf f+ hz->radians  order  make-bandpass
  end-map { bands }
  '( ssbs bands )
;
: transpose ( gen input -- val )
  { gen input }
  gen car  { ssbs }
  gen cadr { bands }
  0.0 ( sum )
  ssbs each { g }
    g  bands i array-ref input bandpass  0.0 ssb-am f+ ( sum += )
  end-each
  ( sum )
;

: make-fdelay ( len pitch scaler -- prc; y self -- val )
  { len pitch scaler }
  len make-delay { dly }
  440.0 440.0 pitch f* 10 make-transposer { ssb }
  1 proc-create dly , ssb , scaler , ( prc )
 does> { y self -- val }
  self @ { dly }
  self cell+ @ { ssb }
  self 2 cells + @ { scaler }
  dly  ssb  dly 0.0 tap  transpose scaler f* y f+  0.0 delay
;
: fdelay ( gen input -- val )
  { gen input }
  gen '( input ) run-proc
;
: transposed-echo ( pitch scaler secs -- val )
  { pitch scaler secs }
  #f srate secs f* fround->s pitch scaler make-fdelay 0 #f #f #f #f #f map-channel
;

\ ;;; vct|channel|spectral-polynomial

: vct-polynomial ( v coeffs -- vct )
  { v coeffs }
  v vct-length coeffs last-ref make-vct { new-v }
  coeffs vct-length 2- 0 ?do
    new-v v vct-multiply! coeffs i vct-ref vct-offset! drop
  -1 +loop
  new-v
;
: channel-polynomial <{ coeffs :optional snd #f chn #f -- vct }>
  snd chn #f frames { len }
  $" %S %s" '( coeffs get-func-name ) string-format { origin }
  0 len snd chn #f channel->vct coeffs vct-polynomial 0 len snd chn #f origin vct->channel
;
\ vct( 0.0 0.5 )         channel-polynomial == x*0.5
\ vct( 0.0 1.0 1.0 1.0 ) channel-polynomial == x*x*x + x*x + x

\ ;;; convolution -> * in freq

: spectral-polynomial <{ coeffs :optional snd #f chn #f -- vct }>
  snd chn #f frames { len }
  0 len snd chn #f channel->vct { sound }
  coeffs vct-length { num-coeffs }
  num-coeffs 2 < if
    len
  else
    2.0  num-coeffs 1.0 f- len f* flog 2.0 flog f/ fceil  f** fround->s
  then { fft-len }
  fft-len 0.0 make-vct { rl1 }
  fft-len 0.0 make-vct { rl2 }
  fft-len 0.0 make-vct { new-sound }
  coeffs 0 vct-ref f0> if
    coeffs 0 vct-ref { dither }
    new-sound map! dither mus-random end-map drop
  then
  num-coeffs 1 > if
    new-sound  sound vct-copy coeffs 1 vct-ref vct-scale!  vct-add! drop
    num-coeffs 2 > if
      snd chn #f maxamp { peak }
      rl1 0.0 vct-scale! sound vct-add! drop
      0.0 { pk }
      num-coeffs 2 ?do
	rl1  rl2 0.0 vct-scale! sound vct-add!  fft-len  convolution drop
	rl1 vct-peak to pk
	new-sound  rl1 vct-copy coeffs i vct-ref peak f* pk f/  vct-scale!  vct-add! drop
      loop
      new-sound vct-peak to pk
      new-sound peak pk f/ vct-scale! drop
    then
  then
  $" %S %s" '( coeffs get-func-name ) string-format { origin }
  new-sound 0  num-coeffs 1- len * len max  snd chn #f origin vct->channel
;

\ ;;; ----------------
\ ;;; SCENTROID
\ ;;;
\ ;;; by Bret Battey
\ ;;; Version 1.0 July 13, 2002
\ ;;; translated to Snd/Scheme Bill S 19-Jan-05
\ ;;;
\ ;;; Returns the continuous spectral centroid envelope of a sound.
\ ;;; The spectral centroid is the "center of gravity" of the spectrum, and it
\ ;;; has a rough correlation to our sense of "brightness" of a sound. 
\ ;;;
\ ;;; [Beauchamp, J., "Synthesis by spectral amplitude and 'brightness' matching
\ ;;; analyzed musical sounds". Journal of Audio Engineering Society 30(6), 396-406]
\ ;;;
\ ;;; The formula used is:
\ ;;;    C = [SUM<n=1toj>F(n)A(n)] / [SUM<n=1toj>A(n)]
\ ;;;    Where j is the number of bins in the analysis, 
\ ;;;    F(n) is the frequency of a given bin,
\ ;;;    A(n) is the magnitude of the given bin.
\ ;;;
\ ;;; If a pitch envelope for the analyzed sound is available, the results
\ ;;; of SCENTROID can be used with the function NORMALIZE-CENTROID, below, 
\ ;;; to provide a "normalized spectral centroid". 
\ ;;;
\ ;;; DB-FLOOR -- Frames below this decibel level (0 dB = max) will be discarded
\ ;;; and returned with spectral centroid = 0
\ ;;;
\ ;;; RFREQ -- Rendering frequency. Number of  measurements per second.
\ ;;;
\ ;;; FFTSIZE -- FFT window size. Must be a power of 2. 4096 is recommended.

: scentroid <{ file :key beg 0.0 dur #f db-floor -40.0 rfreq 100.0 fftsize 4094 -- vals }>
  doc" Returns the spectral centroid envelope of a sound; \
RFREQ is the rendering frequency, the number of measurements per second; \
DB-FLOOR is the level below which data will be ignored."
  file find-file to file
  file false? if 'no-such-file '( get-func-name file ) fth-throw then
  file mus-sound-srate { fsr }
  fsr rfreq f/ fround->s { incrsamps }
  beg fsr f* fround->s { start }
  dur if dur fsr f* else file mus-sound-frames beg b- then { end }
  fftsize 0.0 make-vct { fdr }
  fftsize 0.0 make-vct { fdi }
  end start b- incrsamps b/ 1+ { windows }
  windows 0.0 make-vct { results }
  fftsize 2/ { fft2 }
  fsr fftsize f/ fround->s { binwidth }
  file make-readin { rd }
  0 { loc }
  end start ?do
    rd i set-mus-location drop
    0.0 { sum-of-squares }
    fdr map!
      rd readin { val }
      val dup f* sum-of-squares f+ to sum-of-squares
      val
    end-map
    sum-of-squares fftsize f/ fsqrt linear->db db-floor f>= if
      0.0 0.0 { numsum densum }
      fdi 0.0 vct-fill! drop
      fdr fdi fftsize 1 mus-fft drop
      fdr fdi rectangular->polar drop
      fft2 0 ?do
	fdr i vct-ref binwidth f* i f* numsum f+ to numsum
	fdr i vct-ref densum f+ to densum
      loop
      results loc numsum densum f/ vct-set! drop
    then
    loc 1+ to loc
  incrsamps +loop
  results
;

\ ;;; ----------------
\ ;;;
\ ;;; invert-filter inverts an FIR filter
\ ;;;
\ ;;; say we previously filtered a sound via (filter-channel (vct .5 .25 .125))
\ ;;;   and we want to undo it without using (undo):
\ ;;;   (filter-channel (invert-filter (vct .5 .25 .125)))
\ ;;;
\ ;;; there are a million gotchas here.  The primary one is that the inverse filter
\ ;;;   can "explode" -- the coefficients can grow without bound.  For example, any
\ ;;;   filter returned by spectrum->coeffs above will be a problem (it always returns
\ ;;;   a "linear phase" filter).

: invert-filter ( fcoeffs -- res )
  doc" Tries to return an inverse filter to undo the effect of the FIR filter coeffs."
  { fcoeffs }
  fcoeffs length { flen }
  32 flen + 0.0 make-vct { coeffs }
  coeffs length { order }
  flen 0 do coeffs i  fcoeffs i vct-ref  vct-set! drop loop
  order 0.0 make-vct { nfilt }
  nfilt 0 coeffs 0 vct-ref 1/f vct-set! drop
  order 1 ?do
    i { kk }
    0.0 ( sum )
    i 0 ?do
      nfilt i vct-ref coeffs kk vct-ref f* f+ ( sum += ... )
      kk 1- to kk
    loop
    ( sum ) coeffs 0 vct-ref fnegate f/ nfilt i -rot vct-set! drop
  loop
  nfilt
;

\ ;;; ----------------
\ ;;;
\ ;;; Volterra filter
\ ;;;
\ ;;; one of the standard non-linear filters
\ ;;; this version is taken from Monson Hayes "Statistical DSP and Modeling"
\ ;;;   it is a slight specialization of the form mentioned by J O Smith and others

: make-volterra-filter ( acoeffs bcoeffs -- gen )
  doc" Returns a list for use with volterra-filter, \
producing one of the standard non-linear filters."
  { acoeffs bcoeffs }
  '( acoeffs
     bcoeffs
     acoeffs length bcoeffs length max 0.0 make-vct )
;
: volterra-filter ( flt x -- val )
  doc" Takes FLT, a list returned by make-volterra-filter, \
and an input X, and returns the (non-linear filtered) result."
  { flt x }
  flt car   { as }
  flt cadr  { bs }
  flt caddr { xs }
  as length { x1len }
  bs length { x2len }
  xs length { xlen }
  xs xlen 1- xlen 2- #t vct-move! drop
  xs 0 x vct-set! drop
  as xs x1len dot-product ( sum )
  x2len 0 ?do
    x2len 0 ?do
      bs i vct-ref xs j vct-ref f* xs i vct-ref f* f+ ( sum += ... )
    loop
  loop
  ( sum )
;
\ vct( 0.5 0.1 ) vct( 0.3 0.2 0.1 ) make-volterra-filter value flt
\ lambda: <{ y -- val }> flt y volterra-filter ; map-channel

\ ;;; ----------------
\ ;;;
\ ;;; moving-max generator (the max norm, or uniform norm, infinity-norm)

: make-moving-max <{ :optional size 128 -- gen }>
  doc" Returns a moving-max generator.  \
The generator keeps a running window of the last SIZE inputs, returning the maxamp in that window."
  save-stack { s }
  size make-delay { gen }
  s restore-stack
  gen 0.0 set-mus-scaler drop
  gen
;
: moving-max ( gen y -- scl )
  doc" Returns the maxamp in a moving window over the last few inputs."
  { gen y }
  y fabs { absy }
  gen absy 0.0 delay { mx }
  absy gen mus-scaler f>= if
    gen absy set-mus-scaler
  else
    mx gen mus-scaler f>= if
      gen gen mus-data vct-peak set-mus-scaler
    else
      gen mus-scaler
    then
  then
;

\ ;;; ----------------
\ ;;;
\ ;;; moving-sum generator (the sum norm or 1-norm)

: make-moving-sum <{ :optional size 128 -- gen }>
  doc" Returns a moving-sum generator.  \
The generator keeps a running window of the last SIZE inputs, \
returning the sum of the absolute values of the samples in that window."
  size make-moving-average { gen }
  gen 1.0 set-mus-increment drop
  gen
;
: moving-sum ( gen y -- val )
  doc" Returns the sum of the absolute values in a moving window over the last few inputs."
  ( gen y ) fabs moving-average
;

\ ;;; ----------------
\ ;;;
\ ;;; moving-rms generator

: make-moving-rms <{ :optional size 128 -- gen }>
  doc" Returns a moving-rms generator.  \
The generator keeps a running window of the last SIZE inputs, \
returning the rms of the samples in that window."
  size make-moving-average
;
: moving-rms ( gen y -- val )
  doc" Returns the rms of the values in a window over the last few inputs."
  ( gen y ) dup f* moving-average fsqrt
;

\ ;;; ----------------
\ ;;;
\ ;;; moving-length generator (euclidean norm or 2-norm)

: make-moving-length <{ :optional size 128 -- gen }>
  doc" Returns a moving-length generator.  \
The generator keeps a running window of the last SIZE inputs, \
returning the euclidean length of the vector in that window."
  size make-moving-average { gen }
  gen 1.0 set-mus-increment drop
  gen
;
: moving-length ( gen y -- val )
  doc" Returns the length of the values in a window over the last few inputs."
  ( gen y ) dup f* moving-average fsqrt
;

\ ;;; ----------------
\ ;;;
\ ;;; harmonicizer (each harmonic is split into a set of harmonics via Chebyshev polynomials)
\ ;;;   obviously very similar to ssb-bank above, but splits harmonics individually,
\ ;;;   rather than pitch-shifting them

hide
: harm-mc-cb { bands avgs peaks pcoeffs flt new-mx -- prc; y self -- val }
  1 proc-create 40 ( ctr ) , bands , avgs , peaks , pcoeffs , flt , new-mx , ( prc )
 does> { y self -- val }
  self @ { ctr }
  self cell+ @ { bands }
  self 2 cells + @ { avgs }
  self 3 cells + @ { peaks }
  self 4 cells + @ { pcoeffs }
  self 5 cells + @ { flt }
  self 6 cells + @ { new-mx }
  0.0 ( sum )
  bands each { bd }
    bd y bandpass { sig }
    peaks i array-ref  sig  moving-max { mx }
    avgs  i array-ref  mx f0> if 100.0 mx 1/f fmin else 0.0 then  moving-average { amp }
    amp f0> if pcoeffs amp sig f* polynomial  mx f* f+ ( sum += ... ) then
  end-each
  flt swap ( sum ) filter { val }
  val fabs new-mx fmax self 6 cells + ! ( to new-mx )
  ctr 0= if
    val
  else
    -1 self +! ( ctr-- )
    0.0
  then
;
: harm-aoe-cb { bands avgs peaks pcoeffs flt old-mx beg dur snd chn edpos -- prc; self -- val }
  0 proc-create
  0.0 ( new-mx ) ,
  bands , avgs , peaks , pcoeffs , flt , old-mx ,
  beg , dur , snd , chn , edpos ,
  ( prc )
 does> { self -- val }
  self @ { new-mx }
  self cell+ @ { bands }
  self 2 cells + @ { avgs }
  self 3 cells + @ { peaks }
  self 4 cells + @ { pcoeffs }
  self 5 cells + @ { flt }
  self 6 cells + @ { old-mx }
  self 7 cells + @ { beg }
  self 8 cells + @ { dur }
  self 9 cells + @ { snd }
  self 10 cells + @ { chn }
  self 11 cells + @ { edpos }
  bands avgs peaks pcoeffs flt new-mx harm-mc-cb  beg dur snd chn edpos map-channel ( retval )
  new-mx f0> if
    old-mx new-mx f/ beg dur snd chn #f scale-channel drop
  then
;
set-current
: harmonicizer <{ freq coeffs pairs
     :optional order 40 bw 50.0 beg 0 dur #f snd #f chn #f edpos #f -- val }>
  doc" Splits out each harmonic and replaces it with the spectrum given in coeffs."
  pairs nil make-array map!
    i 1.0 f+ { idx }
    idx freq f/ { aff }
    idx pairs f2* f/ 1.0 f+ bw f* { bwf }
    aff bwf f- hz->2pi  aff bwf f+ hz->2pi  order  make-bandpass
  end-map { bands }
  pairs nil make-array map! 128 make-moving-average end-map { avgs }
  pairs nil make-array map! 128 make-moving-max     end-map { peaks }
  coeffs mus-chebyshev-first-kind partials->polynomial { pcoeffs }
  2 vct( 1 -1 ) vct( 0 -0.9 ) make-filter { flt }
  snd chn #f maxamp { old-mx }
  bands avgs peaks pcoeffs flt old-mx beg dur snd chn edpos harm-aoe-cb  get-func-name as-one-edit
;
previous

\ ;;; ----------------
\ ;;;
\ ;;; linear sampling rate conversion

hide
: lsc-ws-cb { rd sr -- prc; self -- f }
  0 proc-create rd next-sample , rd next-sample , 0.0 , rd , sr , ( prc )
 does> { self -- f }
  self           @ { last }
  self   cell+   @ { next }
  self 2 cells + @ { intrp }
  self 3 cells + @ { rd }
  self 4 cells + @ { sr }
  0 { samp }
  begin
    intrp { pos }
    pos 1.0 f>= if
      pos fround->s { num }
      num 0 ?do
	next to last
	rd next-sample to next
      loop
      pos num f- to pos
    then
    pos sr f+ to intrp
    samp  next last f- pos f* last f+  0 *output*  out-any drop
    samp 1+ to samp
    rd sample-reader-at-end?
  until
  #f
;
set-current
: linear-src-channel <{ sr :optional snd #f chn #f -- file }>
  doc" Performs sampling rate conversion using linear interpolation."
  0 snd chn 1 #f make-sample-reader { rd }
  rd sr lsc-ws-cb :output snd-tempnam :srate snd srate with-sound { tempfile }
  tempfile mus-sound-frames { len }
  0 len 1- tempfile snd chn #t ( trunc ) get-func-name 0 #f ( edpos ) #t ( autodel ) set-samples
;
previous

\ ;;; Mathews/Smith High-Q filter as described in http://ccrma.stanford.edu/~jos/smac03maxjos/

: make-mfilter <{ :key decay 0.99 frequency 1000.0 -- gen }>
  #{ :decay decay
     :frequency frequency
     :eps frequency pi f* mus-srate f/ fsin f2*
     :xn 0.0
     :yn 0.0 }
;
: mfilter <{ m :optional x-input 0.0 y-input 0.0 -- val }>
  m :xn hash-ref  m :eps hash-ref m :yn hash-ref f*  f-  m :decay hash-ref f*  x-input f+ { xn1 }
  m :eps hash-ref xn1 f*  m :yn hash-ref f+  m :decay hash-ref f*  y-input f+ { yn1 }
  m :xn xn1 hash-set!
  m :yn yn1 hash-set!
  yn1
;
0 [if]
: m-ws-cb ( file -- prc; self -- )
  { file }
  file find-file to file
  file false? if 'no-such-file '( get-func-name file ) fth-throw then
  0 file 0 1 #f make-sample-reader { rd }
  make-mfilter { m }
  0 proc-create rd , m , ( prc )
 does> { self -- }
  self       @ { rd }
  self cell+ @ { m }
  10000 0 do i  m rd next-sample 0.1 f* 0.0 mfilter  *output* outa drop loop
;
"now.snd" m-ws-cb with-sound
[then]

\ ;;; -------- spectrum displayed in various frequency scales

'snd-nogui provided? [unless]
  hide
  0 value bark-fft-size
  0 value bark-tick-function

  : bark ( r1 -- r2 )
    { f }
    f 7500.0 f/ { f2 }
    f2 f2 f* fatan 3.5 f*  0.00076 f f* fatan 13.5 f*  f+
  ;
  : mel ( r1 -- r2 )
    { f }
    f 700.0 f/ 1.0 f+ flog 1127.0 f*
  ;
  : erb ( r1 -- r2 )
    { f }
    f 312.0 f+ f 14675.0 f+ f/ flog 11.17 f* 43.0 f+
  ;
  : display-bark-fft-cb <{ snd chn -- f }>
    snd chn left-sample  { ls }
    snd chn right-sample { rs }
    2.0  rs ls - 1+ flog 2.0 flog f/ fceil  f** fround->s { fftlen }
    fftlen 0> if
      ls fftlen snd chn #f channel->vct { data }
      snd chn transform-normalization dont-normalize <> { normalized }
      #t { linear }
      data vct? if
	data
	snd chn fft-window fftlen linear
	snd chn fft-window-beta #f normalized snd-spectrum { fft }
	fft vct? if
	  snd srate { sr }
	  fft vct-peak { mx }
	  fft length { data-len }
	  \ bark settings
	  20.0 bark floor { bark-low }
	  sr f2/ bark fceil { bark-high }
	  data-len bark-high bark-low f- f/ { bark-frqscl }
	  data-len 0.0 make-vct { bark-data }
	  \ mel settings
	  20.0 mel floor { mel-low }
	  sr f2/ bark fceil { mel-high }
	  data-len mel-high mel-low f- f/ { mel-frqscl }
	  data-len 0.0 make-vct { mel-data }
	  \ erb settings
	  20.0 erb floor { erb-low }
	  sr f2/ bark fceil { erb-high }
	  data-len erb-high erb-low f- f/ { erb-frqscl }
	  data-len 0.0 make-vct { erb-data }
	  fftlen to bark-fft-size
	  fft each { val }
	    i fftlen f/ sr f* { frq }
	    frq bark bark-low f- bark-frqscl f* fround->s { bark-bin }
	    frq mel  mel-low  f- mel-frqscl  f* fround->s { mel-bin }
	    frq erb  erb-low  f- erb-frqscl  f* fround->s { erb-bin }
	    bark-bin 0>=
	    bark-bin data-len < && if bark-data bark-bin val object-set+! then
	    mel-bin 0>=
	    mel-bin  data-len < && if mel-data  mel-bin  val object-set+! then
	    erb-bin 0>=
	    erb-bin  data-len < && if erb-data  erb-bin  val object-set+! then
	  end-each
	  normalized if
	    bark-data vct-peak { bmx }
	    mel-data  vct-peak { mmx }
	    erb-data  vct-peak { emx }
	    mx bmx f- fabs 0.01 f> if bark-data mx bmx f/ vct-scale! drop then
	    mx mmx f- fabs 0.01 f> if mel-data  mx mmx f/ vct-scale! drop then
	    mx emx f- fabs 0.01 f> if erb-data  mx emx f/ vct-scale! drop then
	  then
	  '( bark-data mel-data erb-data )
	  "ignored"
	  20.0 sr f2/
	  0.0 normalized if 1.0 else data-len snd chn y-zoom-slider * then
	  snd chn
	  #f show-bare-x-axis graph drop
	then
      then
    then
    #f
  ;
  : scale-pos { axis-x0 axis-x1 sr2 f scale -- n }
    20.0 scale execute { b20 }
    axis-x1 axis-x0 f-  f scale execute b20 f- f*
    sr2 scale execute b20 f-  f/ axis-x0 f+ fround->s
  ;
  : draw-bark-ticks { axis-x0 axis-x1 axis-y0 major-y0 minor-y0 tick-y0 sr2 bark-function snd chn }
    2 snd-font ?dup-if snd chn copy-context set-current-font drop then
    axis-x0 tick-y0 axis-x0 major-y0 snd chn copy-context draw-line drop
    axis-x0 axis-x1 sr2 1000.0  bark-function scale-pos { i1000 }
    axis-x0 axis-x1 sr2 10000.0 bark-function scale-pos { i10000 }
    i1000  tick-y0 i1000  major-y0 snd chn copy-context draw-line drop
    i10000 tick-y0 i10000 major-y0 snd chn copy-context draw-line drop
    "20"    axis-x0     major-y0 snd chn copy-context draw-string drop
    "1000"  i1000  12 - major-y0 snd chn copy-context draw-string drop
    "10000" i10000 24 - major-y0 snd chn copy-context draw-string drop
    $" fft size: %d" '( bark-fft-size ) string-format
    axis-x0 10 + axis-y0 snd chn copy-context draw-string drop
    1000 100 do
      axis-x0 axis-x1 sr2 i bark-function scale-pos { i100 }
      i100 tick-y0 i100 minor-y0 snd chn copy-context draw-line drop
    100 +loop
    10000 2000 do
      axis-x0 axis-x1 sr2 i bark-function scale-pos { i1000 }
      i1000 tick-y0 i1000 minor-y0 snd chn copy-context draw-line drop
    1000 +loop
  ;
  : make-bark-labels <{ snd chn -- f }>
    snd chn copy-context foreground-color { old-foreground-color }
    snd chn lisp-graph axis-info { axinfo }
    axinfo 10 list-ref { axis-x0 }
    axinfo 12 list-ref { axis-x1 }
    axinfo 13 list-ref { axis-y0 }
    axinfo 11 list-ref { axis-y1 }
    15 { label-height }
    8 { char-width }
    snd srate 2/ { sr2 }
    6 { minor-tick-len }
    12 { major-tick-len }
    axis-y1 { tick-y0 }
    axis-y1 minor-tick-len + { minor-y0 }
    axis-y1 major-tick-len + { major-y0 }
    3 snd-font { bark-label-font }
    axis-x1 axis-x0 f- 0.45 f* axis-x0 f+ fround->s { label-pos }

    \ bark label/ticks
    bark-tick-function 0= if
      axis-x0 axis-x1 axis-y0 major-y0 minor-y0 tick-y0 sr2 <'> bark snd chn draw-bark-ticks
    then
    bark-label-font ?dup-if snd chn copy-context set-current-font drop then
    "bark," label-pos axis-y1 label-height + snd chn copy-context draw-string drop

    \ mel label/ticks
    2 snd-color snd chn copy-context set-foreground-color drop
    bark-tick-function 1 = if
      axis-x0 axis-x1 axis-y0 major-y0 minor-y0 tick-y0 sr2 <'> mel snd chn draw-bark-ticks
    then
    bark-label-font ?dup-if snd chn copy-context set-current-font drop then
    "mel," label-pos char-width 6 * + axis-y1 label-height + snd chn copy-context draw-string drop

    \ erb label/ticks
    4 snd-color snd chn copy-context set-foreground-color drop
    bark-tick-function 2 = if
      axis-x0 axis-x1 axis-y0 major-y0 minor-y0 tick-y0 sr2 <'> erb snd chn draw-bark-ticks
    then
    bark-label-font ?dup-if snd chn copy-context set-current-font drop then
    "erb" label-pos char-width 11 * + axis-y1 label-height + snd chn copy-context draw-string drop

    old-foreground-color snd chn copy-context set-foreground-color
  ;
  : choose-bark-ticks <{ snd chn button state x y axis -- f }>
    axis lisp-graph = if
      bark-tick-function 1+ to bark-tick-function
      bark-tick-function 2 > if 0 to bark-tick-function then
      snd chn update-lisp-graph
    else
      #f
    then
  ;
  set-current
  : display-bark-fft <{ :optional off #f -- }>
    off unless
      lisp-graph-hook       <'> display-bark-fft-cb add-hook!
      after-lisp-graph-hook <'> make-bark-labels    add-hook!
      mouse-click-hook      <'> choose-bark-ticks   add-hook!
      sounds each { snd }
	snd channels 0 ?do snd i update-lisp-graph drop loop
      end-each
    else
      lisp-graph-hook       <'> display-bark-fft-cb remove-hook!
      after-lisp-graph-hook <'> make-bark-labels    remove-hook!
      mouse-click-hook      <'> choose-bark-ticks   remove-hook!
      sounds each { snd }
	snd channels 0 ?do #f snd i set-lisp-graph? drop loop
      end-each
    then
  ;
  previous
  : undisplay-bark-fft ( -- ) #t display-bark-fft ;
[then]

\ ;;; -------- lpc-coeffs, lpc-predict

: lpc-coeffs ( data n m -- val )
  doc" Returns M LPC coeffients (in a vector) given N data points in the vct DATA."
  { data n m }
  m 0.0 make-array { d }
  n 0.0 make-array { wk1 }
  n 0.0 make-array { wk2 }
  n 0.0 make-array { wkm }
  wk1   0   data   0  vct-ref  array-set!
  wk2 n 2-  data n 1- vct-ref  array-set!
  n 1- 1 ?do
    wk1 i    data i vct-ref  array-set!
    wk2 i 1- data i vct-ref  array-set!
  loop
  m 0 ?do
    0.0 0.0 { num denom }
    n i - 1- 0 ?do
      wk1 i array-ref wk2 i array-ref f* num f+ to num
      wk1 i array-ref wk2 i array-ref f* dup f* denom f+ to denom
    loop
    denom f0<> if d i num f2* denom f/ array-set! then
    i 0 ?do d i  wkm i array-ref d j array-ref wkm j i - 1- array-ref f* f-  array-set! loop
    i m 1- < if
      i 1+ 0 ?do wkm i  d i array-ref  array-set! loop
      n i - 2- 0 ?do
	wk1 i  wk1 i    array-ref wkm j array-ref wk2 i    array-ref f* f-  array-set!
	wk2 i  wk2 i 1+ array-ref wkm j array-ref wk1 i 1+ array-ref f* f-  array-set!
      loop
    then
  loop
  d
;
: lpc-predict <{ data n coeffs m nf :optional clipped #f -- val }>
  doc" Takes the output of lpc-coeffs (COEFFS, a vector) and the length thereof (M), \
N data points of DATA (a vct), and produces NF new data points (in a vct) as its prediction.  \
If CLIPPED is #t, the new data is assumed to be outside -1.0 to 1.0."
  n 1- { jj }
  m 0.0 make-vct map!
    data jj vct-ref
    jj 1- to jj
  end-map { reg }
  nf 0.0 make-vct map!
    0.0 ( sum ) reg each ( val ) coeffs i array-ref f* f+ ( sum += ... ) end-each { sum }
    0 m 1- do reg i  reg i 1- vct-ref  vct-set! drop -1 +loop
    clipped if
      sum f0> if
	sum  1.0 f< if  1.0 to sum then
      else
	sum -1.0 f> if -1.0 to sum then
      then
    then
    reg 0 sum vct-set! drop
    sum
  end-map ( future )
;

\ ;;; -------- unclip-channel

hide
: uncchn-sc-cb { clip-data unclipped-max -- prc; y self -- f }
  1 proc-create
  clip-data , unclipped-max , 0 ( clip-beg ) , 0 ( samp ) , #f ( in-clip ) ,
  ( prc )
 does> { y self -- f }
  self           @ { clip-data }
  self 1 cells + @ { unclipped-max }
  self 2 cells + @ { clip-beg }
  self 3 cells + @ { samp }
  self 4 cells + @ { in-clip }
  y fabs { absy }
  absy 0.9999 f> if
    in-clip unless
      #t to in-clip
      samp self 2 cells + ! ( to clip-beg )
    then
  else
    absy unclipped-max fmax self 2 cells + ! ( to unclipped-max )
    in-clip if
      #f self 4 cells + ! ( in-clip = #f )
      clip-data '( clip-beg samp 1- ) array-push to clip-data
    then
  then
  1 self 3 cells + +! ( samp++ )
  #f
;
: uncchn-aoe-cb { clip-data max-len snd chn len -- prc; self -- val }
  0 proc-create clip-data , max-len , snd , chn , ( prc )
 does> { self -- val }
  self           @ { clip-data }
  self   cell+   @ { max-len }
  self 2 cells + @ { snd }
  self 3 cells + @ { chn }
  self 4 cells + @ { len }
  32 { min-data-len }
  clip-data each { lst }
    lst car  { clip-beg }
    lst cadr { clip-end }
    clip-end clip-beg = 1+ { clip-len }
    min-data-len clip-len 4 * max { data-len }
    clip-len max-len > if clip-len to max-len then
    data-len { forward-data-len }
    data-len { backward-data-len }
    i 0= if 0 else clip-data i 1- array-ref cadr then { previous-end }
    i clip-data length 3 - < if clip-data i 1+ array-ref car else len then { next-beg }
    clip-beg data-len - previous-end < if clip-beg previous-end - 4 max to forward-data-len then
    clip-len data-len + next-beg > if next-beg clip-end - 4 max to backward-data-len then
    clip-len forward-data-len f2/ fround->s max { forward-predict-len }
    clip-len backward-data-len f2/ fround->s max { backward-predict-len }
    clip-beg forward-data-len - forward-data-len snd chn #f channel->vct { data }
    data forward-data-len
    data forward-data-len forward-predict-len lpc-coeffs
    forward-predict-len clip-len #f lpc-predict { future }
    clip-end 1+ backward-data-len snd chn #f channel->vct vct-reverse! { rdata }
    rdata backward-data-len
    rdata backward-data-len backward-predict-len lpc-coeffs
    backward-predict-len clip-len #f lpc-predict { past }
    clip-len 0.0 make-vct { new-data }
    clip-len 1 > if
      clip-len 1- { jj }
      clip-len 1- { clen }
      new-data map!
	i clen f/ pi f* fcos 1.0 f+ f2/ { sn }
	sn future i vct-ref f*
	1.0 sn f- past jj vct-ref f* f+
	jj 1- to jj
      end-map
    else
      new-data 0
      future 0 vct-ref past 0 vct-ref future 0 vct-ref f0> if fmax else fmin then vct-set! drop
    then
    new-data clip-beg clip-len snd chn #f get-func-name vct->channel drop
  end-each
  #f
;
set-current
: unclip-channel <{ :optional snd #f chn #f -- hash }>
  doc" Looks for clipped portions and tries to reconstruct the original using LPC."
  #() { clip-data }			\ #( '( beg end ) '( ... ) ... )
  0.0 { unclipped-max }
  snd chn #f frames { len }
  clip-data unclipped-max uncchn-sc-cb  0 len snd chn #f scan-channel drop
  clip-data length 0> if
    0 { max-len }
    clip-data max-len snd chn len uncchn-aoe-cb  get-func-name as-one-edit drop
    unclipped-max 0.95 f> if 0.999 to unclipped-max then
    unclipped-max snd chn #f maxamp f/ 0 len snd chn #f scale-channel drop
    #{ :max unclipped-max :clips clip-data length 2/ :max-len max-len }
  else
    #{}
  then
;
previous
: unclip-sound <{ :optional snd #f -- ary-of-hashs }>
  doc" Applies unclip-channel to each channel of SND."
  snd snd-snd to snd
  snd sound? if
    snd channels nil make-array map! snd i unclip-channel end-map ( res )
  else
    'no-such-sound '( get-func-name snd ) fth-throw
  then
;

\ dsp.fs ends here
