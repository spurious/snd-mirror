\ dsp.fs -- dsp.scm|rb --> dsp.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Dec 30 04:52:13 CET 2005
\ Changed: Mon Dec 11 22:39:12 CET 2006

\ Commentary:

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
\ make-asyfm               ( :key frequency 440.0 initial-phase 0.0 ratio 1.0 r 1.0 index 1.0 )
\ asyfm-J                  ( gen input -- val )
\ asyfm-I                  ( gen input -- val )
\ make-cosine-summation    ( :key frequency 440.0 initial-phase 0.0 -- gen )
\ cosine-summation         ( gen r -- val )
\ make-kosine-summation    ( :key frequency 440.0 initial-phase 0.0 -- gen )
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
\ make-hilbert-transform   ( :optional len -- gen )
\ hilbert-transform        ( gen input -- val )
\ 
\ make-butter-high-pass    ( freq -- flt )
\ make-butter-low-pass     ( freq -- flt )
\ make-butter-band-pass    ( freq band -- flt )
\ make-butter-band-reject  ( freq band -- flt )
\ butter                   ( gen sig -- res )

\ Code:

require clm

\ --- src-duration (see src-channel in extsnd.html) ---

: src-duration ( en -- dur )
  doc" ( envelope -- dur )  \
Returns the new duration of a sound after using ENVELOPE for time-varying sampling-rate conversion."
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

\ --- Dolph-Chebyshev window ---

\ formula taken from Richard Lyons, "Understanding DSP"
\ see clm.c for C version (using either GSL's or GCC's complex trig functions)

: dolph ( n gamma -- im )
  doc" ( n gamma -- im )  \
Produces a Dolph-Chebyshev FFT data window of N points using GAMMA as the window parameter."
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

\ this version taken from Julius Smith's "Spectral Audio..." with
\ three changes it does the DFT by hand, and is independent of
\ anything from Snd (fft, vcts etc)

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

\ --- move sound down by n (a power of 2) ---

: down-oct ( n :optional snd chn -- v )
  doc" ( n :optional snd chn -- vct )  Moves a sound down by power of 2 n."
  <{ n :optional snd #f chn #f }>
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

: stretch-sound-via-dft ( factor :optional snd chn -- )
  <{ factor :optional snd #f chn #f }>
  snd chn #f frames { n }
  n f2/ floor f>s { n2 }
  n factor f* fround f>s { out-n }
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

: testunif ( mass xspring damp -- )
  { mass xspring damp }
  128 { size }
  size 0.0 make-vct { x0 }
  size 0.0 make-vct { x1 }
  size 0.0 make-vct { x2 }
  12 0 do
    x1
    size 4 / 6 - i +
    two-pi i f* 12.0 f/ fsin
    vct-set! drop
  loop
  1024 0 do
    size x0 x1 x2 mass xspring damp compute-uniform-circular-string
    x0 "string" 0 1.0 -10.0 10.0 graph drop
  loop
;

: test-scanned-synthesis ( amp dur scanned-synthesis -- )
  { amp dur mass xspring damp }
  256 { size }
  size 0.0 make-vct { x0 }
  size 0.0 make-vct { x1 }
  size 0.0 make-vct { x2 }
  12 0 do
    x1
    size 4 / 6 - i +
    two-pi i f* 12.0 f/ fsin
    vct-set! drop
  loop
  440.0 :wave x1 make-table-lookup { gen1 }
  440.0 :wave x2 make-table-lookup { gen2 }
  30 { recompute-samps }
  recompute-samps 1/f { kincr }
  0.0 { kk }
  dur 0.0 make-vct map!
    kk 1.0 f>= if
      0.0 to kk
      size x0 x1 x2 mass xspring damp compute-uniform-circular-string
    else
      kincr +to kk
    then
    gen1 table-lookup { g1 }
    gen2 table-lookup { g2 }
    g1 g2 f- k f* g2 f+
  end-map { data }
  data vct-peak { curamp }
  data  amp data vct-peak f/ vct-scale! 0 dur vct->channel drop
;
\ 0.1 10000 1.0 0.1 0.0 test-scanned-synthesis

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
: freqdiv-cb ( div n curval -- proc; y self -- val )
  lambda-create , , , latestxt 1 make-proc
 does> ( y self -- val )
  { y self }
  self           @ { curval }
  self   cell+   @ { n }
  self 2 cells + @ { div }
  div f0= if y self ! ( curval ) then
  1 self 2 cells + ! ( div++ )
  div n = if 0 self 2 cells + ! then
  curval
;
set-current
: freqdiv ( n :optional snd chn -- )
  doc" ( n :optional snd chn -- )  \
Repeats each nth sample N times (clobbering the intermediate samples): 8 freqdiv"
  <{ n :optional snd #f chn #f }>
  0 0.0 { div curval }
  div n curval freqdiv-cb 0 #f snd chn #f $" %s %s" '( n get-func-name ) format map-channel drop
;
previous

\ --- "adaptive saturation" -- an effect from sed_sed@my-dejanews.com ---

hide
: adsat-cb ( mn mx n vals -- proc; inval self -- res )
  lambda-create , , , , latestxt 1 make-proc
 does> ( inval self -- res )
  { val self }
  self @ { vals }
  self 1 cells + @ { n }
  self 2 cells + @ { mx }
  self 3 cells + @ { mn }
  vals length n = if
    vals each { x }
      vals i  x f0>= if mx else mn then  vct-set! drop
    end-each
    0   self 1 cells + ! ( n )
    0.0 self 2 cells + ! ( mx )
    0.0 self 2 cells + ! ( mn )
    vals
  else
    vals n val vct-set! drop
    val mx f> if val self 2 cells + ! ( mx ) then
    val mn f< if val self 3 cells + ! ( mn ) then
    n 1+ self 1 cells + ! ( n++ )
    #f
  then
;
set-current
: adsat ( size :optional beg dur snd chn -- )
  doc" ( size :optional beg dur snd chn -- )  An 'adaptive saturation' sound effect."
  <{ size :optional beg 0 dur #f snd #f chn #f }>
  $" %s %s %s %s" '( size beg dur get-func-name ) string-format { origin }
  0.0 0.0 0 size 0.0 make-vct adsat-cb beg dur snd chn #f origin map-channel drop
;
previous

\ --- spike ---

hide
: spike-cb ( snd chn -- proc; x0 self -- res )
  { snd chn }
  snd chn #f maxamp { amp }
  lambda-create 0.0 ( x1 ) , 0.0 ( x2 ) , amp , latestxt 1 make-proc
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
: spike ( :optional snd chn -- )
  doc" ( :optional snd chn -- )  \
Multiplies successive samples together to make a sound more spikey."
  <{ :optional snd #f chn #f }>
  snd chn spike-cb 0 #f snd chn #f get-func-name map-channel drop
;
previous

\ ;;; -------- easily-fooled autocorrelation-based pitch tracker

: spot-freq ( samp :optional snd chn -- )
  <{ samp :optional snd #f chn #f }>
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
\
\ Left sample:
\
\ graph-hook 4 lambda: ( snd chn y0 y1 -- )
\   { snd chn y0 y1 }
\   $" freq: %.3f" '( snd chn LEFT-SAMPLE  snd chn spot-freq ) string-format
\   snd #f report-in-minibuffer drop
\   #f
\ ;proc add-hook!
\
\ At cursor position:
\
\ mouse-click-hook 7 lambda: ( snd chn button state x y axis -- )
\   { snd chn button state x y axis }
\   axis time-graph = if
\     $" freq: %.3f" '( snd chn #f CURSOR  snd chn spot-freq ) string-format
\     snd #f report-in-minibuffer
\   else
\     #f
\   then
\ ;proc add-hook!

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
  doc" ( -- proc; inval self -- val )  Tries to produce the chorus sound effect."
  chorus-size nil make-array map! make-flanger end-map { dlys }
  lambda-create dlys , latestxt 1 make-proc
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
  doc" ( -- proc; x self -- val )  \
Uses harmonically-related comb-filters to bring out a chord in a sound.  \
Global variable CHORDALIZE-CHORD is a list of members of chord such as '( 1 5/4 3/2 )."
  chordalize-chord map
    :scaler chordalize-amount :size chordalize-base *key* r* r>s make-comb
  end-map { combs }
  chordalize-chord length 0.5 f/ { scaler }
  lambda-create combs , scaler , latestxt 1 make-proc
 does> ( x self -- val )
  { x self }
  self       @ { combs }
  self cell+ @ { scaler }
  0.0 ( val ) combs each ( gen ) x 0.0 comb f+ ( val += ... ) end-each scaler f*
;

\ --- zero-phase, rotate-phase ---
\ fft games (from the "phazor" package of Scott McNab)

: zero-phase ( :optional snd chn -- v )
  doc" ( :optional snd chn -- vct )  Calls fft, sets all phases to 0, and un-ffts."
  <{ :optional snd #f chn #f }>
  snd chn #f frames { len }
  2.0  len flog 2.0 flog f/ fceil ( pow2 )  f** fround f>s { fftlen }
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

: rotate-phase ( func :optional snd chn -- v )
  doc" ( func :optional snd chn -- vct )  Calls fft, applies FUNC to each phase, then un-ffts."
  <{ func :optional snd #f chn #f }>
  func proc? func 1 running-word $" a proc" _ assert-type
  snd chn #f frames { len }
  2.0  len flog 2.0 flog f/ fceil ( pow2 )  f** fround f>s { fftlen }
  fftlen 2/ { fftlen2 }
  fftlen 1/f { fftscale }
  0 fftlen snd chn #f channel->vct { rl }
  rl vct-peak { old-pk }
  fftlen 0.0 make-vct { im }
  rl im 1 fft drop
  rl im rectangular->polar drop
  rl fftscale vct-scale! drop
  im 0 0.0 vct-set! drop
  fftlen2 1 ?do
    im i  func  '( im i  object-ref )  run-proc  object-set!
    im i negate im i negate object-ref fnegate   object-set!	\ handles negative index
  loop
  rl im -1 fft drop
  rl vct-peak { pk }
  $" %s %s" '( func proc-source-ref get-func-name ) string-format { origin }
  rl old-pk pk f/ vct-scale! 0 len snd chn #f origin vct->channel
;

\ 1 lambda: { x } 0.0       ;proc rotate-phase \ is the same as zero-phase
\ 1 lambda: { x } pi random ;proc rotate-phase \ randomizes phases
\ 1 lambda: { x } x         ;proc rotate-phase \ returns original
\ 1 lambda: { x } x fnegate ;proc rotate-phase \ reverses original
\ 1 lambda: { x } x f2*     ;proc rotate-phase \ reverb-effect (best with voice)
\ 1 lambda: { x } x 12.0 f* ;proc rotate-phase \ "bruise blood" effect

\ ;;; -------- asymmetric FM (bes-i0 case)

: make-asyfm ( :key frequency 440.0 initial-phase 0.0 ratio 1.0 r 1.0 index 1.0 -- gen )
  <{ :key frequency 440.0 initial-phase 0.0 ratio 1.0 r 1.0 index 1.0 }>
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
  doc" ( gen input -- val )  \
;; this is the same as the CLM asymmetric-fm generator, \
set r != 1.0 to get the asymmetric spectra.\n\
:frequency 2000 :ratio 0.1 make-asyfm value gen\n\
1 lambda: { n } gen 0.0 asyfm-J ;proc map-channel."
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
  { gen r }
  1.0  r r f*  f-
  1.0  r r f*  f+  r f2*  gen 0.0 0.0 oscil f*  f-  f/  1.0 f-
  1.0  r r f*  f-
  1.0  r r f*  f+  r f2*  f*  f/  f*
;
' make-oscil alias make-cosine-summation 
\ 100.0 make-cosine-summation value gen
\ 1 lambda: { y } gen 0.5 cosine-summation 0.2 f* ;proc map-channel

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
  { gen r k }
  1.0 r r f* f+  r f2* gen 0.0 0.0 oscil f*  f-  k fnegate  f**
  1.0 r r f* f+  r f2*  f-  k  f**  f*
;
' make-oscil alias make-kosine-summation 
\ 100.0 make-kosine-summation value gen
\ 1 lambda: { y } gen 0.5 5.0 kosine-summation 0.2 f* ;proc map-channel

\ ;;; -------- legendre, fejer

: fejer-sum ( angle n -- val )
  \ ;; from "Trigonometric Series" Zygmund p88
  { angle n }
  angle f0= if
    1.0
  else
    n 1.0 f+ angle f* f2/ fsin  angle f2/ fsin f2*  f/ ( val )  dup f* n 1.0 f+ f/ f2* 
  then
;
\ 0.0 value angle
\ 1 lambda: { y } angle 3.0 fejer-sum 0.1 f* ( val ) 0.1 +to angle ;proc map-channel

: legendre-sum ( angle n -- val )
  \ ;; from Andrews, Askey, Roy "Special Functions" p 314
  { angle n }
  angle f0= if
    1.0
  else
    n 0.5 f+ angle f* fsin  angle f2/ fsin  f/ dup f*
  then
;
\ 0.0 value angle
\ 1 lambda: { y } angle 3.0 legendre-sum 0.1 f* ( val ) 0.1 +to angle ;proc map-channel

\ ;;; -------- variations on sum-of-cosines
\ ;;; from "Trigonometric Delights" by Eli Maor

: sum-of-n-sines ( angle n -- val )
  { angle n }
  angle f2/ { a2 }
  a2 fsin { den }
  den f0= if 0.0 else n a2 f* fsin  n 1.0 f+ a2 f* fsin f* den f/ then
;
\ 0.0 value angle
\ 1 lambda: { y } angle 3.0 sum-of-n-sines 0.1 f*  0.1 +to angle ;proc map-channel

: sum-of-n-odd-sines ( angle n -- val )
  { angle n }
  angle fsin { den }
  angle n f* fsin dup { na2 }
  den f0= if 0.0 else na2 den f/ then
;

: sum-of-n-odd-cosines ( angle n -- val )
  { angle n }
  angle fsin f2* { den }
  den f0= if n else angle n f* f2*  den f/ then
;

: band-limited-sawtooth ( x a N fi -- val )
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
\ 1 lambda: { y } angle 0.5 8.0 0.2 band-limited-sawtooth  0.2 +to angle ;proc map-channel

: band-limited-square-wave ( theta n -- val ) swap fsin f* ftanh ;
\ 0.0 value angle
\ 1 lambda: { y } angle 10.0 band-limited-square-wave  0.2 +to angle ;proc map-channel

\ ;;; -------- brighten-slightly

hide
: brighten-slightly-cb ( brt mx -- proc; y self -- val )
  lambda-create , , latestxt 1 make-proc
 does> { y self -- val }
  self       @ { mx }
  self cell+ @ { brt }
  brt y f* fsin mx f*
;
set-current
: brighten-slightly ( amount :optional snd chn -- )
  <{ amount :optional snd #f chn #f }>
  doc" ( amount :optional snd chn -- )  \
is a form of contrast-enhancement (AMOUNT between ca 0.1 and 1)."
  snd chn #f maxamp { mx }
  two-pi amount f*  mx f/ { brt }
  brt mx brighten-slightly-cb 0 #f snd chn #f
  $" %.3f %s" '( amount get-func-name ) string-format map-channel drop
;
previous

hide
: brighten-slightly-1-cb ( pcoeffs mx -- proc; y self -- val )
  lambda-create , , latestxt 1 make-proc
 does> { y self -- val }
  self       @ { mx }
  self cell+ @ { pcoeffs }
  pcoeffs y mx f/ polynomial mx f*
;
set-current
: brighten-slightly-1 ( coeffs :optional snd chn -- )
  doc" ( coeffs :optional snd chn -- )  \
is a form of contrast-enhancement (AMOUNT between ca 0.1 and 1)."
  <{ coeffs :optional snd #f chn #f }>
  snd chn #f maxamp { mx }
  coeffs mus-chebyshev-first-kind partials->polynomial { pcoeffs }
  pcoeffs mx brighten-slightly-1-cb 0 #f snd chn #f
  $" %s %s" '( coeffs get-func-name ) string-format map-channel drop
;
\ '( 1 0.5 3 1 ) brighten-slightly-1
previous

\ ;;; -------- FIR filters

\ ;;; Snd's (very simple) spectrum->coefficients procedure is:

: spectrum->coeffs ( order spectr -- vct )
  doc" ( order spectr -- vct )  \
returns FIR filter coefficients given the filter order and desired spectral envelope"
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
  doc" ( order spectr -- )  \
creates an FIR filter from spectrum and order and returns a closure that calls it:\n\
10 vct( 0 1.0 0 0 0 0 0 0 1.0 0 ) fltit-1 map-channel"
  { order spectr }
  :order order :xcoeffs order spectr spectrum->coeffs make-fir-filter
  lambda-create , latestxt 1 make-proc
 does> ( y self -- val )
  @ swap fir-filter
;
\ 10 vct( 0 1.0 0 0 0 0 0 0 1.0 0 ) fltit-1 map-channel

\ --- Hilbert transform

: make-hilbert-transform ( :optional len=30 -- gen )
  doc" ( :optional len=30 -- gen )  Makes a Hilbert transform filter."
  <{ :optional len 30 }>
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
  arrlen arr make-fir-filter
;
' fir-filter alias hilbert-transform

\ --- Butterworth filters (see also further below -- make-butter-lp et al) ---
\ 
\  translated from CLM butterworth.cl:
\ 
\    Sam Heisz, January 1998
\    inspired by some unit generators written for Csound by Paris Smaragdis
\    who based his work on formulas from 
\    Charles Dodge, Computer music: synthesis, composition, and performance.

: make-butter-high-pass ( freq -- flt )
  doc" ( freq -- flt )  Makes a Butterworth filter with high pass cutoff at FREQ."
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
  doc" ( freq -- flt )  Makes a Butterworth filter with low pass cutoff at FREQ.  \
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
  doc" ( freq band -- flt )  \
Makes a bandpass Butterworth filter with low edge at FREQ and width BAND."
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
  doc" ( freq band -- flt )  \
Makes a band-reject Butterworth filter with low edge at FREQ and width BAND."
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

' filter alias butter
' butter $" ( b sig -- res )  The generator side for the various make-butter procedures." help-set!

\ dsp.fs ends here
