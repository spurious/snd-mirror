\ dsp.fs -- dsp.scm|rb --> dsp.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Dec 30 04:52:13 CET 2005
\ Changed: Sun Nov 19 03:46:44 CET 2006

\ Commentary:

\ src-duration            ( en -- dur )
\ dolph                   ( n gamma -- im )
\ dolph-1                 ( n gamma -- im )
\ down-oct                ( n snd chn -- vct )
\ stretch-sound-via-dft   ( factor snd chn -- )
\ 
\ adsat                   ( size beg dur snd chn -- res )
\ spike                   ( snd chn -- res )
\
\ zero-phase              ( snd chn -- vct )
\ rotate-phase            ( func snd chn -- vct)
\ 
\ butter                  ( gen sig -- res )
\ make-butter-high-pass   ( freq -- flt )
\ make-butter-low-pass    ( freq -- flt )
\ make-butter-band-pass   ( freq band -- flt )
\ make-butter-band-reject ( freq band -- flt )


\ Code:

require clm
require examp

\ --- src-duration (see src-channel in extsnd.html) ---

: src-duration { en -- dur }
  doc" ( envelope -- dur )  \
Returns the new duration of a sound after using ENVELOPE for time-varying sampling-rate conversion."
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
      y1 fln y0 fln f-  xy0 xy1 f-  f/  x1 x0 f-  all-x  f/  f*
    then ( area ) fabs ( dur ) f+
  2 +loop
  ( dur )
;

\ --- Dolph-Chebyshev window ---

\ formula taken from Richard Lyons, "Understanding DSP"
\ see clm.c for C version (using either GSL's or GCC's complex trig functions)

: dolph { n gamma -- im }
  doc" ( n gamma -- im )  \
Produces a Dolph-Chebyshev FFT data window of N points using GAMMA as the window parameter."
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

: down-oct { n snd chn -- v }
  doc" ( n snd chn -- vct )  Moves a sound down by power of 2 n."
  snd chn #f frames { len }
  2.0  len fln 2.0 fln f/ fceil ( pow2 )  f** f>s { fftlen }
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

: stretch-sound-via-dft ( factor snd chn -- )
  { factor snd chn }
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
: adsat { size beg dur snd chn -- }
  doc" ( size beg dur snd chn -- res )  An 'adaptive saturation' sound effect."
  $" %s %s %s %s" '( size beg dur get-func-name ) string-format { origin }
  0.0 0.0 0 size 0.0 make-vct adsat-cb beg dur snd chn #f origin map-channel
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
: spike { snd chn -- res }
  doc" ( snd chn -- res )  Multiplies successive samples together to make a sound more spikey."
  snd chn spike-cb 0 #f snd chn #f get-func-name map-channel
;
previous

\ --- zero-phase, rotate-phase ---
\ fft games (from the "phazor" package of Scott McNab)

: zero-phase { snd chn -- v }
  doc" ( snd chn -- vct )  Calls fft, sets all phases to 0, and un-ffts."
  snd chn #f frames { len }
  2.0  len fln 2.0 fln f/ fceil ( pow2 )  f** fround f>s { fftlen }
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

: rotate-phase { func snd chn -- v }
  doc" ( func snd chn -- vct )  Calls fft, applies FUNC to each phase, then un-ffts."
  func proc? func 1 running-word $" a proc" _ assert-type
  snd chn #f frames { len }
  2.0  len fln 2.0 fln f/ fceil ( pow2 )  f** fround f>s { fftlen }
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

\ lambda: { x } 0.0 ;       1 make-proc #f #f rotate-phase \ is the same as zero-phase
\ lambda: { x } pi random ; 1 make-proc #f #f rotate-phase \ randomizes phases
\ lambda: { x } x ;         1 make-proc #f #f rotate-phase \ returns original
\ lambda: { x } x fnegate ; 1 make-proc #f #f rotate-phase \ reverses original

\ --- Butterworth filters (see also further below -- make-butter-lp et al) ---
\ 
\  translated from CLM butterworth.cl:
\ 
\    Sam Heisz, January 1998
\    inspired by some unit generators written for Csound by Paris Smaragdis
\    who based his work on formulas from 
\    Charles Dodge, Computer music: synthesis, composition, and performance.

: butter ( b sig -- res )
  doc" ( b sig -- res )  The generator side for the various make-butter procedures."
  filter
;

: make-butter-high-pass { freq -- flt }
  doc" ( freq -- flt )  Makes a Butterworth filter with high pass cutoff at FREQ."
  pi freq f* #f srate f/ ftan { r }
  r r f* { r2 }
  r 2.0 fsqrt r2 f* f* 1/f { c1 }
  -2.0 c1 f* { c2 }
  c1 { c3 }
  r2 1.0 f- c1 f* f2* { c4 }
  1.0 2.0 fsqrt r f* f- r2 f+ c1 f* { c5 }
  3 vct( c1 c2 c3 ) vct( 0.0 c4 c5 ) make-filter
;

: make-butter-low-pass { freq -- flt }
  doc" ( freq -- flt )  Makes a Butterworth filter with low pass cutoff at FREQ.  \
The result can be used directly: 500.0 make-butter-low-pass filter-sound, \
or via the 'butter' generator."
  pi freq f* #f srate f/ ftan 1/f { r }
  r r f* { r2 }
  r 2.0 fsqrt f* 1.0 r2 f* f* 1/f { c1 }
  c1 f2* { c2 }
  c1 { c3 }
  1.0 r2 f- c1 f* f2* { c4 }
  1.0 2.0 fsqrt r f* f- r2 f+ c1 f* { c5 }
  3 vct( c1 c2 c3 ) vct( 0.0 c4 c5 ) make-filter
;

: make-butter-band-pass { freq bw -- flt }
  doc" ( freq band -- flt )  \
Makes a bandpass Butterworth filter with low edge at FREQ and width BAND."
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

: make-butter-band-reject { freq bw -- flt }
  doc" ( freq band -- flt )  \
Makes a band-reject Butterworth filter with low edge at FREQ and width BAND."
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

\ dsp.fs ends here
