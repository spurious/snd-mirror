\ clm-ins.fs -- clm-ins.scm|rb -> clm-ins.fs -*- snd-forth -*-

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Feb 03 10:36:51 CET 2006
\ Changed: Sun Aug 20 00:57:25 CEST 2006

\ Commentary:
\
\ jc-reverb-fs ( start dur args -- )
\ violin       ( start dur freq amp keyword-args -- )
\ fm-violin-fs ( start dur freq amp keyword-args -- )
\ 
\ clm-ins.scm|rb instruments
\ 
\ pluck        ( start dur freq amp weighting lossfact -- )
\ vox          ( start dur freq amp ampfun freqfun freqscl voxfun index vibscl -- )
\ fofins       ( start dur keyword-args -- )
\ fm-trumpet   ( start dur keyword-args -- )
\ pqw-vox      ( start dur freq spacing-freq amp ampfun freqfun freqscl ... -- )
\
\ Code:

require clm
require env

: get-list-args ( lst key def -- value )
  { lst key def }
  lst key list-index 1+ ?dup-if lst swap list-ref else def then
;

\ clm/jcrev.ins
instrument: jc-reverb-fs { start dur args -- }
  doc" ( start dur args -- )  The Chowning reverb.\n\
\\ keywords and default values:\n\
\\ :volume   -- 1.0\n\
\\ :delay1   -- 0.013\n\
\\ :delay2   -- 0.011\n\
\\ :delay3   -- 0.015\n\
\\ :delay4   -- 0.017\n\
\\ :low-pass -- #f\n\
\\ :doubled  -- #f\n\
\\ :amp-env  -- #f\n\
0 1 440 0.2 ' fm-violin :reverb ' jc-reverb with-sound\n\
0 1 440 0.2 ' fm-violin :reverb-data '( :low-pass #t ) :reverb ' jc-reverb :channels 2 with-sound"
  args list? unless '() to args then
  args :volume   1.0   get-list-args { volume }
  args :delay1   0.013 get-list-args { delay1 }
  args :delay2   0.011 get-list-args { delay2 }
  args :delay3   0.015 get-list-args { delay3 }
  args :delay4   0.017 get-list-args { delay4 }
  args :low-pass #f    get-list-args { low-pass }
  args :doubled  #f    get-list-args { doubled }
  args :amp-env  #f    get-list-args { amp-env }
  *output* mus-channels { chans }
  *reverb* mus-channels { rev-chans }
  *verbose* if rev-chans chans get-func-name reverb-info then
  :feedback -0.7 :feedforward 0.7 :size 1051 make-all-pass { allpass1 }
  :feedback -0.7 :feedforward 0.7 :size  337 make-all-pass { allpass2 }
  :feedback -0.7 :feedforward 0.7 :size  113 make-all-pass { allpass3 }
  :scaler 0.742 :size 4799 make-comb { comb1 }
  :scaler 0.733 :size 4999 make-comb { comb2 }
  :scaler 0.715 :size 5399 make-comb { comb3 }
  :scaler 0.697 :size 5801 make-comb { comb4 }
  chans 1 > { chan2 }
  chans 4 = { chan4 }
                               :size delay1 seconds->samples make-delay              { outdel1 }
  chan2                     if :size delay2 seconds->samples make-delay else #f then { outdel2 }
  doubled chan4 ||          if :size delay3 seconds->samples make-delay else #f then { outdel3 }
  chan4 doubled chan2 && || if :size delay4 seconds->samples make-delay else #f then { outdel4 }
  amp-env if :envelope amp-env :scaler volume :duration dur make-env else #f then { env-a }
  doubled chan4 && if $" jc-reverb is not set up for doubled reverb in quad" _ error then
  0.0 0.0 { comb-sum comb-sum-1 }
  start dur run
    0.0 rev-chans 0 ?do j i *reverb* in-any f+ loop { in-val }
    allpass3  allpass2  allpass1 in-val 0.0 all-pass  0.0 all-pass  0.0 all-pass { allpass-sum }
    comb-sum-1 { comb-sum-2 }
    comb-sum   to comb-sum-1
    comb1 allpass-sum 0.0 comb
    comb2 allpass-sum 0.0 comb f+
    comb3 allpass-sum 0.0 comb f+
    comb4 allpass-sum 0.0 comb f+ to comb-sum
    low-pass if
      comb-sum comb-sum-2 f+ 0.25 f* comb-sum-1 f2/ f+
    else
      comb-sum
    then { all-sums }
    outdel1 all-sums 0.0 delay { del-a }
    doubled if outdel3 all-sums 0.0 delay del-a f+ to del-a then
    env-a ?dup-if env to volume then
    i del-a volume f* *output* outa drop
    chan2 if
      outdel2 all-sums 0.0 delay { del-b }
      doubled if outdel4 all-sums 0.0 delay del-b f+ to del-b then
      i del-b volume f* *output* outb drop
    then
    chan4 if
      i outdel3 all-sums 0.0 delay volume f* *output* outc drop
      i outdel4 all-sums 0.0 delay volume f* *output* outd drop
    then
  loop
;instrument

[undefined] jc-reverb [if] ' jc-reverb-fs alias jc-reverb [then]

\ snd/fm.html
instrument: violin ( start dur freq amp keyword-args -- )
  doc" ( start dur freq amp keyword-args -- )  Violin example from snd/fm.html.\n\
\\ keywords and default values\n\
\\ :fm-index      -- 1.0\n\
\\ :amp-env       -- '( 0 0 25 1 75 1 100 0 )\n\
\\ :index-env     -- '( 0 1 25 0.4 75 0.6 100 0 )\n\
\\ :degree        -- 90.0 random (locsig-degree)\n\
\\ :distance      -- 1.0 (locsig-distance)\n\
\\ :reverb-amount -- 0.01 (locsig-reverb-amount)\n\
0 3 440 0.5 :fm-index 0.5 ' violin with-sound"
  :fm-index      1.0                          get-args { fm-index }
  :amp-env       '( 0 0 25 1 75 1 100 0 )     get-args { amp-env }
  :index-env     '( 0 1 25 0.4 75 0.6 100 0 ) get-args { index-env }
  :degree        90.0 random 		      get-args { degree }
  :distance      1.0         		      get-args { distance }
  :reverb-amount 0.01        		      get-args { reverb-amount }
  { start dur freq amp }
  freq hz->radians { frq-scl }
  frq-scl fm-index f* { maxdev }
  5.0 freq flog f/ maxdev f* { index1 }
  8.5 freq flog f- 3.0 freq 1000.0 f/ f+ f/ maxdev 3.0 f* f* { index2 }
  4.0 freq fsqrt f/ maxdev f* { index3 }
  :frequency freq :initial-phase 0.0 make-oscil { carrier }
  :frequency freq :initial-phase 0.0 make-oscil { fmosc1 }
  :frequency freq 3.0 f* :initial-phase 0.0 make-oscil { fmosc2 }
  :frequency freq 4.0 f* :initial-phase 0.0 make-oscil { fmosc3 }
  :envelope amp-env   :scaler amp    :duration dur make-env { ampf }
  :envelope index-env :scaler index1 :duration dur make-env { indf1 }
  :envelope index-env :scaler index2 :duration dur make-env { indf2 }
  :envelope index-env :scaler index3 :duration dur make-env { indf3 }
  :frequency  5.0 :amplitude 0.0025 frq-scl f* make-triangle-wave { pervib }
  :frequency 16.0 :amplitude 0.005  frq-scl f* make-rand-interp   { ranvib }
  start dur #{ :degree degree :distance distance :reverb reverb-amount } run-instrument
    pervib 0.0 triangle-wave ranvib 0.0 rand-interp f+ { vib }
    carrier
    vib
    fmosc1     vib    0.0 oscil  indf1 env f* f+
    fmosc2 3.0 vib f* 0.0 oscil  indf2 env f* f+
    fmosc3 4.0 vib f* 0.0 oscil  indf3 env f* f+
    0.0 oscil  ampf env f*
  end-run
;instrument

event: violin-test ( keyword-args -- )
  :beg 0 get-args { start }
  :dur 1 get-args { dur }
  start dur 440 0.5 violin
  dur wait
;event

\ === FM-Violin (clm/v.ins, snd/v.scm|rb) ===
instrument: fm-violin-fs ( start dur freq amp keyword-args -- )
  doc" ( start dur freq amp keyword-args -- )  FM-Violin from clm/v.ins|snd/v.scm|rb.\n\
\\ keywords and default values:\n\
\\ :fm-index              	-- 1.0\n\
\\ :amp-env               	-- '( 0 0 25 1 75 1 100 0 )\n\
\\ :periodic-vibrato-rate 	-- 5.0\n\
\\ :periodic-vibrato-amplitude -- 0.0025\n\
\\ :random-vibrato-rate        -- 16.0\n\
\\ :random-vibrato-amplitude   -- 0.005\n\
\\ :noise-freq            	-- 1000.0\n\
\\ :noise-amount          	-- 0.0\n\
\\ :ind-noise-freq        	-- 10.0\n\
\\ :ind-noise-amount      	-- 0.0\n\
\\ :amp-noise-freq        	-- 20.0\n\
\\ :amp-noise-amount      	-- 0.0\n\
\\ :gliss-env             	-- '( 0 0 100 0 )\n\
\\ :glissando-amount      	-- 0.0\n\
\\ :fm1-env               	-- '( 0 1 25 0.4 75 0.6 100 0 )\n\
\\ :fm2-env               	-- '( 0 1 25 0.4 75 0.6 100 0 )\n\
\\ :fm3-env               	-- '( 0 1 25 0.4 75 0.6 100 0 )\n\
\\ :fm1-rat               	-- 1.0\n\
\\ :fm2-rat               	-- 3.0\n\
\\ :fm3-rat               	-- 4.0\n\
\\ :fm1-index             	-- 0.0\n\
\\ :fm2-index             	-- 0.0\n\
\\ :fm3-index             	-- 0.0\n\
\\ :base                  	-- 1.0\n\
\\ :degree                	-- 90.0 random\n\
\\ :distance              	-- 1.0\n\
\\ :reverb-amount         	-- 0.01\n\
\\ :index-type            	-- 'violin (or 'cello)\n\
0 3 440 0.5 :fm-index 0.5 ' fm-violin with-sound"
  :fm-index                   1.0                          get-args { fm-index }
  :amp-env                    '( 0 0 25 1 75 1 100 0 )     get-args { amp-env }
  :periodic-vibrato-rate      5.0         		   get-args { pvrate }
  :periodic-vibrato-amplitude 0.0025      		   get-args { pvamp }
  :random-vibrato-rate        16.0        		   get-args { rvrate }
  :random-vibrato-amplitude   0.005       		   get-args { rvamp }
  :noise-freq                 1000.0      		   get-args { noise-freq }
  :noise-amount               0.0         		   get-args { noise-amount }
  :ind-noise-freq             10.0        		   get-args { ind-noise-freq }
  :ind-noise-amount           0.0         		   get-args { ind-noise-amount }
  :amp-noise-freq             20.0        		   get-args { amp-noise-freq }
  :amp-noise-amount           0.0         		   get-args { amp-noise-amount }
  :gliss-env                  '( 0 0 100 0 )               get-args { gliss-env }
  :glissando-amount           0.0                          get-args { gliss-amount }
  :fm1-env                    '( 0 1 25 0.4 75 0.6 100 0 ) get-args { fm1-env }
  :fm2-env                    '( 0 1 25 0.4 75 0.6 100 0 ) get-args { fm2-env }
  :fm3-env                    '( 0 1 25 0.4 75 0.6 100 0 ) get-args { fm3-env }
  :fm1-rat                    1.0         		   get-args { fm1-rat }
  :fm2-rat                    3.0         		   get-args { fm2-rat }
  :fm3-rat                    4.0         		   get-args { fm3-rat }
  :fm1-index                  #f         		   get-args { fm1-index }
  :fm2-index                  #f         		   get-args { fm2-index }
  :fm3-index                  #f         		   get-args { fm3-index }
  :base                       1.0         		   get-args { base }
  :degree                     90.0 random 		   get-args { degree }
  :distance                   1.0         		   get-args { distance }
  :reverb-amount              0.01        		   get-args { reverb-amount }
  :index-type                 'violin     		   get-args { index-type }
  { start dur freq amp }
  freq fabs 1.0 f<= if
    $" freq = %s? reset to 440.0" _ '( freq ) string-format warning
    440.0 to freq
  then
  freq hz->radians { frq-scl }
  fm-index f0<> { modulate }
  frq-scl fm-index f* { maxdev }
  index-type 'cello <> { vln }
  freq fln { logfreq }
  freq fsqrt { sqrtfreq }
  fm1-index unless maxdev vln if 5.0 else 7.5 then logfreq f/ f* pi fmin to fm1-index then
  fm2-index unless
    maxdev 3.0 f* vln if 8.5 logfreq f- 3.0 freq 0.001 f* f+ f/ else 15.0 sqrtfreq f/ then
    f* pi fmin to fm2-index
  then
  fm3-index unless maxdev vln if 4.0 else 8.0 then sqrtfreq f/ f* pi fmin to fm3-index then
  noise-amount f0=
  fm1-env fm2-env       equal? &&
  fm1-env fm3-env       equal? &&
  fm1-rat fm1-rat floor f- f0= &&
  fm2-rat fm1-rat floor f- f0= &&
  fm2-rat fm2-rat floor f- f0= &&
  fm3-rat fm1-rat floor f- f0= &&
  fm3-rat fm3-rat floor f- f0= && { easy-case }
  easy-case modulate && 1.0 && fm1-index || { norm }
  :frequency freq :initial-phase 0.0 make-oscil { carrier }
  :envelope amp-env :scaler amp :duration dur :base base make-env { ampf }
  #f #f #f { fmosc1 fmosc2 fmosc3 }
  #f #f #f { indf1 indf2 indf3 }
  modulate if
    easy-case if
      :frequency freq fm1-rat f*
      :coeffs
      '( fm1-rat f>s                  fm1-index
	 fm2-rat fm1-rat f/ floor f>s fm2-index
	 fm3-rat fm1-rat f/ floor f>s fm3-index ) 1 partials->polynomial make-polyshape
    else
      :frequency freq fm1-rat f* :initial-phase 0.0 make-oscil
    then to fmosc1
    easy-case unless
      :frequency freq fm2-rat f* :initial-phase 0.0 make-oscil to fmosc2
      :frequency freq fm3-rat f* :initial-phase 0.0 make-oscil to fmosc3
      :envelope fm1-env :scaler norm      :duration dur make-env to indf1
      :envelope fm2-env :scaler fm2-index :duration dur make-env to indf2
      :envelope fm3-env :scaler fm3-index :duration dur make-env to indf3
    then
  then
  :envelope gliss-env :scaler gliss-amount frq-scl f* :duration dur make-env { frqf }
  :frequency pvrate :amplitude pvamp frq-scl f* make-triangle-wave { pervib }
  :frequency rvrate :amplitude rvamp frq-scl f* make-rand-interp { ranvib }
  #f #f #f { fm-noi ind-noi amp-noi }
  noise-amount f0<> if
    :frequency noise-freq :amplitude noise-amount pi f* make-rand to fm-noi
  then
  ind-noise-freq f0<> ind-noise-amount f0<> && if
    :frequency ind-noise-freq :amplitude ind-noise-amount make-rand-interp to ind-noi
  then
  amp-noise-freq f0<> amp-noise-amount f0<> && if
    :frequency amp-noise-freq :amplitude amp-noise-amount make-rand-interp to amp-noi
  then
  0.0 0.0 1.0 1.0 { vib fuzz ind-fuzz amp-fuzz }
  :degree degree :distance distance :reverb reverb-amount :output *output* :revout *reverb*
  :channels *channels* :type *locsig-type* make-locsig { loc }
  modulate if
    easy-case if
      start dur run
	fm-noi if fm-noi 0.0 rand to fuzz then
	frqf env  pervib 0.0 triangle-wave f+  ranvib 0.0 rand-interp f+ to vib
	ind-noi if ind-noi 0.0 rand-interp 1.0 f+ to ind-fuzz then
	amp-noi if amp-noi 0.0 rand-interp 1.0 f+ to amp-fuzz then
	loc i
	carrier  fmosc1 1.0 vib polyshape  ind-fuzz f* vib f+  0.0  oscil
	ampf env f*  amp-fuzz f*  locsig drop
      loop
    else
      start dur run
	fm-noi if fm-noi 0.0 rand to fuzz then
	frqf env  pervib 0.0 triangle-wave f+  ranvib 0.0 rand-interp f+ to vib
	ind-noi if ind-noi 0.0 rand-interp 1.0 f+ to ind-fuzz then
	amp-noi if amp-noi 0.0 rand-interp 1.0 f+ to amp-fuzz then
	loc i
	carrier ( gen )
	fmosc1 fm1-rat vib f* fuzz f+ 0.0 oscil  indf1 env f*
	fmosc2 fm2-rat vib f* fuzz f+ 0.0 oscil  indf2 env f* f+
	fmosc3 fm3-rat vib f* fuzz f+ 0.0 oscil  indf3 env f* f+ ind-fuzz f* vib f+ ( fm )
	0.0 ( pm ) oscil
	ampf env f*  amp-fuzz f*  locsig drop
      loop
    then
  else
    start dur run
      fm-noi if fm-noi 0.0 rand to fuzz then
      frqf env  pervib 0.0 triangle-wave f+  ranvib 0.0 rand-interp f+ to vib
      ind-noi if ind-noi 0.0 rand-interp 1.0 f+ to ind-fuzz then
      amp-noi if amp-noi 0.0 rand-interp 1.0 f+ to amp-fuzz then
      loc i  carrier vib 0.0 oscil  ampf env f*  amp-fuzz f*  locsig drop
    loop
  then
;

[undefined] fm-violin [if] ' fm-violin-fs alias fm-violin [then]

event: fm-violin-test ( keyword-args -- )
  :beg 0 get-args { start }
  :dur 1 get-args { dur }
  start dur 440 0.5 fm-violin-fs
  dur wait
;event

\ === CLM-INS.(RB|SCM) ===
\ (with original comments from clm-ins.scm)

hide
: get-optimum-c { s o p -- t c }
  o 1/f s o fsin f* 1.0 s f- s o fcos f* f+ fatan2 f* { pa }
  p pa f- f>s { tmp_int } tmp_int unless 1 to tmp_int then
  p pa f- tmp_int f- { pc }
  begin pc 0.1 f< while tmp_int 1 - to tmp_int pc 1e f+ to pc repeat
  tmp_int
  o fsin o pc f* fsin f- o o pc f* f+ fsin f/
;
: tune-it { f s1 -- s c t }
  mus-srate f f/ { p }
  s1 f0= if 0.5 else s1 then { s }
  f hz->radians { o }
  s o p get-optimum-c { t1 c1 }
  1.0 s f- o p get-optimum-c { t2 c2 }
  s 0.5 f<> c1 fabs c2 fabs f< and if 1.0 s f- c1 t1 else s c2 t2 then
;
set-current

\ PLUCK
\
\ The Karplus-Strong algorithm as extended by David Jaffe and Julius
\ Smith -- see Jaffe and Smith, "Extensions of the Karplus-Strong
\ Plucked-String Algorithm" CMJ vol 7 no 2 Summer 1983, reprinted in
\ "The Music Machine".  translated from CLM's pluck.ins
instrument: pluck ( start dur freq amp weighting lossfact -- )
  { start dur freq amp weighting lossfact }
  freq weighting tune-it { wt0 c dlen }
  lossfact f0= if 1.0 else 1.0 lossfact fmin then { lf }
  wt0 f0= if 0.5 else 1.0 wt0 fmin then { wt }
  lf 1.0 wt f- f* lf wt f* make-one-zero { allp }
  c 1.0 make-one-zero { feedb }
  dlen 0.0 make-vct map 1.0 2.0 mus-random f- end-map { tab }
  start dur #{ :degree 90.0 random } run-instrument
    tab cycle-ref { val }
    tab i dlen mod 1.0 c f- feedb allp val one-zero one-zero f* vct-set! drop
    amp val f*
  end-run
;instrument
previous

event: pluck-test ( keyword-args -- )
  :beg 0.0 get-args { start }
  :dur 0.1 get-args { dur }
  start 0.1 330 0.1 0.95 0.95 pluck
  dur wait
;event

$" I00"  constant :I:   $" E12"   constant :E:   $" AE24" constant :AE:
$" UH01" constant :UH:  $" A13"   constant :A:   $" OW25" constant :OW:
$" U02"  constant :U:   $" OO14"  constant :OO:  $" ER26" constant :ER:
$" W03"  constant :W:   $" LL15"  constant :LL:  $" R27"  constant :R:
$" Y04"  constant :Y:   $" EE16"  constant :EE:  $" LH28" constant :LH:
$" L05"  constant :L:   $" I217"  constant :I2:  $" B29"  constant :B:
$" D06"  constant :D:   $" G18"   constant :G:   $" M30"  constant :M:
$" N07"  constant :N:   $" NG19"  constant :NG:  $" P31"  constant :P:
$" T08"  constant :T:   $" K20"   constant :K:   $" F32"  constant :F:
$" TH09" constant :TH:  $" S21"   constant :S:   $" SH33" constant :SH:
$" V10"  constant :V:   $" THE22" constant :THE: $" Z34"  constant :Z:
$" ZH11" constant :ZH:  $" ZZ23"  constant :ZZ:  $" VV35" constant :VV:

make-hash value clm-ins-formants

\ formant center frequencies for a male speaker (vox and pqw-vox)
clm-ins-formants :I:   #( 390.0 1990.0 2550.0 ) hash-set!
clm-ins-formants :UH:  #( 520.0 1190.0 2390.0 ) hash-set!
clm-ins-formants :U:   #( 440.0 1020.0 2240.0 ) hash-set!
clm-ins-formants :W:   #( 300.0  610.0 2200.0 ) hash-set!
clm-ins-formants :Y:   #( 300.0 2200.0 3065.0 ) hash-set!
clm-ins-formants :L:   #( 300.0 1300.0 3000.0 ) hash-set!
clm-ins-formants :D:   #( 300.0 1700.0 2600.0 ) hash-set!
clm-ins-formants :N:   #( 280.0 1700.0 2600.0 ) hash-set!
clm-ins-formants :T:   #( 200.0 1700.0 2600.0 ) hash-set!
clm-ins-formants :TH:  #( 200.0 1400.0 2200.0 ) hash-set!
clm-ins-formants :V:   #( 175.0 1100.0 2400.0 ) hash-set!
clm-ins-formants :ZH:  #( 175.0 1800.0 2000.0 ) hash-set!
clm-ins-formants :E:   #( 530.0 1840.0 2480.0 ) hash-set!
clm-ins-formants :A:   #( 730.0 1090.0 2440.0 ) hash-set!
clm-ins-formants :OO:  #( 300.0  870.0 2240.0 ) hash-set!
clm-ins-formants :LL:  #( 380.0  880.0 2575.0 ) hash-set!
clm-ins-formants :EE:  #( 260.0 3500.0 3800.0 ) hash-set!
clm-ins-formants :I2:  #( 350.0 2300.0 3340.0 ) hash-set!
clm-ins-formants :G:   #( 250.0 1350.0 2000.0 ) hash-set!
clm-ins-formants :NG:  #( 280.0 2300.0 2750.0 ) hash-set!
clm-ins-formants :K:   #( 350.0 1350.0 2000.0 ) hash-set!
clm-ins-formants :S:   #( 200.0 1300.0 2500.0 ) hash-set!
clm-ins-formants :THE: #( 200.0 1600.0 2200.0 ) hash-set!
clm-ins-formants :ZZ:  #( 900.0 2400.0 3800.0 ) hash-set!
clm-ins-formants :AE:  #( 660.0 1720.0 2410.0 ) hash-set!
clm-ins-formants :OW:  #( 570.0  840.0 2410.0 ) hash-set!
clm-ins-formants :ER:  #( 490.0 1350.0 1690.0 ) hash-set!
clm-ins-formants :R:   #( 420.0 1300.0 1600.0 ) hash-set!
clm-ins-formants :LH:  #( 280.0 1450.0 1600.0 ) hash-set!
clm-ins-formants :B:   #( 200.0  800.0 1750.0 ) hash-set!
clm-ins-formants :M:   #( 280.0  900.0 2200.0 ) hash-set!
clm-ins-formants :P:   #( 300.0  800.0 1750.0 ) hash-set!
clm-ins-formants :F:   #( 175.0  900.0 4400.0 ) hash-set!
clm-ins-formants :SH:  #( 200.0 1800.0 2000.0 ) hash-set!
clm-ins-formants :Z:   #( 200.0 1300.0 2500.0 ) hash-set!
clm-ins-formants :VV:  #( 565.0 1045.0 2400.0 ) hash-set!

\ MLBVOI
\ 
\ translation from MUS10 of Marc LeBrun's waveshaping voice instrument
\ (using FM here) this version translated (and simplified slightly)
\ from CLM's mlbvoi.ins
instrument: vox ( start dur freq amp ampfun freqfun freqscl voxfun index vibscl -- )
  { start dur freq amp ampfun freqfun freqscl voxfun index vibscl }
  voxfun length { size }
  size 0 make-array { f1 }
  size 0 make-array { f2 }
  size 0 make-array { f3 }
  size 1- 0 ?do
    clm-ins-formants voxfun i 1+ object-ref hash-ref { phon }
    voxfun i object-ref { n }
    f1 i n object-set!
    phon 0 object-ref f1 i 1+ rot object-set!
    f2 i n object-set!
    phon 1 object-ref f2 i 1+ rot object-set!
    f3 i n object-set!
    phon 2 object-ref f3 i 1+ rot object-set!
  2 +loop
  f1 array->list to f1
  f2 array->list to f2
  f3 array->list to f3
  :frequency 0.0 make-oscil { car-os }
  6 make-array map :frequency 0.0 make-oscil end-map { ofs }
  :envelope ampfun :scaler amp :duration dur make-env { ampf }
  :envelope f1 :duration dur make-env { frmf1 }
  :envelope f2 :duration dur make-env { frmf2 }
  :envelope f3 :duration dur make-env { frmf3 }
  :envelope freqfun :duration dur :scaler freqscl freq f* :offset freq make-env { freqf }
  :frequency 6.0 :amplitude freq vibscl f* make-triangle-wave { per-vib }
  :frequency 20.0 :amplitude freq 0.01 f* make-rand-interp { ran-vib }
  6 0.0 make-vct { freqs }
  6 0.0 make-vct { amps }
  start dur #{ :degree 90.0 random } run-instrument
    freqf env per-vib 0.0 triangle-wave f+ ran-vib 0.0 rand-interp f+ { frq }
    frmf1 env    { frm }
    frm frq f/   { frm0 }
    frm0 floor dup f>s { frm-fint frm-int }
    frm-int 2 mod unless
      freqs 0 frm-fint frq f* hz->radians        object-set!
      freqs 1 frm-fint 1.0 f+ frq f* hz->radians object-set!
      amps  1 frm0 frm-fint f-                   object-set!
      amps  0 1.0 amps 1 object-ref f-           object-set!
    else
      freqs 1 frm-fint frq f* hz->radians        object-set!
      freqs 0 frm-fint 1.0 f+ frq f* hz->radians object-set!
      amps  0 frm0 frm-fint f-                   object-set!
      amps  1 1.0 amps 0 object-ref f-           object-set!
    then
    frmf2 env    to frm
    frm frq f/   to frm0
    frm0 floor   to frm-fint
    frm-fint f>s to frm-int
    frm-int 2 mod unless
      freqs 2 frm-fint frq f* hz->radians        object-set!
      freqs 3 frm-fint 1.0 f+ frq f* hz->radians object-set!
      amps  3 frm0 frm-fint f-                   object-set!
      amps  2 1.0 amps 3 object-ref f-           object-set!
    else
      freqs 3 frm-fint frq f* hz->radians        object-set!
      freqs 2 frm-fint 1.0 f+ frq f* hz->radians object-set!
      amps  2 frm0 frm-fint f-                   object-set!
      amps  3 1.0 amps 2 object-ref f-           object-set!
    then
    frmf3 env    to frm
    frm frq f/   to frm0
    frm0 floor   to frm-fint
    frm-fint f>s to frm-int
    frm-int 2 mod unless
      freqs 4 frm-fint frq f* hz->radians        object-set!
      freqs 5 frm-fint 1.0 f+ frq f* hz->radians object-set!
      amps  5 frm0 frm-fint f-                   object-set!
      amps  4 1.0 amps 5 object-ref f-           object-set!
    else
      freqs 5 frm-fint frq f* hz->radians        object-set!
      freqs 4 frm-fint 1.0 f+ frq f* hz->radians object-set!
      amps  4 frm0 frm-fint f-                   object-set!
      amps  5 1.0 amps 4 object-ref f-           object-set!
    then
    car-os frq hz->radians 0.0 oscil index f* { caros }
    ofs 0 object-ref caros 0.2 f* freqs 0 object-ref f+ 0.0 oscil amps 0 object-ref f*
    ofs 1 object-ref caros 0.2 f* freqs 1 object-ref f+ 0.0 oscil amps 1 object-ref f* f+ 0.80 f*
    ofs 2 object-ref caros 0.5 f* freqs 2 object-ref f+ 0.0 oscil amps 2 object-ref f*
    ofs 3 object-ref caros 0.5 f* freqs 3 object-ref f+ 0.0 oscil amps 3 object-ref f* f+ 0.15 f* f+
    ofs 4 object-ref caros        freqs 4 object-ref f+ 0.0 oscil amps 4 object-ref f*
    ofs 5 object-ref caros        freqs 5 object-ref f+ 0.0 oscil amps 5 object-ref f* f+ 0.05 f* f+
    ampf env f*
  end-run
;instrument

event: vox-test ( keyword-args -- )
  :beg 0.0 get-args { start }
  :dur 1.0 get-args { dur }
  '( 0 0 25 1 75 1 100 0 ) { amp-env }
  '( 0 0 5 0.5 10 0 100 1 ) { frq-env }
  #( 0 :E: 25 :AE: 35 :ER: 65 :ER: 75 :I: 100 :UH: ) { examp1 }
  #( 0 :I: 5 :OW: 10 :I: 50 :AE: 100 :OO: ) { examp2 }

  start dur 170 0.4 amp-env frq-env 0.1 examp1 0.05 0.1 vox
  start dur 0.2 f+ f+ to start
  start dur 300 0.4 amp-env frq-env 0.1 examp2 0.02 0.1 vox
  start dur 0.2 f+ f+ to start
  start 5 600 0.4 amp-env frq-env 0.1 examp2 0.01 0.1 vox
  dur f2* 0.4 f+ 5 f+ wait
;event

\ FOF example
\
\ snd/clm.html, section wave-train
instrument: fofins ( start dur keyword-args -- )
  doc" ( start dur keyword-args -- )\n\
\\ keywords and default values\n\
\\ :frequency -- 270\n\
\\ :amplitude -- 0.2\n\
\\ :vibrato   -- 0.001\n\
\\ :f0        -- 730\n\
\\ :a0        -- 0.6\n\
\\ :f1        -- 1090\n\
\\ :a1        -- 0.3\n\
\\ :f2        -- 2440\n\
\\ :a2        -- 0.1\n\
\\ :vib-env   -- '( 0 1 100 1 )\n\
\\ :amp-env   -- '( 0 0 25 1 75 1 100 0 )\n\
\\ :degree    -- 90.0 random (locsig-degree)\n\
\\ :distance  -- 1.0 (locsig-distance)\n\
0 1 ' fofins with-sound\n\n\
'( 0 0 40 0 75 0.2 100 1 ) value ve\n\
'( 0 0 0.5 1 3 0.5 10 0.2 20 0.1 50 0.1 60 0.2 85 1 100 0 ) value ae\n\
1.2 4 :vibrato 0.005 :vib-env ve :amp-env ae ' fofins with-sound"
  :frequency 270       		get-args { freq }
  :amplitude 0.2       		get-args { amp }
  :vibrato   0.001     		get-args { vib }
  :f0        730       		get-args { f0 }
  :a0        0.6       		get-args { a0 }
  :f1        1090      		get-args { f1 }
  :a1        0.3       		get-args { a1 }
  :f2        2440      		get-args { f2 }
  :a2        0.1       		get-args { a2 }
  :vib-env   '( 0 1 100 1 )           get-args { ve }
  :amp-env   '( 0 0 25 1 75 1 100 0 ) get-args { ae }
  :degree    90.0 random              get-args { loc-degr }
  :distance  1.0                      get-args { loc-dist }
  { start dur }
  :envelope ae :scaler amp :duration dur make-env { ampf }
  :frequency 6.0 make-oscil { vibr }
  :envelope ve :scaler vib :duration dur make-env { vibenv }
  f0 hz->radians { frq0 }
  f1 hz->radians { frq1 }
  f2 hz->radians { frq2 }
  mus-srate 22050.0 f= if 100 else 200 then { foflen }
  two-pi foflen f/ { win-freq }
  foflen 0.0 make-vct map
    a0 i frq0 f* fsin f*
    a1 i frq1 f* fsin f* f+
    a2 i frq2 f* fsin f* f+ f2/
    1.0 i win-freq f* fcos f- f*
  end-map { foftab }
  :frequency freq :wave foftab make-wave-train { wt0 }
  start dur #{ :degree loc-degr :distance loc-dist } run-instrument
    ampf env  wt0  vibenv env vibr 0.0 0.0 oscil f*  wave-train  f*
  end-run
;instrument

event: fofins-test ( keyword-args -- )
  :beg 0.0 get-args { start }
  :dur 1.0 get-args { dur }

  start dur fofins

  '( 0 0 40 0 75 0.2 100 1 ) { ve }
  '( 0 0 0.5 1 3 0.5 10 0.2 20 0.1 50 0.1 60 0.2 85 1 100 0 ) { ae }
  start dur 0.2 f+ f+ 4 :vibrato 0.005 :vib-env ve :amp-env ae fofins

  '( 0 0 0.5 0.5 3 0.25 6 0.1 10 0.1 50 0.1 60 0.2 85 1 100 0 ) to ae
  start dur 0.2 f+ f+ 4 :frequency 6/5 540 f* :vibrato 0.005 :vib-env ve :amp-env ae fofins

  '( 0 0 1 3 3 1 6 0.2 10 0.1 50 0.1 60 0.2 85 1 100 0 ) to ae
  start dur 0.2 f+ f+ 4 :frequency 135 :vibrato 0.005 :vib-env ve :amp-env ae fofins
  dur 0.2 f+ 4 f+ wait
;event

\ FM TRUMPET
\
\ Dexter Morrill's FM-trumpet: from CMJ feb 77 p51
instrument: fm-trumpet ( start dur keyword-args -- )
  doc" ( start dur keyword-args -- )\n\
\\ keywords and default values\n\
\\ :frq1     -- 250\n\
\\ :frq2     -- 1500\n\
\\ :amp1     -- 0.5\n\
\\ :amp2     -- 0.1\n\
\\ :ampatt1  -- 0.03\n\
\\ :ampdec1  -- 0.35\n\
\\ :ampatt2  -- 0.03\n\
\\ :ampdec2  -- 0.3\n\
\\ :modfrq1  -- 250\n\
\\ :modind11 -- 0\n\
\\ :modind12 -- 2.66\n\
\\ :modfrq2  -- 250\n\
\\ :modind21 -- 0\n\
\\ :modind22 -- 1.8\n\
\\ :rvibamp  -- 0.007\n\
\\ :rvibfrq  -- 125\n\
\\ :vibamp   -- 0.007\n\
\\ :vibfrq   -- 7\n\
\\ :vibatt   -- 0.6\n\
\\ :vibdec   -- 0.2\n\
\\ :frqskw   -- 0.03\n\
\\ :frqatt   -- 0.06\n\
\\ :ampenv1  -- '( 0 0 25 1 75 0.9 100 0 )\n\
\\ :ampenv2  -- '( 0 0 25 1 75 0.9 100 0 )\n\
\\ :indenv1  -- '( 0 0 25 1 75 0.9 100 0 )\n\
\\ :indenv2  -- '( 0 0 25 1 75 0.9 100 0 )\n\
0 2 ' fm-trumpet with-sound"
  make-hash { args }
  :frq1     250    		       get-args { frq1 }
  :frq2     1500   		       get-args { frq2 }
  :amp1     0.5    		       get-args { amp1 }
  :amp2     0.1    		       get-args { amp2 }
  :ampatt1  0.03   		       get-args { ampatt1 }
  :ampdec1  0.35   		       get-args { ampdec1 }
  :ampatt2  0.03   		       get-args { ampatt2 }
  :ampdec2  0.3    		       get-args { ampdec2 }
  :modfrq1  250    		       get-args { modfrq1 }
  :modind11 0      		       get-args { modind11 }
  :modind12 2.66   		       get-args { modind12 }
  :modfrq2  250    		       get-args { modfrq2 }
  :modind21 0      		       get-args { modind21 }
  :modind22 1.8    		       get-args { modind22 }
  :rvibamp  0.007  		       get-args { rvibamp }
  :rvibfrq  125    		       get-args { rvibfrq }
  :vibamp   0.007  		       get-args { vibamp }
  :vibfrq   7      		       get-args { vibfrq }
  :vibatt   0.6    		       get-args { vibatt }
  :vibdec   0.2    		       get-args { vibdec }
  :frqskw   0.03   		       get-args { frqskw }
  :frqatt   0.06   		       get-args { frqatt }
  :ampenv1  '( 0 0 25 1 75 0.9 100 0 ) get-args { ampenv1 }
  :ampenv2  '( 0 0 25 1 75 0.9 100 0 ) get-args { ampenv2 }
  :indenv1  '( 0 0 25 1 75 0.9 100 0 ) get-args { indenv1 }
  :indenv2  '( 0 0 25 1 75 0.9 100 0 ) get-args { indenv2 }
  { start dur }
  :envelope
  '( 0 1  25 0.1  75 0  100 0 )
  25
  100 vibatt dur f/ f* 45.0 fmin
  75
  100 1 vibdec dur f/ f- f* 55.0 fmax stretch-envelope
  :scaler vibamp :duration dur make-env { per-vib-f }
  :frequency rvibfrq :amplitude rvibamp make-rand-interp { ran-vib }
  :frequency vibfrq make-oscil { per-vib }
  75 100 1 0.01 dur f/ f- f* fmax { dec-01 }
  :envelope
  '( 0 0  25 1  75 1  100 0 )
  25 25 100 frqatt dur f/ f* fmin
  75 dec-01 stretch-envelope
  :scaler frqskw :duration dur make-env { frq-f }
  25 100 ampatt1 dur f/ f* fmin { ampattpt1 }
  75 100 1 ampdec1 dur f/ f- f* fmax { ampdecpt1 }
  25 100 ampatt2 dur f/ f* fmin { ampattpt2 }
  75 100 1 ampdec2 dur f/ f- f* fmax { ampdecpt2 }
  :envelope indenv1 25 ampattpt1 75 dec-01 stretch-envelope
  :scaler modfrq1 modind12 modind11 f- f* :duration dur make-env { mod1-f }
  :frequency 0.0 make-oscil { mod1 }
  :frequency 0.0 make-oscil { car1 }
  :envelope ampenv1 25 ampattpt1 75 ampdecpt1 stretch-envelope
  :scaler amp1 :duration dur make-env { car1-f }
  :envelope indenv2 25 ampattpt2 75 dec-01 stretch-envelope
  :scaler modfrq2 modind22 modind21 f- f* :duration dur make-env { mod2-f }
  :frequency 0.0 make-oscil { mod2 }
  :frequency 0.0 make-oscil { car2 }
  :envelope ampenv2 25 ampattpt2 75 ampdecpt2 stretch-envelope
  :scaler amp2 :duration dur make-env { car2-f }
  start dur #{ :degree 90.0 random } run-instrument
    ran-vib 0.0 rand-interp 1.0 f+
    1.0 per-vib-f env per-vib 0.0 0.0 oscil f* f+ f*
    1.0 frq-f env f+ f* hz->radians { frq-change }
    car1-f env
    car1 mod1 modfrq1 frq-change f* 0.0 oscil mod1-f env f* frq1 f+ frq-change f* 0.0 oscil f*
    car2-f env
    car2 mod2 modfrq2 frq-change f* 0.0 oscil mod2-f env f* frq2 f+ frq-change f* 0.0 oscil f* f+
  end-run
;instrument

event: fm-trumpet-test ( keyword-args -- )
  :beg 0 get-args { start }
  :dur 1 get-args { dur }
  start dur fm-trumpet
  dur wait
;event

struct
  cell% field sin-evens
  cell% field cos-evens
  cell% field sin-odds
  cell% field cos-odds
  cell% field frmfs
  cell% field sin-coeffs
  cell% field cos-coeffs
  cell% field amps
end-struct pqw-vox%

\ PQWVOX
\
\ translation of CLM pqwvox.ins (itself translated from MUS10 of MLB's
\ waveshaping voice instrument (using phase quadrature waveshaping))
instrument: pqw-vox ( start dur freq spacing-freq amp ampfun freqfun freqscl phonemes formant-amps formant-shapes -- )
  { start dur freq spacing-freq amp ampfun freqfun freqscl phonemes formant-amps formant-shapes }
  :frequency 0.0 make-oscil { car-sin }
  :frequency 0.0 :initial-phase half-pi make-oscil { car-cos }
  :envelope ampfun :scaler amp :duration dur make-env { ampf }
  :envelope freqfun :scaler freqscl freq f* :duration dur :offset freq make-env { freqf }
  :frequency 6.0 :amplitude freq 0.1 f* make-triangle-wave { per-vib }
  :frequency 20.0 :amplitude freq 0.05 f* make-rand-interp { ran-vib }
  phonemes length { plen }
  plen #f make-array { phone1 }
  plen #f make-array { phone2 }
  plen #f make-array { phone3 }
  plen 1- 0 ?do
    phonemes i object-ref { ph }
    phone1 i ph array-set!
    phone2 i ph array-set!
    phone3 i ph array-set!
    clm-ins-formants phonemes i 1+ object-ref hash-ref { ary }
    phone1 i 1+ ary 0 object-ref array-set!
    phone2 i 1+ ary 1 object-ref array-set!
    phone3 i 1+ ary 2 object-ref array-set!
  2 +loop
  phone1 array->list
  phone2 array->list
  phone3 array->list 3 >array { phones }
  nil { pv }
  formant-amps length #f make-array map
    pqw-vox% %alloc to pv
    :frequency 0.0 make-oscil pv sin-evens !
    :frequency 0.0 make-oscil pv sin-odds !
    :frequency 0.0 :initial-phase half-pi make-oscil pv cos-evens !
    :frequency 0.0 :initial-phase half-pi make-oscil pv cos-odds !
    formant-shapes i object-ref normalize-partials { shape }
    shape 1 partials->polynomial pv cos-coeffs !
    shape 0 partials->polynomial pv sin-coeffs !
    :envelope phones i array-ref :duration dur make-env pv frmfs !
    formant-amps i object-ref pv amps !
    pv
  end-map { values }
  4 0.0 make-vct { vals }
  spacing-freq freq f/ { frq-ratio }
  start dur #{ :degree 90.0 random } run-instrument
    freqf env per-vib 0.0 triangle-wave f+ ran-vib 0.0 rand-interp f+ { frq }
    frq frq-ratio f* hz->radians { frqscl }
    car-sin frqscl 0.0 oscil { carsin }
    car-cos frqscl 0.0 oscil { carcos }
    0.0 ( sum )
    values each
      to pv
      pv frmfs @ env frq f/ { frm0 }
      frm0 floor            { frm-fint }
      frm-fint f>s 2 mod unless
	vals 0 frm-fint frq f* hz->radians       vct-set! drop ( even-freq )
	vals 1 frm-fint 1e f+ frq f* hz->radians vct-set! drop ( odd-freq )
	vals 3 frm0 frm-fint f-                  vct-set! drop ( odd-amp )
	vals 2 1.0 vals 3 vct-ref f-             vct-set! drop ( even-amp )
      else
	vals 1 frm-fint frq f* hz->radians       vct-set! drop ( odd-freq )
	vals 0 frm-fint 1e f+ frq f* hz->radians vct-set! drop ( even-freq )
	vals 2 frm0 frm-fint f-                  vct-set! drop ( even-amp )
	vals 3 1.0 vals 2 vct-ref f-             vct-set! drop ( odd-amp )
      then
      pv cos-coeffs @ carcos polynomial { fax }
      pv sin-coeffs @ carcos polynomial carsin f* { yfax }
      pv sin-evens @ vals 0 vct-ref 0.0 oscil yfax f*
      pv cos-evens @ vals 0 vct-ref 0.0 oscil fax f* f- vals 2 vct-ref f*
      pv sin-odds @  vals 1 vct-ref 0.0 oscil yfax f*
      pv cos-odds @  vals 1 vct-ref 0.0 oscil fax f* f- vals 3 vct-ref f* f+ pv amps f@ f* f+
    end-each
    ampf env f*
  end-run
;instrument

event: pqw-vox-test ( keyword-args -- )
  :beg 0 get-args { start }
  :dur 1 get-args { dur }
  '( 0 0 50 1 100 0 ) { ampfun }
  '( 0 0 100 0 ) { freqfun }
  '( 0 0 100 1 ) { freqramp }
  #( '( 1 1 2 0.5 ) '( 1 0.5 2 0.5 3 1 ) '( 1 1 4 0.5 ) ) { shapes1 }
  #( '( 1 1 2 0.5 ) '( 1 1 2 0.5 3 0.2 4 0.1 ) '( 1 1 3 0.1 4 0.5 ) ) { shapes2 }
  #( '( 1 1 2 0.5 ) '( 1 1 4 0.1 ) '( 1 1 2 0.1 4 0.05 ) ) { shapes3 }
  #( '( 1 1 2 0.5 3 0.1 4 0.01 ) '( 1 1 4 0.1 ) '( 1 1 2 0.1 4 0.05 ) ) { shapes4 }

  start dur 300 300 0.5 ampfun freqfun 0 #( 0 :L: 100 :L: ) '( 0.33 0.33 0.33 ) shapes1 pqw-vox
  start dur 0.2 f+ f+ to start
  start dur 200 200 0.5 ampfun freqramp 0.1 #( 0 :UH: 100 :ER: ) '( 0.8 0.15 0.05 ) shapes2 pqw-vox
  start dur 0.2 f+ f+ to start
  start dur 100 314 0.5 ampfun freqramp 0.1 #( 0 :UH: 100 :ER: ) '( 0.8 0.15 0.05 ) shapes2 pqw-vox
  start dur 0.2 f+ f+ to start
  start dur 200 314 0.5 ampfun freqramp 0.01 #( 0 :UH: 100 :ER: ) '( 0.8 0.15 0.05 ) shapes3 pqw-vox
  start dur 0.2 f+ f+ to start
  start dur 100 414 0.5 ampfun freqramp 0.01 #( 0 :OW: 50 :E: 100 :ER: ) '( 0.8 0.15 0.05 ) shapes4
  pqw-vox
  dur 5 f* wait
;event

\ clm-ins.fs ends here
