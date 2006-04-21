\ clm-ins.fs -- clm-ins.scm|rb -> clm-ins.fs -*- snd-forth -*-

\ Translator/Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Fri Feb 03 10:36:51 CET 2006
\ Changed: Thu Mar 02 11:26:55 CET 2006

\ Commentary:
\
\ jc-reverb-fs ( start dur args -- )
\ violin       ( start dur freq amp keyword-args -- )
\ fm-violin-fs ( start dur freq amp keyword-args -- )
\
\ Code:

: get-list-args ( lst key def -- value )
  { lst key def }
  lst key list-index 1+ ?dup-if lst swap list-ref else def then
;

\ clm-3/jcrev.ins
instrument: jc-reverb-fs ( start dur args -- )
  doc" ( start dur args -- )  The Chowning reverb.\n\
keywords and default values:\n\
    :volume   -- 1.0\n\
    :delay1   -- 0.013\n\
    :delay2   -- 0.011\n\
    :delay3   -- 0.015\n\
    :delay4   -- 0.017\n\
    :low-pass -- #f\n\
    :doubled  -- #f\n\
    :amp-env  -- #f\n\
' 0 1 440 0.2 fm-violin :reverb ' jc-reverb with-sound\n\
' 0 1 440 0.2 fm-violin :reverb-data '( :low-pass #t ) :reverb ' jc-reverb :channels 2 with-sound"
  { start dur args }
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

\ snd-7/fm.html
instrument: violin ( start dur freq amp keyword-args -- )
  doc" ( start dur freq amp keyword-args -- )  Violin example from snd-7/fm.html.\n\
keywords and default values\n\
    :fm-index      -- 1.0\n\
    :amp-env       -- '( 0 0 25 1 75 1 100 0 )\n\
    :index-env     -- '( 0 1 25 0.4 75 0.6 100 0 )\n\
    :degree        -- 90.0 random (locsig-degree)\n\
    :distance      -- 1.0 (locsig-distance)\n\
    :reverb-amount -- 0.01 (locsig-reverb-amount)\n\
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

\ === FM-Violin (clm-3/v.ins, snd-7/v.scm|rb) ===
instrument: fm-violin-fs ( start dur freq amp keyword-args -- )
  doc" ( start dur freq amp keyword-args -- )  FM-Violin from clm-3/v.ins|snd-7/v.scm|rb.\n\
keywords and default values:\n\
    :fm-index              	-- 1.0\n\
    :amp-env               	-- '( 0 0 25 1 75 1 100 0 )\n\
    :periodic-vibrato-rate 	-- 5.0\n\
    :periodic-vibrato-amplitude -- 0.0025\n\
    :random-vibrato-rate        -- 16.0\n\
    :random-vibrato-amplitude   -- 0.005\n\
    :noise-freq            	-- 1000.0\n\
    :noise-amount          	-- 0.0\n\
    :ind-noise-freq        	-- 10.0\n\
    :ind-noise-amount      	-- 0.0\n\
    :amp-noise-freq        	-- 20.0\n\
    :amp-noise-amount      	-- 0.0\n\
    :gliss-env             	-- '( 0 0 100 0 )\n\
    :glissando-amount      	-- 0.0\n\
    :fm1-env               	-- '( 0 1 25 0.4 75 0.6 100 0 )\n\
    :fm2-env               	-- '( 0 1 25 0.4 75 0.6 100 0 )\n\
    :fm3-env               	-- '( 0 1 25 0.4 75 0.6 100 0 )\n\
    :fm1-rat               	-- 1.0\n\
    :fm2-rat               	-- 3.0\n\
    :fm3-rat               	-- 4.0\n\
    :fm1-index             	-- 0.0\n\
    :fm2-index             	-- 0.0\n\
    :fm3-index             	-- 0.0\n\
    :base                  	-- 1.0\n\
    :degree                	-- 90.0 random\n\
    :distance              	-- 1.0\n\
    :reverb-amount         	-- 0.01\n\
    :index-type            	-- 'violin (or 'cello)\n\
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

\ clm-ins.fs ends here
