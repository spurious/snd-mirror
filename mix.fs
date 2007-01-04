\ -*- snd-forth -*-
\ mix.fs -- mix.scm -> mix.fs

\ Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Tue Oct 11 18:23:12 CEST 2005
\ Changed: Tue Jan 02 23:27:17 CET 2007

\ Commentary:
\
\ various mix and track related utilities (see mix.scm)
\
\ mix-sound            ( file start -- mix-id )
\ delete-all-mixes     ( -- )
\ find-mix             ( sample snd chn -- mx )
\ pan-mix              ( name beg envelope snd chn auto-delete -- mx )
\ pan-mix-selection    ( beg envelope snd chn -- mx )
\ pan-mix-region       ( reg beg envelope snd chn -- mx )
\ pan-mix-vct          ( v beg envelope snd chn -- mx )
\ mix->vct             ( id -- vct )
\ save-mix             ( id filename -- )
\ mix-maxamp           ( id -- max-amp )
\ snap-mix-to-beat     ( at-tag-position -- )
\
\ mix-properties       ( id -- props )
\ set-mix-properties   ( id props -- )
\ mix-property         ( id key -- val )
\ set-mix-property     ( id key val -- )
\ mix-name->id         ( name -- id )
\ mix-click-sets-amp   ( -- )
\
\ delete-all-tracks    ( -- )
\ reverse-track        ( trk -- )
\ track->vct           ( trk chn -- v )
\ save-track           ( trk filename chan -- )
\ track-maxamp         ( id chan -- max-amp )
\ transpose-track      ( trk semitones -- )
\ retempo-track        ( trk tempo -- )
\ filter-track         ( trk fir-filter-coeffs -- )
\
\ track-properties     ( id -- props )
\ set-track-properties ( id props -- )
\ track-property       ( id key -- val )
\ set-track-property   ( id key val -- )
\ mix-click-info       ( id -- #t )

\ Code:

require clm
require examp

: tree-for-each ( proc-or-xt tree -- ?? )
  doc" Applies PROC-OR-XT to every leaf of TREE."
  { proc-or-xt tree }
  tree null? unless
    tree pair? if
      proc-or-xt tree car recurse
      proc-or-xt tree cdr recurse
    else
      proc-or-xt xt? if
	tree proc-or-xt execute
      else
	proc-or-xt proc? if
	  proc-or-xt tree run-proc
	else
	  #f
	then
      then
    then
  then
;

\ === Mixes ===

: mix-sound <{ file :optional start 0 -- id }>
  doc" Mixes FILE (all chans) at START in the currently selected sound."
  file start #t undef undef undef undef undef mix
;

hide
: delete-mix-xt       <{ id -- }> id delete-mix drop ;
: delete-all-mixes-cb <{ -- }> ['] delete-mix-xt undef undef mixes tree-for-each ;
set-current
: delete-all-mixes ( -- )
  doc" Removes all mixes (sets all amps to 0)"
  ['] delete-all-mixes-cb undef as-one-edit drop
;
previous

: find-mix <{ samp :optional snd #f chn #f -- mx }>
  doc" Returns the id of the mix at the given SAMPLE, or #f."
  #f					\ flag
  snd snd-snd chn snd-chn mixes each { id }
    id mix-position samp = if
      drop				\ drop flag
      id
      leave
    then
  end-each
;

\ --- pan-mix ---

hide
: pan-mix-cb ( name beg idx inchan chan0 chan1 auto-delete track-func -- prc ; self -- mx )
  { name beg idx inchan chan0 chan1 auto-delete track-func }
  0 proc-create name , beg , idx , inchan , chan0 , chan1 , auto-delete , track-func ,
 does> ( self -- mx )
  { self }
  self @ { name }
  self 1 cells + @ { beg }
  self 2 cells + @ { idx }
  self 3 cells + @ { inchan }
  self 4 cells + @ { chan0 }
  self 5 cells + @ { chan1 }
  self 6 cells + @ { auto-delete }
  self 7 cells + @ { track-func }
  '() make-track { trk }
  name beg 0      idx chan0 #t auto-delete trk mix { mix0 }
  name beg inchan idx chan1 #t auto-delete trk mix { mix1 }
  mix1 #t set-mix-inverted? drop
  trk track-func set-track-amp-env drop
  mix0
;
set-current
: pan-mix <{ name :optional beg 0 envelope 1.0 snd #f chn 0 auto-delete #f -- mx }>
  doc" Mixes FILE into the sound SND starting at BEG \
(in samples) using ENVELOPE to pan (0: all chan 0, 1: all chan 1).  \
So, \"oboe.snd\" 0.1 '( 0 0 1 1 ) pan-mix goes from all chan 0 to all chan 1.  \
If the variable with-tags is #t, the resultant mixes are placed in their own track, \
and the track envelope controls the panning.  \
If ENVELOPE is a scaler, it is turned into an evelope at that value.  \
AUTO-DELETE determines whether the in-coming file should be treated as a temporary file \
and deleted when the mix is no longer accessible."
  name find-file to name
  name false? if 'no-such-file '( get-func-name name ) fth-throw then
  snd snd-snd dup sound? unless 'no-such-sound '( get-func-name snd ) fth-throw then { idx }
  with-mix-tags { old-with-mix-tag }
  #t set-with-mix-tags drop
  name mus-sound-chans { incoming-chans }
  idx channels { receiving-chans }
  idx sync { old-sync }
  envelope list? if envelope else '( 0 envelope 1 envelope ) then { track-func }
  receiving-chans 1 = if
    incoming-chans 1 = if
      name beg 0 idx 0 #t auto-delete mix { id }
      envelope list? if
	id 0 envelope set-mix-amp-env drop
      else
	id 0 envelope set-mix-amp drop
      then
      id
    else
      \ incoming chans > 2 ignored
      name beg idx 1 0 0 auto-delete track-func pan-mix-cb undef as-one-edit drop
    then
  else
    chn { chan0 }
    chn 1+ receiving-chans mod { chan1 }
    incoming-chans 1 = if
      idx #f set-sync drop
      name beg idx 0 chan0 chan1 auto-delete track-func pan-mix-cb undef as-one-edit drop
      idx old-sync set-sync drop
    else
      0 { new-sync }
      \ incoming chans > 2 ignored
      sounds each { s } s sync new-sync > if s sync 1+ to new-sync then end-each
      idx new-sync set-sync drop
      name beg idx 1 chan0 chan1 auto-delete track-func pan-mix-cb undef as-one-edit drop
      idx old-sync set-sync drop
    then
  then { new-mix }
  old-with-mix-tag set-with-mix-tags drop
  new-mix mix? old-with-mix-tag not && if
    new-mix mix-track track? if
      new-mix mix-track lock-track drop
    else
      new-mix #t set-mix-locked? drop
    then
  then
  new-mix
;
previous

: pan-mix-selection <{ :optional beg 0 envelope 1.0 snd #f chn 0 -- mx }>
  doc" Mixes the current selection into the sound SND \
starting at START (in samples) using ENVELOPE to pan (0: all chan 0, 1: all chan 1)."
  selection? if
    snd-tempnam save-selection beg envelope snd chn #t pan-mix
  else
    'no-active-selection '( get-func-name ) fth-throw
  then
;

: pan-mix-region <{ reg :optional beg 0 envelope 1.0 snd #f chn 0 -- mx }>
  doc" Mixes the given region into the sound SND \
starting at START (in samples) using ENVELOPE to pan (0: all chan 0, 1: all chan 1)."
  reg region? if
    reg snd-tempnam save-region beg envelope snd chn #t pan-mix
  else
    'no-such-region '( get-func-name reg ) fth-throw
  then
;

: pan-mix-vct <{ v :optional beg 0 envelope 1.0 snd #f chn 0 -- mx }>
  snd-tempnam { temp-file }
  temp-file snd srate 1 mus-sound-open-output { fd }
  fd 0 v vct-length 1- 1 v undef undef vct->sound-data mus-sound-write drop
  fd v vct-length 4 * mus-sound-close-output drop
  temp-file beg envelope snd chn #t pan-mix
;

: mix->vct ( id -- v )
  doc" Returns mix's data in vct."
  { id }
  id mix? if
    id 0 make-mix-sample-reader { reader }
    id mix-frames 0.0 make-vct map! reader read-mix-sample end-map
    reader free-sample-reader drop
  else
    'no-such-mix '( get-func-name id ) fth-throw
  then
;

: save-mix ( id fname -- )
  doc" Saves mix data (as floats) in FILENAME."
  { id fname }
  id mix->vct { v }
  fname #f srate mus-sound-open-output { fd }
  fd 0 v vct-length 1- 1 v undef undef vct->sound-data mus-sound-write drop
  fd v vct-length 4 * mus-sound-close-output drop
;

: mix-maxamp ( id -- max-amp )
  doc" Returns the max amp in the given mix."
  mix->vct vct-peak
;

hide
: snap-mix-to-beat-cb ( at-tag-position -- proc; id samps self -- #t )
  2 proc-create swap ,
 does> ( id samps self -- #t )
  { id samps self }
  self @ ( at-tag-position ) if id mix-tag-position else 0 then { offset }
  id mix-position samps + offset + { samp }
  id mix-home car { snd }
  id mix-home cadr { chn }
  snd chn beats-per-minute 60.0 f/ { bps }
  snd srate { sr }
  samp bps f* sr f/ floor { beat }
  beat sr f* bps f/ floor f>s { lower }
  beat 1.0 f+ sr f* bps f/ floor f>s { higher }
  id
  samp lower - higher samp - < if
    0 lower offset - max
  else
    higher offset -
  then set-mix-position drop
  #t
;
set-current
: snap-mix-to-beat <{ :optional at-tag-position? #f -- }>
  doc" Forces a dragged mix to end up on a beat (see beats-per-minute).  \
Resets mix-release-hook to cancel."
  mix-release-hook at-tag-position? snap-mix-to-beat-cb add-hook!
;
previous

\ --- Mix Properties ---
: mix-properties ( id -- props )
  doc" Returns mix ID's entire property hash."
  { id }
  id mix? unless 'no-such-mix '( get-func-name id ) fth-throw then
  :mix-property id object-id property-ref
;
: set-mix-properties ( id props -- )
  { id props }
  id mix? unless 'no-such-mix '( get-func-name id ) fth-throw then
  :mix-property id object-id props property-set!
;
: mix-property ( id key -- val )
  doc" Returns the value associated with KEY in the given mix's property list, or #f."
  { id key }
  id mix? unless 'no-such-mix '( get-func-name id ) fth-throw then
  id mix-properties ?dup-if key hash-ref else #f then
;
: set-mix-property ( id key val -- )
  doc" Sets VAL to KEY in the given mix's property list."
  { id key val }
  id mix? unless 'no-such-mix '( get-func-name id ) fth-throw then
  id mix-properties ?dup-if
    key val hash-set!
  else
    id #{ key val } set-mix-properties
  then
;

hide
: mix-name->id-xt ( name --; id self -- nothing or id )
  1 proc-create swap ,
 does> ( id self -- nothing or id )
  { id self }
  self @ ( name ) id mix-name string= if id then
;
set-current
: mix-name->id ( name -- id )
  depth >r ( name ) mix-name->id-xt undef undef mixes tree-for-each depth r> = if #f then
;
previous

hide
: mix-click-sets-amp-cb <{ id -- #t }>
  id :zero mix-property not if
    id mix-chans 1- 0.0 make-array map! id i mix-amp end-map :amps swap id -rot set-mix-property
    id mix-chans 0 ?do id i 0.0 set-mix-amp drop loop
    id :zero #t set-mix-property
  else
    id :amps mix-property dup false? if drop #() then each id i rot set-mix-amp drop end-each
    id :zero #f set-mix-property
  then
  #t
;
set-current
: mix-click-sets-amp ( -- ) mix-click-hook ['] mix-click-sets-amp-cb add-hook! ;
previous

\ === Tracks ===

hide
: delete-track-mix-xt  <{ id -- }> dup mix-track 0<> if delete-mix then drop ;
: delete-all-tracks-cb <{ -- }> ['] delete-track-mix-xt undef undef mixes tree-for-each ;
set-current
: delete-all-tracks ( -- )
  doc" Removes all mixes that have an associated track (sets all amps to 0)."
  ['] delete-all-tracks-cb undef as-one-edit drop
;
previous

hide
: track-sort-xt ( a b -- -1|0|1 )
  { a b }
  a mix-position b mix-position < if
    1
  else
    a mix-position b mix-position = if
      0
    else
      -1
    then
  then
;
: reverse-track-cb ( trk -- proc; self -- )
  ( trk ) 0 track ['] track-sort-xt sort 
  0 proc-create swap ,
 does> ( self -- )
  @ { ids-in-order }
  ids-in-order length 1 > if
    ids-in-order map *key* mix-position end-map list-reverse { ids-pos }
    ids-in-order each { id }
      ids-pos i list-ref { pos }
      id pos set-mix-position drop
    end-each
  then
;
set-current
: reverse-track ( trk -- )
  doc" Reverses the order of its mixes (it changes various mix begin times)."
  { trk }
  #f trk 0 track each mix? if not ( toggle flag ) leave then end-each if
    trk reverse-track-cb undef as-one-edit drop
  else
    'no-such-track '( get-func-name trk ) fth-throw
  then
;
previous

: track->vct <{ trk :optional chn 0 -- v }>
  doc" Places track data in vct."
  trk track? if
    trk track-chans chn > if
      trk chn 0 make-track-sample-reader { rd }
      trk chn track-frames 0.0 make-vct map! rd read-track-sample end-map
    else
      'no-such-channel '( get-func-name chn ) fth-throw
    then
  else
    'no-such-track '( get-func-name trk ) fth-throw
  then
;

: save-track <{ trk fname :optional chn #t -- }>
  doc" Saves track data (as floats) in file FILENAME."
  trk track? if
    trk track-chans { chans }
    nil nil { fd v }
    chn #t  =
    chans 1 = &&
    chn integer? chn chans < && || if
      chn #t = if 0 else chn then { current-chan }
      trk current-chan track->vct to v
      fname #f srate 1 #f #f $" written by save-track" _ mus-sound-open-output to fd
    else
      chn #t = chans 0> && if
	fname #f srate chans #f #f $" written by save-track" _ mus-sound-open-output to fd
	trk track-position { pos }
	trk track-frames chans * 0 make-vct to v
	chans 0 ?do
	  trk i track-frames { chan-len }
	  trk i track-position pos - { chan-pos }
	  trk i 0 make-track-sample-reader { reader }
	  chan-len 0 ?do
	    v i chans chan-pos j + * + reader read-track-sample vct-set! drop
	  loop
	loop
      else
	'no-such-channel '( get-func-name chn ) fth-throw
      then
    then
    fd 0 v vct-length 1- 1 v undef undef vct->sound-data mus-sound-write drop
    fd v vct-length 4 * mus-sound-close-output drop
  else
    'no-such-track '( get-func-name trk ) fth-throw
  then
;

: track-maxamp ( id chan -- amp )
  doc" Returns the max amp in the given track"
  ( id chan ) track->vct vct-peak
;

: transpose-track ( trk semitones -- )
  doc" Transposes each mix in track by semitones."
  { trk semitones }
  trk  2.0 semitones 12.0 f/ f**  trk track-speed f*  set-track-speed drop
;

\ 1 2.0 retempo-track
: retempo-track ( trk tempo -- )
  doc" Changes the inter-mix begin times of mixes in track by TEMPO (> 1.0 is faster)."
  { trk tempo }
  trk dup track-tempo tempo f* set-track-tempo drop
;

hide
: filter-track-cb { flt reader -- proc; y self -- r }
  1 proc-create reader , flt ,
 does> ( y self -- r )
  { y self }
  self @ { reader }
  self cell+ @ { flt }
  reader read-track-sample { val }
  flt val fir-filter y f+ val f-
;
set-current
: filter-track ( trk fir-filter-coeffs -- )
  doc" Filter track data using FIR filter coeffs:\n\
track-id '( 0.1 0.2 0.3 0.3 0.2 0.1 ) filter-track"
  { trk fir-filter-coeffs }
  trk track? if
    fir-filter-coeffs length { order }
    fir-filter-coeffs list->vct { fir-vct }
    trk track-chans 0 ?do
      trk i track-position { beg }
      trk i track-frames { dur }
      order fir-vct make-fir-filter { flt }
      trk i 0 make-track-sample-reader { reader }
      flt reader filter-track-cb beg dur order + #f #f #f get-func-name map-channel drop
    loop
  else
    'no-such-track '( get-func-name trk ) fth-throw
  then
;
previous

\ --- Track Properties ---
: track-properties ( id -- props )
  doc" Returns track ID's entire property hash."
  { id }
  id track? unless 'no-such-track '( get-func-name id ) fth-throw then
  :track-property id object-id property-ref
;
: set-track-properties ( id props -- )
  { id props }
  id track? unless 'no-such-track '( get-func-name id ) fth-throw then
  :track-property id object-id props property-set!
;
: track-property ( id key -- val )
  doc" Returns the value associated with KEY in the given track's property list, or #f."
  { id key }
  id track? unless 'no-such-track '( get-func-name id ) fth-throw then
  id track-properties ?dup-if key hash-ref else #f then
;
: set-track-property ( id key val -- )
  doc" Sets VAL to KEY in the given track's property list."
  { id key val }
  id track? unless 'no-such-track '( get-func-name id ) fth-throw then
  id track-properties ?dup-if
    key val hash-set!
  else
    id #{ key val } set-track-properties
  then
;

\ mix-click-info

: mix-click-info <{ id -- #t }>
  doc" A mix-click-hook function that describes a mix and its properties.\n\
mix-click-hook ' mix-click-info add-hook!"
  id mix-home car { mid }
  id mix-name if $"  (%S)"  '( id mix-name ) string-format else "" then { mname }
  $"        mix id: %d%s\n" '( id mname )    string-format { info-string }
  $"      position: %d (%.3f secs)\n" '( id mix-position dup mid srate f/ ) string-format
  info-string swap << to info-string
  $"        length: %d (%.3f secs)\n" '( id mix-frames dup mid srate f/ )   string-format
  info-string swap << to info-string
  $"            in: %s[%d]%s%s\n" '( mid short-file-name id mix-home cadr
  id mix-locked? if $"  (locked)" else "" then
  id mix-inverted? if $"  (inverted)" else "" then ) string-format
  info-string swap << to info-string
  id mix-track if
    $"         track: %s\n" '( id mix-track ) string-format
    info-string swap << to info-string
  then
  $"       scalers: %s\n" id mix-chans 0 ?do
    id i mix-amp
  loop id mix-chans >list 1 >list string-format
  info-string swap << to info-string
  $"         speed: %.3f\n" '( id mix-speed ) string-format
  info-string swap << to info-string
  $"          envs: %s\n" id mix-chans 0 ?do
    id i mix-amp-env
  loop id mix-chans >list 1 >list string-format
  info-string swap << to info-string
  id mix-tag-position if
    $"  tag-position: %d\n" '( id mix-tag-position ) string-format
    info-string swap << to info-string
  then
  id mix-properties if
    $"    properties: %s\n" '( id mix-properties ) string-format
    info-string swap << to info-string
  then
  $" Mix info" info-string  info-dialog drop
  #t
;
\ mix-click-hook ' mix-click-info add-hook!

\ mix.fs ends here
