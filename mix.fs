\ -*- snd-forth -*-
\ mix.fs -- mix.scm -> mix.fs

\ Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Tue Oct 11 18:23:12 CEST 2005
\ Changed: Sun Aug 20 01:00:35 CEST 2006

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
\ set-mix-properties   ( id val -- )
\ mix-property         ( key id -- val )
\ set-mix-property     ( key val id -- )
\ mix-name             ( id -- name )
\ set-mix-name         ( id name -- )
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
\ set-track-properties ( id val -- )
\ track-property       ( key id -- val )
\ set-track-property   ( key val id -- )
\ mix-click-info       ( -- proc; id self -- #t )

\ Code:

require clm
require examp

: tree-for-each { xt tree -- ?? }
  doc" ( xt tree -- ?? )  Applies XT to every leaf of TREE."
  tree null? unless
    tree pair? if
      xt tree car recurse
      xt tree cdr recurse
    else
      tree xt execute
    then
  then
;

\ === Mixes ===

: mix-sound { file start -- id }
  doc" ( file start -- mix-id )  Mixes FILE (all chans) at START in the currently selected sound."
  file start #t undef undef undef undef undef mix
;

hide
: delete-mix-xt       ( id -- ) delete-mix drop ;
: delete-all-mixes-cb ( -- ) ['] delete-mix-xt undef undef mixes tree-for-each ;
set-current
: delete-all-mixes ( -- )
  doc" ( -- )  Removes all mixes (sets all amps to 0)"
  ['] delete-all-mixes-cb 0 make-proc undef as-one-edit drop
;
previous

: find-mix { samp snd chn -- mx }
  doc" ( sample snd chn -- mx )  Returns the id of the mix at the given SAMPLE, or #f."
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
  lambda-create name , beg , idx , inchan , chan0 , chan1 , auto-delete , track-func ,
  latestxt 0 make-proc
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
: pan-mix { name beg envelope snd chn auto-delete -- mx }
  doc" ( name beg envelope snd chn auto-delete -- mx )  Mixes FILE into the sound SND \
starting at BEG (in samples) using ENVELOPE to pan (0: all chan 0, 1: all chan 1).  \
So, \"oboe.snd\" 0.1 '( 0 0 1 1 ) #f 0 #f pan-mix goes from all chan 0 to all chan 1.  \
If the variable with-tags is #t, the resultant mixes are placed in their own track, \
and the track envelope controls the panning.  \
If ENVELOPE is a scaler, it is turned into an evelope at that value.  \
AUTO-DELETE determines whether the in-coming file should be treated as a temporary file \
and deleted when the mix is no longer accessible."
  name file-exists? unless
    'no-such-file '( get-func-name name ) fth-throw
  then
  snd snd-snd dup sound? unless
    'no-such-sound '( get-func-name snd ) fth-throw
  then { idx }
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

: pan-mix-selection { beg envelope snd chn -- mx }
  doc" ( beg envelope snd chn -- mx )  Mixes the current selection into the sound SND \
starting at START (in samples) using ENVELOPE to pan (0: all chan 0, 1: all chan 1)."
  selection? if
    snd-tempnam save-selection beg envelope snd chn #t pan-mix
  else
    'no-active-selection '( get-func-name ) fth-throw
  then
;

: pan-mix-region { reg beg envelope snd chn -- mx }
  doc" ( reg beg envelope snd chn -- mx )  Mixes the given region into the sound SND \
starting at START (in samples) using ENVELOPE to pan (0: all chan 0, 1: all chan 1)."
  reg region? if
    reg snd-tempnam save-region beg envelope snd chn #t pan-mix
  else
    'no-such-region '( get-func-name reg ) fth-throw
  then
;

: pan-mix-vct ( v beg envelope snd chn -- mx )
  { v beg envelope snd chn }
  snd-tempnam { temp-file }
  :file temp-file :channels 1 :srate snd srate open-sound-file { fd }
  fd v v vct-length vct->sound-file drop
  fd v vct-length 4 * close-sound-file drop
  temp-file beg envelope snd chn #t pan-mix
;

: mix->vct { id -- v }
  doc" ( id -- vct )  Returns mix's data in vct."
  id mix? if
    id 0 make-mix-sample-reader { reader }
    id mix-frames 0.0 make-vct map! reader read-mix-sample end-map
    reader free-sample-reader drop
  else
    'no-such-mix '( get-func-name id ) fth-throw
  then
;

: save-mix { id fname -- }
  doc" ( id filename -- )  Saves mix data (as floats) in FILENAME."
  id mix->vct { v }
  :file fname :srate #f srate open-sound-file { fd }
  fd v v vct-length vct->sound-file drop
  fd v vct-length 4 * close-sound-file drop
;

: mix-maxamp ( id -- max-amp )
  doc" ( id -- max-amp )  Returns the max amp in the given mix."
  mix->vct vct-peak
;

hide
: snap-mix-to-beat-cb ( at-tag-position -- proc; id samps self -- #t )
  lambda-create , latestxt 2 make-proc
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
: snap-mix-to-beat { pos? -- }
  doc" ( at-tag-position -- )  Forces a dragged mix to end up on a beat (see beats-per-minute).  \
Resets mix-release-hook to cancel."
  mix-release-hook pos? snap-mix-to-beat-cb add-hook!
;
previous

\ --- Mix Properties ---

: mix-properties     ( id -- props ) object-id 'mix-property property-ref ;
: set-mix-properties ( id val -- )   swap object-id 'mix-property rot property-set! ;

: mix-property { key id -- val }
  doc" ( key id -- val )  \
Returns the value associated with KEY in the given mix's property list, or #f."
  id mix? if
    id mix-properties dup hash? if key hash-ref else drop #f then
  else
    'no-such-mix '( get-func-name id ) fth-throw
  then
;

: set-mix-property { key val id -- }
  doc" ( key val id -- )  Sets the value VAL to KEY in the given mix's property list."
  id mix? if
    id mix-properties { props }
    props hash? if
      props key val hash-set!
    else
      make-hash to props
      props key val hash-set!
      id props set-mix-properties
    then
  else
    'no-such-mix '( get-func-name id ) fth-throw
  then
;

: mix-name     ( id -- name ) :name swap mix-property ;
: set-mix-name ( id name -- ) :name swap rot set-mix-property ;

hide
: mix-name->id-xt ( name --; id self -- nothing or id )
  lambda-create , latestxt
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
: mix-click-sets-amp-cb ( id -- #t )
  { id }
  :zero id mix-property not if
    id mix-chans 1- 0.0 make-array map! id i mix-amp end-map :amps swap id set-mix-property
    id mix-chans 0 ?do id i 0.0 set-mix-amp drop loop
    :zero #t id set-mix-property
  else
    :amps id mix-property dup false? if drop #() then each id i rot set-mix-amp drop end-each
    :zero #f id set-mix-property
  then
  #t
;
set-current
: mix-click-sets-amp ( -- ) mix-click-hook ['] mix-click-sets-amp-cb 1 make-proc add-hook! ;
previous

\ === Tracks ===

hide
: delete-track-mix-xt  ( id -- ) dup mix-track 0<> if delete-mix then drop ;
: delete-all-tracks-cb ( -- ) ['] delete-track-mix-xt undef undef mixes tree-for-each ;
set-current
: delete-all-tracks ( -- )
  doc" ( -- )  Removes all mixes that have an associated track (sets all amps to 0)."
  ['] delete-all-tracks-cb 0 make-proc undef as-one-edit drop
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
  lambda-create , latestxt 0 make-proc
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
: reverse-track { trk -- }
  doc" ( trk -- )  Reverses the order of its mixes (it changes various mix begin times)."
  #f trk 0 track each mix? if not ( toggle flag ) leave then end-each if
    trk reverse-track-cb undef as-one-edit drop
  else
    'no-such-track '( get-func-name trk ) fth-throw
  then
;
previous

: track->vct { trk chn -- v }
  doc" ( trk chan -- v )  Places track data in vct."
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

: save-track { trk fname chn -- }
  doc" ( trk filename chan -- )  Saves track data (as floats) in file FILENAME."
  trk track? if
    trk track-chans { chans }
    nil nil { fd v }
    chn #t  =
    chans 1 = &&
    chn integer? chn chans < && || if
      chn #t = if 0 else chn then { current-chan }
      trk current-chan track->vct to v
      :file fname :channels 1 :srate #f srate
      :comment $" written by save-track" _ open-sound-file to fd
    else
      chn #t = chans 0> && if
	:file fname :channels chans :srate #f srate
	:comment $" written by save-track" _ open-sound-file to fd
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
    fd v v length vct->sound-file drop
    fd v length 4 * close-sound-file drop
  else
    'no-such-track '( get-func-name trk ) fth-throw
  then
;

: track-maxamp { id chan -- amp }
  doc" ( id chan -- max-amp )  Returns the max amp in the given track"
  id chan track->vct vct-peak
;

: transpose-track { trk semitones -- }
  doc" ( trk semitones -- )  Transposes each mix in track by semitones."
  trk  2.0 semitones 12.0 f/ f**  trk track-speed f*  set-track-speed drop
;

\ 1 2.0 retempo-track
: retempo-track { trk tempo -- }
  doc" ( trk tempo -- )  \
Changes the inter-mix begin times of mixes in track by TEMPO (> 1.0 is faster)."
  trk dup track-tempo tempo f* set-track-tempo drop
;

hide
: filter-track-cb ( flt reader -- proc; y self -- r )
  lambda-create , , latestxt 1 make-proc
 does> ( y self -- r )
  { y self }
  self @ { reader }
  self cell+ @ { flt }
  reader read-track-sample { val }
  flt val fir-filter y f+ val f-
;
set-current
: filter-track { trk fir-filter-coeffs -- }
  doc" ( track-id fir-filter-coeffs -- )  Filter track data using FIR filter coeffs:\n\
track-id '( 0.1 0.2 0.3 0.3 0.2 0.1 ) filter-track"
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

: track-properties     ( id -- props ) object-id 'track-property property-ref ;
: set-track-properties ( id val -- )   swap object-id 'track-property rot property-set! ;

: track-property { key id -- val }
  doc" ( key id -- val )  \
Returns the value associated with KEY in the given track's property list, or #f."
  id track? if
    id track-properties dup hash? if key hash-ref else drop #f then
  else
    'no-such-track '( get-func-name id ) fth-throw
  then
;

: set-track-property { key val id -- }
  doc" ( key val id -- )  Sets the value VAL to KEY in the given track's property list."
  id track? if
    id track-properties { props }
    props hash? if
      props key val hash-set!
    else
      make-hash to props
      props key val hash-set!
      id props set-track-properties
    then
  else
    'no-such-track '( get-func-name id ) fth-throw
  then
;

\ mix-click-info

: mix-click-info { id -- #t }
  doc" ( id -- #t )  A mix-click-hook function that describes a mix and its properties.\n\
mix-click-hook ' mix-click-info 1 make-proc add-hook!"
  id mix-home car { mid }
  id mix-name if $"  (\"%s\")" '( id mix-name ) string-format else "" then { mname }
  $"        mix id: %d%s\n"    '( id mname )    string-format { info-string }
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

\ mix.fs ends here
