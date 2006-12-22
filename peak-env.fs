\ -*- snd-forth -*-
\ peak-env.fs -- peak-env.scm -> peak-env.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Wed Dec 21 17:37:13 CET 2005
\ Changed: Thu Dec 21 18:37:28 CET 2006

\ Commentary:
\ 
\ installs save-peak-env-info if save-peak-env-info? is true

\ Code:

'snd provided? [unless]
  'forth-error '( $" peak-env.fs" $" snd is needed for peak-env-info" ) fth-throw
[then]

require clm

#t value save-peak-env-info?
$" ~/peaks" value save-peak-env-info-directory

\ defined in src/ficl/softcore/softcore.fr
\ 
\ HIDE
\ <definitions to hide>
\ SET-CURRENT
\ <words that use hidden defs>
\ PREVIOUS ( pop HIDDEN off the search order )

hide

make-hash value saved-peak-info

: peak-env-info-file-name ( snd chn -- name )
  { snd chn }
  snd file-name { fname }
  fname each { ch } [char] / ch = [char] . ch = || if fname i [char] _ string-set! then end-each
  $" %s/%s-fth-peaks-%d" '( save-peak-env-info-directory fname chn ) string-format
;

\ intended as an initial-graph-hook-function
: restore-peak-env-info-upon-open <{ snd chn dur -- #f }>
  saved-peak-info snd file-name hash-ref { peak-info }
  peak-info if
    peak-info 'data-format hash-ref snd data-format =
    peak-info 'channels    hash-ref snd channels    = &&
  else
    #t
  then if
    snd chn peak-env-info-file-name mus-expand-filename { peak-file }
    peak-file file-exists?
    peak-file file-write-date
    snd file-name file-write-date b> && if
      make-hash { vals }
      vals 'data-format snd data-format hash-set!
      vals 'channels    snd channels    hash-set!
      saved-peak-info snd file-name vals hash-set!
      snd chn peak-file read-peak-env-info-file drop
    then
  then
  #f
;

: peak-env-info-update-cb <{ snd -- }>
  save-peak-env-info? if
    snd channels 0 ?do snd i peak-env-info-file-name mus-expand-filename file-delete loop
  then
;

\ intended as a close-hook function
: save-peak-env-info <{ snd -- }>
  initial-graph-hook ['] restore-peak-env-info-upon-open add-hook!
  update-hook        ['] peak-env-info-update-cb         add-hook!
  save-peak-env-info?
  snd 0 0 peak-env-info length 0> && if
    #f { saved }
    snd channels 0 ?do
      snd i peak-env-info-file-name mus-expand-filename { peak-file }
      peak-file file-exists? not
      peak-file file-write-date
      snd file-name file-write-date b< || if
	saved unless
	  make-hash { vals }
	  vals 'data-format snd data-format hash-set!
	  vals 'channels    snd channels    hash-set!
	  saved-peak-info snd file-name vals hash-set!
	  #t to saved
	then
	snd i peak-file ['] write-peak-env-info-file 'no-such-envelope nil fth-catch 2drop
      then
    loop
  then
;

set-current

close-hook ' save-peak-env-info add-hook!
exit-hook lambda: <{ -- }> sounds each save-peak-env-info end-each ; add-hook!

previous

\ peak-env.fs ends here
