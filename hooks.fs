\ -*- snd-forth -*-
\ hooks.fs -- hooks.scm -> hooks.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Tue Aug 08 23:27:50 CEST 2006
\ Changed: Sat Dec 23 04:26:23 CET 2006

\ Commentary:

\ snd-hooks       Array with all Snd hooks.
\ reset-all-hooks ( -- )
\ with-local-hook ( hook local-hook-procs thunk -- result )

\ Code:

require examp

[defined] after-apply-controls-hook [if]
  #( after-apply-controls-hook
     after-graph-hook
     after-lisp-graph-hook
     after-open-hook
     after-save-as-hook
     after-save-state-hook
     after-transform-hook
     bad-header-hook
     before-close-hook
     before-exit-hook
     before-save-as-hook
     before-save-state-hook
     before-transform-hook
     clip-hook
     close-hook
     color-hook
     dac-hook
     draw-mark-hook
     drop-hook
     during-open-hook
     enved-hook
     exit-hook
     graph-hook
     help-hook
     initial-graph-hook
     key-press-hook
     lisp-graph-hook
     listener-click-hook
     mark-click-hook
     mark-drag-hook
     mark-drag-triangle-hook
     mark-hook
     mix-click-hook
     mix-drag-hook
     mix-release-hook
     mouse-click-hook
     mouse-drag-hook
     mouse-enter-graph-hook
     mouse-enter-label-hook
     mouse-enter-listener-hook
     mouse-enter-text-hook
     mouse-leave-graph-hook
     mouse-leave-label-hook
     mouse-leave-listener-hook
     mouse-leave-text-hook
     mouse-press-hook
     mus-error-hook
     name-click-hook
     new-sound-hook
     new-widget-hook
     open-hook
     open-raw-sound-hook
     optimization-hook
     orientation-hook
     output-comment-hook
     output-name-hook
     play-hook
     print-hook
     read-hook
     'snd-nogui provided? [unless] recorder-file-hook [then]
     save-hook
     save-state-hook
     select-channel-hook
     select-sound-hook
     snd-error-hook
     snd-warning-hook
     start-hook
     start-playing-hook
     start-playing-selection-hook
     stop-dac-hook
     stop-playing-hook
     stop-playing-selection-hook
     update-hook
     view-files-select-hook
     window-property-changed-hook )
[else]
  #()
[then] constant snd-hooks

: reset-all-hooks ( -- )
  doc" Removes all Snd hook functions."
  snd-hooks each ( hook ) dup hook? if reset-hook! else drop then end-each
  all-chans each { lst }
    lst  car { snd }
    lst cadr { chn }
    snd chn edit-hook       dup hook? if reset-hook! else drop then
    snd chn after-edit-hook dup hook? if reset-hook! else drop then
    snd chn undo-hook       dup hook? if reset-hook! else drop then
  end-each
;

: with-local-hook ( hook local-hook-procs thunk -- result )
  doc" Evaluates THUNK (an xt) with HOOK set to LOCAL-HOOK-PROCS (a list of procs), \
then restores HOOK to its previous state."
  { hook local-hook-procs thunk }
  hook hook->list { old-procs }
  hook reset-hook!
  local-hook-procs each ( proc ) hook swap add-hook! end-each
  thunk xt? if
    thunk execute
  else
    thunk proc? if
      thunk '() run-proc
    else
      #f
    then
  then ( result )
  hook reset-hook!
  old-procs each ( proc ) hook swap add-hook! end-each
;

\ draw.fs ends here
