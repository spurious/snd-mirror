(export all-pass
all-pass?
amplitude-modulate
array-interp
asymmetric-fm
asymmetric-fm?
autocorrelate
bartlett-window
bartlett-hann-window
blackman2-window
blackman3-window
blackman4-window
blackman5-window
blackman6-window
blackman7-window
blackman8-window
blackman9-window
blackman10-window
bohman-window
cauchy-window
clear-array
clear-sincs
clm-print
clm-table-size
comb
comb?
connes-window
continue-sample->file
continue-frame->file
contrast-enhancement
convolution
convolve
convolve-files
convolve?
correlate
db->linear
degrees->radians
delay
delay-tick
delay?
dolph-chebyshev-window
dot-product
env
env-interp
env?
exponential-window
file->frame
file->frame?
file->sample
file->sample?
filter
filter?
filtered-comb
filtered-comb?
fir-filter
fir-filter?
flat-top-window
firmant
firmant?
formant
formant-bank
formant?
frame->file
frame->file?
frame->frame
frame->list
frame->sample
frame+
frame*
frame?
frame-ref
frame-set!
gaussian-window
granulate
granulate?
hamming-window
hann-window
hann-poisson-window
hz->radians
iir-filter
iir-filter?
in-any
ina
inb
kaiser-window
linear->db
locsig
locsig?
locsig-ref
locsig-reverb-ref
locsig-reverb-set!
locsig-set!
locsig-type
make-all-pass
make-asymmetric-fm
make-comb
make-convolve
make-delay
make-env
make-fft-window
make-file->frame
make-file->sample
make-filter
make-filtered-comb
make-fir-coeffs
make-fir-filter
make-firmant
make-formant
make-frame
make-frame->file
make-granulate
make-iir-filter
make-locsig
make-mixer
make-move-sound
make-moving-average
make-notch
make-one-pole
make-one-zero
make-oscil
make-phase-vocoder
make-polyshape
make-pulse-train
make-rand
make-rand-interp
make-readin
make-sample->file
make-sawtooth-wave
make-scalar-mixer
make-sine-summation
make-square-wave
make-src
make-ssb-am
make-sum-of-cosines
make-sum-of-sines
make-ncos
make-nsin
make-nrxycos
make-nrxysin
make-table-lookup
make-triangle-wave
make-two-pole
make-two-zero
make-wave-train
make-waveshape
mixer*
mixer+
mixer?
mixer-ref
mixer-set!
mlt-sine-window
move-locsig
move-sound
move-sound?
moving-average
moving-average?
multiply-arrays
mus-apply
mus-array-print-length
mus-channel
mus-channels
mus-chebyshev-first-kind
mus-chebyshev-second-kind
mus-close
mus-data
mus-describe
mus-feedback
mus-feedforward
mus-fft
mus-file-buffer-size
mus-file-name
mus-float-equal-fudge-factor
mus-frequency
mus-generator?
mus-hop
mus-increment
mus-input?
mus-interpolate
mus-interp-all-pass
mus-interp-bezier
mus-interp-hermite
mus-interp-lagrange
mus-interp-linear
mus-interp-none
mus-interp-sinusoidal
mus-interp-type
mus-length
mus-location
mus-mix
mus-name
mus-offset
mus-order
mus-output?
mus-phase
mus-ramp
mus-rand-seed
mus-random
mus-reset
mus-run
mus-scaler
mus-set-formant-radius-and-frequency
mus-srate
mus-width
mus-xcoeff
mus-xcoeffs
mus-ycoeff
mus-ycoeffs
notch
notch?
one-pole
one-pole?
one-zero
one-zero?
oscil
oscil?
out-any
outa
outb
outc
outd
partials->polynomial
partials->wave
partials->waveshape
parzen-window
phase-vocoder
phase-vocoder?
phase-partials->wave
poisson-window
polar->rectangular
polynomial
polyshape
polyshape?
pulse-train
pulse-train?
phase-vocoder-amp-increments
phase-vocoder-amps
phase-vocoder-freqs
phase-vocoder-phase-increments
phase-vocoder-phases
radians->degrees
radians->hz
rand
rand-interp
rand-interp?
rand?
readin
readin?
rectangular->polar
rectangular-window
riemann-window
ring-modulate
rv2-window
rv3-window
rv4-window
samaraki-window
sample->file
sample->file?
sample->frame
samples->seconds
sawtooth-wave
sawtooth-wave?
seconds->samples
sine-bank
sine-summation
sine-summation?
spectrum
square-wave
square-wave?
src
src?
ssb-am
ssb-am?
sum-of-cosines
sum-of-cosines?
sum-of-sines
sum-of-sines?
ncos
ncos?
nsin
nsin?
nrxycos
nrxycos?
nrxysin
nrxysin?
table-lookup
table-lookup?
tap
triangle-wave
triangle-wave?
tukey-window
two-pole
two-pole?
two-zero
two-zero?
ultraspherical-window
wave-train
wave-train?
waveshape
waveshape?
welch-window
array->file
file->array
make-sound-data
mus-aifc
mus-aiff
mus-alaw
mus-alsa-buffer-size
mus-alsa-buffers
mus-alsa-capture-device
mus-alsa-device
mus-alsa-playback-device
mus-alsa-squelch-warning
mus-audio-adat-in
mus-audio-adat-out
mus-audio-aes-in
mus-audio-aes-out
mus-audio-amp
mus-audio-aux-input
mus-audio-aux-output
mus-audio-bass
mus-audio-cd
mus-audio-channel
mus-audio-close
mus-audio-dac-filter
mus-audio-dac-out
mus-audio-default
mus-audio-digital-in
mus-audio-digital-out
mus-audio-direction
mus-audio-duplex-default
mus-audio-format
mus-audio-igain
mus-audio-imix
mus-audio-line
mus-audio-line1
mus-audio-line2
mus-audio-line3
mus-audio-line-in
mus-audio-line-out
mus-audio-microphone
mus-audio-mixer
mus-audio-mixer-read
mus-audio-mixer-write
mus-audio-ogain
mus-audio-open-input
mus-audio-open-output
mus-audio-pcm
mus-audio-pcm2
mus-audio-port
mus-audio-read
mus-audio-reclev
mus-audio-report
mus-audio-samples-per-channel
mus-audio-spdif-in
mus-audio-spdif-out
mus-audio-speakers
mus-audio-srate
mus-audio-synth
mus-audio-systems
mus-audio-treble
mus-audio-write
mus-b24int
mus-bdouble
mus-bdouble-unscaled
mus-bfloat
mus-bfloat-unscaled
mus-bicsf
mus-bint
mus-bintn
mus-bshort
mus-byte
mus-bytes-per-sample
mus-caff
mus-clipping
mus-data-format-name
mus-data-format->string
mus-error-type->string
mus-expand-filename
mus-file-clipping
mus-file-prescaler
mus-header-raw-defaults
mus-header-type-name
mus-header-type->string
mus-ircam
mus-l24int
mus-ldouble
mus-ldouble-unscaled
mus-lfloat
mus-lfloat-unscaled
mus-lint
mus-lintn
mus-lshort
mus-mulaw
mus-netbsd-set-outputs
mus-next
mus-nist
mus-oss-set-buffers
mus-out-format
mus-prescaler
mus-raw
mus-rf64
mus-riff
mus-sound-chans
mus-sound-close-input
mus-sound-close-output
mus-sound-comment
mus-sound-data-format
mus-sound-data-location
mus-sound-datum-size
mus-sound-duration
mus-sound-forget
mus-sound-frames
mus-sound-header-type
mus-sound-length
mus-sound-loop-info
mus-sound-mark-info
mus-sound-maxamp
mus-sound-maxamp-exists?
mus-sound-open-input
mus-sound-open-output
mus-sound-prune
mus-sound-read
mus-sound-reopen-output
mus-sound-report-cache
mus-sound-samples
mus-sound-seek-frame
mus-sound-srate
mus-sound-type-specifier
mus-sound-write
mus-sound-write-date
mus-soundfont
mus-sun-set-outputs
mus-svx
mus-ubshort
mus-ubyte
mus-ulshort
mus-unknown
mus-unsupported
mus-voc
new-sound-hook
sound-data-add!
sound-data-chans
sound-data-copy
sound-data-fill!
sound-data-length
sound-data-maxamp
sound-data-multiply!
sound-data-offset!
sound-data?
sound-data-peak
sound-data-ref
sound-data-reverse!
sound-data-scale!
sound-data-set!
sound-data->vct
vct->sound-data
sound-data*
sound-data+
abort
add-colormap
add-directory-to-view-files-list
add-file-filter
add-file-sorter
add-file-to-view-files-list
add-mark
add-player
add-sound-file-extension
add-source-file-extension
add-to-main-menu
add-to-menu
add-transform
add-watcher
after-apply-controls-hook
after-edit-hook
after-graph-hook
after-lisp-graph-hook
after-open-hook
after-save-as-hook
after-save-state-hook
after-transform-hook
amp-control
amp-control-bounds
apply-controls
as-one-edit
ask-before-overwrite
audio-input-device
audio-output-device
auto-resize
auto-update
auto-update-interval
autocorrelation
axis-info
axis-label-font
axis-numbers-font
background-gradient
bad-header-hook
basic-color
beats-per-measure
beats-per-minute
before-close-hook
before-exit-hook
before-save-as-hook
before-save-state-hook
before-transform-hook
bind-key  
bold-peaks-font
bomb
c-g?
c-g!
cepstrum
channel-amp-envs
channel-data
channel-properties
channel-style
channel->vct
channel-widgets
channels
channels-combined
channels-separate
channels-superimposed
chans
clear-listener
clear-minibuffer
clip-hook
clipping
clm-channel
close-hook
close-sound
color-cutoff
color-dialog
color-hook
color-inverted
color?
color-scale
color->list
colormap
colormap-name
colormap?
colormap-ref
colormap-size
comment
contrast-control
contrast-control-amp
contrast-control-bounds
contrast-control?
controls->channel
convolve-selection-with
convolve-with
copy-context
copy-sample-reader
count-matches
current-edit-position
current-font
cursor
cursor-color
cursor-context
cursor-cross
cursor-in-middle
cursor-in-view
cursor-line
cursor-location-offset
cursor-on-left
cursor-on-right
cursor-position
cursor-size
cursor-style
cursor-update-interval
dac-combines-channels
dac-hook
dac-size
data-color
data-format
data-location
data-size
default-output-chans
default-output-data-format
default-output-srate
default-output-header-type
define-envelope
delete-colormap
delete-file-filter
delete-file-sorter
delete-mark
delete-marks
delete-sample
delete-samples
delete-selection
delete-transform
delete-watcher
dialog-widgets
display-edits
doit-again-button-color
doit-button-color
dont-normalize
dot-size
draw-axes
draw-dot
draw-dots
draw-line
draw-lines
draw-mark-hook
draw-mix-hook
draw-string
drop-hook
during-open-hook
edit-fragment
edit-header-dialog
edit-hook
edit-list->function
edit-position
edit-properties
edit-tree
edits
env-channel
env-channel-with-base
env-selection
env-sound
enved-add-point
enved-amplitude
enved-base
enved-clip?
enved-delete-point
enved-dialog
enved-envelope
enved-filter
enved-filter-order
enved-hook
enved-in-dB
enved-move-point
enved-power
enved-spectrum
enved-srate
enved-style
enved-target
enved-wave?
enved-waveform-color
envelope-exponential
envelope-linear
eps-bottom-margin
eps-file
eps-left-margin
eps-size
exit
exit-hook
expand-control
expand-control-bounds
expand-control-hop
expand-control-jitter
expand-control-length
expand-control?
expand-control-ramp
fft
fft-log-frequency
fft-log-magnitude
fft-window
fft-window-alpha
fft-window-beta
file-name
fill-polygon
fill-rectangle
filter-channel
filter-control-coeffs
filter-control-envelope
filter-control-in-dB
filter-control-in-hz
filter-control-order
filter-control?
filter-control-waveform-color
filter-selection
filter-sound
find-channel
find-dialog
find-mark
find-sound
finish-progress-report
focus-widget
foreground-color
forget-region
fourier-transform
frames
free-player
free-sample-reader
gc-off
gc-on
gl-graph->ps
goto-listener-end
graph
graph-as-sonogram
graph-as-spectrogram
graph-as-wavogram
graph-color
graph-cursor
graph-data
graph-dots
graph-dots-and-lines
graph-filled
graph-hook
graph-lines
graph-lollipops
graph-once
graph-style
graph->ps
graphs-horizontal
grid-density
haar-transform
header-type
help-button-color
help-dialog
help-hook
hide-widget
highlight-color
html-dir
html-program
in
info-dialog
initial-graph-hook
insert-file-dialog
insert-region
insert-sample
insert-samples
insert-selection
insert-silence
insert-sound
just-sounds
key
key-binding
key-press-hook
keyboard-no-action
ladspa-dir
left-sample
lisp-graph
lisp-graph-hook
lisp-graph?
lisp-graph-style
listener-click-hook
listener-color
listener-font
listener-prompt
listener-selection
listener-text-color
log-freq-start
main-menu
main-widgets
make-color
make-graph-data
make-mix-sample-reader
make-player
make-region
make-region-sample-reader
make-sample-reader
make-snd->sample
make-variable-graph
map-chan
map-channel
mark-click-hook
mark-color
mark-context
mark-drag-hook
mark-drag-triangle-hook
mark-home
mark-hook
mark-name
mark?
mark-sample
mark-sync
mark-sync-max
mark-tag-height
mark-tag-width
marks
max-regions
max-transform-peaks
max-virtual-ptrees
maxamp
maxamp-position
menu-widgets
min-dB
minibuffer-history-length
mix
mix-amp
mix-amp-env
mix-click-hook
mix-color
mix-dialog-mix
mix-drag-hook
mix-file-dialog
mix-length
mix-home
mix?
mix-name
mix-position
mix-properties
mix-region
mix-release-hook
mix-sample-reader?
mix-selection
mix-speed
mix-sync
mix-sync-max
mix-tag-height
mix-tag-width
mix-tag-y
mix-vct
mix-waveform-height
mixes
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
mus-audio-describe
mus-error-hook
name-click-hook
new-sound
new-sound-dialog
new-widget-hook
next-sample
normalize-by-channel
normalize-by-sound
normalize-channel
normalize-globally
open-file-dialog
open-file-dialog-directory
open-hook
open-raw-sound
open-raw-sound-hook
open-sound
optimization
optimization-hook
orientation-dialog
orientation-hook
output-comment-hook
output-name-hook
pad-channel
pausing
peak-env-hook
peak-env-info
peaks
peaks-font
play
play-and-wait
play-channel
play-hook
playing
play-mix
play-region
play-selection
player-home
player?
players
position-color
position->x
position->y
preferences-dialog
previous-sample
print-dialog
print-hook
print-length
progress-report
prompt-in-minibuffer
ptree-channel
pushed-button-color
quit-button-color
ramp-channel
read-hook
read-mix-sample
read-only
read-peak-env-info-file
read-region-sample
read-sample
recorder-dialog
redo
region-chans
region-frames
region-graph-style
region-home
region-maxamp
region-maxamp-position
region?
region-position
region-sample
region-sample-reader?
region-srate
region->vct
regions
remove-from-menu
report-in-minibuffer
reset-button-color
reset-controls
reset-listener-cursor
restore-controls
restore-region
reverb-control-decay
reverb-control-feedback
reverb-control-length
reverb-control-length-bounds
reverb-control-lowpass
reverb-control?
reverb-control-scale
reverb-control-scale-bounds
reverse-channel
reverse-selection
reverse-sound
revert-sound
right-sample
run
sample
sample-reader-at-end?
sample-reader-home
sample-reader?
sample-reader-position
samples
sash-color
save-controls
save-dir
save-edit-history
save-envelopes
save-hook
save-listener
save-macros
save-marks
save-region
save-region-dialog
save-selection
save-selection-dialog
save-sound
save-sound-as
save-sound-dialog
save-state
save-state-file
save-state-hook
scale-by
scale-channel
scale-selection-by
scale-selection-to
scale-to 
scan-chan
scan-channel
script-arg
script-args
search-procedure
select-all
select-channel
select-channel-hook
select-sound
select-sound-hook
selected-channel
selected-data-color
selected-graph-color
selected-sound
selection-chans
selection-color
selection-context
selection-creates-region
selection-frames
selection-maxamp
selection-maxamp-position
selection-member?
selection?
selection-position
selection-srate
send-mozilla
short-file-name
show-all-axes
show-all-axes-unlabelled
show-axes
show-backtrace
show-bare-x-axis
show-controls
show-grid
show-indices
show-listener
show-marks
show-mix-waveforms
show-no-axes
show-selection-transform
show-sonogram-cursor
show-transform-peaks
show-widget
show-x-axis
show-x-axis-unlabelled
show-y-zero
sinc-width
smooth-channel
smooth-selection
smooth-sound
snd-color
snd-error
snd-error-hook
snd-font
snd-gcs
snd-help
*snd-opened-sound*
snd-print
snd-spectrum
snd-tempnam
snd->sample
snd->sample?
snd-url
snd-urls
snd-version
snd-warning
snd-warning-hook
sound-file-extensions
sound-file?
sound-files-in-directory
sound-loop-info
sound?
sound-properties
sound-widgets
soundfont-info
sounds
spectro-cutoff
spectro-hop
spectro-start
spectro-x-angle
spectro-x-scale
spectro-y-angle
spectro-y-scale
spectro-z-angle
spectro-z-scale
speed-control
speed-control-as-float
speed-control-as-ratio
speed-control-as-semitone
speed-control-bounds
speed-control-style
speed-control-tones
squelch-update
srate
src-channel
src-selection
src-sound
start-hook
start-playing
start-playing-hook
start-playing-selection-hook
start-progress-report
stop-dac-hook
stop-player
stop-playing
stop-playing-hook
stop-playing-selection-hook
swap-channels
sync
sync-max
syncd-marks
temp-dir
text-focus-color
time-graph
time-graph?
time-graph-style
time-graph-type
tiny-font
tracking-cursor-style
transform-dialog
transform-frames
transform-graph
transform-graph?
transform-graph-style
transform-graph-type
transform-normalization
transform?
transform-sample
transform-size
transform->vct
transform-type
trap-segfault
unbind-key  
undo
undo-hook
update-hook
update-lisp-graph
update-sound
update-time-graph
update-transform-graph
variable-graph?
vct->channel
view-files-amp
view-files-amp-env
view-files-dialog
view-files-files
view-files-selected-files
view-files-select-hook
view-files-sort
view-files-speed
view-files-speed-style
view-mixes-dialog
view-regions-dialog
view-sound
walsh-transform
wavelet-transform
wavelet-type
wavo-hop
wavo-trace
widget-position
widget-size
widget-text
window-height
window-property
window-property-changed-hook
window-width
window-x
window-y
with-background-processes
with-file-monitor
with-gl
with-mix-tags
with-relative-panes
with-tracking-cursor
with-verbose-cursor
write-peak-env-info-file
x-axis-as-clock
x-axis-as-percentage
x-axis-in-beats
x-axis-in-measures
x-axis-in-samples
x-axis-in-seconds
x-axis-label
x-axis-style
x-bounds
x-position-slider
x->position
x-zoom-slider
xramp-channel
y-axis-label
y-bounds
y-position-slider
y->position
y-zoom-slider
zero-pad
zoom-color
zoom-focus-active
zoom-focus-left
zoom-focus-middle
zoom-focus-right
zoom-focus-style

make-vct
vct-add!
vct-subtract!
vct-copy
vct-length
vct-multiply!
vct-offset!
vct-ref
vct-scale!
vct-fill!
vct-set!
vct-map!
vct-peak
vct?
list->vct
vct->list
vector->vct
vct->vector
vct-move!
vct-subseq
vct
vct-reverse!
vct->string
vct*
vct+

pi
verbose-cursor
set-sample
set-samples
snd-transform
data-clipped
analyze-ladspa
mus-file-data-clipped
restore-marks
run-internal
run-eval
cursor-follows-play
continuation?
little-endian?
fmod
bes-i0
declare
define+
*snd-loaded-files*
*snd-remember-paths*
)

(if (defined? 'bes-j0)
    (export
bes-j0
bes-j1
bes-jn
bes-y0
bes-y1
bes-yn
erf
erfc
lgamma))

(if (defined? 'gsl-elliptk)
    (export
gsl-ellipk
gsl-ellipj
gsl-roots
gsl-gegenbauer
))

