

;; Dave Phillips ladspa help.

(provide 'snd-ladspa-help.scm)

(define ladspa-help-texts (list

"cmt" "canyon_delay" "Canyon delay"
 "A deep stereo cross-delay with built-in low pass filters. Note: This effect works only with stereo soundfiles !"

"delay" "delay_5s"

"Delay 5s" "The delay time unit is in seconds. The balance slider controls the strength of the delays."

"delayorama_1402" "delayorama"

"Delayorama" "Random seed: Controls the random numbers that will be used to stagger the delays and amplitudes if random is turned up on them. Changing this forces the random values to be recalulated. Input gain (dB): Controls the gain of the input signal in decibels. Feedback (%): Controls the amount of output signal fed back into the input. Number of taps: Controls the number of taps in the delay. First delay (s): The time (in seconds) of the first delay. Delay range (s): The time difference (in seconds) between the first and last delay. Delay change: The scaling factor between one delay and the next. Delay random (%): The random factor applied to the delay. Amplitude change: The scaling factor between one amplitude and the next. Amplitude random (%): The random factor applied to the amplitude. Dry/wet mix: The level of delayed sound mixed into the output."

"cmt" "fbdelay_5s"

"Feedback delay (5 sec)" "Feedback delay with maximum delay time of 5 seconds."
"fad_delay_1192" "fadDelay"

"Fractionally addressed delay line"
 "A fixed ring buffer delay implementation. Has different dynamics than a normal delay, more suitable for certain things. Changes in delay length are generally more pleasing, but delays >2s long have reduced sound quality."

"cmt" "grain_scatter"

"Granular scatter processor"
 "This plugin generates an output audio stream by scattering short `grains' of sound from an input stream. It is possible to control the length and envelope of these grains, how far away from their source time grains may be scattered and the density (grains/sec) of the texture produced."


"tape_delay_1211" "tapeDelay"


"Tape delay" "Correctly models the tape motion and some of the smear effect, but there is no simulation for the head saturation yet. The way the tape accelerates and decelerates gives a nicer delay effect for many purposes."


"decimator_1202" "decimator"


"Decimator" "Decimates by reducing the effective sample rate and reducing the bit depth of the input signal. Allows non-integer values for smooth transitions between clean and low-fidelity signals. Bit depth: The bit depth that the signal will be reduced to. Sample rate scalar: Multiplies (scales) the signal sample rate by this value."

"diode_1185" "diode"

"Diode" "Mangles the signal as if it had been passed through a diode rectifier network. You should probably follow this with a DC offset remover, unless you want the offset. Mode: 0 for none, 1 for half wave, 2 for full wave, 3 for silence. The mode parameter is continuously variable from through to half-wave rectification to full-wave to silence."

"foverdrive_1196" "foverdrive" 
"Fast overdrive" "A simple overdrive. Compresses the extreme peaks to make a sound similar to an overdriven amplifier. Level: Controls the point at which the signal starts to distort and the degree of distortion."
"foldover_1213" "foldover"
"Foldover distortion" "Uses a sine wave approximation to simulate valve-style foldover distortion. Probably should have a DC offset remover on the output, but it's not always necessary. Drive: Controls the degree of distortion. Skew: Controls the asymmetry of the waveform."


"gsm_1215" "gsm"

"GSM simulator" "Encodes and decodes a signal using the GSM voice compression system. Has the effect of making the signal sound like it is being sent over a European mobile phone network. Dry/wet mix: 0 will give you the dry signal (but with the appropriate amount of delay), 1 will give you a totally wet signal.  Number of passes: The number of times the signal is sent through the encode/decode process. Increases the CPU consumption almost linearly, and it will become more peaky so less friendly to realtime processing.  Error rate (bits/block): The number of simulated bits that get changed during the transmission process."

"overdrive_1182" "overdrive"

"Mono overdrive" "A basic overdrive effect, with controls for the degree of compression and for the distortion. Amps limit (in dB relative to nominal 0): Mostly this parameter is used to artificially lower the headroom of the amp, but it can also be used to counteract the effects of hosts that ignore the audio input range hints. If the host is using 16-bit int WAV files then a good guess for limit is +80dB. Drive level: This controls the degree of amplifier compression. Values above 1.0 will work, but they produce unpredictable output levels. Lowering the limit is a better way of increasing the distortion. Low-density coloration: Controls the amplitude of some low (input space) frequency amplitude distortion. High-density coloration: Controls the amplitude of some high (input space) frequency amplitude distortion."

"satan_maximiser_1408" "satanMaximiser"

"Barry's Satan maximizer" 
"Compresses signals with a stupidly short attack and decay, infinite ratio and hard knee. Not really a compressor, but good harsh distortion. Decay time (samples): Controls the envelope decay time. Knee point (dB): Controls the knee roll-off point, i.e. the point above which the compression kicks in. 0 will have no effect, -90 will remove virtually the entire dynamic range."

"sifter_1210" "sifter"
"Signal sifter" "Sorts and mixes blocks of the input signal to give a 'bumpy ramp' effect. Certain types of input will produce silence on the output (mostly ones with only low frequency components). This is a very odd effect, and doesn't really have any music applications, but can produce some interesting noises."

"overdrive_s_1183" "overdrive_s"
"Stereo overdrive" "A basic overdrive_s effect, with controls for the degree of compression and for the distortion. Amps limit (in dB relative to nominal 0): Mostly this parameter is used to artificially lower the headroom of the amp, but it can also be used to counteract the effects of hosts that ignore the audio input range hints. If the host is using 16-bit int WAV files then a good guess for limit is +80dB. Drive level: This controls the degree of amplifier compression. Values above 1.0 will work, but they produce unpredictable output levels. Lowering the limit is a better way of increasing the distortion. Low-density coloration: Controls the amplitude of some low (input space) frequency amplitude distortion. High-density coloration: Controls the amplitude of some high (input space) frequency amplitude distortion."

"transient_1206" "transient"
"Transient mangler" "No help yet, sorry"

"valve_1209" "valve" 

"Valve saturation" "A model of valve (tube) distortion, lacking some of the harmonics you would get in a real tube amp, but sounds good nonetheless. Distortion level: How hard the signal is driven against the limit of the amplifier. Distortion character: The hardness of the sound, low for softer, high for harder."

"shaper_1187" "shaper"

"Wave shaper" "Reshapes the wave by an exponential function. Inspiration was taken from the Nord module of the same name. If you are getting rubbish out then it's probably because the host isn't using the input/output range hints, which are very important for this plugin. Waveshape: Positive values have an expanding effect, negative values have a compressing effect."
 "comb_1190" "comb"
"Comb filter"
                                                      
"Band separation controls the distance between the filter's peaks. Feedback level increases the distinctive wooshy phaser sound."

"Karaoke"
                                                      
"Attempts to strip the vocals from a stereo signal. Vocal volume (dB): Controls the attenuation of the vocal (center channel) in decibels. The greater the attenuation the greater the loss of the stereo field."
"svf_1214" "svf"

"State variable filter"
                                                      
"An oversampled state variable filter with a few tweaks. Quite nice, tends to be unstable with high resonance and Q values, but good when kept under control. Filter type: (0=none, 1=LP, 2=HP, 3=BP, 4=BR, 5=AP) Select between no filtering, low-pass, high-pass, band-pass, band-reject and all-pass. Filter frequency: Cutoff frequency, beware of high values with low sample rates. Filter Q: The filter's cutoff slope. Filter resonance: The filter's resonance is sort of separate from Q but very related (implemented with feedback). Do not use with the bandpass mode."
"vcf-303 ladspa plugin"
"VCF-303"
                                                      
"Emulates the voltage controlled resonant filter of the bass synthesizer of Roland's TB-303. Move the sliders to set the filter parameters."
"mbeq_1197" "mbeq"
"multiband eq"
"Multiband equalizer"
                                                      
"This is a fairly typical multiband graphic equalizer. It's implemented using a FFT, so it takes quite a lot of CPU power, but it should have less phase effect than an equivalent filter implementation. If the input signal is at too low a sample rate then the top bands will be ignored, i.e., the highest useful band will always be a high shelf."
"cmt" "compress_peak"
"Compressor (peak tracking)"
"Move the sliders to set the compressor parameters."

"cmt" "compress_rms"
"Compressor (rms tracking)"
"Move the sliders to set the compressor parameters."
"dyson_compress_1403" "dysonCompress"
"Dyson compressor"
"Peak limit (dB) controls the desired limit of the output signal in decibels. Release time (s) controls the time in seconds taken for the compressor to relax its gain control over the input signal. (No information is available regarding the significance of the fast and normal compression ratio settings)."
"cmt" "expand_peak"
"Expander (peak tracking)"
"Move the sliders to set the expander parameters."
"cmt" "expand_rms"
"Expander (rms tracking)"
"Move the sliders to set the expander parameters."
"cmt" "limit_peak"
"Limiter (peak tracking)"
"Move the sliders to set the limiter parameters."
"cmt" "limit_rms"
"Limiter (rms tracking)"
"Move the sliders to set the limiter parameters."
"am_pitchshift_1433" "amPitchshift"
"AM Pitchshifter" "This plugin works by running a single write pointer (monotonic) and two read pointers (pitchscaled) over a ringbuffer. The output is faded between the two read pointers according to the sine of the distance from the write pointer. The design is based on the mechanism of a mechanical pitchshifter I saw in the Gemeentemuseum in Den Haag, though I'm sure it is a common enough algorithm. Pitch shift: The multiple of the output pitch. Thus, a value of 2.0 will increase the pitch by one octave. Buffer size: The order of magnitude of the buffer size. Small buffers will sound fluttery, large buffers will have flangey sounding echoes. I recommend a buffer size of 3 for a reasonable compromise, with wideband material at around 48KHz. For drums you might have to lower it, and for voiced background noises it can go higher."
"harmonic_gen_1220" "harmonicGen"
"Harmonic generator" "Allows you to add harmonics and remove the fundamental from any audio signal. Known bugs: There is no bandwidth limiting filter on the output, so it is easy to create excessive high-frequency harmonics that could cause aliasing problems. However, in practice this doesn't seem to be a serious problem. Examples: There are many interesting effects you can achieve with sine waves. One example is producing band-limited square waves from sine waves. To do this, set the parameters to 1, 0, -0.3333, 0, 0.2, 0, -0.14285, 0, 0.11111. To get a triangle-like signal use 1, 0, -0.3333, 0, -0.2, 0, -0.14285, 0, -0.11111. Fundamental magnitude: The amplitude of the fundamental of the signal. Reduce it to 0 to remove the base signal altogether, or -1 to phase-invert it. 2nd harmonic magnitude: The 2nd harmonic, its frequency is twice the frequency of the fundamental. 3rd harmonic magnitude: The 3rd harmonic, its frequency is three times the frequency of the fundamental. Even harmonics add a distorted feel to the sound. Valve (tube) amplifiers introduce distortions at all the harmonics, transistor amplifiers only introduce distortion into the odd harmonics."
"pitch_scale_1194" "pitchScaleHQ"
"Pitch scaler" "A pitch shifter implementation that scales the harmonics appropriately with the base frequencies. It is an implementation of Stephen M. Sprenger's pitch scaler design. It gives reasonable, general purpose results for small changes, but won't give Antares or Eventide anything to worry about. The FFT block size and oversampling has been kept at reasonable levels to keep the CPU usage low. Pitch coefficient: The pitch scaling factor. A value of 2.0 will increase the pitch by one octave, .50 will lower it by one octave, etc.""pitch_scale_1194" "pitchScaleHQ"
"Pitch scaler"
"A pitch shifter implementation that scales the harmonics appropriately with the base frequencies. It is an implementation of Stephen M. Sprengler's pitch scaler design. It gives reasonable, general purpose results for small changes, but won't give Antares or Eventide anything to worry about. The FFT block size and oversampling has been kept at reasonable levels to keep the CPU usage low. Pitch coefficient: The pitch scaling factor, a value of 2.0 will increase the pitch by one octave, etc."
"multivoice_chorus_1201" "multivoiceChorus"
"Multivoice chorus"
"This is an implementation of a Multivoice (as opposed to Multiscale) chorus algorithm. It uses a novel sinc-based noise interpolation method to produce a subtle modulation law, which makes it possible to get away with larger numbers of voices without the metallic, artificial sound common in chorus effects. Voice separation (ms): The individual voices can either be running at the same base delay (set this to zero) or staggered. Setting this to non-zero values can make the output sound richer, but will make it sound grainy with some type of signals. Detune (%): The maximum amount that a voice will be detuned by. A value of 1 is recommended, but you may be able to get away with higher values if the signal is less harmonic. LFO frequency (Hz): The frequency that the detune effect will be modulated at. A matter of taste, for most types of input a lower value will be more subtle. Output attenuation (dB): With large numbers of voices the output can become too high, so use this to trim the amplitude to a more helpful level."
"dj_flanger_1438" "djFlanger"
"DJ Flanger" "This is a flanger which is more or less typical of DJ mixing desks. Requested by Patrick Shirkey. LFO Sync: When turned from off to on it resets the phase of the LFO back to the start of the cycle. Used to sync the LFO to the track. LFO Period: The cycle period of the LFO in seconds. LFO Depth: The maximum delay the LFO will use to flange, in milliseconds. Feedback: The amount of the delayed output that is mixed back into the delay."
"flanger_1191" "flanger"
"Flanger"
"A digital flanger implementation. Uses a novel zero excursion and a controlled bandwidth modulation function that should make the modulation less repetitive and noticeable. This effect is similar in character to a phaser, the main difference is that a phaser sounds more regular and stable. Delay base (ms): This is the offset from the input time that the detuned delay moves around. 10 is probably a good starting value. Max slowdown (ms): This is the maximum delay that will be applied to the delayed signal, relative to the dry signal. LFO frequency (Hz): This is the core frequency that the 'LFO' will move at. The LFO isn't actually an oscillator, but it does change periodically. Feedback: Feedback applied from the output to the input. Increases the depth of the effect, but makes it sound less like a real flanger."
"phasers_1217" "lfoPhaser"
"retro_flange_1208" "retroFlange" 
"Retro flanger"
"A model of someone flanging the input. Models the tape saturation effects and frequency smear. The smear could probably be done better. Average stall (ms): The average time difference between the two tapes, per stall. Flange frequency (Hz): The rate the tape is stalled at."
"ringmod_1188" "ringmod_1i1o1l"
"Ring modulation Help"
"Ring or amplitude modulation with LFO. Move the sliders to set the ring modulation parameters. Modulation depth key: 0=none 1=AM 2=RM"
"cmt" "freeverb3"
"Freeverb3" "Jezar's famous reverb. Move the sliders to set the reverb parameters. This effect works only with stereo soundfiles! See the Freeverb Web page at http://www.jw015a0732.pwp.blueyonder.co.uk/freeverb.htm for more information."
"gverb_1216" "gverb"
"Gverb" "A mono in, stereo out reverb implementation by Juhana Sadeharju (kouhia at nic.funet.fi). Steve Harris ported it to LADSPA and did some testing. Please contact Juhana directly regarding any bugs you find. Roomsize (m): The size of the room, in meters. Excessivly large, and excessivly small values will make it sound a bit unrealistic. Values of around 30 sound good. Reverb time (s): Reverb decay time, in seconds. 7 is a good place to start. Damping: This controls the high-frequency damping (a lowpass filter). Values near 1 will make it sound very bright, values near 0 will make it sound very dark. Input bandwidth: This is like a damping control for the input. It has a similar effect to the damping control, but is subtly different. Dry signal level (dB): The amount of dry signal to be mixed with the reverbed signal. Early reflection level (dB): The quantity of early reflections (scatter reflections directly from the source). Think of Lexicon's ambience patches. Tail level (dB): The level of the classic reverb tail reflections."
"plate_1423" "plate"
"Plate reverb" "A physical model of a steel plate reverb. Based on Josep Comajuncosas' gong model, it uses 8 linear waveguides to model the plate. Reverb time: Controls the RT60 time of the reverb. Actually controls the size of the plate. The mapping betwwen plate size and RT60 time is just a heuristic, so it's not very accurate. Damping: Controls the degree that the surface of the plate is damped. Dry/wet mix: Controls the balance between the dry and wet signals."
"imp_1199" "imp"
"Impulse convolver" "This is a convolver for a set of fairly short impulses. The set of impulses has to be compiled in; currently they are:
ID   Impulse source

1 Unit impulse (identity)
2 My flat (light natural reverb)
3 Yamaha Marshall stack simulator
4 Fender 68 Vibrolux (SM57 on axis)
5 Fender 68 Vibrolux (SM57 off axis)
6 Fender 68 Vibrolux (Audio-technica AT4050)
7 Fender 68 Vibrolux (Neumann U87)
8 Fender Bassman (SM57 on axis)
9 Fender Bassman (SM57 off axis)
10 Fender Bassman (Audio-technica AT4050)
11 Fender Bassman (Neumann U87)
12 Fender Superchamp (SM57 on axis)
13 Fender Superchamp (SM57 off axis)
14 Fender Superchamp (Audio-technica AT4050)
15 Fender Superchamp (Neumann U87)
16 Marshall JCM2000 (SM57 on axis)
17 Marshall JCM2000 (SM57 off axis)
18 Marshall Plexi (SM57 on axis)
19 Marshall Plexi (SM57 off axis)
20 Matchless Chieftain (SM57 on axis)
21 Matchless Chieftain (SM57 off axis)

The impulse ID selects the impulse to convolve with. High latency mode: If you are running with blocks that are not whole powers of two long, or you are hearing distortion, try changing this to 1. Gain (dB): Controls the gain of the output signal in decibels."

))
