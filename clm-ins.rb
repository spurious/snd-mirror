# clm-ins.rb -- CLM instruments translated to Snd/Ruby

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Last: Thu Sep 18 04:17:32 CEST 2003

# pluck
# vox
# fofins
# bes_fm
# fm_trumpet
# pqw_vox
# stereo_flute
# fm_bell
# fm_insect
# fm_drum
# gong
# attract
# pqw
# tubebell
# wurley
# rhodey
# hammondoid
# metal

# TODO:
# drone
# canter
# nrev
# reson
# cellon
# jl_reverb
# gran_synth
# touch_tone
# spectra
# two_tab
# lbj_piano
# resflt
# scratch
# pins
# zc
# zn
# za
# exp_snd
# expfil
# graph_eq
# anoi
# fullmix

require "ws"
include Env
include Math

TWO_PI = PI * 2.0 unless defined? TWO_PI
HALF_PI = PI * 0.5 unless defined? HALF_PI

def normalize_partials(partials)
  sum = 0.0
  parts = partials.dup
  len = parts.length
  1.step(len - 1, 2) do |i| sum += parts[i].abs end
  1.step(len - 1, 2) do |i| parts[i] /= sum end
  parts
end

# PLUCK
#
# The Karplus-Strong algorithm as extended by David Jaffe and Julius
#  Smith -- see Jaffe and Smith, "Extensions of the Karplus-Strong
#  Plucked-String Algorithm" CMJ vol 7 no 2 Summer 1983, reprinted in
#  "The Music Machine".  translated from CLM's pluck.ins
def pluck(start, dur, freq, amp, weighting = 0.5, lossfact = 0.9)
  get_optimum_c = lambda do |s, o, p|
    pa = (1.0 / o) * atan2(s * sin(o), (1.0 - s) + s * cos(o))
    tmp_int = (p - pa).floor
    pc = p - pa - tmp_int
    until pc >= 0.1
      tmp_int -= 1
      pc += 1.0
    end
    [tmp_int, (sin(o) - sin(o * pc)) / sin(o + o * pc)]
  end
  tune_it = lambda do |f, s1|
    p = mus_srate() / f
    s = s1.zero? ? 0.5 : s1
    o = hz2radians(f)
    t1, c1 = get_optimum_c.call(s, o, p)
    t2, c2 = get_optimum_c.call(1.0 - s, o, p)
    if s != 0.5 and c1.abs < c2.abs
      [1.0 - s, c1, t1]
    else
      [s, c2, t2]
    end
  end
  wt0, c, dlen = tune_it.call(freq, weighting)
  lf = lossfact.zero? ? 1.0 : [1.0, lossfact].min
  wt = wt0.zero? ? 0.5 : [1.0, wt0].min
  tab = make_vct(dlen)
  # get initial waveform in "tab" -- here we can introduce 0's to
  # simulate different pick positions, and so on -- see the CMJ
  # article for numerous extensions.  The normal case is to load it
  # with white noise (between -1 and 1).
  allp = make_one_zero(lf * (1.0 - wt), lf * wt)
  feedb = make_one_zero(c, 1.0)     # or feedb = make_one_zero(1.0, c)
  dlen.times do |i| vct_set!(tab, i, 1.0 - rbm_random(2.0)) end
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    val = vct_ref(tab, i % dlen)
    vct_set!(tab, i % dlen, (1.0 - c) * one_zero(feedb, one_zero(allp, val)))
    outa(i, amp * val, $rbm_output)
  end
end
# with_sound() do pluck(0.05, 0.01, 330, 0.1, 0.95, 0.95) end

# formant center frequencies for a male speaker (vox and pqw_vox)
Formants = {:I  => [390, 1990, 2550], :E   => [530, 1840, 2480], :AE => [660, 1720, 2410],
            :UH => [520, 1190, 2390], :A   => [730, 1090, 2440], :OW => [570,  840, 2410],
            :U  => [440, 1020, 2240], :OO  => [300,  870, 2240], :ER => [490, 1350, 1690],
            :W  => [300,  610, 2200], :LL  => [380,  880, 2575], :R  => [420, 1300, 1600],
            :Y  => [300, 2200, 3065], :EE  => [260, 3500, 3800], :LH => [280, 1450, 1600],
            :L  => [300, 1300, 3000], :I2  => [350, 2300, 3340], :B  => [200,  800, 1750],
            :D  => [300, 1700, 2600], :G   => [250, 1350, 2000], :M  => [280,  900, 2200],
            :N  => [280, 1700, 2600], :NG  => [280, 2300, 2750], :P  => [300,  800, 1750],
            :T  => [200, 1700, 2600], :K   => [350, 1350, 2000], :F  => [175,  900, 4400],
            :TH => [200, 1400, 2200], :S   => [200, 1300, 2500], :SH => [200, 1800, 2000],
            :V  => [175, 1100, 2400], :THE => [200, 1600, 2200], :Z  => [200, 1300, 2500],
            :ZH => [175, 1800, 2000], :ZZ  => [900, 2400, 3800], :VV => [565, 1045, 2400]}

# MLBVOI
# 
# translation from MUS10 of Marc LeBrun's waveshaping voice instrument
# (using FM here) this version translated (and simplified slightly)
# from CLM's mlbvoi.ins
def vox(start, dur, freq, amp, ampfun, freqfun, freqscl, voxfun, index, vibscl)
  f1 = []
  f2 = []
  f3 = []
  (voxfun.length - 1).step(1, -2) do |i|
    phon = Formants[voxfun[i]]
    x = voxfun[i - 1]
    f1.unshift(phon[0])
    f1.unshift(x)
    f2.unshift(phon[1])
    f2.unshift(x)
    f3.unshift(phon[2])
    f3.unshift(x)
  end
  beg, len = times2samples(start, dur)
  car_os = make_oscil(:frequency, 0)
  of0 = make_oscil(:frequency, 0)
  of1 = make_oscil(:frequency, 0)
  of2 = make_oscil(:frequency, 0)
  of3 = make_oscil(:frequency, 0)
  of4 = make_oscil(:frequency, 0)
  of5 = make_oscil(:frequency, 0)
  ampf = make_env(:envelope, ampfun, :scaler, amp, :duration, dur)
  frmf1 = make_env(:envelope, f1, :duration, dur)
  frmf2 = make_env(:envelope, f2, :duration, dur)
  frmf3 = make_env(:envelope, f3, :duration, dur)
  freqf = make_env(:envelope, freqfun, :duration, dur, :scaler, freqscl * freq, :offset, freq)
  per_vib = make_triangle_wave(:frequency, 6, :amplitude, freq * vibscl)
  ran_vib = make_rand_interp(:frequency, 20, :amplitude, freq * 0.01)
  ws_interrupt?()
  (beg..len).each do |i|
    frq = env(freqf) + triangle_wave(per_vib) + rand_interp(ran_vib)
    car = index * oscil(car_os, hz2radians(frq))
    frm = env(frmf1)
    frm0 = frm / frq.to_f
    frm_int = frm0.floor
    if frm_int.even?
      frq0 = hz2radians(frm_int * frq)
      frq1 = hz2radians((frm_int + 1) * frq)
      amp1 = frm0 - frm_int
      amp0 = 1.0 - amp1
    else
      frq1 = hz2radians(frm_int * frq)
      frq0 = hz2radians((frm_int + 1) * frq)
      amp0 = frm0 - frm_int
      amp1 = 1.0 - amp0
    end
    frm = env(frmf2)
    frm0 = frm / frq.to_f
    frm_int = frm0.floor
    if frm_int.even?
      frq2 = hz2radians(frm_int * frq)
      frq3 = hz2radians((frm_int + 1) * frq)
      amp3 = frm0 - frm_int
      amp2 = 1.0 - amp3
    else
      frq3 = hz2radians(frm_int * frq)
      frq2 = hz2radians((frm_int + 1) * frq)
      amp2 = frm0 - frm_int
      amp3 = 1.0 - amp2
    end
    frm = env(frmf3)
    frm0 = frm / frq.to_f
    frm_int = frm0.floor
    if frm_int.even?
      frq4 = hz2radians(frm_int * frq)
      frq5 = hz2radians((frm_int + 1) * frq)
      amp5 = frm0 - frm_int
      amp4 = 1.0 - amp5
    else
      frq5 = hz2radians(frm_int * frq)
      frq4 = hz2radians((frm_int + 1) * frq)
      amp4 = frm0 - frm_int
      amp5 = 1.0 - amp4
    end
    outa(i, env(ampf) *
                       (0.8 * (amp0 * oscil(of0, frq0 + 0.2 * car) +
                                     amp1 * oscil(of1, frq1 + 0.2 * car)) +
                             0.15 * (amp2 * oscil(of2, frq2 + 0.5 * car) +
                                           amp3 * oscil(of3, frq3 + 0.5 * car)) +
                             0.05 * (amp4 * oscil(of4, frq4 + car) +
                                           amp5 * oscil(of5, frq5 + car))), $rbm_output)
  end
end
=begin
with_sound() do
  amp_env = [0, 0, 25, 1, 75, 1, 100, 0]
  frq_env = [0, 0, 5, 0.5, 10, 0, 100, 1]
  beg = 0
  vox(beg, 2, 170, 0.4, amp_env, frq_env, 0.1,
      [0, :E, 25, :AE, 35, :ER, 65, :ER, 75, :I, 100, :UH], 0.05, 0.1)
  beg += 2.2
  vox(beg, 2, 300, 0.4, amp_env, frq_env, 0.1,
      [0, :I, 5, :OW, 10, :I, 50, :AE, 100, :OO], 0.02, 0.1)
  beg += 2.2
  vox(beg, 5, 600, 0.4, amp_env, frq_env, 0.1,
      [0, :I, 5, :OW, 10, :I, 50, :AE, 100, :OO], 0.01, 0.1)
end
=end

# FOF example
def fofins(start, dur, frq, amp, vib, f0, a0, f1, a1, f2, a2, ae = [0, 0, 25, 1, 75, 1, 100,0])
  beg, len = times2samples(start, dur)
  ampf = make_env(:envelope, ae, :scaler, amp, :duration, dur)
  frq0 = hz2radians(f0)
  frq1 = hz2radians(f1)
  frq2 = hz2radians(f2)
  foflen = mus_srate() == 22050.0 ? 100 : 200
  vibr = make_oscil(:frequency, 6)
  win_freq = TWO_PI / foflen
  wt0 = make_wave_train(:size, foflen, :frequency, frq)
  foftab = mus_data(wt0)
  foflen.times do |i|
    vct_set!(foftab, i, (a0 * sin(i * frq0) +
                             a1 * sin(i * frq1) +
                             a2 * sin(i * frq2)) * 0.5 * (1.0 - cos(i * win_freq)))
  end
  ws_interrupt?()
  (beg..len).each do |i| outa(i, env(ampf) * wave_train(wt0, vib * oscil(vibr)), $rbm_output) end
end
# with_sound() do fofins(0, 4, 270, 0.4, 0.001, 730, 0.6, 1090, 0.3, 2440, 0.1) end

# BES-FM
def j0(x)
  if x.abs < 8.0
    y = x * x
    ans1 = 57568490574.0 + y *
           (-13362590354.0 + y *
                            (651619640.7 + y *
                                          (-11214424.18 + y *
                                                         (77392.33017 + y * -184.9052456))))
    ans2 = 57568490411.0 + y *
           (1029532985.0 + y * (9494680.718 + y * (59272.64853 + y * (267.8532712 + y))))
    ans1 / ans2
  else
    ax = x.abs
    z = 8.0 / ax
    y = z * z
    xx = ax - 0.785398164
    ans1 = 1.0 + y *
           (-0.1098628627e-2 + y *
                           (0.2734510407e-4 + y * (-0.2073370639e-5 + y * 0.2093887211e-6)))
    ans2 = -0.1562499995e-1 + y *
           (0.1430488765e-3 + y * (-0.6911147651e-5 + y * (0.7621095161e-6 + y * -0.934945152e-7)))
    sqrt(0.636619772 / ax) * cos(xx) * ans1 - z * sin(xx) * ans2
  end
end

def bes_fm(start, dur, freq, amp, ratio, index)
  car_ph = mod_ph = 0.0
  car_incr = hz2radians(freq)
  mod_incr = ratio.to_f * car_incr
  ampenv = make_env(:envelope, [0, 0, 25, 1, 75, 1, 100, 0], :scaler, amp, :duration, dur)
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    outa(i, env(ampenv) * j0(car_ph), $rbm_output)
    car_ph += car_incr + index.to_f * j0(mod_ph)
    mod_ph += mod_incr
  end
end
# with_sound() do bes_fm(0, 0.5, 440, 5, 1, 8) end

# FM TRUMPET
#
# Dexter Morrill's FM-trumpet: from CMJ feb 77 p51
def fm_trumpet(start, dur, *args)
  frq1          = get_args(args, :frq1, 250.0)
  frq2          = get_args(args, :frq2, 1500.0)
  amp1          = get_args(args, :amp1, 0.5)
  amp2          = get_args(args, :amp2, 0.1)
  ampatt1       = get_args(args, :ampatt1, 0.03)
  ampdec1       = get_args(args, :ampdec1, 0.35)
  ampatt2       = get_args(args, :ampatt2, 0.03)
  ampdec2       = get_args(args, :ampdec2, 0.3)
  modfrq1       = get_args(args, :modfrq1, 250.0)
  modind11      = get_args(args, :modind11, 0.0)
  modind12      = get_args(args, :modind12, 2.66)
  modfrq2       = get_args(args, :modfrq2, 250.0)
  modind21      = get_args(args, :modind21, 0.0)
  modind22      = get_args(args, :modind22, 1.8)
  rvibamp       = get_args(args, :rvibamp, 0.007)
  rvibfrq       = get_args(args, :rvibfrq, 125.0)
  vibamp        = get_args(args, :vibamp, 0.007)
  vibfrq        = get_args(args, :vibfrq, 7.0)
  vibatt        = get_args(args, :vibatt, 0.6)
  vibdec        = get_args(args, :vibdec, 0.2)
  frqskw        = get_args(args, :frqskw, 0.03)
  frqatt        = get_args(args, :frqatt, 0.06)
  ampenv1       = get_args(args, :ampenv1, [0, 0, 25, 1, 75, 0.9, 100, 0])
  ampenv2       = get_args(args, :ampenv2, [0, 0, 25, 1, 75, 0.9, 100, 0])
  indenv1       = get_args(args, :indenv1, [0, 0, 25, 1, 75, 0.9, 100, 0])
  indenv2       = get_args(args, :indenv2, [0, 0, 25, 1, 75, 0.9, 100, 0])
  degree        = get_args(args, :degree, 0.0)
  distance      = get_args(args, :distance, 1.0)
  reverb_amount = get_args(args, :reverb_amount, 0.005)
  dur = dur.to_f
  per_vib_f = make_env(:envelope, stretch_envelope([0, 1, 25, 0.1, 75, 0, 100, 0],
                                                   25,
                                                   [100 * (vibatt / dur), 45].min,
                                                   75,
                                                   [100 * (1.0 - vibdec / dur), 55].max),
                       :scaler, vibamp, :duration, dur)
  ran_vib = make_rand_interp(:frequency, rvibfrq, :amplitude, rvibamp)
  per_vib = make_oscil(:frequency, vibfrq)
  dec_01 = [75, 100 * (1.0 - 0.01 / dur)].max
  frq_f = make_env(:envelope, stretch_envelope([0, 0, 25, 1, 75, 1, 100, 0],
                                               25,
                                               [25, 100 * (frqatt / dur)].min,
                                               75,
                                               dec_01),
                   :scaler, frqskw, :duration, dur)
  ampattpt1 = [25, 100 * (ampatt1 / dur)].min
  ampdecpt1 = [75, 100 * (1.0 - ampdec1 / dur)].max
  ampattpt2 = [25, 100 * (ampatt2 / dur)].min
  ampdecpt2 = [75, 100 * (1.0 - ampdec2 / dur)].max
  mod1_f = make_env(:envelope, stretch_envelope(indenv1, 25, ampattpt1, 75, dec_01),
                    :scaler, modfrq1 * (modind12 - modind11), :duration, dur)
  mod1 = make_oscil(:frequency, 0.0)
  car1 = make_oscil(:frequency, 0.0)
  # set frequency to zero here because it is handled multiplicatively
  # below
  car1_f = make_env(:envelope, stretch_envelope(ampenv1, 25, ampattpt1, 75, ampdecpt1),
                    :scaler, amp1, :duration, dur)
  mod2_f = make_env(:envelope, stretch_envelope(indenv2, 25, ampattpt2, 75, dec_01),
                    :scaler, modfrq2 * (modind22 - modind21), :duration, dur)
  mod2 = make_oscil(:frequency, 0.0)
  car2 = make_oscil(:frequency, 0.0)
  car2_f = make_env(:envelope, stretch_envelope(ampenv2, 25, ampattpt2, 75, ampdecpt2),
                    :scaler, amp2, :duration, dur)
  loc = make_locsig(:degree, degree, :distance, distance, :channels, mus_channels($rbm_output),
                    :reverb, reverb_amount, :output, $rbm_output, :revout, $rbm_reverb)
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    frq_change = hz2radians((1.0 + rand_interp(ran_vib)) * \
                            (1.0 + env(per_vib_f) * oscil(per_vib)) * (1.0 + env(frq_f)))
    locsig(loc, i,
           env(car1_f) * \
           oscil(car1, frq_change * (frq1 + env(mod1_f) * oscil(mod1, modfrq1 * frq_change))) + \
           env(car2_f) * \
           oscil(car2, frq_change * (frq2 + env(mod2_f) * oscil(mod2, modfrq2 * frq_change))))
  end
end
# with_sound() do fm_trumpet(0, 1) end

# PQWVOX
#
# translation of CLM pqwvox.ins (itself translated from MUS10 of MLB's
# waveshaping voice instrument (using phase quadrature waveshaping))
def pqw_vox(start, dur, freq, spacing_freq, amp, ampfun, freqfun, freqscl,
            phonemes, formant_amps, formant_shapes)
  vox_fun = lambda do |phons, which, newenv|
    # make an envelope from which-th entry of phoneme data referred to
    # by phons
    if phons.empty?
      newenv
    else
      vox_fun.call(phons[2..-1], which, newenv + [phons[0], Formants[phons[1]][which]])
    end
  end
  car_sin = make_oscil(:frequency, 0.0)
  car_cos = make_oscil(:frequency, 0.0, "initial-phase".to_sym, HALF_PI)
  frq_ratio = spacing_freq / freq.to_f
  fs = formant_amps.length
  sin_evens = Array.new(fs)
  cos_evens = Array.new(fs)
  sin_odds = Array.new(fs)
  cos_odds = Array.new(fs)
  amps = Array.new(fs)
  frmfs = Array.new(fs)
  sin_coeffs = Array.new(fs)
  cos_coeffs = Array.new(fs)
  ampf = make_env(:envelope, ampfun, :scaler, amp, :duration, dur)
  freqf = make_env(:envelope, freqfun, :scaler, freqscl * freq, :duration, dur, :offset, freq)
  per_vib = make_triangle_wave(:frequency, 6.0, :amplitude, freq * 0.1)
  ran_vib = make_rand_interp(:frequency, 20.0, :amplitude, freq * 0.05)
  fs.times do |i|
    sin_evens[i] = make_oscil(:frequency, 0.0)
    sin_odds[i] = make_oscil(:frequency, 0.0)
    cos_evens[i] = make_oscil(:frequency, 0.0, "initial-phase".to_sym, HALF_PI)
    cos_odds[i] = make_oscil(:frequency, 0.0, "initial-phase".to_sym, HALF_PI)
    amps[i] = formant_amps[i]
    shape = normalize_partials(formant_shapes[i])
    cos_coeffs[i] = partials2polynomial(shape, 1)
    sin_coeffs[i] = partials2polynomial(shape, 0)
    frmfs[i] = make_env(:envelope, vox_fun.call(phonemes, i, []), :duration, dur)
  end
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    frq = env(freqf) + triangle_wave(per_vib) + rand_interp(ran_vib)
    frqscl = hz2radians(frq * frq_ratio)
    carsin = oscil(car_sin, frqscl)
    carcos = oscil(car_cos, frqscl)
    sum = 0.0
    fs.times do |j|
      frm = env(frmfs[j])
      frm0 = frm / frq
      frm_int = frm0.floor
      if frm_int.even?
        even_freq = hz2radians(frm_int * frq)
        odd_freq = hz2radians((frm_int + 1.0) * frq)
        odd_amp = frm0 - frm_int
        even_amp = 1.0 - odd_amp
      else
        odd_freq = hz2radians(frm_int * frq)
        even_freq = hz2radians((frm_int + 1.0) * frq)
        even_amp = frm0 - frm_int
        odd_amp = 1.0 - even_amp
      end
      fax = polynomial(cos_coeffs[j], carcos)
      yfax = carsin * polynomial(sin_coeffs[j], carcos)
      sum += amps[j] *
             (even_amp * (yfax * oscil(sin_evens[j], even_freq) -
                                fax * oscil(cos_evens[j], even_freq)) +
                        odd_amp * (yfax * oscil(sin_odds[j], odd_freq) -
                                         fax * oscil(cos_odds[j], odd_freq)))
    end
    outa(i, env(ampf) * sum, $rbm_output)
  end
end
=begin
with_sound() do
  ampfun = [0, 0, 50, 1, 100, 0]
  freqfun = [0, 0, 100, 0]
  freqramp = [0, 0, 100, 1]
  pqw_vox(0, 1, 300, 300, 0.1, ampfun, freqfun, 0, [0, :L, 100, :L], [0.33, 0.33, 0.33],
          [[1, 1, 2, 0.5], [1, 0.5, 2, 0.5, 3, 1], [1, 1, 4, 0.5]])
  pqw_vox(1.2, 2, 200, 200, 0.1, ampfun, freqramp, 0.1, [0, :UH, 100, :ER], [0.8, 0.15, 0.05],
          [[1, 1, 2, 0.5], [1, 1, 2, 0.5, 3, 0.2, 4, 0.1], [1, 1, 3, 0.1, 4, 0.5]])
  pqw_vox(3.4, 2, 100, 314, 0.1, ampfun, freqramp, 0.1, [0, :UH, 100, :ER], [0.8, 0.15, 0.05],
          [[1, 1, 2, 0.5], [1, 1, 2, 0.5, 3, 0.2, 4, 0.1], [1, 1, 3, 0.1, 4, 0.5]])
  pqw_vox(5.6, 2, 200, 314, 0.1, ampfun, freqramp, 0.01, [0, :UH, 100, :ER], [0.8, 0.15, 0.05],
          [[1, 1, 2, 0.5], [1, 1, 4, 0.1], [1, 1, 2, 0.1, 4, 0.05]])
  pqw_vox(7.8, 2, 100, 414, 0.2, ampfun, freqramp, 0.01, [0, :OW, 50, :E, 100, :ER],
          [0.8, 0.15, 0.05],
          [[1, 1, 2, 0.5, 3, 0.1, 4, 0.01], [1, 1, 4, 0.1], [1, 1, 2, 0.1, 4, 0.05]])
end
=end

# STEREO-FLUTE
def stereo_flute(start, dur, freq, flow, *args)
  flow_env   = get_args(args, :flow_envelope, [0, 1, 100, 1])
  # additional time for instrument to decay
  decay      = get_args(args, :decay, 0.01) 
  noise      = get_args(args, :noise, 0.0356)
  emb_size   = get_args(args, :embouchure_size, 0.5)
  # these two are crucial for good results
  fbk_scl1   = get_args(args, :fbk_scl1, 0.5)
  fbk_scl2   = get_args(args, :fbk_scl2, 0.55)
  # from 0.0 to 1.0 along the bore
  offset_pos = get_args(args, :offset_pos, 0.764264)
  out_scl    = get_args(args, :out_scl, 1.0)
  # filter coefficients
  a0         = get_args(args, :a0, 0.7)
  b1         = get_args(args, :b1, -0.3)
  vib_rate   = get_args(args, :vib_rate, 5)
  vib_amount = get_args(args, :vib_amount, 0.03)
  ran_rate   = get_args(args, :ran_rate, 5)
  ran_amount = get_args(args, :ran_amount, 0.03)
  chns       = mus_channels($rbm_output)
  beg, len   = times2samples(start, dur)
  flowf      = make_env(:envelope, flow_env, :scaler, flow,
                        :start, beg, :end, beg + seconds2samples(dur - decay))
  periodic_vib = make_oscil(:frequency, vib_rate)
  random_vib = make_rand_interp(:frequency, ran_rate)
  breath     = make_rand(:frequency, mus_srate() / 2.0, :amplitude, 1)
  period_samples = (mus_srate() / freq).floor
  embouchure_samples = (emb_size * period_samples).floor
  embouchure = make_delay(embouchure_samples, "initial-element".to_sym, 0.0)
  bore       = make_delay(period_samples)
  offset     = (period_samples * offset_pos).floor
  reflection_lp_filter = make_one_pole(a0, b1)
  out_sig = current_diff = 0.0
  previous_out_sig = previous_tap_sig = previous_dc_blocked_a = previous_dc_blocked_b = 0.0
  ws_interrupt?()
  (beg..len).each do |i|
    delay_sig = delay(bore, out_sig)
    emb_sig = delay(embouchure, current_diff)
    current_flow = vib_amount * oscil(periodic_vib) + ran_amount * rand_interp(random_vib) +
                                                     env(flowf)
    current_diff = (current_flow + noise * current_flow * rand(breath)) + fbk_scl1 * delay_sig
    current_exitation = emb_sig - emb_sig * emb_sig * emb_sig
    out_sig = one_pole(reflection_lp_filter, current_exitation + fbk_scl2 * delay_sig)
    tap_sig = tap(bore, offset)
    # NB the DC blocker is not in the cicuit. It is applied to the
    # out-sig but the result is not fed back into the system.
    dc_blocked_a = (out_sig - previous_out_sig) + 0.995 * previous_dc_blocked_a
    dc_blocked_b = (tap_sig - previous_tap_sig) + 0.995 * previous_dc_blocked_b
    outa(i, out_scl * dc_blocked_a, $rbm_output)
    outb(i, out_scl * dc_blocked_b, $rbm_output) if chns > 1
    previous_out_sig = out_sig
    previous_dc_blocked_a = dc_blocked_a
    previous_tap_sig = tap_sig
    previous_dc_blocked_b = dc_blocked_b
  end
end
# with_sound() do stereo_flute(0, 2, 440, 0.55, :flow_envelope, [0, 0, 1, 1, 2, 1, 3, 0]) end

# FM-BELL
def fm_bell(start, dur, freq, amp,
            amp_env = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0],
            index_env = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2],
            index = 1.0)
  fm_ind1 = hz2radians(32.0 * freq)
  fm_ind2 = hz2radians(4.0 * (8.0 - freq / 50.0))
  fm_ind3 = fm_ind2 * 0.705 * (1.4 - freq / 250.0)
  fm_ind4 = hz2radians(32.0 * (20.0 - freq / 20.0))
  mod1 = make_oscil(:frequency, freq * 2.0)
  mod2 = make_oscil(:frequency, freq * 1.41)
  mod3 = make_oscil(:frequency, freq * 2.82)
  mod4 = make_oscil(:frequency, freq * 2.4)
  car1 = make_oscil(:frequency, freq)
  car2 = make_oscil(:frequency, freq)
  car3 = make_oscil(:frequency, freq * 2.4)
  indf = make_env(:envelope, index_env, :scaler, index, :duration, dur)
  ampf = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    fmenv = env(indf)
    outa(i, env(ampf) * \
         (oscil(car1, fmenv * fm_ind1 * oscil(mod1)) + \
          0.15 * oscil(car2, fmenv * (fm_ind2 * oscil(mod2) + fm_ind3 * oscil(mod3))) + \
          0.15 * oscil(car3, fmenv * fm_ind4 * oscil(mod4))), $rbm_output)
  end
end
# with_sound() do
#   fbell = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
#   abell = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
#   fm_bell(0, 1, 220, 0.5, abell, fbell, 1)
# end

# FM-INSECT
def fm_insect(start, dur, freq, amp, amp_env,
              mod_freq, mod_skew, mod_freq_env, mod_index, mod_index_env,
              fm_index, fm_ratio, *args)
  degree        = get_args(args, :degree, 0.0)
  distance      = get_args(args, :distance, 1.0)
  reverb_amount = get_args(args, :reverb_amount, 0.005)
  carrier = make_oscil(:frequency, freq)
  fm1_osc = make_oscil(:frequency, mod_freq)
  fm2_osc = make_oscil(:frequency, fm_ratio * freq)
  ampf = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
  indf = make_env(:envelope, mod_index_env, :scaler, hz2radians(mod_index), :duration, dur)
  modfrqf = make_env(:envelope, mod_freq_env, :scaler, hz2radians(mod_skew), :duration, dur)
  fm2_amp = hz2radians(fm_index * fm_ratio * freq)
  loc = make_locsig(:degree, degree, :distance, distance, :channels, mus_channels($rbm_output),
                    :reverb, reverb_amount, :output, $rbm_output, :revout, $rbm_reverb)
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    garble_in = env(indf) * oscil(fm1_osc, env(modfrqf))
    garble_out = fm2_amp * oscil(fm2_osc, garble_in)
    locsig(loc, i, env(ampf) * oscil(carrier, garble_out + garble_in))
  end
end
=begin
with_sound() do
  locust = [0, 0, 40, 1, 95, 1, 100, 0.5]
  bug_hi = [0, 1, 25, 0.7, 75, 0.78, 100, 1]
  amp = [0, 0, 25, 1, 75, 0.7, 100, 0]
    fm_insect(0.000, 1.699, 4142.627, 0.015, amp, 60, -16.707, locust, 500.866, bug_hi, 0.346, 0.5)
    fm_insect(0.195, 0.233, 4126.284, 0.030, amp, 60, -12.142, locust, 649.490, bug_hi, 0.407, 0.5)
    fm_insect(0.217, 2.057, 3930.258, 0.045, amp, 60,  -3.011, locust, 562.087, bug_hi, 0.591, 0.5)
    fm_insect(2.100, 1.500,  900.627, 0.060, amp, 40, -16.707, locust, 300.866, bug_hi, 0.346, 0.5)
    fm_insect(3.000, 1.500,  900.627, 0.060, amp, 40, -16.707, locust, 300.866, bug_hi, 0.046, 0.5)
    fm_insect(3.450, 1.500,  900.627, 0.090, amp, 40, -16.707, locust, 300.866, bug_hi, 0.006, 0.5)
    fm_insect(3.950, 1.500,  900.627, 0.120, amp, 40, -10.707, locust, 300.866, bug_hi, 0.346, 0.5)
    fm_insect(4.300, 1.500,  900.627, 0.090, amp, 40, -20.707, locust, 300.866, bug_hi, 0.246, 0.5)
end
=end

# FM-DRUM
#
# Jan Mattox's fm drum:
def fm_drum(start, dur, freq, amp, index, high = false,
            degree = 0.0, distance = 1.0, reverb_amount = 0.01)
  casrat = high ? 8.525 : 3.515
  fmrat = high ? 3.414 : 1.414
  glsf = make_env(:envelope, [0, 0, 25, 0, 75, 1, 100, 1],
                  :scaler, high ? hz2radians(66) : 0.0, :duration, dur)
  ampfun = [0, 0, 3, 0.05, 5, 0.2, 7, 0.8, 8, 0.95, 10, 1.0, 12, 0.95, 20, 0.3, 30, 0.1, 100, 0]
  atdrpt = 100 * (high ? 0.01 : 0.015) / dur
  ampf = make_env(:envelope, stretch_envelope(ampfun, 10, atdrpt, 15,
                                              [atdrpt + 1, 100 - 100 * ((dur - 0.2) / dur)].max),
                  :scaler, amp, :duration, dur)
  indxfun = [0, 0, 5, 0.014, 10, 0.033, 15, 0.061, 20, 0.099,
             25, 0.153, 30, 0.228, 35, 0.332, 40, 0.477,
             45, 0.681, 50, 0.964, 55, 0.681, 60, 0.478, 65, 0.332,
             70, 0.228, 75, 0.153, 80, 0.099, 85, 0.061,
             90, 0.033, 95, 0.0141, 100, 0]
  indxpt = 100 - 100 * ((dur - 0.1) / dur)
  divindxf = stretch_envelope(indxfun, 50, atdrpt, 65, indxpt)
  indxf = make_env(:envelope, divindxf, :duration, dur,
                   :scaler, [hz2radians(index * fmrat * freq), PI].min)
  mindxf = make_env(:envelope, divindxf, :duration, dur,
                    :scaler, [hz2radians(index * casrat * freq), PI].min)
  devf = make_env(:envelope, stretch_envelope(ampfun, 10, atdrpt, 90,
                                              [atdrpt + 1, 100 - 100 * ((dur - 0.05) / dur)].max),
                  :scaler, [hz2radians(7000), PI].min, :duration, dur)
  rn = make_rand(:frequency, 7000, :amplitude, 1)
  carrier = make_oscil(:frequency, freq)
  fmosc = make_oscil(:frequency, freq * fmrat)
  cascade = make_oscil(:frequency, freq * casrat)
  loc = make_locsig(:degree, degree, :distance, distance, :channels, mus_channels($rbm_output),
                    :reverb, reverb_amount, :output, $rbm_output, :revout, $rbm_reverb)
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    gls = env(glsf)
    locsig(loc, i,
           env(ampf) * oscil(carrier,
                             gls + env(indxf) * oscil(fmosc,
                                                      gls * fmrat +
                                                           env(mindxf) * oscil(cascade,
                                                                               gls * casrat +
                                                                                    env(devf) *
                                                                                    rand(rn)))))
  end
end
# with_sound() do
#   fm_drum(0, 1.5, 55, 0.3, 5, false)
#   fm_drum(2, 1.5, 66, 0.3, 4, true)
# end

# FM-GONG
#
# Paul Weineke's gong.
def gong(start, dur, freq, amp, *args)
  degree        = get_args(args, :degree, 0.0)
  distance      = get_args(args, :distance, 1.0)
  reverb_amount = get_args(args, :reverb_amount, 0.005)
  mfq1 = freq * 1.16
  mfq2 = freq * 3.14
  mfq3 = freq * 1.005
  indx01 = hz2radians(0.01 * mfq1)
  indx11 = hz2radians(0.30 * mfq1)
  indx02 = hz2radians(0.01 * mfq2)
  indx12 = hz2radians(0.38 * mfq2)
  indx03 = hz2radians(0.01 * mfq3)
  indx13 = hz2radians(0.50 * mfq3)
  atpt = 5
  atdur = 100 * (0.002 / dur)
  expf = [0, 0, 3, 1, 15, 0.5, 27, 0.25, 50, 0.1, 100, 0]
  rise = [0, 0, 15, 0.3, 30, 1.0, 75, 0.5, 100, 0]
  fmup = [0, 0, 75, 1.0, 98, 1.0, 100, 0]
  fmdwn = [0, 0, 2, 1.0, 100, 0]
  ampfun = make_env(:envelope, stretch_envelope(expf, atpt, atdur), :scaler, amp, :duration, dur)
  indxfun1 = make_env(:envelope, fmup, :scaler, indx11 - indx01, :duration, dur, :offset, indx01)
  indxfun2 = make_env(:envelope, fmdwn, :scaler, indx12 - indx02, :duration, dur, :offset, indx02)
  indxfun3 = make_env(:envelope, rise, :scaler, indx13 - indx03, :duration, dur, :offset, indx03)
  carrier = make_oscil(:frequency, freq)
  mod1 = make_oscil(:frequency, mfq1)
  mod2 = make_oscil(:frequency, mfq2)
  mod3 = make_oscil(:frequency, mfq3)
  loc = make_locsig(:degree, degree, :distance, distance, :channels, mus_channels($rbm_output),
                    :reverb, reverb_amount, :output, $rbm_output, :revout, $rbm_reverb)
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    locsig(loc, i, env(ampfun) * oscil(carrier,
                                       env(indxfun1) * oscil(mod1) + \
                                       env(indxfun2) * oscil(mod2) + \
                                       env(indxfun3) * oscil(mod3)))
  end  
end
# with_sound() do gong(0, 3, 261.61, 0.6) end

# ATTRACT
#
# by James McCartney, from CMJ vol 21 no 3 p 6
def attract(start, dur, amp, c)
  a = b = 0.2
  dt = 0.04
  scale = (0.5 * amp) / c
  x = -1.0
  y = z = 0.0
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    x1 = x - dt * (y + z)
    y += dt * (x + a * y)
    z += dt * ((b + x * z) - c * z)
    x = x1
    outa(i, scale * x, $rbm_output)
  end  
end
# with_sound() do attract(0, 0.25, 0.5, 2.0) end

# PQW
#
# phase-quadrature waveshaping used to create asymmetric (i.e. single
# side-band) spectra.  The basic idea here is a variant of sin x sin y
# - cos x cos y = cos (x + y)
def pqw(start, dur, spacing_freq, carrier_freq, amp, ampfun, indexfun, partials, *args)
  degree        = get_args(args, :degree, 0.0)
  distance      = get_args(args, :distance, 1.0)
  reverb_amount = get_args(args, :reverb_amount, 0.005)
  normalized_partials = normalize_partials(partials)
  spacing_cos = make_oscil(:frequency, spacing_freq, "initial-phase".to_sym, HALF_PI)
  spacing_sin = make_oscil(:frequency, spacing_freq)
  carrier_cos = make_oscil(:frequency, carrier_freq, "initial-phase".to_sym, HALF_PI)
  carrier_sin = make_oscil(:frequency, carrier_freq)
  sin_coeffs = partials2polynomial(normalized_partials, 0)
  cos_coeffs = partials2polynomial(normalized_partials, 1)
  amp_env = make_env(:envelope, ampfun, :scaler, amp, :duration, dur)
  ind_env = make_env(:envelope, indexfun, :duration, dur)
  r = carrier_freq / spacing_freq.to_f
  tr = make_triangle_wave(:frequency, 5, :amplitude, hz2radians(0.005 * spacing_freq))
  rn = make_rand_interp(:frequency, 12, :amplitude, hz2radians(0.005 * spacing_freq))
  loc = make_locsig(:degree, degree, :distance, distance, :channels, mus_channels($rbm_output),
                    :reverb, reverb_amount, :output, $rbm_output, :revout, $rbm_reverb)
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    vib = triangle_wave(tr) + rand_interp(rn)
    ax = [1.0, env(ind_env)].min * oscil(spacing_cos, vib)
    fax = polynomial(cos_coeffs, ax)
    yfax = oscil(spacing_sin, vib) * polynomial(sin_coeffs, ax)
    locsig(loc, i, env(amp_env) * (oscil(carrier_sin, vib * r) * yfax - \
                                   oscil(carrier_cos, vib * r) * fax))
  end
end
# with_sound() do
#   pqw(0, 0.5, 200, 1000, 0.2, [0, 0, 25, 1, 100, 0], [0, 1, 100, 0], [2, 0.1, 3, 0.3, 6, 0.5])
# end
# to see the asymmetric spectrum most clearly, set the index function
# above to [0, 1, 100, 1]

# taken from Perry Cook's stkv1.tar.Z (Synthesis Toolkit), but I was
# in a bit of a hurry and may not have made slavishly accurate
# translations.  Please let me (bil@ccrma.stanford.edu) know of any
# serious (non-envelope) errors.
#
# from Perry Cook's TubeBell.cpp
def tubebell(start, dur, freq, amp, base = 32.0)
  osc0 = make_oscil(freq * 0.995)
  osc1 = make_oscil(freq * 0.995 * 1.414)
  osc2 = make_oscil(freq * 1.005)
  osc3 = make_oscil(freq * 1.414)
  ampenv1 = make_env(:envelope, [0, 0, 0.005, 1, dur, 0], :base, base, :duration, dur)
  ampenv2 = make_env(:envelope, [0, 0, 0.001, 1, dur, 0], :base, 2 * base, :duration, dur)
  ampmod = make_oscil(:frequency, 2.0)
  g0 = 0.5 * amp * 0.707
  g1 = 0.203
  g2 = 0.5 * amp
  g3 = 0.144
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    outa(i, (0.007 * oscil(ampmod) + 0.993) * \
         (g0 * env(ampenv1) * oscil(osc0, g1 * oscil(osc1)) + \
          g2 * env(ampenv2) * oscil(osc2, g3 * oscil(osc3))), $rbm_output)
  end  
end
# with_sound() do tubebell(0, 2, 440, 0.2) end

# from Perry Cook's Wurley.cpp
def wurley(start, dur, freq, amp)
  osc0 = make_oscil(freq)
  osc1 = make_oscil(freq * 4.0)
  osc2 = make_oscil(510.0)
  osc3 = make_oscil(510.0)
  ampmod = make_oscil(:frequency, 8.0)
  g0 = 0.5 * amp
  g1 = 0.307
  g2 = 0.5 * amp * 0.307
  g3 = 0.117
  ampenv = make_env(:envelope, [0, 0, 1, 1, 9, 1, 10, 0], :duration, dur)
  indenv = make_env(:envelope, [0, 0, 0.001, 1, 0.15, 0, dur, 0], :duration, dur)
  resenv = make_env(:envelope, [0, 0, 0.001, 1, 0.25, 0, dur, 0], :duration, dur)
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    outa(i, env(ampenv) * \
         (1.0 + 0.007 * oscil(ampmod)) * \
         (g0 * oscil(osc0, g1 * oscil(osc1)) + \
          env(resenv) * g2 * oscil(osc2, g3 * env(indenv) * oscil(osc3))), $rbm_output)
  end  
end
# with_sound() do wurley(0, 0.25, 440, 0.2) end

# from Perry Cook's Rhodey.cpp
def rhodey(start, dur, freq, amp, base = 0.5)
  osc0 = make_oscil(freq)
  osc1 = make_oscil(freq * 0.5)
  osc2 = make_oscil(freq)
  osc3 = make_oscil(freq * 15.0)
  ampenv1 = make_env(:envelope, [0, 0, 0.005, 1, dur, 0], :base, base, :duration, dur)
  ampenv2 = make_env(:envelope, [0, 0, 0.001, 1, dur, 0], :base, base * 1.5, :duration, dur)
  ampenv3 = make_env(:envelope, [0, 0, 0.001, 1, 0.25, 0, dur, 0], :base, base * 4, :duration, dur)
  g0 = 0.5 * amp
  g1 = 0.535
  g2 = 0.5 * amp
  g3 = 0.109
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    outa(i, g0 * env(ampenv1) * oscil(osc0, g1 * oscil(osc1)) + \
         g2 * env(ampenv2) * oscil(osc2, env(ampenv3) * g3 * oscil(osc3)), $rbm_output)
  end  
end
# with_sound() do rhodey(0, 0.25, 440, 0.2) end

# from Perry Cook's BeeThree.cpp
def hammondoid(start, dur, freq, amp)
  osc0 = make_oscil(freq * 0.999)
  osc1 = make_oscil(freq * 1.997)
  osc2 = make_oscil(freq * 3.006)
  osc3 = make_oscil(freq * 6.009)
  ampenv1 = make_env(:envelope, [0, 0, 0.005, 1, dur - 0.008, 1, dur, 0], :duration, dur)
  ampenv2 = make_env(:envelope, [0, 0, 0.005, 1, dur, 0], :duration, dur)
  g0 = 0.25 * 0.75 * amp
  g1 = 0.25 * 0.75 * amp
  g2 = 0.5 * amp
  g3 = 0.5 * 0.75 * amp
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    outa(i, env(ampenv1) * (g0 * oscil(osc0) + g1 * oscil(osc1) + g2 * oscil(osc2)) + \
         env(ampenv2) * g3 * oscil(osc3), $rbm_output)
  end  
end
# with_sound() do hammondoid(0, 0.25, 440, 0.2) end

# from Perry Cook's HeavyMtl.cpp
def metal(start, dur, freq, amp)
  osc0 = make_oscil(freq)
  osc1 = make_oscil(freq * 4.0 * 0.999)
  osc2 = make_oscil(freq * 3.0 * 1.001)
  osc3 = make_oscil(freq * 0.5 * 1.002)
  ampenv0 = make_env(:envelope, [0, 0, 0.001, 1, dur - 0.002, 1, dur, 0], :duration, dur)
  ampenv1 = make_env(:envelope, [0, 0, 0.001, 1, dur - 0.011, 1, dur, 0], :duration, dur)
  ampenv2 = make_env(:envelope, [0, 0, 0.01, 1, dur - 0.015, 1, dur, 0], :duration, dur)
  ampenv3 = make_env(:envelope, [0, 0, 0.03, 1, dur - 0.04, 1, dur, 0], :duration, dur)
  g0 = 0.615 * amp
  g1 = 0.202
  g2 = 0.574
  g3 = 0.116
  beg, len = times2samples(start, dur)
  ws_interrupt?()
  (beg..len).each do |i|
    outa(i, g0 * env(ampenv0) * oscil(osc0,
                                      g1 * env(ampenv1) * oscil(osc1,
                                                                g2 * env(ampenv2) * oscil(osc2)) +\
                                      g3 * env(ampenv3) * oscil(osc3)), $rbm_output)
  end
end
# with_sound() do metal(0, 0.25, 440, 0.2) end

# clm-ins.rb ends here
