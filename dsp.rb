# dsp.rb -- dsp.scm --> dsp.rb -*- snd-ruby -*-

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Created: Mon Mar 07 13:50:44 CET 2005
# Last: Tue Mar 08 13:23:58 CET 2005

# Commentary:
#
# comments are taken mostly from dsp.scm
#
# module Dsp
#  dolph(n, gamma)
#  dolph_1(n, gamma)
#  down_oct(n, snd = false, chn = false)
#  stretch_sound_via_dft(factor, snd = false, chn = false)
#  compute_uniform_circular_string(size, x0, x1, x2, xspring, damp)
#  testunif(mass, xspring, damp)
#  test_scanned_synthesis(amp, dur, mass, xspring, damp)
#  compute_string(size, x0, x1, x2, masses, xsprings, esprings, damps, haptics)
#  freqdiv(n, snd = false, chn = false)
#  adsat(size, beg = false, dur = false, snd = false, chn = false)
#  spike(snd = false, chn = false)
#  spot_freq(samp = 0, snd = false, chn = false)
#  class Flanger
#   initialize(time = 0.05, amount = 20.0, speed = 10.0)
#   flanger(inval)
#  make_flanger(time = 0.05, amount = 20.0, speed = 10.0)
#  flanger?(obj)
#  flanger(gen, inval)
#  chorus(size = 5)
#  chordalize(amount = 0.95, base = 100, chord = [1.00, 0.75, 1.25])
#  zero_phase(snd = false, chn = false)
#  rotate_phase(snd = false, chn = false, &body)
#  class Asyfm
#   initialize(*args)
#   asyfm_J(input)
#   asyfm_I(input)
#  make_asyfm(*args)
#  asyfm?(obj)
#  asyfm_J(gen, input)
#  asyfm_I(gen, input)
#  cosine_summation(gen, r)
#  kosine_summation(gen, r, k)
#  fejer_sum(angle, n)
#  legendre_sum(angle, n)
#  sum_of_n_sines(angle, n)
#  sum_of_n_odd_sines(angle, n)
#  sum_of_n_odd_cosines(angle, n)
#  band_limited_sawtooth(x, a, n, fi)
#  brighten_slightly(amount, snd = false, chn = false)
#  spectrum2coeffs(order, spectr)
#  fltit_1(order, spectr)
#  make_hilbert_transform(len = 30)
#  make_highpass(fc, len = 30)
#  make_lowpass(fc, len = 30)
#  make_bandpass(flo, fhi, len = 30)
#  make_bandstop(flo, fhi, len = 30)
#  make_differentiator(len = 30)
#  make_butter_high_pass(freq)
#  make_butter_low_pass(freq)
#  make_butter_band_pass(freq, band)
#  make_butter_band_reject(freq, band)
#  make_biquad(a0, a1, a2, b1, b2)
#  make_iir_low_pass_1(fc)
#  make_iir_high_pass_1(fc)
#  make_iir_low_pass_2(fc, din = false)
#  make_iir_high_pass_2(fc, din = false)
#  make_iir_band_pass_2(f1, f2)
#  make_iir_band_stop_2(f1, f2)
#  make_eliminate_hum(hum_freq = 60.0, hum_harmonics = 5, bandwidth = 10)
#  eleminate_hum(gens, x0)

# Code:

require "examp"
require "complex"

module Dsp
  # Dolph-Chebyshev window
  # 
  # formula taken from Richard Lyons, "Understanding DSP"
  # see clm.c for C version (using either GSL's or GCC's complex trig functions)
  def dolph(n, gamma)
    alpha = cosh(acosh(10.0 ** gamma) / n)
    den = 1.0 / cosh(n * acosh(alpha))
    freq = PI / n
    rl = make_vct(n)
    im = make_vct(n)
    phase = 0.0
    n.times do
      val = den * cos(n * acos(alpha * cos(phase)))
      rl[i] = val.real
      im[i] = val.image
      phase += freq
    end
    fft(rl, im, -1)
    vct_scale!(rl, 1.0 / vct_peak(rl))
    j = n / 2
    n.times do |i|
      im[i] = rl[j]
      j += 1
      if j == n
        j = 0
      end
    end
    im
  end

  # this version taken from Julius Smith's "Spectral Audio..." with
  # three changes it does the DFT by hand, and is independent of
  # anything from Snd (fft, vcts etc)
  def dolph_1(n, gamma)
    alpha = cosh(acosh(10.0 ** gamma) / n)
    den = 1.0 / cosh(n * acosh(alpha))
    freq = PI / n
    vals = make_vct(n)
    w = make_array(n)
    pk = 0.0
    mult = -1.0
    phase = -(PI / 2)
    n.times do
      vals[i] = mult * den * cos(n * acos(alpha * cos(phase)))
      mult *= -1.0
    end
    n.times do |i|
      sum = 0.0
      n.times do |j|
        sum += vals[j] * exp((2.0 * Complex(0, 1) * PI * j * i) / n)
      end
      w[i] = magnitude(sum)
      if w[i] > pk
        pk = w[i]
      end
    end
    w.map! do |val| val / pk end
  end
  
  # move sound down by n (a power of 2)
  # I think this is "stretch" in DSP jargon -- to interpolate in the
  # time domain we're squeezing the frequency domain the power-of-2
  # limitation is based on the underlying fft function's insistence on
  # power-of-2 data sizes see stretch-sound-via-dft below for a
  # general version
  def down_oct(n, snd = false, chn = false)
    len = frames(snd, chn)
    pow2 = (log(len) / log(2)).ceil
    fftlen = (2 ** pow2).round
    fftscale = 1.0 / fftlen
    rl1 = channel2vct(0, fftlen, snd, chn)
    im1 = make_vct(fftlen)
    fft(rl1, im1, 1)
    vct_scale!(rl1, fftscale)
    vct_scale!(im1, fftscale)
    rl2 = make_vct(2 * fftlen)
    im2 = make_vct(2 * fftlen)
    k = fftlen - 1
    j = fftlen * n - 1
    (0...(fftlen / 2)).each do |i|
      vct_set!(rl2, i, rl1[i])
      vct_set!(rl2, j, rl1[k])
      vct_set!(im2, i, im1[i])
      vct_set!(im2, j, im1[k])
      k -= 1
      j -= 1
    end
    fft(rl2, im2, -1)
    vct2channel(rl2, 0, n * len, snd, chn, false, format("down_oct %d", n))
  end

  def stretch_sound_via_dft(factor, snd = false, chn = false)
    factor = factor.to_f
    n = frames(snd, chn)
    n2 = (n / 2.0).floor
    out_n = (n * factor).round
    in_data = channel2vct(0, n, snd, chn)
    out_data = make_vct(out_n)
    fr = make_array(out_n, 0.0)
    freq = (PI * 2) / n
    n.times do |i|
      break if c_g?
      if i < n2
        fr[i] = edot_product(freq * Complex(0, 1) * i, in_data)
      else
        fr[i + (out_n - n - 1)] = edot_product(freq * Complex(0, 1) * i, in_data)
      end
    end
    freq = (PI * 2) / out_n
    out_n.times do |i|
      break if c_g?
      out_data[i] = (edot_product(freq * Complex(0, 1) * i, fr) / n).real
    end
    vct2channel(out_data, 0, out_n, snd, chn, false, format("stretch_sound_via_dft %f", factor))
  end if defined? edot_product

  # compute-uniform-circular-string
  # 
  # this is a simplification of the underlying table-filling routine
  # for "scanned synthesis".  To watch the wave, open some sound (so
  # Snd has some place to put the graph), turn off the time domain
  # display (to give our graph all the window -- to do this in a much
  # more elegant manner, see snd-motif.scm under scanned-synthesis).
  def compute_uniform_circular_string(size, x0, x1, x2, xspring, damp)
    circle_vct_ref = lambda do |v, i|
      if i < 0
        v[i + size]
      elsif i >= size
        v[i - size]
      else
        v[i]
      end
    end
    dm = damp / mass.to_f
    km = xspring / mass.to_f
    denom = 1.0 + dm
    p1 = (2.0 + (dm - 2.0 * km)) / denom
    p2 = km / denom
    p3 = -1.0 / denom
    size.times do |i|
      x0[i] = p1 * x1[i] +
              p2 * (circle_vct_ref.call(x1, i - 1) + circle_vct_ref.call(x1, i + 1)) +
              p3 * x2[i]
    end
    vct_fill!(x2, 0.0)
    vct_add!(x2, x1)
    vct_fill!(x1, 0.0)
    vct_add!(x1, x0)
  end

  def testunif(mass, xspring, damp)
    size = 128
    x0 = make_vct(size)
    x1 = make_vct(size)
    x2 = make_vct(size)
    12.times do |i| x1[i + size / 4 - 6] = sin((TWO_PI * i) / 12.0) end
    1024.times do |i|
      break if c_g?
      compute_uniform_circular_string(size, x0, x1, x2, mass, xspring, damp)
      graph(x0, "string", 0, 1.0, -10.0, 10.0)
    end
  end

  def test_scanned_synthesis(amp, dur, mass, xspring, damp)
    size = 256
    x0 = make_vct(size)
    gx1 = make_vct(size)
    gx2 = make_vct(size)
    12.times do |i| x1[i + size / 4 - 6] = sin((TWO_PI * i) / 12.0) end
    gen1 = make_table_lookup(440.0, :wave, gx1)
    gen2 = make_table_lookup(440.0, :wave, gx2)
    x1 = gen1.data
    x2 = gen2.data
    recompute_samps = 30.0
    k = 0.0
    kincr = 1.0 / recompute_samps
    data = make_vct!(dur) do |i|
      if k >= 1.0
        k = 0.0
        compute_uniform_circular_string(size, x0, x1, x2, mass, xspring, damp)
      else
        k += kincr
      end
      g1 = table_lookup(gen1)
      g2 = table_lookup(gen2)
      g2 + k * (g1 - g2)
    end
    vct_scale!(data, amp / vct_peak(data))
    vct2channel(data, 0, dur)
  end

  # this is the more general form
  def compute_string(size, x0, x1, x2, masses, xsprings, esprings, damps, haptics)
    circle_vct_ref = lambda do |v, i|
      if i < 0
        v[i + size]
      elsif i >= size
        v[i - size]
      else
        v[i]
      end
    end
    size.times do |i|
      dm = damps[i] / masses[i]
      km = xsprings[i] / masses[i]
      cm = esprings[i] / masses[i]
      denom = 1.0 + dm + cm
      p1 = (2.0 + (dm - 2.0 * km)) / denom
      p2 = km / denom
      p3 = -1.0 / denom
      p4 = haptics / (masses[i] * denom)
      x0[i] = p1 * x1[i] +
              p2 * (circle_vct_ref.call(x1, i - 1) + circle_vct_ref.call(x1, i + 1)) +
              p3 * x2[i] +
              p4
    end
    size.times do |i| x2[i], x1[i] = x1[i], x0[i] end
  end

  # "frequency division" -- an effect from sed_sed@my-dejanews.com
  def freqdiv(n, snd = false, chn = false)
    div = 0
    curval = 0.0
    map_channel(lambda do |val|
                  curval = val if div.zero?
                  div += 1
                  div = 0 if div == n
                  curval
                end, 0, false, snd, chn, false, format("freqdiv %d", n))
  end

  # "adaptive saturation" -- an effect from sed_sed@my-dejanews.com
  # 
  # a more extreme effect is "saturation":
  # (map-channel (lambda (val) (if (< (abs val) .1) val (if (>= val 0.0) 0.25 -0.25))))
  def adsat(size, beg = false, dur = false, snd = false, chn = false)
    mn = 0.0
    mx = 0.0
    n = 0
    vals = make_vct(size)
    map_channel(lambda do |val|
                  if n == size
                    size.times do |i|
                      if vals[i] >= 0.0
                        vals[i] = mx
                      else
                        vals[i] = mn
                      end
                    end
                    n = 0
                    mx = 0.0
                    mn = 0.0
                    vals
                  else
                    vals[n] = val
                    mx = val if val > mx
                    mn = val if val < mn
                    n += 1
                    false
                  end
                end, beg, dur, snd, chn, false, format("adsat %d %d %d", size, beg, dur))
  end

  # spike
  # 
  # makes sound more spikey -- sometimes a nice effect
  def spike(snd = false, chn = false)
    x1 = x2 = 0.0
    amp = maxamp(snd, chn)
    map_channel(lambda do |x0|
                  res = (x0 / (amp * amp)) * x2.abs * x1.abs
                  x2, x1 = x1, x0
                  res
                end, 0, false, snd, chn, false, "spike")
  end

  # easily-fooled autocorrelation-based pitch tracker
  def spot_freq(samp = 0, snd = false, chn = false)
    pow2 = (log(srate(snd) / 20.0) / log(2)).ceil
    fftlen = (2 ** pow2).round
    data = autocorrelate(channel2vct(samp, fftlen, snd, chn))
    cor_peak = vct_peak(data)
    callcc do |ret|
      (1...fftlen - 2).each do |i|
        if data[i] < data[i + 1] and data[i + 1] > data[i + 2]
          logla = log10((cor_peak + data[i]) / (2.0 * cor_peak))
          logca = log10((cor_peak + data[i + 1]) / (2.0 * cor_peak))
          logra = log10((cor_peak + data[i + 2]) / (2.0 * cor_peak))
          offset = (0.5 * (logla - logra)) / (logla + logra + -2.0 * logca)
          ret.call(srate(snd) / (2 * (i + 1 + offset)))
        end
      end
      0.0
    end
  end
  # $graph_hook.add_hook!("examp-left-sample-hook") do |snd, chn, y0, y1|
  #   report_in_minibuffer(format("(freq: %.3f)", spot_freq(left_sample(snd, chn))))
  # end
  #
  # or
  #
  # $mouse_click_hook.add_hook!("examp-cursor-hook") do |snd, chn, button, state, x, y, axis|
  #   if axis == Time_graph
  #     report_in_minibuffer(format("(freq: %.3f)", spot_freq(cursor(snd, chn))))
  #   end
  # end

  # chorus (doesn't always work and needs speedup)
  class Flanger
    def initialize(time = 0.05, amount = 20.0, speed = 10.0)
      @randind = make_rand_interp(:frequency, speed, :amplitude, amount)
      len = rbm_random(3.0 * time * srate()).floor
      @flanger = make_delay(:size, len, :max_size, (len + amount + 1).to_i)
    end

    def flanger(inval)
      inval + delay(@flanger, inval, rand_interp(@randind))
    end
  end

  def make_flanger(time = 0.05, amount = 20.0, speed = 10.0)
    Flanger.new(time, amount, speed)
  end

  def flanger?(obj)
    obj.kind_of?(Flanger)
  end
  
  def flanger(gen, inval)
    gen.flanger(inval)
  end

  def chorus(size = 5)
    dlys = make_array(size) do make_flanger end
    sum = 0.0
    lambda do |inval|
      dlys.each do |dly| sum += dly.flanger(inval) end
      sum * 0.25
    end
  end

  # chordalize (comb filters to make a chord using chordalize-amount
  # and chordalize-base)
  def chordalize(amount = 0.95, base = 100, chord = [1.00, 0.75, 1.25])
    combs = chord.map do |interval| make_comb(:scaler, amount, :size, (base * interval).round) end
    scaler = 0.5 / chord.length
    lambda do |x|
      val = 0.0
      combs.each do |c| val += comb(c, x) end
      scaler * val
    end
  end
  
  # zero-phase, rotate-phase
  # fft games (from the "phazor" package of Scott McNab)
  def zero_phase(snd = false, chn = false)
    len = frames(snd, chn)
    pow2 = (log(len) / log(2)).ceil
    fftlen = (2 ** pow2).round
    fftscale = 1.0 / fftlen
    rl = channel2vct(0, fftlen, snd, chn)
    old_pk = vct_peak(rl)
    im = make_vct(fftlen)
    fft(rl, im, 1)
    rectangular2polar(rl, im)
    vct_scale!(rl, fftscale)
    vct_scale!(im, 0.0)
    fft(rl, im, -1)
    pk = vct_peak(rl)
    vct2channel(vct_scale!(rl, old_pk / pk), 0, len, snd, chn, false, "zero_phase")
  end

  def rotate_phase(snd = false, chn = false, &body)
    len = frames(snd, chn)
    pow2 = (log(len) / log(2)).ceil
    fftlen = (2 ** pow2).round
    fftlen2 = (fftlen / 2).floor
    fftscale = 1.0 / fftlen
    rl = channel2vct(0, fftlen, snd, chn)
    old_pk = vct_peak(rl)
    im = make_vct(fftlen)
    fft(rl, im, 1)
    rectangular2polar(rl, im)
    vct_scale!(rl, fftscale)
    vct_set!(im, 0, 0.0)
    j = fftlen - 1
    (1...fftlen2).each do |i|
      vct_set!(im, i, body.call(vct_ref(im, i)))
      vct_set!(im, j, -vct_ref(im, i))
      j -= 1
    end
    polar2rectangular(rl, im)
    fft(rl, im, -1)
    pk = vct_peak(rl)
    vct2channel(vct_scale!(rl, old_pk / pk), 0, len, snd, chn, false, "rotate_phase")
  end
  # rotate_phase do |x| 0.0 end    # is the same as (zero-phase)
  # rotate_phase do |x| rbm_random(PI) end # randomizes phases
  # rotate_phase do |x| x end      # returns original
  # rotate_phase do |x| -x end     # reverses original (might want to write fftlen samps here)

  # asymmetric FM (bes-i0 case)
  class Asyfm
    def initialize(*args)
      @frequency  = hz2radians(get_args(args, :frequency, 440.0).to_f)
      @phase      = get_args(args, :initial_phase, 0.0).to_f
      @ratio      = get_args(args, :ratio, 1.0).to_f
      @r          = get_args(args, :r, 1.0).to_f
      @index      = get_args(args, :index, 1.0).to_f
    end
    attr_accessor :frequency, :phase, :ratio, :r, :index

    def inspect
      format("#<%s freq: %1.3f, phase: %1.3f, ratio: %1.3f, r: %1.3f, index: %1.3f>",
             self.class, @frequency, @phase, @ratio, @r, @index)
    end
    
    def asyfm_J(input)
      r1 = 1.0 / @r
      modphase = @ratio * @phase
      result = exp(0.5 * @index * (@r - r1) * cos(modphase)) *
        sin(@phase + 0.5 * @index * (@r + r1) * sin(modphase))
      @phase += input + @frequency
      result
    end
    
    def asyfm_I(input)
      r1 = 1.0 / @r
      modphase = @ratio * @phase
      result = exp((0.5 * @index * (@r + r1) * cos(modphase)) -
                     (0.5 * log(bess_i0(@index * (@r + r1))))) *
        sin(@phase + 0.5 * @index * (@r - r1) * sin(modphase))
      @phase += input + @frequency
      result
    end
    
  end

  def make_asyfm(*args)
    Asyfm.new(*args)
  end

  def asyfm?(obj)
    obj.kind_of?(Asyfm)
  end
  
  def asyfm_J(gen, input)
    gen.asyfm_J(input)
  end

  def asyfm_I(gen, input)
    gen.asyfm_I(input)
  end

  # cosine-summation (a simpler version of sine-summation)
  # 
  # from Andrews, Askey, Roy "Special Functions" 5.1.16
  def cosine_summation(gen, r)
    r2 = r * r
    (((1.0 - r2) / ((1.0 + r2) - 2 * r * oscil(gen))) - 1.0) * ((1.0 - r2) / (2 * r * (1.0 + r2)))
  end
  alias make_cosine_summation make_oscil

  # kosine-summation
  # 
  # from Askey "Ramanujan and Hypergeometric Series" in Berndt and
  # Rankin "Ramanujan: Essays and Surveys"
  # 
  # this gives a sum of cosines of decreasing amp where the "k"
  # parameter determines the "index" (in FM nomenclature) -- higher k
  # = more cosines; the actual amount of the nth cos involves
  # hypergeometric series (looks like r^n/n! (~=e^n?) with a million
  # other terms).
  def kosine_summation(gen, r, k)
    r2 = r * r
    ((1.0 + r2) - 2 * r * oscil(gen)) ** -k * ((1.0 + r2) - 2 * r) ** k
  end
  alias make_kosine_summation make_oscil

  # legendre, fejer
  def fejer_sum(angle, n)
    if angle.zero?
      1.0
    else
      val = sin(0.5 * (n + 1) * angle) / (2.0 * sin(0.5 * angle))
      2.0 * ((val * val) / (n + 1))
    end
  end

  def legendre_sum(angle, n)
    val = sin(angle * (n + 0.5)) / sin(0.5 * angle)
    val * val
  end

  # variations on sum-of-cosines
  # from "Trigonometric Delights" by Eli Maor
  def sum_of_n_sines(angle, n)
    a2 = angle * 0.5
    den = sin(a2)
    if den.zero?
      0.0
    else
      (sin(n * a2) * sin((n + 1) * a2)) / den
    end
  end

  def sum_of_n_odd_sines(angle, n)
    angle = angle.to_f
    n = n.to_f
    den = sin(angle)
    na = sin(n * angle)
    if den.zero?
      0.0
    else
      (na * na) / den
    end
  end

  def sum_of_n_odd_cosines(angle, n)
    angle = angle.to_f
    n = n.to_f
    den = 2.0 * sin(angle)
    if den.zero?
      n
    else
      sin(2.0 * n * angle) / den
    end
  end
  
  # x = current phase, a = amp (more or less), N = 1..10 or
  # thereabouts, fi = phase increment Alexander Kritov suggests
  # time-varying "a" is good (this is a translation of his code) from
  # Stilson/Smith apparently -- was named "Discrete Summation Formula"
  # which doesn't convey anything to me
  def band_limited_sawtooth(x, a, n, fi)
    s4 = 1.0 + -2.0 * a * cos(x) + a * a
    if s4.zero?
      0.0
    else
      s1 = a ** (n - 1.0) * sin((n - 1.0) * x + fi)
      s2 = a ** n * sin(n * x + fi)
      s3 = a * sin(x + fi)
      (sin(fi) + -s3 + -s2 + s1) / s4
    end
  end

  # brighten-slightly
  def brighten_slightly(amount, snd = false, chn = false)
    mx = maxamp
    brt = (TWO_PI * amount) / mx
    map_channel(lambda do |y|
                  mx * sin(y * brt)
                end, 0, false, snd, chn, false, "brighten_slightly %1.3" % amount)
  end

  # FIR filters

  # Snd's (very simple) spectrum->coefficients procedure is:
  def spectrum2coeffs(order, spectr)
    coeffs = make_vct(order)
    n = order
    m = ((n + 1) / 2.0).floor
    am = 0.5 * (n + 1)
    q = (PI * 2.0) / n
    jj = n - 1
    m.times do |j|
      xt = 0.5 * spectr[0]
      (1...m).each do |i| xt += spectr[i] * cos(q * i * (am - j - 1)) end
      coeff = 2.0 * (xt / n)
      coeffs[j] = coeff
      coeffs[jj] = coeff
      jj -= 1
    end
    coeffs
  end

  def fltit_1(order, spectr)
    flt = make_fir_filter(order, spectrum2coeffs(order, spectr))
    lambda do |x| fir_filter(flt, x) end
  end

  # Hilbert transform
  def make_hilbert_transform(len = 30)
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = 1.0 - cos(PI * i)
      if i.zero?
        arr[k] = 0.0
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end
  alias hilbert_transform fir_filter

  # highpass filter 
  def make_highpass(fc, len = 30)
    fc = fc.to_f
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = -sin(fc * i)
      if i.zero?
        arr[k] = 1.0 - fc / PI
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end
  alias highpass fir_filter

  # lowpass filter
  def make_lowpass(fc, len = 30)
    fc = fc.to_f
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = sin(fc * i)
      if i.zero?
        arr[k] = fc / PI
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end
  alias lowpass fir_filter

  # bandpass filter
  def make_bandpass(flo, fhi, len = 30)
    flo = flo.to_f
    fhi = fhi.to_f
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = sin(fhi * i) - sin(flo * i)
      if i.zero?
        arr[k] = (fhi - flo) / PI
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end
  alias bandpass fir_filter

  # bandstop filter
  def make_bandstop(flo, fhi, len = 30)
    flo = flo.to_f
    fhi = fhi.to_f
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      denom = PI * i
      num = sin(flo * i) - sin(fhi * i)
      if i.zero?
        arr[k] = 1.0 - (fhi - flo) / PI
      else
        arr[k] = (num / denom) * (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end
  alias bandstop fir_filter

  # differentiator
  def make_differentiator(len = 30)
    arrlen = len * 2 + 1
    arr = make_vct(arrlen)
    (-len...len).each do |i|
      k = i + len
      if i.zero?
        arr[k] = 0.0
      else
        arr[k] = (cos(PI * i) / i - sin(PI * i) / (PI * i * i)) *
          (0.54 + 0.46 * cos((PI * i) / len))
      end
    end
    make_fir_filter(arrlen, arr)
  end
  alias differentiator fir_filter

  # IIR filters
  #
  # Butterworth filters (see also further below -- make-butter-lp et al)
  #
  # translated from CLM butterworth.cl:
  #
  # Sam Heisz, January 1998
  # inspired by some unit generators written for Csound by Paris
  # Smaragdis who based his work on formulas from Charles Dodge,
  # Computer music: synthesis, composition, and performance.

  alias butter filter
  def make_butter_high_pass(freq)
    r = tan(PI * freq / srate())
    r2 = r * r
    c1 = 1.0 / (1.0 + r * sqrt(2.0) + r2)
    c2 = -2.0 * c1
    c3 = c1
    c4 = 2.0 * (r2 - 1.0) * c1
    c5 = ((1.0 - r * sqrt(2.0)) + r2) * c1
    make_filter(3, vector2vct([c1, c2, c3]), vector2vct([0.0, c4, c5]))
  end
  
  def make_butter_low_pass(freq)
    r = 1.0 / tan(PI * freq / srate())
    r2 = r * r
    c1 = 1.0 / (1.0 + r * sqrt(2.0) + r2)
    c2 = 2.0 * c1
    c3 = c1
    c4 = 2.0 * (1.0 - r2) * c1
    c5 = ((1.0 - r * sqrt(2.0)) + r2) * c1
    make_filter(3, vector2vct([c1, c2, c3]), vector2vct([0.0, c4, c5]))
  end
  
  def make_butter_band_pass(freq, band)
    d = 2.0 * cos(2.0 * PI * freq / srate())
    c = 1.0 / tan(PI * band / srate())
    c1 = 1.0 / (1.0 + c)
    c2 = 0.0
    c3 = -c1
    c4 = -c * d * c1
    c5 = (c - 1.0) * c1
    make_filter(3, vector2vct([c1, c2, c3]), vector2vct([0.0, c4, c5]))
  end
  
  def make_butter_band_reject(freq, band)
    d = 2.0 * cos(2.0 * PI * freq / srate())
    c = tan(PI * band / srate())
    c1 = 1.0 / (1.0 + c)
    c2 = -d * c1
    c3 = c1
    c4 = c2
    c5 = (1.0 - c) * c1
    make_filter(3, vector2vct([c1, c2, c3]), vector2vct([0.0, c4, c5]))
  end

  # from "DSP Filter Cookbook" by Lane et al, Prompt Pubs, 2001
  # 
  # use with the filter generator
  #   (define gen (make-iir-high-pass-2 1000))
  #   (filter gen 1.0)
  #   etc
  def make_biquad(a0, a1, a2, b1, b2)
    make_filter(3, vct(a0, a1, a2), vct(0.0, b1, b2))
  end

  def make_iir_low_pass_1(fc)
    fc = fc.to_f
    theta = (2 * PI * fc) / mus_srate()
    gamma = cos(theta) / (1.0 + sin(theta))
    xc = (1.0 - gamma) / 2.0
    make_filter(2, vct(xc, xc), vct(0.0, -gamma))
  end

  def make_iir_high_pass_1(fc)
    fc = fc.to_f
    theta = (2 * PI * fc) / mus_srate()
    gamma = cos(theta) / (1.0 + sin(theta))
    xc = (1.0 + gamma) / 2.0
    make_filter(2, vct(xc, -xc), vct(0.0, -gamma))
  end

  # din=(sqrt 2.0) for example (suggested range 0.2.. 10)
  def make_iir_low_pass_2(fc, din = false)
    fc = fc.to_f
    theta = (TWO_PI * fc) / mus_srate()
    d = (din or sqrt(2.0))
    beta = 0.5 * ((1.0 - (d / 2.0) * sin(theta)) / (1.0 + (d / 2.0) * sin(theta)))
    gamma = (0.5 + beta) * cos(theta)
    alpha = 0.5 * (0.5 + beta + -gamma)
    make_filter(3, vct(alpha, 2.0 * alpha, alpha), vct(0.0, -2.0 * gamma, 2.0 * beta))
  end

  def make_iir_high_pass_2(fc, din = false)
    fc = fc.to_f
    theta = (TWO_PI * fc) / mus_srate()
    d = (din or sqrt(2.0))
    beta = 0.5 * ((1.0 - (d / 2.0) * sin(theta)) / (1.0 + (d / 2.0) * sin(theta)))
    gamma = (0.5 + beta) * cos(theta)
    alpha = 0.5 * (0.5 + beta + gamma)
    make_filter(3, vct(alpha, -2.0 * alpha, alpha), vct(0.0, -2.0 * gamma, 2.0 * beta))
  end

  def make_iir_band_pass_2(f1, f2)
    f1 = f1.to_f
    f2 = f2.to_f
    theta = (TWO_PI * sqrt(f1 * f2)) / mus_srate()
    q = sqrt(f1 * f2) / (f2 - f1)
    t2 = tan(theta / (2 * q))
    beta = 0.5 * ((1.0 - t2) / (1.0 + t2))
    gamma = (0.5 + beta) * cos(theta)
    alpha = 0.5 - beta
    make_filter(3, vct(alpha, 0.0, alpha), vct(0.0, -2.0 * gamma, 2.0 * beta))
  end

  def make_iir_band_stop_2(f1, f2)
    f1 = f1.to_f
    f2 = f2.to_f
    theta = (TWO_PI * sqrt(f1 * f2)) / mus_srate()
    q = sqrt(f1 * f2) / (f2 - f1)
    t2 = tan(theta / (2 * q))
    beta = 0.5 * ((1.0 - t2) / (1.0 + t2))
    gamma = (0.5 + beta) * cos(theta)
    alpha = 0.5 + beta
    make_filter(3, vct(alpha, -2.0 * gamma, alpha), vct(0.0, -2.0 * gamma, 2.0 * beta))
  end

  def make_eliminate_hum(hum_freq = 60.0, hum_harmonics = 5, bandwidth = 10)
    b2 = 0.5 * bandwidth
    make_array(hum_harmonics) do |i|
      center = (i + 1.0) * hum_freq
      make_iir_band_stop_2(center - b2, center + b2)
    end
  end

  def eleminate_hum(gens, x0)
    val = x0
    gens.each do |gen| val = filter(gen, val) end
    val
  end

  def cascade2canonical(a)
    conv = lambda do |m, h, l, x, y|
      (l + m).times do |i|
        y[i] = 0.0
        ([0, -(i + 1 + l)].max...[i, m].min).each do |j|
          y[i] += h[j] * x[i - j]
        end
      end
    end
    k = a.length
    d = make_vct(2 * k + 1)
    a1 = make_vct(2 * k + 1)
    a1[0] = 1.0
    k.times do |i|
      conv.call(2, a[i], 2 * i + 1, a1, d)
      (2 * i + 3).times do |j|
        a1[j] = d[j]
      end
    end
    a1
  end
end

include Dsp

# dsp.rb ends here
