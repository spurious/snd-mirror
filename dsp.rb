# dsp.rb -- dsp.scm --> dsp.rb -*- snd-ruby -*-

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Created: Mon Mar 07 13:50:44 CET 2005
# Changed: Mon Dec 11 04:59:58 CET 2006

# Commentary:
#
# comments are taken mostly from dsp.scm
#
# module Dsp
#  src_duration(e)
#  dolph(n, gamma)
#  dolph_1(n, gamma)
#  down_oct(n, snd = false, chn = false)
#  edot_product(freq, data)
#  stretch_sound_via_dft(factor, snd = false, chn = false)
#  compute_uniform_circular_string(size, x0, x1, x2, mass, xspring, damp)
#  testunif(mass, xspring, damp)
#  test_scanned_synthesis(amp, dur, mass, xspring, damp)
#  compute_string(size, x0, x1, x2, masses, xsprings, esprings, damps, haptics)
#  freqdiv(n, snd = false, chn = false)
#  adsat(size, beg = false, dur = false, snd = false, chn = false)
#  spike(snd = false, chn = false)
#  spot_freq(samp = 0, snd = false, chn = false)

#  class Flanger < Musgen
#   initialize(time = 0.05, amount = 20.0, speed = 10.0)
#   inspect
#   to_s
#   run_func(val1, val2)
#   flanger(inval)

#  make_flanger(time = 0.05, amount = 20.0, speed = 10.0)
#  flanger?(obj)
#  flanger(gen, inval)

#  chorus(size = 5)
#  chordalize(amount = 0.95, base = 100, chord = [1.00, 0.75, 1.25])

#  zero_phase(snd = false, chn = false)
#  rotate_phase(func, snd = false, chn = false)

#  class Asyfm < Musgen
#   initialize(*args)
#   inspect
#   to_s
#   run_func(val1, val2)
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
#  brighten_slightly_1(coeffs, snd = false, chn = false)
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
#  make_iir_low_pass_2(fc, din = false)
#  make_iir_high_pass_2(fc, din = false)
#  make_iir_band_pass_2(f1, f2)
#  make_iir_band_stop_2(f1, f2)

#  make_eliminate_hum(hum_freq = 60.0, hum_harmonics = 5, bandwidth = 10)
#  eliminate_hum(gens, x0)
#  make_peaking_2(f1, f2, m)
#  cascade2canonical(a)

#  make_butter_lp(m, fc)
#  make_butter_hp(m, fc)
#  make_butter_bp(m, f1, f2)
#  make_butter_bs(m, f1, f2)

#  make_notch_frequency_response(cur_srate, freqs, notch_width = 2)
#  notch_channel(freqs, filter_order, beg, dur, snd, chn, edpos, truncate, notch_width)
#  notch_sound(freqs, filter_order = false, snd = false, chn = false, notch_width = 2)
#  notch_selection(freqs, filter_order = false, snd = false, chn = false, notch_width = 2)
#  fractional_fourier_transform(fr, fi, n, v)
#  z_transform(f, n, z)
#  dht(data)
#  find_sine(freq, beg, dur)
#  goertzel(freq, beg = 0, dur = frames())
#  make_spencer_filter

#  any_random(amount, e = false)
#  gaussian_distribution(s)
#  pareto_distribution(a)
#  inverse_integrate(dist, data_size = 512, e_size = 50)
#  gaussian_envelope(s)

#  channel_mean(snd = false, chn = false)
#  channel_total_energy(snd = false, chn = false)
#  channel_average_power(snd = false, chn = false)
#  channel_rms(snd = false, chn = false)
#  channel_variance(snd = false, chn = false)
#  channel_norm(snd = false, chn = false)
#  channel_lp(p, snd = false, chn = false)
#  channel_lp_inf(snd = false, chn = false)
#  channel2_inner_product(s1, c1, s2, c2)
#  channel2_angle(s1, c1, s2, c2)
#  channel2_orthogonal?(s1, c1, s2, c2)
#  channel2_coefficient_of_projection(s1, c1, s2, c2)
#  channel_distance(s1, c1, s2, c2)

#  periodogram(n)
#  shift_channel_pitch(freq, order, beg, dur, snd, chn, edpos)
#  hz_to_2pi(freq)
#  ssb_bank(old_freq, new_freq, pairs, order, bw, beg, dur, snd, chn, edpos)
#  ssb_bank_env(old_freq, new_freq, freq_env, pairs, order, bw, beg, dur, snd, chn, edpos)
#
#  vct_polynomial(v, coeffs)
#  channel_polynomial(coeffs, snd = false, chn = false)
#  spectral_polynomial(coeffs, snd = false, chn = false)
#  scentroid(file, *args)
#  invert_filter(fcoeffs)
#
#  class Volterra_filter < Musgen
#   initialize(acoeffs, bcoeffs)
#   inspect
#   to_s
#   run_func(val1, val2)
#   volterra_filter(x)
#
#  make_volterra_filter(acoeffs, bcoeffs)
#  volterra_filter(flt, x)
#
#  class Moving_max < Musgen
#   initialize(size = 128)
#   inspect
#   to_s
#   run_func(val1, val2)
#   moving_max(y)
#   
#  make_moving_max(size = 128)
#  moving_max(gen, y)
#  make_moving_sum(size = 128)
#  moving_sum(gen, y)
#  make_moving_rms(size = 128)
#  moving_rms(gen, y)
#  make_moving_length(size = 128)
#  moving_length(gen, y)
#  harmonicizer(freq, coeffs, pairs, order, bw, beg, dur, snd, chn, edpos)
#  linear_src_channel(srinc, snd = false, chn = false)
#
#  class Mfilter < Musgen
#   initialize(decay, freq)
#   inspect
#   to_s
#   mfilter(x_input, y_input)
#
#  make_mfilter(*args)
#  mfilter(m, x_input, y_input)
#
#  class Display_bark_fft
#   display_bark_fft(snd, chn)
#   mark_bark_labels(snd, chn)
#   choose_bark_ticks(snd, chn, button, state, x, y, axis)
#
#  display_bark_fft(off)
#  undisplay_bark_fft

# Code:

require "examp"
require "env"
require "complex"

module Dsp
  # src_duration (see src-channel in extsnd.html)
  add_help(:src_duration,
           "src_duration(envelope)  returns the new duration of a sound after using 'envelope' \
for time-varying sampling-rate conversion.")
  def src_duration(e)
    e.map! do |x| x.to_f end
    ex0 = e.first
    ex1 = e[-2]
    all_x = ex1 - ex0
    dur = 0.0
    0.step(e.length - 3, 2) do |i|
      x0, xy0, x1, xy1 = e[i, 4]
      y0 = xy0.zero? ? 1.0 : (1.0 / xy0)
      y1 = xy1.zero? ? 1.0 : (1.0 / xy1)
      area = if (xy0 - xy1).abs < 0.0001
               y0 * ((x1 - x0) / all_x)
             else
               ((log(y1) - log(y0)) / (xy0 - xy1)) * ((x1 - x0) / all_x)
             end
      dur += area.abs
    end
    dur
  end

  # Dolph-Chebyshev window
  # 
  # formula taken from Richard Lyons, "Understanding DSP"
  # see clm.c for C version (using either GSL's or GCC's complex trig functions)
  add_help(:dolph,
           "dolph(n, gamma) produces a Dolph-Chebyshev FFT data window of 'n' points \
using 'gamma' as the window parameter.")
  def dolph(n, gamma)
    alpha = cosh(acosh(10.0 ** gamma) / n)
    den = 1.0 / cosh(n * acosh(alpha))
    freq = PI / n
    rl = make_vct(n)
    im = make_vct(n)
    phase = 0.0
    n.times do |i|
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
  end if defined? acosh

  # this version taken from Julius Smith's "Spectral Audio..." with
  # three changes it does the DFT by hand, and is independent of
  # anything from Snd (fft, vcts etc)
  def dolph_1(n, gamma)
    alpha = cosh(acosh(10.0 ** gamma) / n)
    den = 1.0 / cosh(n * acosh(alpha))
    freq = PI / n
    vals = make_array(n)
    w = make_array(n)
    pk = 0.0
    mult = -1.0
    phase = -HALF_PI
    n.times do |i|
      vals[i] = mult * den * cos(n * acos(alpha * cos(phase)))
      mult *= -1.0
      phase += freq
    end
    n.times do |i|
      sum = 0.0
      n.times do |j|
        sum += vals[j] * exp((2.0 * Complex(0.0, 1.0) * PI * j * i) / n)
      end
      w[i] = sum.abs
      if w[i] > pk
        pk = w[i]
      end
    end
    w.map! do |val| val / pk end
  end if defined? acosh
  
  # move sound down by n (a power of 2)
  # I think this is "stretch" in DSP jargon -- to interpolate in the
  # time domain we're squeezing the frequency domain the power-of-2
  # limitation is based on the underlying fft function's insistence on
  # power-of-2 data sizes see stretch-sound-via-dft below for a
  # general version
  add_help(:down_oct, "down_oct(n, [snd=false, [chn=false]]) moves a sound down by power of 2 n")
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
    vct2channel(rl2, 0, n * len, snd, chn, false, format("%s(%s", get_func_name, n))
  end

  add_help(:edot_product, "edot_product(freq, data): sum of (e^freq*i) * data[i]")
  def edot_product(freq, data)
    sum = 0.0
    data.each_with_index do |val, i| sum += exp(i.to_f * freq) * val end
    sum
  end unless defined? edot_product
  
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
        fr[i] = edot_product(freq * Complex(0.0, 1.0) * i, in_data)
      else
        fr[i + (out_n - n - 1)] = edot_product(freq * Complex(0.0, 1.0) * i, in_data)
      end
    end
    freq = (PI * 2) / out_n
    out_n.times do |i|
      break if c_g?
      out_data[i] = (edot_product(freq * Complex(0.0, 1.0) * i, fr) / n).real
    end
    vct2channel(out_data, 0, out_n, snd, chn, false, format("%s(%s", get_func_name, factor))
  end

  # compute-uniform-circular-string
  # 
  # this is a simplification of the underlying table-filling routine
  # for "scanned synthesis".  To watch the wave, open some sound (so
  # Snd has some place to put the graph), turn off the time domain
  # display (to give our graph all the window -- to do this in a much
  # more elegant manner, see snd-motif.scm under scanned-synthesis).
  def compute_uniform_circular_string(size, x0, x1, x2, mass, xspring, damp)
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
    12.times do |i| gx1[i + size / 4 - 6] = sin((TWO_PI * i) / 12.0) end
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
  add_help(:freqdiv,
           "freqdiv(n, [snd=false, [chn=false]]) \
repeats each nth sample n times (clobbering the intermediate samples): freqdiv(8)")
  def freqdiv(n, snd = false, chn = false)
    div = 0
    curval = 0.0
    map_channel(lambda do |val|
                  curval = val if div.zero?
                  div += 1
                  div = 0 if div == n
                  curval
                end, 0, false, snd, chn, false, format("%s(%s", get_func_name, n))
  end

  # "adaptive saturation" -- an effect from sed_sed@my-dejanews.com
  # 
  # a more extreme effect is "saturation":
  # (map-channel (lambda (val) (if (< (abs val) .1) val (if (>= val 0.0) 0.25 -0.25))))
  add_help(:adsat, "adsat(size, [beg=false, [dur=false, [snd=false, [chn=false]]]]) \
is an 'adaptive saturation' sound effect")
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
                end, beg, dur, snd, chn, false,
                       format("%s(%s, %s, %s", get_func_name, size, beg, dur))
  end

  # spike
  # 
  # makes sound more spikey -- sometimes a nice effect
  add_help(:spike,
           "spike([snd=false, [chn=false]]) \
multiplies successive samples together to make a sound more spikey")
  def spike(snd = false, chn = false)
    x1 = x2 = 0.0
    amp = maxamp(snd, chn)
    map_channel(lambda do |x0|
                  res = (x0 / (amp * amp)) * x2.abs * x1.abs
                  x2, x1 = x1, x0
                  res
                end, 0, false, snd, chn, false, "spike(")
  end

  # easily-fooled autocorrelation-based pitch tracker
  add_help(:spot_freq,
           "spot_freq([samp=0, [snd=false, [chn=false]]]) \
tries to determine the current pitch: spot_freq(left_sample)")
  def spot_freq(samp = 0, snd = false, chn = false)
    pow2 = (log(srate(snd) / 20.0) / log(2)).ceil
    fftlen = (2 ** pow2).round
    data = autocorrelate(channel2vct(samp, fftlen, snd, chn))
    cor_peak = vct_peak(data)
    cor_peak2 = 2.0 * cor_peak
    callcc do |ret|
      (1...fftlen - 2).each do |i|
        if data[i] < data[i + 1] and data[i + 2] < data[i + 1]
          logla = log10((cor_peak + data[i])     / cor_peak2)
          logca = log10((cor_peak + data[i + 1]) / cor_peak2)
          logra = log10((cor_peak + data[i + 2]) / cor_peak2)
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
  class Flanger < Musgen
    def initialize(time = 0.05, amount = 20.0, speed = 10.0)
      super()
      @time = time
      @amount = amount
      @speed = speed
      @randind = make_rand_interp(:frequency, speed, :amplitude, amount)
      @data = @randind.data
      @length = @randind.length
      len = random(3.0 * time * mus_srate()).floor
      @flanger = make_delay(:size, len, :max_size, (len + amount + 1).to_i)
    end

    def inspect
      format("%s.new(%s, %s, %s)", self.class, @time, @amount, @speed)
    end

    def to_s
      format("#<%s time: %1.3f, amount: %1.3f, speed: %1.3f>", self.class, @time, @amount, @speed)
    end

    def run_func(val1 = 0.0, val2 = 0.0)
      flanger(val1)
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

  add_help(:chorus, "chorus([size=5]) tries to produce the chorus sound effect")
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
  add_help(:chordalize,
           "chordalize([amount=0.95, [base=100, [chord=[1.00, 0.75, 1.25]]]]) \
uses harmonically-related comb-filters to bring out a chord in a sound")
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
  add_help(:zero_phase, "zero_phase([snd=false, [chn=false]]) \
calls fft, sets all phases to 0, and un-ffts")
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
    vct2channel(vct_scale!(rl, old_pk / pk), 0, len, snd, chn, false, "zero_phase(")
  end

  # (set_)edit_list_proc_counter is defined in examp.rb
  # it's necessary to produce a uniq method name
  def rotate_phase(func, snd = false, chn = false)
    func_name = format("%s_%d", get_func_name, set_edit_list_proc_counter).intern
    # Proc converted to Method (ie. normal function) for edit_list2function
    func.to_method(func_name)
    len = frames(snd, chn)
    pow2 = (log(len) / log(2)).ceil
    fftlen = (2 ** pow2).round
    fftlen2 = (fftlen / 2).floor
    fftscale = 1.0 / fftlen
    rl = channel2vct(0, fftlen, snd, chn)
    im = make_vct(fftlen)
    old_pk = rl.peak
    fft(rl, im, 1)
    rectangular2polar(rl, im)
    rl.scale!(fftscale)
    im[0] = 0.0
    j = fftlen - 1
    (1...fftlen2).each do |i|
      im[i] = snd_func(func_name, im[i])
      im[j] = -im[i]
      j -= 1
    end
    polar2rectangular(rl, im)
    fft(rl, im, -1)
    pk = rl.peak
    vct2channel(rl.scale(old_pk / pk), 0, len, snd, chn, false,
                format("%s(Proc.new {|val| %s(val) }", get_func_name, func_name))
  end
  # rotate_phase(lambda {|x| 0.0 })  # is the same as (zero-phase)
  # rotate_phase(lambda {|x| random(PI) }) # randomizes phases
  # rotate_phase(lambda {|x| x })    # returns original
  # rotate_phase(lambda {|x| -x })   # reverses original (might want to write fftlen samps here)

  # asymmetric FM (bes-i0 case)
  class Asyfm < Musgen
    def initialize(*args)
      super()
      frequency, initial_phase, ratio, r, index = nil
      optkey(args, binding,
             [:frequency, 440.0],
             [:initial_phase, 0.0],
             [:ratio, 1.0],
             [:r, 1.0],
             [:index, 1.0])
      @frequency = hz2radians(frequency)
      @phase = initial_phase.to_f
      @ratio = ratio.to_f
      @r = r.to_f
      @index = index.to_f
    end
    attr_accessor :ratio, :r, :index

    def inspect
      format("%s.new(:frequency, %s, :initial_phase, %s, :ratio, %s, :r, %s, :index, %s)",
             self.class, @frequency, @phase, @ratio, @r, @index)
    end

    def to_s
      format("#<%s freq: %1.3f, phase: %1.3f, ratio: %1.3f, r: %1.3f, index: %1.3f>",
             self.class, @frequency, @phase, @ratio, @r, @index)
    end

    def run_func(val1 = 0.0, val2 = 0.0)
      asyfm_J(val1)
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
                     (0.5 * log(bes_i0(@index * (@r + r1))))) *
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
                end, 0, false, snd, chn, false, format("%s(%s", get_func_name, amount))
  end

  def brighten_slightly_1(coeffs, snd = false, chn = false)
    # another version: brighten_slightly_1([1, 0.5, 3, 1])
    pcoeffs = partials2polynomial(coeffs)
    mx = maxamp(snd, chn)
    map_channel(lambda do |y| mx * polynomial(pcoeffs, y / mx) end,
                0, false, snd, chn, false, format("%s(%s", get_func_name, coeffs))
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

  add_help(:fltit_1,
           "fltit_1(order, spectrum) \
creates an FIR filter from spectrum and order and returns a closure that calls it: 
map_channel(fltit_1(10, vct(0, 1.0, 0, 0, 0, 0, 0, 0, 1.0, 0)))")
  def fltit_1(order, spectr)
    flt = make_fir_filter(order, spectrum2coeffs(order, spectr))
    lambda do |x| fir_filter(flt, x) end
  end

  # Hilbert transform
  add_help(:make_hilbert_transform,
           "make_hilbert_transform([len=30]) makes a Hilbert transform filter")
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
  add_help(:make_highpass, "make_highpass(fc, [len=30]) makes an FIR highpass filter")
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
  add_help(:make_lowpass, "make_lowpass(fc, [len=30]) makes an FIR lowpass filter")
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
  add_help(:make_bandpass, "make_bandpass(flo, fhi, [len=30]) makes an FIR bandpass filter")
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
  add_help(:make_bandstop, "make_bandstop(flo, fhi, [len=30]) makes an FIR bandstop (notch) filter")
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
  add_help(:make_differentiator,
           "make_differentiator([len=30]) makes an FIR differentiator (highpass) filter")
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

  add_help(:butter,
           "butter(b, sig) is the generator side for the various make_butter procedure")
  alias butter filter
  add_help(:make_butter_high_pass,
           "make_butter_high_pass(freq) makes a Butterworth filter with high pass cutoff at 'freq'")
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
  
  add_help(:make_butter_low_pass,
           "make_butter_low_pass(freq) makes a Butterworth filter with high pass cutoff at 'freq'. \
The result can be used directly: \
filter_sound(make_butter_low_pass(500.0)), or via the 'butter' generator")
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
  
  add_help(:make_butter_band_pass,
           "make_butter_band_pass(freq, band) \
makes a bandpass Butterworth filter with low edge at 'freq' and width 'band'")
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

  add_help(:make_butter_band_reject,
           "make_butter_band_reject(freq, band) \
makes a band-reject Butterworth filter with low edge at 'freq' and width 'band'")
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
  add_help(:make_biquad,
           "make_biquad(a0, a1, a2, b1, b2) returns a biquad filter (use with the CLM filter gen)")
  def make_biquad(a0, a1, a2, b1, b2)
    make_filter(3, vct(a0, a1, a2), vct(0.0, b1, b2))
  end

  # din=(sqrt 2.0) for example (suggested range 0.2...10)
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
    make_filter(3, vct(alpha, 0.0, -alpha), vct(0.0, -2.0 * gamma, 2.0 * beta))
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

  def eliminate_hum(gens, x0)
    val = x0
    gens.each do |gen| val = filter(gen, val) end
    val
  end

  # bandpass, m is gain at center of peak
  # use map-channel with this one (not clm-channel or filter)
  def make_peaking_2(f1, f2, m)
    f1 = f1.to_f
    f2 = f2.to_f
    theta = (TWO_PI * sqrt(f1 * f2)) / mus_srate()
    q = sqrt(f1 * f2) / (f2 - f1)
    t2 = (4.0 / (m + 1.0)) * tan(theta / (2 * q))
    beta = 0.5 * ((1.0 - t2) / (1.0 + t2))
    gamma = (0.5 + beta) * cos(theta)
    alpha = 0.5 - beta
    flt = make_filter(3, vct(alpha, 0.0, -alpha), vct(0.0, -2.0 * gamma, 2.0 * beta))
    lambda do |x| x + (m - 1.0) * filter(flt, x) end
  end

  # convert cascade coeffs to canonical form
  # from Orfanidis "Introduction to Signal Processing
  def c2c_conv(m, h, l, x, y)
    (l + m).times do |i|
      y[i] = 0.0
      ([0, i - (1 + l)].max..[i, m].min).each do |j|
        y[i] += h[j] * x[i - j]
      end
    end
  end

  def cascade2canonical(a)
    k = a.length
    d = make_vct(2 * k + 1)
    a1 = make_vct(2 * k + 1)
    a1[0] = 1.0
    k.times do |i|
      c2c_conv(2, a[i], 2 * i + 1, a1, d)
      (2 * i + 3).times do |j|
        a1[j] = d[j]
      end
    end
    a1
  end

  # order is M*2, fc is cutoff freq (Hz)
  def make_butter_lp(m, fc)
    fc = fc.to_f
    xcoeffs = make_array(m)
    ycoeffs = make_array(m)
    theta = (TWO_PI * fc) / mus_srate()
    st = sin(theta)
    ct = cos(theta)
    m.times do |k|
      d = 2.0 * sin((PI * (2.0 * (k + 1.0) - 1.0)) / (4.0 * m))
      beta = 0.5 * ((1.0 - 0.5 * d * st) / (1.0 + 0.5 * d * st))
      gamma = ct * (0.5 + beta)
      alpha = 0.25 * (0.5 + beta + -gamma)
      xcoeffs[k] = vct(2.0 * alpha, 4.0 * alpha, 2.0 * alpha)
      ycoeffs[k] = vct(1.0, -2.0 * gamma, 2.0 * beta)
    end
    make_filter(2 * m + 1, cascade2canonical(xcoeffs), cascade2canonical(ycoeffs))
  end

  # order is M*2, fc is cutoff freq (Hz)
  def make_butter_hp(m, fc)
    fc = fc.to_f
    xcoeffs = make_array(m)
    ycoeffs = make_array(m)
    theta = (TWO_PI * fc) / mus_srate()
    st = sin(theta)
    ct = cos(theta)
    m.times do |k|
      d = 2.0 * sin((PI * (2.0 * (k + 1.0) - 1.0)) / (4.0 * m))
      beta = 0.5 * ((1.0 - 0.5 * d * st) / (1.0 + 0.5 * d * st))
      gamma = ct * (0.5 + beta)
      alpha = 0.25 * (0.5 + beta + gamma)
      xcoeffs[k] = vct(2.0 * alpha, -4 * alpha, 2.0 * alpha)
      ycoeffs[k] = vct(1.0, -2.0 * gamma, 2.0 * beta)
    end
    make_filter(2 * m + 1, cascade2canonical(xcoeffs), cascade2canonical(ycoeffs))
  end

  # order is M*2, f1 and f2 are band edge freqs (Hz)
  def make_butter_bp(m, f1, f2)
    f1 = f1.to_f
    f2 = f2.to_f
    xcoeffs = make_array(m)
    ycoeffs = make_array(m)
    f0 = sqrt(f1 * f2)
    q = f0 / (f2 - f1)
    theta0 = (TWO_PI * f0) / mus_srate()
    de = (2.0 * tan(theta0 / (2.0 * q))) / sin(theta0)
    de2 = de / 2.0
    tn0 = tan(theta0 * 0.5)
    k = j = 1
    m.times do |i|
      dk = 2.0 * sin((PI * (2.0 * k - 1.0)) / (2.0 * m))
      ak = (1.0 + de2 * de2) / (dk * de2)
      dk1 = sqrt((de * dk) / (ak + sqrt(ak * ak - 1.0)))
      bk = de2 * (dk / dk1)
      wk = (bk + sqrt(bk * bk - 1.0)).real
      thetajk = ((j == 1) ? (2.0 * atan(tn0 / wk)) : (2.0 * atan(tn0 * wk)))
      betajk = 0.5 * ((1.0 - 0.5 * dk1 * sin(thetajk)) / (1.0 + 0.5 * dk1 * sin(thetajk)))
      gammajk = (0.5 + betajk) * cos(thetajk)
      wk2 = (wk - 1.0 / wk) / dk1
      alphajk = 0.5 * (0.5 - betajk) * sqrt(1.0 + wk2 * wk2)
      xcoeffs[i] = vct(2.0 * alphajk, 0.0, -2.0 * alphajk)
      ycoeffs[i] = vct(1.0, -2.0 * gammajk, 2.0 * betajk)
      if j == 1
        j = 2
      else
        k += 1
        j = 1
      end
    end
    make_filter(2 * m + 1, cascade2canonical(xcoeffs), cascade2canonical(ycoeffs))
  end

  # order is M*2, f1 and f2 are band edge freqs (Hz)
  def make_butter_bs(m, f1, f2)
    f1 = f1.to_f
    f2 = f2.to_f
    xcoeffs = make_array(m)
    ycoeffs = make_array(m)
    f0 = sqrt(f1 * f2)
    q = f0 / (f2 - f1)
    theta0 = (TWO_PI * f0) / mus_srate()
    de = (2.0 * tan(theta0 / (2.0 * q))) / sin(theta0)
    de2 = de / 2.0
    ct = cos(theta0)
    tn0 = tan(theta0 * 0.5)
    k = j = 1
    m.times do |i|
      dk = 2.0 * sin((PI * (2.0 * k - 1.0)) / (2.0 * m))
      ak = (1.0 + de2 * de2) / (dk * de2)
      dk1 = sqrt((de * dk) / (ak + sqrt(ak * ak - 1.0)))
      bk = de2 * (dk / dk1)
      wk = (bk + sqrt(bk * bk - 1.0)).real
      thetajk = ((j == 1) ? (2.0 * atan(tn0 / wk)) : (2.0 * atan(tn0 * wk)))
      betajk = 0.5 * ((1.0 - 0.5 * dk1 * sin(thetajk)) / (1.0 + 0.5 * dk1 * sin(thetajk)))
      gammajk = (0.5 + betajk) * cos(thetajk)
      alphajk = 0.5 * (0.5 + betajk) * ((1.0 - cos(thetajk)) / (1.0 - ct))
      xcoeffs[i] = vct(2.0 * alphajk, -4.0 * ct * alphajk, 2.0 * alphajk)
      ycoeffs[i] = vct(1.0, -2.0 * gammajk, 2.0 * betajk)
      if j == 1
        j = 2
      else
        k += 1
        j = 1
      end
    end
    make_filter(2 * m + 1, cascade2canonical(xcoeffs), cascade2canonical(ycoeffs))
  end

  # notch filters
  def make_notch_frequency_response(cur_srate, freqs, notch_width = 2)
    cur_srate = cur_srate.to_f
    notch_width = notch_width.to_f
    freq_response = [0.0, 1.0]
    freqs.each do |f|
      freq_response.push((2.0 * (f - notch_width)) / cur_srate) # left upper y hz
      freq_response.push(1.0)                     # left upper y resp
      freq_response.push((2.0 * (f - notch_width / 2.0)) / cur_srate) # left bottom y hz
      freq_response.push(0.0)                     # left bottom y resp
      freq_response.push((2.0 * (f + notch_width / 2.0)) / cur_srate) # right bottom y hz
      freq_response.push(0.0)                     # right bottom y resp
      freq_response.push((2.0 * (f + notch_width)) / cur_srate) # right upper y hz
      freq_response.push(1.0)                     # right upper y resp
    end
    freq_response.push(1.0, 1.0) 
  end

  add_help(:notch_channel,
           "notch_channel(freqs, [filter_order=false, [beg=false, [dur=false, \
[snd=false, [chn=false [edpos=false, [truncate=true, [notch_width=2]]]]]]]] \
-> notch filter removing freqs")
  def notch_channel(freqs,
                    filter_order = false,
                    beg = false,
                    dur = false,
                    snd = false,
                    chn = false,
                    edpos = false,
                    truncate = true,
                    notch_width = 2)
    filter_channel(make_notch_frequency_response(srate(snd).to_f, freqs, notch_width),
                   (filter_order or (2 ** (log(srate(snd).to_f / notch_width) / log(2.0)).ceil)),
                   beg, dur, snd, chn, edpos, truncate,
                   format("%s(%s, %s, %s, %s",
                          get_func_name, freqs.inspect, filter_order, beg, dur))
  end

  add_help(:notch_sound,
           "notch_sound(freqs, [filter_order=false, [snd=false, [chn=false [notch_width=2]]]] \
-> notch filter removing freqs")
  def notch_sound(freqs, filter_order = false, snd = false, chn = false, notch_width = 2)
    filter_sound(make_notch_frequency_response(srate(snd).to_f, freqs, notch_width),
                 (filter_order or (2 ** (log(srate(snd).to_f / notch_width) / log(2.0)).ceil)),
                 snd, chn, false,
                 format("%s(%s, %s, 0, false", get_func_name, freqs.inspect, filter_order))
  end

  add_help(:notch_selection,
           "notch_selection(freqs, [filter_order=false, [notch_width=2]] \
-> notch filter removing freqs")
  def notch_selection(freqs, filter_order = false, snd = false, chn = false, notch_width = 2)
    if selection?
      filter_selection(make_notch_frequency_response(selection_srate.to_f, freqs, notch_width),
                       (filter_order or
                        (2 ** (log(selection_srate.to_f / notch_width) / log(2.0)).ceil)))
    end
  end

  # fractional Fourier Transform, z transform
  #
  # translated from the fxt package of Joerg Arndt
  add_help(:fractional_fourier_transform,
           "fractional_fourier_transform(real, imaginary, n, angle) \
performs a fractional Fourier transform on data; if angle=1.0, you get a normal Fourier transform")
  def fractional_fourier_transform(fr, fi, n, v)
    hr = make_vct(n)
    hi = make_vct(n)
    ph0 = (v * TWO_PI) / n
    n.times do |w|
      sr = 0.0
      si = 0.0
      n.times do |k|
        phase = ph0 * k * w
        c = cos(phase)
        s = sin(phase)
        x = fr[k]
        y = fi[k]
        r = x * c - y * s
        i = y * c + x * s
        sr += r
        si += i
        hr[w] = sr
        hi[w] = si
      end
    end
    [hr, hi]
  end

  # using vector to allow complex sums (z=e^2*pi*i/n -> fourier transform)
  # z_transform(data, n, exp(Complex(0.0, (2.0 / n) * PI)))
  add_help(:z_transform,
           "z_transform(data, n, z) performs a Z transform on data; \
if z=e^2*pi*j/n you get a Fourier transform; complex results in returned vector")
  def z_transform(f, n, z)
    make_array(n) do |w|
      sum = 0.0
      t = 1.0
      m = z ** w
      n.times do |k|
        sum += f[k] * t
        t *= m
      end
      sum
    end
  end

  # slow Hartley transform
  #
  # taken from Perry Cook's SignalProcessor.m (the slow version of the
  # Hartley transform)
  add_help(:dht, "dht(data) returns the Hartley transform of 'data'.")
  def dht(data)
    len = data.length
    arr = make_vct(len)
    w = (2.0 * PI) / len
    len.times do |i|
      data.each_with_index do |val, j|
        arr[i] += val * (cos(i * j * w) + sin(i * j * w))
      end
    end
    arr
  end

  add_help(:find_sine,
           "find_sine(freq, beg, dur) \
returns the amplitude and initial-phase (for sin) at freq between beg and dur")
  def find_sine(freq, beg, dur)
    incr = hz2radians(freq)
    sw = 0.0
    cw = 0.0
    reader = make_sample_reader(beg, false, false, 1, false)
    dur.times do |i|
      samp = next_sample(reader)
      sw += samp * sin(i * incr)
      cw += samp * cos(i * incr)
    end
    [2.0 * (sqrt(sw * sw + cw * cw) / dur), atan2(cw, sw)]
  end

# this is a faster version of find-sine using the "Goertzel algorithm"
# taken from R Lyons "Understanding DSP" p 529
# it returns the same result as find_sine above if you take (* 2 (/
# (goertzel...) dur)) -- see snd-test.rb examples

  def goertzel(freq, beg = 0, dur = frames())
    y0 = 0.0
    y1 = 0.0
    y2 = 0.0
    rfreq = (TWO_PI * freq) / srate()
    cs = 2.0 * cos(rfreq)
    scan_channel(lambda do |y|
                   y2, y1 = y1, y0
                   y0 = (y1 * cs - y2) + y
                   false
                 end, beg, dur)
    (y0 - y1 * exp(Complex(0.0, -rfreq))).abs
  end

  def make_spencer_filter
    data = [-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3].map do |n| n / 320.0 end
    make_fir_filter(15, vector2vct(data))
  end

  # any-random
  #
  # arbitrary random number distributions via the "rejection method"
  def any_random(amount, e = false)
    if amount.zero?
      0.0
    else
      unless e
        random(amount)
      else
        next_random = lambda do | |
          len = e.length
          x = random(e[len - 2].to_f)
          y = random(1.0)
          if y <= envelope_interp(x, e) or c_g?
            x
          else
            next_random.call
          end
        end.call
      end
    end
  end

  def gaussian_distribution(s)
    e = []
    den = 2.0 * s * s
    x = 0.0
    y = -4.0
    21.times do |i|
      e.push(exp(-((y * y) / den)))
      e.push(x)
      x += 0.05
      y += 0.4
    end
    e
  end

  def pareto_distribution(a)
    e = []
    scl = 1.0 ** (a + 1.0) / a
    x = 0.0
    y = 1.0
    21.times do |i|
      e.push(scl * (a / y ** (a + 1.0)))
      e.push(x)
      x += 0.05
      y += 0.2
    end
    e
  end
  # map_channel(lambda do |y| any_random(1.0, [0, 1, 1, 1])) # uniform distribution
  # map_channel(lambda do |y| any_random(1.0, [0, 0, 0.95, 0.1, 1, 1])) # mostly toward 1.0
  # let(gaussian-distribution(1.0)) do |g|  map_channel(lambda do |y| any_random(1.0, g)) end
  # let(pareto-distribution(1.0))   do |g| map_channel(lambda do |y| any_random(1.0, g)) end

  # this is the inverse integration function used by CLM to turn a
  # distribution function into a weighting function
  def inverse_integrate(dist, data_size = 512, e_size = 50)
    first_sum = sum = dist[1].to_f
    x0 = dist[0].to_f
    x1 = dist[-2].to_f
    xincr = (x1 - x0) / e_size.to_f
    x = x0
    e = make_array(e_size * 2)
    0.step(e_size * 2 - 1, 2) do |i|
      e[i + 1] = x
      e[i] = sum
      sum += envelope_interp(x, dist)
      x += xincr
    end
    incr = (e[-2] - first_sum) / (data_size - 1)
    x = first_sum - incr
    make_vct!(data_size) do
      x += incr
      envelope_interp(x, e)
    end
  end

  def gaussian_envelope(s)
    e = []
    den = 2.0 * s * s
    x = -1.0
    y = -4.0
    e = make_array(42)
    0.step(41, 2) do |i|
      e[i] = x
      e[i + 1] = exp(-((y * y) / den))
      x += 0.1
      y += 0.4
    end
    e
  end
  # make_rand(:envelope, gaussian-envelope(1.0))

  # Julius Smith stuff
  #
  # these are from "Mathematics of the DFT", W3K Pubs
  def channel_mean(snd = false, chn = false)
    sum = 0.0
    n = frames(snd, chn)
    scan_channel(lambda do |y| sum += y; false; end, 0, n, snd, chn)
    sum / n
  end

  def channel_total_energy(snd = false, chn = false)
    sum = 0.0
    scan_channel(lambda do |y| sum += y * y; false; end, 0, frames(snd, chn), snd, chn)
    sum
  end

  def channel_average_power(snd = false, chn = false)
    channel_total_energy(snd, chn) / frames(snd, chn)
  end

  def channel_rms(snd = false, chn = false)
    sqrt(channel_average_power(snd, chn))
  end

  def channel_variance(snd = false, chn = false)
    n = frames(snd, chn)
    mu = (n / (n - 1)) * channel_mean(snd, chn)
    p = channel_total_energy(snd, chn)
    p - mu * mu
  end

  def channel_norm(snd = false, chn = false)
    sqrt(channel_total_energy(snd, chn))
  end

  def channel_lp(p, snd = false, chn = false)
    sum = 0.0
    n = frames(snd, chn)
    scan_channel(lambda do |y| sum += y.abs ** p; false; end, 0, n, snd, chn)
    sum ** (1.0 / p)
  end

  def channel_lp_inf(snd = false, chn = false)
    mx = 0.0
    n = frames(snd, chn)
    scan_channel(lambda do |y| mx = [mx, y.abs].max; false; end, 0, n, snd, chn)
    mx
  end

  def channel2_inner_product(s1, c1, s2, c2)
    sum = 0.0
    r1 = make_sample_reader(0, s1, c1)
    r2 = make_sample_reader(0, s2, c2)
    frames(s1, c1).times do |i| sum += r1.call * r2.call end
    sum
  end

  def channel2_angle(s1, c1, s2, c2)
    inprod = channel2_inner_product(s1, c1, s2, c2)
    norm1 = channel_norm(s1, c1)
    norm2 = channel_norm(s2, c2)
    acos(inprod / (norm1 * norm2))
  end if defined? acos

  def channel2_orthogonal?(s1, c1, s2, c2)
    channel2_inner_product(s1, c1, s2, c2).zero?
  end

  def channel2_coefficient_of_projection(s1, c1, s2, c2)
    inprod = channel2_inner_product(s1, c1, s2, c2)
    norm1 = channel_norm(s1, c1)
    inprod / (norm1 * norm1)
  end

  # end of JOS stuff

  def channel_distance(s1, c1, s2, c2)
    r1 = make_sample_reader(0, s1, c1)
    r2 = make_sample_reader(0, s2, c2)
    sum = 0.0
    [frames(s1, c1), frames(s2, c2)].min.times do
      diff = r1.call - r2.call
      sum += diff * diff
    end
    sqrt(sum)
  end

  def periodogram(n)
    # the "Bartlett" version, apparently
    len = frames()
    average_data = make_vct(n)
    rd = make_sample_reader(0)
    n2 = n * 2
    rl = make_vct(n2)
    im = make_vct(n2)
    len.times do
      vct_scale!(rl, 0.0)
      vct_scale!(im, 0.0)
      n.times do |k| rl[k] = rd.call end
      mus_fft(rl, im)
      n.times do |k| average_data[k] += rl[k] * rl[k] + im[k] * im[k] end
    end
    graph(vct_scale!(average_data, 1.0 / (len / n).ceil))
  end

  # ssb-am friends

  def shift_channel_pitch(freq, order = 40, beg = 0, dur = false,
                          snd = false, chn = false, edpos = false)
    gen = make_ssb_am(freq, order)
    map_channel(lambda do |y| ssb_am(gen, y) end, beg, dur, snd, chn, edpos,
                format("%s(%s, %s, %s, %s", get_func_name, freq, order, beg, dur))

  end

  def hz_to_2pi(freq)
    (TWO_PI * freq) / srate()
  end
  
  def ssb_bank(old_freq, new_freq, pairs,
               order = 40, bw = 50.0, beg = 0, dur = false,
               snd = false, chn = false, edpos = false)
    factor = (new_freq - old_freq.to_f) / old_freq
    mx = maxamp
    ssbs = make_array(pairs)
    bands = make_array(pairs) do |i|
      aff = (i + 1.0) * old_freq
      bwf = bw * (1.0 + (i + 1.0) / (2.0 * pairs))
      ssbs[i] = make_ssb_am((i + 1.0) * factor * old_freq)
      make_bandpass(hz_to_2pi(aff - bwf), hz_to_2pi(aff + bwf), order)
    end
    as_one_edit_rb("%s(%s, %s, %s, %s, %s, %s, %s",
                   get_func_name, old_freq, new_freq, pairs, order, bw, beg, dur) do | |
      nmx = 0.0
      map_channel_rb(beg, dur, snd, chn, edpos) do |y|
        sum = 0.0
        ssbs.zip(bands) do |sbs, bds| sum += ssb_am(sbs, bandpass(bds, y)) end
        nmx = [nmx, sum.abs].max
        sum
      end
      scale_channel(mx / nmx, beg, dur, snd, chn)
    end
  end

  # this version adds a frequency envelope
  # ssb_bank_env(557, 880, [0, 0, 1, 100.0], 7)
  def ssb_bank_env(old_freq, new_freq, freq_env, pairs,
                   order = 40, bw = 50.0, beg = 0, dur = false,
                   snd = false, chn = false, edpos = false)
    factor = (new_freq - old_freq.to_f) / old_freq
    mx = maxamp
    ssbs = make_array(pairs)
    frenvs = make_array(pairs)
    bands = make_array(pairs) do |i|
      aff = (i + 1.0) * old_freq
      bwf = bw * (1.0 + (i + 1.0) / (2.0 * pairs))
      ssbs[i] = make_ssb_am((i + 1.0) * factor * old_freq)
      frenvs[i] = make_env(:envelope, freq_env, :scaler, hz2radians(i.to_f), :end, frames() - 1)
      make_bandpass(hz_to_2pi(aff - bwf), hz_to_2pi(aff + bwf), order)
    end
    as_one_edit_rb("%s(%s, %s, %s, %s, %s, %s, %s, %s",
                   get_func_name, old_freq, new_freq, freq_env.inspect,
                   pairs, order, bw, beg, dur) do | |
      nmx = 0.0
      map_channel_rb(beg, dur, snd, chn, edpos) do |y|
        sum = 0.0
        ssbs.each_with_index do |sbs, i|
          sum += ssb_am(sbs, bandpass(bands[i], y), env(frenvs[i]))
        end
        nmx = [nmx, sum.abs].max
        sum
      end
      scale_channel(mx / nmx, beg, dur, snd, chn)
    end
  end

  #vct|channel|spectral-polynomial
  def vct_polynomial(v, coeffs)
    new_v = Vct.new(v.length, coeffs.last)
    (coeffs.length - 2).downto(0) do |i|
      new_v.multiply!(v).offset!(coeffs[i])
    end
    new_v
  end

  def channel_polynomial(coeffs, snd = false, chn = false)
    len = frames(snd, chn)
    vct2channel(vct_polynomial(channel2vct(0, len, snd, chn), coeffs), 0, len, snd, chn, false,
                format("%s(%s", get_func_name, coeffs.to_str))
  end

  # channel_polynomial(vct(0.0, 0.5)) = x*0.5
  # channel_polynomial(vct(0.0, 1.0, 1.0, 1.0)) = x*x*x + x*x + x
  # 
  # convolution -> * in freq

  def spectral_polynomial(coeffs, snd = false, chn = false)
    len = frames(snd, chn)
    sound = channel2vct(0, len, snd, chn)
    num_coeffs = coeffs.length
    fft_len = if num_coeffs < 2
                len
              else
                (2.0 ** (log((num_coeffs - 1.0) * len) / log(2.0)).ceil).to_i
              end
    rl1 = make_vct(fft_len)
    rl2 = make_vct(fft_len)
    new_sound = make_vct(fft_len)
    if coeffs[0] > 0.0
      new_sound.map! do mus_random(coeffs[0]) end
    end
    if num_coeffs > 1
      new_sound.add!(sound.scale(coeffs[1]))
      if num_coeffs > 2
        peak = maxamp(snd, chn)
        rl1.scale!(0.0).add!(sound)
        (2...num_coeffs).each do |i|
          convolution(rl1, rl2.scale!(0.0).add(sound), fft_len)
          new_sound.add!(rl1.scale((coeffs[i] * peak) / rl1.peak))
        end
        new_sound.scale!(peak / new_sound.peak)
      end
    end
    vct2channel(new_sound, 0, [len, len * (num_coeffs - 1)].max, snd, chn, false,
                format("%s(%s", get_func_name, coeffs.to_str))
  end

  # SCENTROID
  # 
  # by Bret Battey
  # Version 1.0 July 13, 2002
  # translated to Snd/Scheme Bill S 19-Jan-05
  # 
  # Returns the continuous spectral centroid envelope of a sound.  The
  # spectral centroid is the "center of gravity" of the spectrum, and it
  # has a rough correlation to our sense of "brightness" of a sound.
  # 
  # [Beauchamp, J., "Synthesis by spectral amplitude and 'brightness' matching
  # analyzed musical sounds". Journal of Audio Engineering Society 30(6), 396-406]
  # 
  # The formula used is:
  #    C = [SUM<n=1toj>F(n)A(n)] / [SUM<n=1toj>A(n)]
  #    Where j is the number of bins in the analysis, 
  #    F(n) is the frequency of a given bin,
  #    A(n) is the magnitude of the given bin.
  # 
  # If a pitch envelope for the analyzed sound is available, the results
  # of SCENTROID can be used with the function NORMALIZE-CENTROID,
  # below, to provide a "normalized spectral centroid".
  # 
  # DB-FLOOR -- Frames below this decibel level (0 dB = max) will be
  # discarded and returned with spectral centroid = 0
  # 
  # RFREQ -- Rendering frequency. Number of measurements per second.
  # 
  # FFTSIZE -- FFT window size. Must be a power of 2. 4096 is
  # recommended.

  def scentroid(file, *args)
    beg, dur, db_floor, rfreq, fftsize = nil
    optkey(args, binding,
           [:beg, 0.0],
           :dur,
           [:db_floor, -40.0],
           [:rfreq, 100.0],
           [:fftsize, 4096])
    assert_type(File.exist?(file), file, 0, "an existing file")
    fsr = mus_sound_srate(file)
    incrsamps = (fsr / rfreq).floor
    start = (beg * fsr).floor
    ende = start + (dur ? (dur.to_f * fsr).to_i : (mus_sound_frames(file) - beg))
    fdr = make_vct(fftsize)
    fdi = make_vct(fftsize)
    windows = ((ende - start.to_f) / incrsamps).floor + 1
    results = make_vct(windows)
    fft2 = (fftsize / 2.0).floor
    binwidth = fsr / fftsize.to_f
    rd = make_readin(file)
    loc = 0
    start.step(ende, incrsamps) do |i|
      rd.location = i
      sum_of_squares = 0.0
      fdr.map! do
        val = readin(rd)
        sum_of_squares += val * val
        val
      end
      if linear2db(sqrt(sum_of_squares / fftsize.to_f)) >= db_floor
        numsum = 0.0
        densum = 0.0
        clear_array(fdi)
        mus_fft(fdr, fdi, fftsize)
        rectangular2polar(fdr, fdi)
        fft2.times do |j|
          numsum += j * binwidth * fdr[j]
          densum += fdr[j]
        end
        results[loc] = numsum / densum
      end
      loc += 1
    end
    results
  end
  
  # invert_filter inverts an FIR filter
  #
  # say we previously filtered a sound via filter_channel([0.5, 0.25, 0.125].to_vct)
  #   and we want to undo it without using undo_edit:
  #   filter_channel(invert_filter([0.5, 0.25, 0.125].to_vct))
  # 
  # there are a million gotchas here.  The primary one is that the inverse filter
  # can "explode" -- the coefficients can grow without bound.  For example, any
  # filter returned by spectrum2coeffs above will be a problem (it always returns
  # a "linear phase" filter).

  add_help(:invert_filter,
           "invert_filter(coeffs)  \
tries to return an inverse filter to undo the effect of the FIR filter coeffs.")
  def invert_filter(fcoeffs)
    order = fcoeffs.length + 32
    coeffs = Vct.new(order)
    fcoeffs.each_with_index do |val, i| coeffs[i] = val end
    nfilt = Vct.new(order)
    nfilt[0] = 1.0 / coeffs.first
    (1...order).each do |i|
      sum = 0.0
      k = i
      i.times do |j|
        sum += nfilt[j] * coeffs[k]
        k -= 1
        nfilt[i] = sum / -coeffs.first
      end
    end
    nfilt
  end

  # Volterra filter
  #
  # one of the standard non-linear filters
  # this version is taken from Monson Hayes "Statistical DSP and Modeling"
  # it is a slight specialization of the form mentioned by J O Smith and others

  class Volterra_filter < Musgen
    def initialize(acoeffs, bcoeffs)
      super()
      @as = acoeffs
      @bs = bcoeffs
      @xs = Vct.new([acoeffs.length, bcoeffs.length].max)
    end

    def inspect
      format("%s.new(%s, %s)", self.class, @as.to_str, @bs.to_str)
    end

    def to_s
      format("#<%s acoeffs: %s, bcoeffs: %s>", self.class, @as, @bs)
    end

    def run_func(val1 = 0.0, val2 = 0.0)
      volterra_filter(val1)
    end

    def volterra_filter(x)
      xlen = @xs.length
      @xs.move!(xlen - 1, xlen - 2, true)
      @xs.first = x
      sum = dot_product(@as, @xs, @as.length)
      @bs.length.times do |i|
        @bs.length.times do |j|
          sum += @bs[j] * @xs[i] * @xs[j]
        end
      end
      sum
    end
  end
  
  def make_volterra_filter(acoeffs, bcoeffs)
    Volterra_filter.new(acoeffs, bcoeffs)
  end

  def volterra_filter(flt, x)
    flt.volterra_filter(x)
  end
  # flt = make_volterra_filter([0.5, 0.1].to_vct, [0.3, 0.2, 0.1].to_vct)
  # map_channel(lambda do |y| volterra_filter(flt, y) end)

  class Moving_max < Musgen
    def initialize(size = 128)
      super()
      @length = size
      @gen = make_delay(:size, size)
      @gen.scaler = 0.0
      @data = @gen.data
    end

    def inspect
      format("%s.new(%s)", self.class, @length)
    end

    def to_s
      format("#<%s size: %s, scaler: %s>", self.class, @length, @gen.scaler)
    end

    def run_func(val1 = 0.0, val2 = 0.0)
      moving_max(val1)
    end
    
    def moving_max(y)
      absy = y.abs
      mx = delay(@gen, absy)
      pk = @gen.scaler - 0.001
      if absy > pk
        @gen.scaler = absy
      else
        if mx >= pk
          @gen.scaler = @gen.data.peak
        end
      end
      @gen.scaler
    end
  end

  add_help(:make_moving_max,
           "make_moving_max(size = 128)  returns a windowed-maxamp generator.  \
The generator keeps a running window of the last 'size' inputs, \
returning the maxamp in that window.")
  def make_moving_max(size = 128)
    Moving_max.new(size)
  end

  add_help(:moving_max,
           "moving_max(gen, input)  \
returns the maxamp in a running window on the last few inputs.")
  def moving_max(gen, y)
    gen.moving_max(y)
  end
  alias windowed_maxamp                 moving_max
  alias make_windowed_maxamp            make_moving_max

  # moving-sum generator (the sum norm or 1-norm)
  
  add_help(:make_moving_sum,
           "make_moving_sum([size=128])  returns a moving-sum generator.  \
The generator keeps a running window of the last 'size' inputs, \
returning the sum of the absolute values of the samples in that window.")
  def make_moving_sum(size = 128)
    gen = make_moving_average(size)
    gen.increment = 1.0
    gen
  end

  add_help(:moving_sum,
           "moving_sum(gen, y)  \
returns the sum of the absolute values in a moving window over the last few inputs.")
  def moving_sum(gen, y)
    moving_average(gen, y.abs)
  end

  # moving-rms generator

  add_help(:make_moving_rms,
           "make_moving_rms([size=128])  returns a moving-rms generator.  \
The generator keeps a running window of the last 'size' inputs, \
returning the rms of the samples in that window.")
  def make_moving_rms(size = 128)
    make_moving_average(size)
  end

  add_help(:moving_rms,
           "moving_rms(gen, y)  \
returns the rms of the values in a window over the last few inputs.")
  def moving_rms(gen, y)
    sqrt(moving_average(gen, y * y))
  end

  # moving-length generator (euclidean norm or 2-norm)

  add_help(:make_moving_length,
           "make_moving_length([size=128])  returns a moving-length generator.  \
The generator keeps a running window of the last 'size' inputs, \
returning the euclidean length of the vector in that window.")
  def make_moving_length(size = 128)
    gen = make_moving_average(size)
    gen.increment = 1.0
    gen
  end

  add_help(:moving_length,
           "moving_length(gen, y)  \
returns the length of the values in a window over the last few inputs.")
  def moving_length(gen, y)
    sqrt(moving_average(gen, y * y))
  end

  
  # harmonicizer (each harmonic is split into a set of harmonics via Chebyshev polynomials)
  # obviously very similar to ssb_bank above, but splits harmonics
  # individually, rather than pitch-shifting them
  add_help(:harmonicizer,
           "harmonicizer(freq, coeffs, pairs,
[order=40, [bw=50.0, [beg=0, [dur=false, [snd=false [chn=false, [edpos=false]]]]]]])  \
splits out each harmonic and replaces it with the spectrum given in coeffs")
  def harmonicizer(freq, coeffs, pairs,
                   order = 40,
                   bw = 50.0,
                   beg = 0,
                   dur = false,
                   snd = false,
                   chn = false,
                   edpos = false)
    bands = make_array(pairs)
    pcoeffs = partials2polynomial(coeffs)
    avgs = make_array(pairs)
    peaks = make_array(pairs)
    flt = make_filter(2, vct(1, -1), vct(0, -0.9))
    old_mx = maxamp
    new_mx = 0.0
    ctr = 40
    1.upto(pairs) do |i|
      aff = i * freq
      bwf = bw * (1.0 + i / (2 * pairs))
      peaks[i - 1] = make_moving_max(128)
      avgs[i - 1] = make_moving_average(128)
      bands[i - 1] = make_bandpass(hz_to_2pi(aff - bwf), hz_to_2pi(aff + bwf), order)
    end
    as_one_edit_rb do
      map_channel_rb(beg, dur, snd, chn, edpos) do |y|
        sum = 0.0
        bands.zip(peaks, avgs) do |bs, ps, as|
          sig = bandpass(bs, y)
          mx = moving_max(ps, sig)
          amp = moving_average(as, mx > 0.0 ? [100.0, 1.0 / mx].min : 0.0)
          if amp > 0.0
            sum += mx * polynomial(pcoeffs, amp * sig)
          end
        end
        val = filter(flt, sum)
        new_mx = [new_mx, val.abs].max
        if ctr.zero?
          val
        else
          ctr -= 1
          0.0
        end
      end
      if new_mx > 0.0
        scale_channel(old_mx / new_mx, beg, dur, snd, chn)
      end
    end
  end

  # linear sampling rate conversion

  add_help(:linear_src_channel,
          "linear_src_channel(sr, [snd=false, [chn=false]]) \
performs sampling rate conversion using linear interpolation.")
  def linear_src_channel(srinc, snd = false, chn = false)
    rd = make_sample_reader(0, snd, chn)
    last = rd.call
    nxt = rd.call
    intrp = 0.0
    tempfile = with_sound(:clm, true, :output, snd_tempnam, :srate, srate()) do
      samp = 0
      until sample_reader_at_end?(rd)
        if (pos = intrp) >= 1.0
          pos.floor.times do |i| last, nxt = nxt, rd.call end
          pos -= pos.floor
        end
        intrp = pos + pos.floor
        out_any(samp, last + pos * (nxt - last), 0, $output)
        samp += 1
      end
    end.output
    len = mus_sound_frames(tempfile)
    set_samples(0, len - 1, tempfile, snd, chn, true, "%s(%s", get_func_name, srinc, 0, false, true)
    # first true=truncate to new length, false=at current edpos, true=auto delete temp file
  end

  # Mathews/Smith High-Q filter as described in http://ccrma.stanford.edu/~jos/smac03maxjos/

  class Mfilter < Musgen
    def initialize(decay, freq)
      super()
      @decay = decay.to_f
      @frequency = freq.to_f
      @eps = 2.0 * sin((PI * freq) / mus_srate())
      @xn = @yn = 0.0
    end
    attr_accessor :decay, :eps

    def inspect
      format("%s.new(%0.3f, %0.3f)", self.class, @decay, @frequency)
    end

    def to_s
      format("#<%s decay: %0.3f, frequency: %0.3f>", self.class, @decay, @frequency)
    end
    
    def mfilter(x_input = 0.0, y_input = 0.0)
      @xn = x_input + @decay * (@xn - @eps * @yn)
      @yn = y_input + @decay * (@eps * @xn + @yn)
    end
  end

  def make_mfilter(*args)
    Mfilter.new(get_args(args, :decay, 0.99), get_args(args, :frequency, 1000.0))
  end

  def mfilter(m, x_input = 0.0, y_input = 0.0)
    m.mfilter(x_input, y_input)
  end
=begin
  with_sound(:clm, true) do
    rd = make_sample_reader(0, "now.snd")
    m = make_mfilter
    10000.times do |i| outa(i, mfilter(m, 0.1 * rd.call), $output) end
  end
=end
  #
  # sweep center freq:
=begin
  with_sound(:clm, true) do
    rd = make_sample_reader(0, "oboe.snd")
    m = make_mfilter(:decay, 0.99, :frequency, 1000)
    e = make_env([0, 100, 1, 2000], :end, 10000)
    10000.times do |i|
      outa(i, mfilter(m, 0.1 * rd.call), $output)
      m.eps = 2.0 * sind((PI * env(e)) / mus_srate())
    end
  end
=end
  #
  # harmonics:
=begin
  with_sound(:clm, true, :statistics, true) do
    noi = make_rand(10000)
    filters = make_array(9) do make_mfilter(:decay, 0.999, :frequency, 400.0 * (i + 1)) end
    10000.times do |i|
      sum = 0.0
      input = 0.01 * rand(noi)
      filters.each do |f| sum += (1.0 / (j + 1)) * mfilter(f, input) end
      outa(i, sum $output)
    end
  end
=end
  
  #
  # spectrum displayed in various frequency scales
  #
  class Display_bark_fft
    # click in lisp-graph to change the tick placement choice
    def initialize
      @bark_fft_size = 0
      @bark_tick_function = 0
    end
    attr_reader :bark_tick_function

    def display_bark_fft(snd, chn)
      ls = left_sample(snd, chn)
      rs = right_sample(snd, chn)
      if (fftlen = (2 ** (log((rs - ls) + 1.0) / log(2.0)).ceil.to_i).to_i) > 0
        data = channel2vct(ls, fftlen, snd, chn)
        normalized = (transform_normalization(snd, chn) != Dont_normalize)
        linear = true
        if vct?(data)
          fft = snd_spectrum(data,
                             fft_window(snd, chn), fftlen, linear,
                             fft_window_beta(snd, chn), false, normalized)
          if vct?(fft)
            sr = srate(snd)
            mx = fft.peak
            data_len = fft.length

            # bark settings
            bark_low    = bark(20.0).floor
            bark_high   = bark(0.5 * sr).ceil
            bark_frqscl = data_len / (bark_high - bark_low)
            bark_data   = Vct.new(data_len)

            # mel settings
            mel_low     = mel(20.0).floor
            mel_high    = mel(0.5 * sr).ceil
            mel_frqscl  = data_len / (mel_high - mel_low)
            mel_data    = Vct.new(data_len)

            # erb settings
            erb_low     = erb(20.0).floor
            erb_high    = erb(0.5 * sr).ceil
            erb_frqscl  = data_len / (erb_high - erb_low)
            erb_data    = Vct.new(data_len)

            @bark_fft_size = fftlen
            fftlenf = fftlen.to_f

            fft.each_with_index do |val, i|
              frq = sr * (i / fftlenf)
              bark_bin = (bark_frqscl * (bark(frq) - bark_low)).round
              mel_bin  = (mel_frqscl  * (mel(frq)  -  mel_low)).round
              erb_bin  = (erb_frqscl  * (erb(frq)  -  erb_low)).round
              if bark_bin.between?(0, data_len - 1) then bark_data[bark_bin] += val end
              if  mel_bin.between?(0, data_len - 1) then mel_data[mel_bin]   += val end
              if  erb_bin.between?(0, data_len - 1) then erb_data[erb_bin]   += val end
            end

            if normalized
              bmx = bark_data.peak
              mmx = mel_data.peak
              emx = erb_data.peak
              if (mx - bmx).abs > 0.01 then bark_data.scale!(mx / bmx) end
              if (mx - mmx).abs > 0.01 then mel_data.scale!(mx / mmx)  end
              if (mx - emx).abs > 0.01 then erb_data.scale!(mx / emx)  end
            end
            graph([bark_data, mel_data, erb_data],
                  "ignored",
                  20.0, 0.5 * sr,
                  0.0, (normalized ? 1.0 : data_len * y_zoom_slider(snd, chn)),
                  snd, chn,
                  false, Show_bare_x_axis)
          end
        end
      end
      false
    end

    def mark_bark_labels(snd, chn)
      # at this point the x axis has no markings, but there is room for labels and ticks
      old_foreground_color = foreground_color(snd, chn, Copy_context)
      # assume at start the foreground color is correct
      axinfo = axis_info(snd, chn, Lisp_graph)
      axis_x0 = axinfo[10]
      axis_x1 = axinfo[12]
      axis_y0 = axinfo[13]
      axis_y1 = axinfo[11]
      label_height = 15
      char_width = 8
      sr2 = 0.5 * srate(snd)
      minor_tick_len = 6
      major_tick_len = 12
      tick_y0 = axis_y1
      minor_y0 = axis_y1 + minor_tick_len
      major_y0 = axis_y1 + major_tick_len
      label_pos = (axis_x0 + 0.45 * (axis_x1 - axis_x0)).to_i
      bark_label_font = snd_font(3)
      bark_numbers_font = snd_font(2)
      scale_position = lambda do |scale, f|
        b20 = scale.call(20.0)
        (axis_x0 +
         ((axis_x1 - axis_x0) * (scale.call(f) - b20)) /
         (scale.call(sr2) - b20)).round
      end
      bark_position = lambda do |f| scale_position.call(method(:bark).to_proc, f) end
      mel_position  = lambda do |f| scale_position.call(method(:mel).to_proc, f)  end
      erb_position  = lambda do |f| scale_position.call(method(:erb).to_proc, f)  end
      draw_bark_ticks = lambda do |bark_function|
        if bark_numbers_font then set_current_font(bark_numbers_font, snd, chn, Copy_context) end
        draw_line(axis_x0, tick_y0, axis_x0, major_y0, snd, chn, Copy_context)
        i1000  = scale_position.call(bark_function,  1000.0)
        i10000 = scale_position.call(bark_function, 10000.0)
        draw_line( i1000, tick_y0,  i1000, major_y0, snd, chn, Copy_context)
        draw_line(i10000, tick_y0, i10000, major_y0, snd, chn, Copy_context)
        draw_string(   "20",        axis_x0, major_y0, snd, chn, Copy_context)
        draw_string( "1000",  i1000 - 3 * 4, major_y0, snd, chn, Copy_context)
        draw_string("10000", i10000 - 6 * 4, major_y0, snd, chn, Copy_context)
        draw_string("fft size: #{@bark_fft_size}", axis_x0 + 10, axis_y0, snd, chn, Copy_context)
        100.step(1000, 100) do |i|
          i100 = scale_position.call(bark_function, i)
          draw_line(i100, tick_y0, i100, minor_y0, snd, chn, Copy_context)
        end
        2000.step(10000, 1000) do |i|
          i1000 = scale_position.call(bark_function, i)
          draw_line(i1000, tick_y0, i1000, minor_y0, snd, chn, Copy_context)
        end
      end

      # bark label/ticks
      if self.bark_tick_function.zero?
        draw_bark_ticks.call(bark_position)
      end
      if bark_label_font then set_current_font(bark_label_font, snd, chn, Copy_context) end
      draw_string("bark,", label_pos, axis_y1 + label_height, snd, chn, Copy_context)

      # mel label/ticks
      set_foreground_color(snd_color(2), snd, chn, Copy_context)
      if self.bark_tick_function == 1
        draw_bark_ticks.call(mel_position)
      end
      if bark_label_font then set_current_font(bark_label_font, snd, chn, Copy_context) end
      draw_string("mel,", char_width * 6 + label_pos, axis_y1 + label_height,
                  snd, chn, Copy_context)

      # erb label/ticks
      set_foreground_color(snd_color(4), snd, chn, Copy_context)
      if self.bark_tick_function == 2
        draw_bark_ticks.call(erb_position)
      end
      if bark_label_font then set_current_font(bark_label_font, snd, chn, Copy_context) end
      draw_string("erb", char_width * (6 + 5) + label_pos, axis_y1 + label_height,
                  snd, chn, Copy_context)

      set_foreground_color(old_foreground_color, snd, chn, Copy_context)
    end
    
    # mouse click = move to next scale's ticks
    def choose_bark_ticks(snd, chn, button, state, x, y, axis)
      if axis == Lisp_graph
        @bark_tick_function += 1
        if @bark_tick_function > 2 then @bark_tick_function = 0 end
        update_lisp_graph(snd, chn)
      end
    end

    private
    def bark(f)
      f2 = f / 7500.0
      13.5 * atan(0.00076 * f) + (3.5 * atan(f2 * f2))
    end

    def mel(f)
      1127.0 * log(1.0 + f / 700.0)
    end

    def erb(f)
      43.0 + 11.17 * log((f + 312.0) / (f + 14675.0))
    end
  end
  # user's view of display-bark-fft function
  def display_bark_fft(off = false)
    if off.kind_of?(FalseClass)
      db = Display_bark_fft.new
      $lisp_graph_hook.add_hook!("display-bark-fft") do |snd, chn|
        db.display_bark_fft(snd, chn)
      end
      $after_lisp_graph_hook.add_hook!("make-bark-label") do |snd, chn|
        db.mark_bark_labels(snd, chn)
      end
      $mouse_click_hook.add_hook!("choose-bark-ticks") do |snd, chn, button, state, x, y, axis|
        db.choose_bark_ticks(snd, chn, button, state, x, y, axis)
      end
      Snd.sounds.each do |snd|
        channels(snd).times do |chn| update_lisp_graph(snd, chn) end
      end
    else
      $lisp_graph_hook.remove_hook!("display-bark-fft")
      $after_lisp_graph_hook.remove_hook!("make-bark-label")
      $mouse_click_hook.remove_hook!("choose-bark-ticks")
      Snd.sounds.each do |snd|
        channels(snd).times do |chn| set_lisp_graph?(false, snd, chn) end
      end
    end
    off
  end

  def undisplay_bark_fft
    display_bark_fft(true)
  end
end

include Dsp

# dsp.rb ends here
