# env.rb -- snd-7/env.scm

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Sat Oct 30 14:47:37 CEST 2004

# Commentary:
#
# Module Env (see env.scm)
#  envelope_interp(x, en, base)
#  window_envelope(beg, dur, en)
#  map_envelopes(en1, en2, &func)
#  multiply_envelopes(en1, en2)
#  add_envelopes(en1, en2)
#  max_envelope(en)
#  min_envelope(en)
#  integrate_envelope(en)
#  envelope_last_x(en)
#  stretch_envelope(fn, old_att, new_att, old_dec, new_dec)
#  scale_envelope(en, scale, offset)
#  reverse_envelope(en) -- envelope_reverse
#  concatenate_envelopes(*envs) -- envelope_concatenate
#  repeat_envelope(ur_env, repeats, reflect, x_normalized) -- envelope_repeat
#  make_power_env(*args)
#  power_env(pe)
#  power_env_channel(pe, *args)
#  envelope_length(en)
#  normalize_envelope(en, new_max)
#  x_norm(en, xmax)

# Code:

require "examp"

module Env
  def envelope_interp(x, en = [], base = 1.0)
    doc("envelope_interp(*args)
envelope_interp(x, env, base = 1.0) -> value of env at x;
base controls connecting segment type:
envelope_interp(0.3, [0, 0, 0.5, 1, 1, 0]) -> 0.6\n") if x == :help
    en.map! do |y| y.to_f end unless en.empty?
    if en.empty?
      0.0
    else
      if x <= en[0] or en[2..-1].empty?
        en[1]
      else
        if en[2] > x
          if en[1] == en[3] or base.zero?
            en[1]
          else
            if base == 1.0
              en[1] +
                (x - en[0]) *
                ((en[3] - en[1]) / (en[2] - en[0]))
            else
              en[1] +
                ((en[3] - en[1]) / (base - 1.0)) *
                ((base ** ((x - en[0]) / (en[2] - en[0]))) - 1.0)
            end
          end
        else
          envelope_interp(x, en[2..-1], base)
        end
      end
    end
  end

  def window_envelope(beg, dur = 0.0, en = [])
    doc("window_envelope(beg, dur, env)
portion of env lying between x axis values beg and end:
window_envelope(1.0, 3.0, [0.0, 0.0, 5.0, 1.0]) -> [1.0, 0.2, 3.0, 0.6]\n") if beg == :help
    en.map! do |x| x.to_f end unless en.empty?
    nenv = []
    lasty = en.empty? ? 0.0 : en[1]
    len = en.length
    0.step(len - 1, 2) do |i|
      x = en[i]
      y = lasty = en[i + 1]
      if nenv.empty?
        if x >= beg
          nenv.push(beg, envelope_interp(beg, en))
          unless x == beg
            if x >= dur
              return nenv.push(dur, envelope_interp(dur, en))
            else
              nenv.push(x, y)
            end
          end
        end
      else
        if x <= dur
          nenv.push(x, y)
          return nenv if x == dur
        else
          if x > dur
            return nenv.push(dur, envelope_interp(dur, en))
          end
        end
      end
    end
    nenv.push(dur, lasty)
  end

  def map_envelopes(en1, en2 = [], &func)
    doc("map_envelopes(env1, env2) { |x, y| ... }
maps { ... } function over the breakpoints in env1 and env2 returning
a new envelope\n") if en1 == :help
    en1.map! do |x| x.to_f end unless en1.empty?
    en2.map! do |x| x.to_f end if en2.kind_of?(Array) and (not en2.empty?)
    xs = []
    at0 = lambda do |e|
      diff = e.first
      lastx = e[-2]
      0.step(e.length - 1, 2) do |i|
        x = (e[i] - diff) / lastx
        xs.push(x)
        e[i] = x
      end
      e
    end
    if en1.empty?
      at0.call(en2)
    else
      if en2.empty?
        at0.call(en1)
      else
        ee1 = at0.call(en1)
        ee2 = at0.call(en2)
        newe = []
        xs.uniq.sort.each do |x|
          newe.push(x, func.call(envelope_interp(x, ee1), envelope_interp(x, ee2)))
        end
        newe
      end
    end
  end

  def multiply_envelopes(en1, en2 = [])
    doc("multiply_envelopes(env1, env2)
multiplies break-points of env1 and env2 returning a new envelope:
multiply_envelopes([0, 0, 2, 0.5], [0, 0, 1, 2, 2, 1]) -> [0.0, 0.0, 0.5, 0.5, 1.0, 0.5]
") if en1 == :help
    map_envelopes(en1, en2) do |x, y| x * y end
  end

  def add_envelopes(en1, en2)
    doc("add_envelopes(env1, env2)
adds break-points of env1 and env2 returning a new envelope\n") if en1 == :help
    map_envelopes(en1, en2) do |x, y| x + y end
  end

  def max_envelope(en)
    doc("max_envelope(env) -> max y value in env\n") if en == :help
    mx = en[1].to_f
    1.step(en.length - 1, 2) do |i| mx = [mx, en[i]].max.to_f end
    mx
  end
  
  def min_envelope(en)
    doc("min_envelope(env) -> min y value in env\n") if en == :help
    mn = en[1].to_f
    1.step(en.length - 1, 2) do |i| mn = [mn, en[i]].min.to_f end
    mn
  end

  def integrate_envelope(en)
    doc("integrate_envelope(env) -> area under env\n") if en == :help
    sum = 0.0
    0.step(en.length - 3, 2) do |i| sum += (en[i + 1] + en[i + 3]) * (en[i + 2] - en[i]) * 0.5 end
    sum
  end

  def envelope_last_x(en)
    doc("envelope_last_x(env) -> max x axis break point position\n") if en == :help
    en.empty? ? 0.0 : en[-2]
  end
  
  def stretch_envelope(fn, old_att = 0.0, new_att = 0.0, old_dec = false, new_dec = false)
    doc("stretch_envelope(fn, old_att, new_att, old_dec, new_dec)
Takes FN and returns a new envelope based on it but with the attack
and optionally decay portions stretched or squeezed; OLD_ATT is the
original x axis attack end point, NEW_ATT is where that section should
end in the new envelope.  Similarly for OLD_DEC and NEW_DEC.  This
mimics divseg in early versions of CLM and its antecedents in Sambox
and Mus10 (linen).
stretch_envelope([0, 0, 1, 1], 0.1, 0.2)
                 -> [0, 0, 0.2, 0.1, 1.0, 1]
stretch_envelope([0, 0, 1, 1, 2, 0], 0.1, 0.2, 1.5, 1.6)
                 -> [0, 0, 0.2, 0.1, 1.1, 1, 1.6, 0.5, 2.0, 0]\n") if fn == :help
    unless fn.kind_of?(Array)
      error("%s: need an envelope, %s", get_func_name, fn.inspect)
    end
    fn.map! do |x| x.to_f end unless fn.empty?
    if old_att.kind_of?(Numeric) and !new_att.kind_of?(Numeric)
      error("%s: wrong number of arguments, old_att %s, new_att %s",
            get_func_name, old_att.inspect, new_att.inspect)
    else
      if !new_att
        fn
      else
        if old_dec.kind_of?(Numeric) and !new_dec.kind_of?(Numeric)
          error("%s: wrong number of arguments, old_dec %s, new_dec %s",
                get_func_name, old_dec.inspect, new_dec.inspect)
        else
          new_x = x0 = fn[0]
          last_x = fn[-2]
          y0 = fn[1]
          new_fn = [x0, y0]
          scl = (new_att - x0) / [0.0001, old_att - x0].max
          old_dec += 0.000001 * last_x if old_dec and old_dec == old_att
          fn[2..-1].each_pair do |x1, y1|
            if x0 < old_att and x1 >= old_att
              y0 = if x1 == old_att
                     y1
                   else
                     y0 + (y1 - y0) * ((old_att - x0) / (x1 - x0))
                   end
              x0 = old_att
              new_x = new_att
              new_fn.push(new_x, y0)
              scl = if old_dec
                      (new_dec - new_att) / (old_dec - old_att)
                    else
                      (last_x - new_att) / (last_x - old_att)
                    end
            end
            if old_dec and x0 < old_dec and x1 >= old_dec
              y0 = if x1 == old_dec
                     y1
                   else
                     y0 + (y1 - y0) * ((old_dec - x0) / (x1 - x0))
                   end
              x0 = old_dec
              new_x = new_dec
              new_fn.push(new_x, y0)
              scl = (last_x - new_dec) / (last_x - old_dec)
            end
            if x0 != x1
              new_x += scl * (x1 - x0)
              new_fn.push(new_x, y1)
              x0, y0 = x1, y1
            end
          end
          new_fn
        end
      end
    end
  end

  def scale_envelope(en, scale = 1.0, offset = 0.0)
    doc("scale_envelope(env, scale, offset = 0.0)
scales y axis values by SCALER and optionally adds OFFSET\n") if en == :help
    1.step(en.length - 1, 2) do |i| en[i] = en[i] * scale + offset end
    en
  end

  def reverse_envelope(en1)
    doc("reverse_envelope(env) reverses the breakpoints in ENV\n") if en1 == :help
    len = en1.length
    if len.zero? or len == 2
      en1
    else
      en2 = en1.dup
      xmax = en1[-2]
      0.step(len - 2, 2) do |i|
        en2[-(i + 2)], en2[-(i + 1)] = xmax - en1[i], en1[i + 1]
      end
      en2
    end
  end
  alias envelope_reverse reverse_envelope

  def concatenate_envelopes(*envs)
    doc("concatenate_envelopes(*envs)
concatenates its arguments into a new envelope\n") if envs.first == :help
    if envs.length == 1
      envs.first
    else
      xoff = 0.0
      ren = []
      envs.each do |en|
        en.map! do |x| x.to_f end unless en.empty?
        firstx = en.first
        0.step(en.length - 1, 2) do |i|
          ren.push(xoff + (en[i] - firstx), en[i + 1])
        end
        xoff = 0.01 + ren[-2]
      end
      ren
    end
  end
  alias envelope_concatenate concatenate_envelopes

  def repeat_envelope(ur_env, repeats = 1, reflected = false, x_normalized = false)
    doc("repeat_envelope(ur_env, repeats, reflected = false, x_normalized = false)
repeats ENV REPEATS times.
repeat_envelope([0, 0, 100, 1] 2) -> [0, 0, 100, 1, 101, 0, 201, 1]
If the final y value is different from the first y value, a quick ramp
is inserted between repeats. X_NORMALIZED causes the new envelope's x
axis to have the same extent as the original's. REFLECTED causes every
other repetition to be in reverse.\n") if ur_env == :help
    tms = (reflected ? (repeats / 2) : repeats)
    en = if reflected
           lastx = ur_env[-2].to_f
           new_env = ur_env.reverse
           rev_env = new_env[2..-1]
           0.step(rev_env.length - 1, 2) do |i|
        new_env.unshift(lastx + (lastx - rev_env[i + 1]))
        new_env.unshift(rev_env[i])
      end
           new_env.reverse
         else
           ur_env
         end
    first_y = en[1].to_f
    x_max = en[-2].to_f
    x = en.first.to_f
    first_y_is_last_y = (first_y == en.last)
    new_env = [first_y, x]
    len = en.length
    tms.times do |i|
      2.step(len - 1, 2) do |j|
        x += en[j] - en[j - 2]
        new_env.unshift(x)
        new_env.unshift(en[j + 1])
      end
      if (i < tms - 1) and (not first_y_is_last_y)
        x += x_max / 100.0
        new_env.unshift(x)
        new_env.unshift(first_y)
      end
    end
    new_env.reverse!
    if x_normalized
      scl = x_max / x
      0.step(new_env.length - 1, 2) do |i| new_env[i] += scl end
    end
    new_env
  end
  alias envelope_repeat repeat_envelope

  # Power envelope
  Power_env = Struct.new("Power_env", :envs, :total_envs, :current_env, :current_pass)

  def make_power_env(*args)
    envelope = get_args(args, :envelope, [0, 0, 100, 1]).map do |x| x.to_f end
    scaler   = get_args(args, :scaler, 1.0)
    offset   = get_args(args, :offset, 0.0)
    dur      = get_args(args, :duration, 0.0)
    len = envelope.length / 3 - 1
    pe = Power_env.new(Array.new(len), len, 0, 0)
    xext = envelope[-3] - envelope.first
    j = 0
    len.times do |i|
      x0 = envelope[j]
      x1 = envelope[j + 3]
      y0 = envelope[j + 1]
      y1 = envelope[j + 4]
      base = envelope[j + 2]
      pe.envs[i] = make_env(:envelope, [0.0, y0, 1.0, y1],
                            :base, base,
                            :scaler, scaler,
                            :offset, offset,
                            :duration, dur * ((x1 - x0) / xext))
      j += 3
    end
    pe.current_pass = mus_length(pe.envs[0])
    pe
  end

  def power_env(pe)
    val = env(pe.envs[pe.current_env])
    pe.current_pass -= 1
    if pe.current_pass.zero? and pe.current_env < (pe.total_envs - 1)
      pe.current_env += 1
      pe.current_pass = mus_length(pe.envs[pe.current_env])
    end
    val
  end

  def power_env_channel(pe, *args)
    beg    = get_args(args, :beg, 0)
    dur    = get_args(args, :dur, false)
    snd    = get_args(args, :snd, false)
    chn    = get_args(args, :chn, false)
    edpos  = get_args(args, :edpos, false)
    edname = get_args(args, :edname, "power_env_channel")
    curbeg = beg
    as_one_edit(lambda do | |
                  pe.envs.each do |en|
                    len = mus_length(en) + 1
                    env_channel(en, curbeg, len, snd, chn, edpos)
                    curbeg += len
                  end
                end)
    edname
  end

  def envelope_exp(en, power = 1.0, xgrid = 100)
    doc("envelope_exp(env, power = 1.0, xgrid = 100)

by Anders Vinjar:

envelope_exp can be used to create exponential segments to include in
envelopes.  Given 2 or more breakpoints, it approximates the curve
between them using 'xgrid linesegments and 'power as the exponent.

ENV is a list of x-y-breakpoint-pairs,
POWER applies to whole envelope,
XGRID is how fine a solution to sample our new envelope with.\n") if en == :help
    en.map! do |x| x.to_f end unless en.empty?
    mn = min_envelope(en)
    largest_diff = max_envelope(en) - mn
    x_min = en.first.to_f
    x_max = en[-2].to_f
    x_incr = (x_max - x_min) / xgrid.to_f
    new_en = []
    x_min.step(x_max, x_incr) do |x|
      y = envelope_interp(x, en)
      new_en.push(x, (largest_diff.zero? ?
                      y :
                         (mn + largest_diff * (((y - mn) / largest_diff) ** power))))
    end
    new_en
  end

  def envelope_length(en)
    en.length / 2
  end

  def normalize_envelope(en, new_max = 1.0)
    scale_envelope(en, new_max / max_envelope(en))
  end
  
  def x_norm(en, xmax)
    scl = xmax / en[-2].to_f
    en.each_pair do |x, y| [x * scl, y.to_f] end.flatten
  end
end

=begin
# power envelope test (clm-2/env.lisp)

def test_power_env(start, dur, en)
  os = make_oscil()
  pe = make_power_env(:envelope, en, :duration, dur, :scaler, 0.5)
  beg, len = times2samples(start, dur)
  (beg..len).each do |i| outa(i, power_env(pe) * oscil(os), $rbm_output) end
end

with_sound(:channels, 1, :play, 1) do
  test_power_env(0, 1, [0, 0, 0.325,   1, 1, 32,   2, 0, 0])
  test_power_env(1.2, 1, [0, 0, 0.325,   1, 1, 32,   2, 0.5, 1,   3, 1, 0.1234,   4, 0, 0])
  test_power_env(2.4, 1, [0, 0, 0,   1, 1, 1,   2, 0.5, 0.123,   3, 1, 321,   4, 0, 0])
end
=end

# env.rb ends here
