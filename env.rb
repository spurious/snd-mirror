# env.rb -- snd-6/env.scm

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Sun Sep 21 01:28:36 CEST 2003

# Commentary:
#
# Module Env (see env.scm)
#  max_envelope(en)
#  min_envelope(en)
#  envelope_interp(*args)
#  stretch_envelope(*args)

# Code:

RBM_ENV_VERSION = "21-Sep-2003 (RCS 1.2)"

require "examp"

module Env
  def max_envelope(en)
    mx = en[1].to_f
    1.step(en.length - 1, 2) do |i| mx = [mx, en[i]].max.to_f end
    mx
  end
  
  def min_envelope(en)
    mn = en[1].to_f
    1.step(en.length - 1, 2) do |i| mn = [mn, en[i]].min.to_f end
    mn
  end

  def envelope_interp(*args)
    doc("envelope_interp(*args)
envelope_interp(x, env, base = 1.0) -> value of env at x;
base controls connecting segment type:
envelope_interp(0.3, [0, 0, 0.5, 1, 1, 0]) -> 0.6\n") if args.first == :help
    x = args[0]
    env = args[1]
    base = args[2]
    if (not env) or env.empty?
      0.0
    elsif x <= env[0] or env[2..-1].empty?
      env[1]
    elsif env[2] > x
      if env[1] == env[3] or (base and base == 0.0)
        env[1]
      elsif (not base) or base == 1.0
        env[1] + (x - env[0]) * ((env[3] - env[1]) / (env[2] - env[0]))
      else
        env[1] + ((env[3] - env[1]) / (base - 1.0)) *
                                     ((base ** ((x - env[0]) / (env[2] - env[0]))) - 1.0)
      end
    else
      envelope_interp(x, env[2..-1])
    end
  end unless defined?(envelope_interp)

  def stretch_envelope(*args)
    doc("stretch_envelope(*args)
args: env, old_attack, new_attack, old_decay, new_decay
Takes ENV and returns a new envelope based on it but with the attack
and optionally decay portions stretched or squeezed; OLD_ATTACK is the
original x axis attack end point, NEW_ATTACK is where that section
should end in the new envelope.  Similarly for OLD_DECAY and
NEW_DECAY.  This mimics divseg in early versions of CLM and its
antecedents in Sambox and Mus10 (linen).
stretch_envelope([0, 0, 1, 1], 0.1, 0.2)
                 -> [0, 0, 0.2, 0.1, 1.0, 1]
stretch_envelope([0, 0, 1, 1, 2, 0], 0.1, 0.2, 1.5, 1.6)
                 -> [0, 0, 0.2, 0.1, 1.1, 1, 1.6, 0.5, 2.0, 0]\n") if args[0] == :help
    fn = args[0].map do |x| x.to_f end
    old_att = args[1] ? args[1].to_f : false
    new_att = args[2] ? args[2].to_f : false
    old_dec = args[3] ? args[3].to_f : false
    new_dec = args[4] ? args[4].to_f : false
    if old_att and (not new_att)
      warn "wrong number of arguments"
    elsif not new_att
      fn
    elsif old_dec and (not new_dec)
      warn "wrong number of arguments"
    else
      new_x = x0 = fn[0]
      last_x = fn[-2]
      y0 = fn[1]
      new_fn = [y0, x0]
      scl = (new_att - x0) / [0.0001, old_att - x0].max
      stretch_envelope_1 = lambda do |new_fn, old_fn|
        if old_fn.empty?
          new_fn
        else
          x1 = old_fn[0]
          y1 = old_fn[1]
          if x0 < old_att and x1 >= old_att
            if x1 == old_att
              y0 = y1
            else
              y0 += (y1 - y0) * ((old_att - x0) / (x1 - x0))
            end
            x0 = old_att
            new_x = new_att
            new_fn.unshift(new_x).unshift(y0)
            scl = if old_dec
                    (new_dec - new_att) / (old_dec - old_att)
                  else
                    (last_x - new_att) / (last_x - old_att)
                  end
          end
          if old_dec and x0 < old_dec and x1 >= old_dec
            if x1 == old_dec
              y0 = y1
            else
              y0 += (y1 - y0) * ((old_dec - x0) / (x1 - x0))
            end
            x0 = old_dec
            new_x = new_dec
            new_fn.unshift(new_x).unshift(y0)
            scl = (last_x - new_dec) / (last_x - old_dec)
          end
          unless x0 == x1
            new_x += scl * (x1 - x0)
            new_fn.unshift(new_x).unshift(y1)
            x0, y0 = x1, y1
          end
          stretch_envelope_1.call(new_fn, old_fn[2..-1])
        end
      end
      if old_dec and old_dec == old_att
        old_dec = 0.000001 * last_x
      end
      stretch_envelope_1.call(new_fn, fn[2..-1]).reverse
    end
  end unless defined?(stretch_envelope)
end

# env.rb ends here
