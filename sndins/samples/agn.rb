#!/usr/bin/env ruby
# -*- snd-ruby -*-
# agn.rb -- Bill Schottstaedt's agn.cl (see clm-2/clm-example.clm and clm-2/bess5.cl)

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Sat May 24 20:35:03 CEST 2003
# Last: Mon May 10 19:10:32 CEST 2004

# This file is part of Sndins.

# Type do_agn
# or start the script in a shell.

require "examp"
require "ws"
require "env"
require "sndins"

$rbm_play = 1
$rbm_statistics = true
$rbm_verbose = true
$rbm_srate = 44100
$rbm_channels = 2
$rbm_reverb_func = :freeverb
$rbm_reverb_data = [:room_decay, 0.9]
$rbm_reverb_channels = 2
$rbm_delete_reverb = true
$rbm_locsig_type = Mus_interp_sinusoidal
# Mus_interp_linear or Mus_interp_sinusoidal

class Agn
  include Math
  include Env
  
  def initialize
    mode = [0, 0, 2, 4, 11, 11, 5, 6, 7, 0, 0, 0, 0]
    @lim = 256
    @time = 60
    @octs = make_array(@lim + 1) do |i| (4 + 2 * rbell(rbm_random(1.0))).floor end
    @pits = make_array(@lim + 1) do |i| mode[(12 * rbm_random(1.0)).floor] end
    @rhys = make_array(@lim + 1) do |i| (4 + 6 * rbm_random(1.0)).floor end
    @amps = make_array(@lim + 1) do |i| (1 + 8 * rbell(rbm_random(1.0))).floor end
  end
  
  def tune(x)
    [1.0, 256.0 / 243, 9.0 / 8, 32.0 / 27, 81.0 / 64,
     4.0 / 3, 1024.0 / 729, 3.0 / 2, 128.0 / 81, 27.0 / 16,
     16.0 / 9, 243.0 / 128, 2.0].at(x % 12) * 2 ** x.divmod(12).first
  end

  def rbell(x)
    envelope_interp(x * 100, [0, 0, 10, 0.25, 90, 1.0, 100, 1.0])
  end

  def agn(file)
    File.open(file, "w") do |f|
      f << "# from agn.cl (see clm-2/clm-example.clm and clm-2/bess5.cl)\n"
      f << "# " << make_default_comment() << "\n\n"
      wins = [[0, 0, 40, 0.1, 60, 0.2, 75, 0.4, 82, 1, 90, 1, 100, 0],
              [0, 0, 60, 0.1, 80, 0.2, 90, 0.4, 95, 1, 100, 0],
              [0, 0, 10, 1, 16, 0, 32, 0.1, 50, 1, 56, 0, 60, 0, 90, 0.3, 100, 0],
              [0, 0, 30, 1, 56, 0, 60, 0, 90, 0.3, 100, 0],
              [0, 0, 50, 1, 80, 0.3, 100, 0],
              [0, 0, 40, 0.1, 60, 0.2, 75, 0.4, 82, 1, 90, 1, 100, 0],
              [0, 0, 40, 0.1, 60, 0.2, 75, 0.4, 82, 1, 90, 1, 100, 0],
              [0, 0, 10, 1, 32, 0.1, 50, 1, 90, 0.3, 100, 0],
              [0, 0, 60, 0.1, 80, 0.3, 95, 1, 100, 0],
              [0, 0, 80, 0.1, 90, 1, 100, 0]]
      (1..3).each do |i|
        cellbeg, cellsiz, cellctr = 0, 4, 0
        whichway, base, mi, winnum, mytempo = 1, i, i - 1, 0, 0.2
        nextbeg = revamt = ranamt = beg = dur = freq = ampl = ind = 0.0
        while beg < @time and cellctr < @lim
          beg += nextbeg
          nextbeg = dur = [0.25, mytempo * (0.9 + 0.2 * rbm_random(1.0)) * @rhys[cellctr]].max
          freq = (16.352 / 2 ** mi) * tune(@pits[cellctr]) * 2 ** @octs[cellctr]
          dur += dur if freq < 100
          ampl = [0.003, @amps[cellctr] * (1.0 / (60 * base))].max
          ind = rbm_random(1.0) * 2 * base
          revamt = base * 0.1
          winnum = (10 * (beg - beg.floor)).floor
          ranamt = 0.00001 * (logn(freq, 2.0) - 4) ** 4
          f << format("fm_violin(%.2f, %.2f, %.3f, %.2f, " +
                      ":fm_index, %.2f, :reverb_amount, %.2f, :noise_amount, %.2f, " +
                      ":amp_env, %s)\n",
                      beg, dur, freq, ampl, ind, revamt, ranamt, wins[winnum].inspect)
          cellctr += 1
          if cellctr > (cellsiz + cellbeg)
            cellbeg += 1
            if rbm_random(1.0) > 0.5
              cellsiz += whichway
            end
            if cellsiz > 16 and rbm_random(1.0) > 0.99
              whichway = -2
              if cellsiz > 12 and rbm_random(1.0) > 0.999
                whichway = -1
                if cellsiz < 4
                  whichway = 1
                end
              end
            end
            cellbeg += 3
            cellctr = cellbeg
          end
        end
      end
    end
    file
  end
end

def do_agn(file = "agn.rbm")
  sndfile = File.basename(file, ".*") + ".snd"
  message("Writing %s", file.inspect)
  Agn.new.agn(file)
  rbm_load(file, :output, sndfile)
end

do_agn((ARGV[0] or "agn.rbm")) unless provided? "snd"

# agn.rb ends here
