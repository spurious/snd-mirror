# ws.rb -- with_sound and friends for Snd/Ruby

# Copyright (C) 2003 Michael Scholz

# Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Apr 08 17:05:03 CEST 2003
# Last: Wed Apr 09 05:14:28 CEST 2003
# Version: $Revision: 1.2 $

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.

# This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA

# Functions:

# with_sound(*args) { |start| ... }
# sound_let(fname, *args) { |tmp_fname| ... }
# fm_play(*args) { |len| ... }

# Code:

require "examp"

$rbm_output = false
$rbm_file_name = "test.snd"
$rbm_srate = 22050
$rbm_channels = 1
$rbm_header_type = Mus_next
$rbm_data_format = Mus_bshort
$rbm_comment = "created by #{ENV["USER"]}"
$rbm_statistics = false
$rbm_play = 0
$rbm_player = "sndplay"
$rbm_reverb = false
$rbm_reverb_file_name = "reverb.snd"
$rbm_reverb_channels = 1
$rbm_reverb_func = false
$rbm_locsig_type = Mus_sinusoidal

def with_sound(*args)
  doc("with_sound(*args) { |start| ... }
	:output,            $rbm_file_name (#$rbm_file_name)
	:continue_old_file, false
	:channels,          $rbm_channels (#$rbm_channels)
	:statistics,        $rbm_statistics (#$rbm_statistics)
	:play,              $rbm_play (#$rbm_play)
	:player,            $rbm_player (#$rbm_player)
	:srate,             $rbm_srate (#$rbm_srate)
	:header_type,       $rbm_header_type (#$rbm_header_type)
	:data_format,       $rbm_data_format (#$rbm_data_format)
	:comment,           $rbm_comment (#$rbm_comment)
	:reverb,            $rbm_reverb_func (#$rbm_reverb_func)
	:revfile,           $rbm_reverb_file_name (#$rbm_reverb_file_name)
        :decay_time,        1.0
	:reverb_channels,   $rbm_reverb_channels (#$rbm_reverb_channels)
	:reverb_data,       []
	:scaled_to,         false
	:scaled_by,         false
        :snd_mix,           false

Usage: with_sound(:play, 1, :statistics, true) { fm_violin }\n") if get_args(args, :help, false)
  output            = get_args(args, :output, $rbm_file_name)
  continue_old_file = get_args(args, :continue_old_file, false)
  channels          = get_args(args, :channels, $rbm_channels)
  statistics        = get_args(args, :statistics, $rbm_statistics)
  play              = get_args(args, :play, $rbm_play)
  player            = get_args(args, :player, $rbm_player)
  srate             = get_args(args, :srate, $rbm_srate)
  header_type       = get_args(args, :header_type, $rbm_header_type)
  data_format       = get_args(args, :data_format, $rbm_data_format)
  comment           = get_args(args, :comment, $rbm_comment)
  reverb            = get_args(args, :reverb, $rbm_reverb_func)
  revfile           = get_args(args, :revfile, $rbm_reverb_file_name)
  decay_time        = get_args(args, :decay_time, 1.0)
  reverb_channels   = get_args(args, :reverb_channels, $rbm_reverb_channels)
  reverb_data       = get_args(args, :reverb_data, [])
  scaled_to         = get_args(args, :scaled_to, false)
  scaled_by         = get_args(args, :scaled_by, false)
  snd_mix           = get_args(args, :snd_mix, false)
  $rbm_file_name = output
  $rbm_srate = srate
  $rbm_channels = channels
  $rbm_reverb_channels = reverb_channels
  play = case play
         when true
           1
         when false, nil
           0
         else
           play.abs
         end
  play, statistics = 0, false if snd_mix or continue_old_file
  if $IN_SND and (snd = find_sound(output))
    close_sound(snd)
  end
  unless continue_old_file
    old_srate = mus_srate
    set_mus_srate(srate)
    $rbm_output = $rbm_reverb = false
    File.unlink(output) if File.exist?(output)
    $rbm_output = make_sample2file(output, channels, data_format, header_type, comment)
    if reverb
      File.unlink(revfile) if File.exist?(revfile)
      $rbm_reverb = make_sample2file(revfile, reverb_channels, data_format, header_type, "rev")
    end
  else
    $rbm_output = continue_sample2file(output)
    if $rbm_reverb
      $rbm_reverb = continue_sample2file(revfile)
    else
      File.unlink(revfile) if File.exist?(revfile)
      $rbm_reverb = make_sample2file(revfile, reverb_channels, data_format, header_type, "rev")
    end
  end
  startime = mus_sound_duration(output)
  atime = Time.now if statistics
  yield(startime)
  if reverb
    mus_close($rbm_reverb)
    $rbm_reverb = make_file2sample(revfile)
    dur = decay_time + mus_sound_duration(output) - startime
    if reverb.kind_of?(Proc)
      reverb.call(startime, dur, *reverb_data)
    else
      send(reverb, startime, dur, *reverb_data)
    end
  end
  unless continue_old_file
    if reverb
      mus_close($rbm_reverb)
      $rbm_reverb = false
    end
    mus_close($rbm_output)
    $rbm_output = false
    set_mus_srate(old_srate)
  end
  if $IN_SND and snd_mix and (snd = find_sound(output))
    close_sound(snd)
  end
  if $IN_SND and (not snd_mix)
    snd = open_sound(output)
    olds = sync(snd)
    set_sync(true, snd)
    scale_to(scaled_to, snd) if scaled_to
    scale_by(scaled_by, snd) if scaled_by
    save_sound(snd) if scaled_to or scaled_by
    set_sync(olds, snd)
  end
  if statistics
    rtime = Time.now - atime
    samps = mus_sound_samples(output)
    max_amp = mus_sound_maxamp(output)
    srate = $rbm_srate.to_f
    message("    Sound File: %s", output)
    message("      Duration: %.4f", (samps / srate / channels))
    message("  Compute time: %.3f, Compute ratio: %.2f", rtime,
	    rtime * (srate / samps) * channels)
    out_chan = 64.chr
    0.step(2 * channels - 1, 2) do |i|
      message("  Out%s max amp: %.3f (near %.3f secs)",
              out_chan.next!, max_amp[i + 1], max_amp[i] / srate)
    end
    if reverb
      max_amp = mus_sound_maxamp(revfile)
      out_chan = 64.chr
      0.step(2 * reverb_channels - 1, 2) do |i|
        message("  Rev%s max amp: %.3f (near %.3f secs)",
                out_chan.next!, max_amp[i + 1], max_amp[i] / srate)
      end
    end
  end
  1.upto(play) do |i| ($IN_SND ? play_and_wait(snd) : system("#{player} #{output}")) end
  output
rescue
  die get_func_name
end

=begin
# Examples:

with_sound { fm_violin }

with_sound(:channels, 2,
	   :play, 3,
	   :statistics, true,
	   :reverb_channels, 2,
	   :reverb, :jc_reverb,	# or :reverb, "jc_reverb",
	   :reverb_data, [:volume, 0.3],
           :decay_time, 0.8,
	   :reverb_channels, 1) { 
  0.upto(3) { |i| fm_violin(i, 1, 220 * (i + 1), 0.3, :distance, i * 0.4) }
}

with_sound(:play, 1,
	   :channels, 2,
	   :scaled_to, 0.3,
	   :reverb, :jc_reverb,
	   :statistics, true) { 
  0.upto(20) { |i| 
    metalamp = [0, 0, 0.5, 1, 5, 1, 10, 0.5, 15, 0.25, 35, 0.1, 100, 0]

    fm_violin(i * 0.1, 1, 220 + i * 10, 0.1, 
	      :fm_index, i * 0.5, :distance, i * 0.05, :amp_env, metalamp)

    fm_violin(i * 0.1, 1, 2200 - i * 10, 0.1, 
	      :fm_index, i * 0.5, :distance, i * -0.05, :amp_env, metalamp)
  }
}

with_sound(:play, 1) { 
  fm_violin(0, 1, 440)
  with_sound(:continue_old_file, true, :play, 0) {
    fm_violin(1, 1, 220)
  }
  with_sound(:continue_old_file, true, :play, 0) {
    fm_violin(2, 1, 880)
    with_sound(:continue_old_file, true, :play, 0) {
      fm_violin(3, 1, 660)
    }
  }
  fm_violin(4, 1, 440)
}

=end

def sound_let(fname = nil, *args)
  doc("sound_let(fname = nil, *args) { |tmp_fname| ... }
mimics more or less CLM's and Snd/Guile's (sound-let)

FNAME means a temporary sound filename; if not given snd_tempnam()
creates a unique one.  *ARGS means with_sound()-args.  FNAME will be
deleted afterwards and the result will be mixed in
with_sound()-output.  Returns the used temporary filename.

Usage: with_sound() {
  sound_let() { |tmp_file|
    fm_violin(0, 2, 220, 0.5)
  }
}\n") if fname == :help
  tfname = (fname or snd_tempnam())
  args += [:output, tfname]
  old_file_name = $rbm_file_name
  old_output = $rbm_output
  old_channels = $rbm_channels
  old_reverb = $rbm_reverb
  old_reverb_channels = $rbm_reverb_channels
  result = with_sound(:snd_mix, true, *args) do |start| yield(tfname) end
  mus_mix(old_file_name, $rbm_file_name, mus_sound_frames(old_file_name))
  $rbm_reverb_channels = old_reverb_channels
  $rbm_reverb = old_reverb
  $rbm_channels = old_channels
  $rbm_output = old_output
  $rbm_file_name = old_file_name
  File.unlink(tfname) if File.exists?(tfname)
  result
rescue
  die get_func_name
end

def fm_play(*args)
  doc("fm_play(*args) { |len| ... }
  VCT options:
	:start,             false
	:dur,               false
	:degree,            kernel_rand(90.0)
	:reverb,            false [true means n_rev]
        :reverb_data,       []    [:amount, 0.1, :filter, 0.5, :feedback, 1.09]
	:distance,          1.0
	:scaled_to,         false
	:scaled_by,         false

  PLAYING options:
	:play,              $rbm_play (#$rbm_play)
	:statistics,        $rbm_statistics (#$rbm_statistics)

  If the above two options are set, options below will be used for the
  new file:
	:output,            false
	:save_after,        false

  options for new sound file:
	:channels,          $rbm_channels (#$rbm_channels)
	:srate,             $rbm_srate (#$rbm_srate)
	:header_type,       $rbm_header_type (#$rbm_header_type)
	:data_format,       $rbm_data_format (#$rbm_data_format)
	:comment,           \"created by fm_play()\"

If :start and :dur are given, the block may return an out_data vct
which will be set to the channel(s), e.g.:

fm_play(:start, 0, :dur, 9.8,
        :channels, 4, :output, \"noise.snd\") { |len|
  vct_map!(make_vct(len), make_fm_noise(len, 500))
}

If :output is false, the default, the current sound is used, otherwise
a new sound will be opened with values of :channels, :srate, etc.

fm_play(:output, \"bell.snd\") {
  fbell = [0, 1, 2, 1.1000, 25, 0.7500, 75, 0.5000, 100, 0.2000]
  abell = [0, 0, 0.1000, 1, 10, 0.6000, 25, 0.3000, 50, 0.1500, 90, 0.1000, 100, 0]
  fm_bell_snd(0.0, 1.0, 220.0, 0.5, abell, fbell, 1.0)
}\n") if get_args(args, :help, false)
  start       = get_args(args, :start, false)
  dur         = get_args(args, :dur, false)
  degree      = get_args(args, :degree, kernel_rand(90.0))
  reverb      = get_args(args, :reverb, false)
  reverb_data = get_args(args, :reverb_data, [])
  distance    = get_args(args, :distance, 1.0)
  scaled_to   = get_args(args, :scaled_to, false)
  scaled_by   = get_args(args, :scaled_by, false)
  play        = get_args(args, :play, $rbm_play)
  statistics  = get_args(args, :statistics, $rbm_statistics)
  output      = get_args(args, :output, false)
  save_after  = get_args(args, :save_after, false)
  chns        = get_args(args, :channels, $rbm_channels)
  srate       = get_args(args, :srate, $rbm_srate)
  header_type = get_args(args, :header_type, $rbm_header_type)
  data_format = get_args(args, :data_format, $rbm_data_format)
  comment     = get_args(args, :comment, "created by " + get_func_name + "()")
  play = case play
         when true
           1
         when false, nil
           0
         else
           play.abs
         end
  if output
    if snd = find_sound(output)
      close_sound(snd)
    end
    snd = new_sound(output, header_type, data_format, srate, chns, comment)
  else
    snd = false
  end
  chns = (channels(snd) rescue $rbm_channels) unless chns
  if start and dur
    beg = (start * srate(snd)).round
    len = (dur * srate(snd)).round
    loc = make_locsig(:degree, degree, :distance, distance, :channels, chns,
                      :type, $rbm_locsig_type)
  end
  atime = Time.now if statistics
  data = yield((len or 0))
  if data and loc
    chns.times do |i|
      mix_vct(vct_scale!(vct_copy(data), locsig_ref(loc, i)), beg, snd, i, false)
    end
  end
  chns.times do |i| scale_to(scaled_to, snd, i) end if scaled_to
  chns.times do |i| scale_by(scaled_by, snd, i) end if scaled_by
  n_rev(*reverb_data) if reverb
  save_sound(snd) if save_after
  if statistics
    rtime = Time.now - atime
    samps = frames(snd)
    srate = srate(snd).to_f
    message("    Sound File: %s", output)
    message("      Duration: %.4f", samps / srate)
    message("  Compute time: %.3f, Compute ratio: %.2f", rtime, rtime * srate / samps)
    out_chan = 64.chr
    chns.times do |i| message("  Out%s max amp: %.3f", out_chan.next!, maxamp(snd, i)) end  # .
    message("        Reverb: n_rev(%s)", reverb_data.inspect) if reverb
  end
  1.upto(play) do |i| play(0, snd) end
  output
rescue
  die get_func_name
end

# ws.rb ends here
