# ws.rb -- with_sound and friends for Snd/Ruby

# Copyright (C) 2003 Michael Scholz

# Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Apr 08 17:05:03 CEST 2003
# Last: Tue Apr 22 05:01:00 CEST 2003
# Version: $Revision: 1.12 $

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
#
# module WS
#
# user methods
#  with_sound(*args) { |start| ... }
#  rbm_load(rbm_file_name, *args)
#  sound_let(fname, *args) { |tmp_fname| ... }
#  with_current_sound(*args) { ... }
#  scaled_to(scale) { ... }
#  scaled_by(scale) { ... }
#  with_offset(secs) { ... }
#  with_mix(*args)
#  rbm_mix(filename, *args)
#  with_snd(*args) { |len| ... } alias fm_play
#
# help methods
#  ws_error(*args)
#  play_sound(output, play)
#  statistics(output, beg, reverb, revfile)
#  make_reverb_file_name(snd_name)
#  make_default_comment()
#  sample2filename(fgen)
#  remove_file(file)
#  tempnam()

# Usage:
#
# Global variables can be set in ~/.snd-ruby.rb or in other scripts
# before loading ws.rb.
#
# with_sound(:play, 3, :statistics, true, :reverb, :jc_reverb) do
#   fm_violin(0, 1, 440, 0.3)
# end
#
# rbm_load("test.rbm", :play, 1, :statistics, true, :verbose, true)
#
# scaled_to(scale, &body), scaled_by(scale, &body), with_offset(secs,
# &body), with_current_sound(*args, &body), sound_let(fname, *args,
# &body), and with_mix(*args) are callable within with_sound() and
# rbm_load().  with_mix() doesn't use a block but a string as "body":
#
# with_sound() do
#   fm_violin(0, 0.1, 440, 0.1)
#   with_mix "sec1", 0.5, %Q{
#     fm_violin(0, 0.1, 550, 0.1)
#     fm_violin(0.1, 0.1, 660, 0.1)
#   }
#   with_mix :reverb, :jc_reverb, "sec2", 1.0, %Q{
#     fm_violin(0, 0.1, 880, 0.1, :reverb_amount, 0.2)
#     fm_violin(0.1, 0.1, 1320, 0.1, :reverb_amount, 0.2)
#   }
#   fm_violin(2, 0.1, 220, 0.1)
# end
#
# sound_let(): sound_let(fname = tempnam(), *args) do |temp_file_name|
#                  ...
#              end
#           or sound_let([[fname1 = tempnam(), *args, lambda do ... end],
#                         [fname2 = tempnam(), *args, lambda do ... end],
#                         ...]) do |temp_file_array|
#                  ...
#              end
#
# sound_let([["tmp", lambda do fm_violin(0, 1, 440, 0.1) end],
#            ["tmp1", :reverb, :nrev, lambda do rbm_mix("oboe.snd") end]]) do
#   rbm_mix("tmp1")
#   rbm_mix("tmp", :scale, 0.2, :output_frame, seconds2samples(1))
# end
#
# rbm_mix(filename, *args) is similar CLM's mix but mus_mix() may also
# work perfectly.

# CLM examples (see clm.html) and their Snd/Ruby counterparts:
=begin
;; CLM examples
 (with-sound () 
   (mix (with-sound (:output "hiho.snd") 
             (fm-violin 0 1 440 .1))
           :amplitude .5))

 (with-sound ()
   (with-mix () "s1" 0
     (sound-let ((tmp ()
                   (fm-violin 0 1 440 .1)))
       (mix tmp))))

 (with-sound (:verbose t)
   (with-mix () "s6" 0
     (sound-let ((tmp ()
                   (fm-violin 0 1 440 .1))
                 (tmp1 (:reverb nrev)
                   (mix "oboe.snd")))
       (mix tmp1)
       (mix tmp :amplitude .2 :output-frame *srate*))
     (fm-violin .5 .1 330 .1)))

 (with-sound (:verbose t)
   (sound-let ((tmp ()
                 (with-mix () "s7" 0
                   (sound-let ((tmp ()
                                 (fm-violin 0 1 440 .1))
                               (tmp1 ()
                                 (mix "oboe.snd")))
                    (mix tmp1)
                    (mix tmp :output-frame *srate*))
                  (fm-violin .5 .1 330 .1))))
     (mix tmp :amplitude .5)))

# corresponding Snd/Ruby examples
with_sound() do
  rbm_mix(with_sound(:output, "hiho.snd") do
            fm_violin(0, 1, 440, 0.1)
          end, :scale, 0.5)
end

with_sound() do
  with_mix "s1", %Q{
  sound_let([["tmp", lambda do fm_violin(0, 1, 440, 0.1) end]]) do
    rbm_mix("tmp")
  end
  }
end

with_sound(:verbose, true) do
  with_mix "s6", %Q{
  sound_let([["tmp", lambda do fm_violin(0, 1, 440, 0.1) end],
             ["tmp1", :reverb, :nrev, lambda do rbm_mix("oboe.snd") end]]) do
    rbm_mix("tmp1")
    rbm_mix("tmp", :scale, 0.2, :output_frame, seconds2samples(1))
  end
  fm_violin(0.5, 0.1, 330, 0.1)
  }
end

with_sound(:verbose, true) do
  sound_let([["tmp0", lambda do
                  with_mix "s7", 0, %Q{
                  sound_let([["tmp", lambda do fm_violin(0, 1, 440, 0.1) end],
                             ["tmp1", lambda do rbm_mix("oboe.snd") end]]) do
                    rbm_mix("tmp1")
                    rbm_mix("tmp", :output_frame, $rbm_srate)
                  end
                  fm_violin(0.5, 0.1, 330, 0.1)
                }
                end]]) do
    rbm_mix("tmp0", :scale, 0.5)
  end
end
=end

# Code:

$IN_SND = true unless defined? $IN_SND
$HAVE_SNDLIB_SO = false unless defined? $HAVE_SNDLIB_SO

require "sndlib" unless $HAVE_SNDLIB_SO
require "examp"
require "etc"
require "socket"

$rbm_version = "22-Apr-03"
$rbm_output = nil
$rbm_reverb = nil
$rbm_file_name = "test.snd"         unless defined? $rbm_file_name
$rbm_srate = 22050                  unless defined? $rbm_srate
$rbm_channels = 1                   unless defined? $rbm_channels
$rbm_header_type = Mus_next         unless defined? $rbm_header_type
$rbm_data_format = Mus_bshort       unless defined? $rbm_data_format
$rbm_comment = nil                  unless defined? $rbm_comment
$rbm_statistics = false             unless defined? $rbm_statistics
$rbm_play = 0                       unless defined? $rbm_play
$rbm_player = "sndplay"             unless defined? $rbm_player
$rbm_reverb_file_name = nil         unless defined? $rbm_reverb_file_name
$rbm_reverb_channels = 1            unless defined? $rbm_reverb_channels
$rbm_reverb_func = nil              unless defined? $rbm_reverb_func
$rbm_locsig_type = Mus_sinusoidal   unless defined? $rbm_locsig_type
$rbm_delete_reverb = false          unless defined? $rbm_delete_reverb
$rbm_verbose = ($VERBOSE or $DEBUG) unless defined? $rbm_verbose

module WS
  @@file_nr = 0

  class WSError < StandardError
  end

  #
  # user methods
  #
  
  def with_sound(*args, &body)
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
        :verbose,           $rbm_verbose (#$rbm_verbose)

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
    verbose           = get_args(args, :verbose, $rbm_verbose)
    play = case play
           when true
             1
           when false, nil
             0
           else
             play.abs
           end
    play, statistics = 0, false if continue_old_file
    reverb = $rbm_reverb = false if $rbm_reverb_channels.zero?
    comment = make_default_comment() unless comment
    revfile = make_reverb_file_name(output) unless revfile
    old_file_name = $rbm_file_name
    old_channels = $rbm_channels
    old_reverb_channels = $rbm_reverb_channels
    old_srate = mus_srate()
    old_output = $rbm_output
    old_reverb = $rbm_reverb
    $rbm_reverb_channels = reverb_channels
    $rbm_channels = channels
    $rbm_file_name = output
    $rbm_srate = srate
    set_mus_srate(srate)
    if $IN_SND and (snd = find_sound(output))
      close_sound(snd)
    end
    if continue_old_file
      $rbm_output = continue_sample2file(output)
      $rbm_reverb = continue_sample2file(revfile) if reverb
    else
      $rbm_output = $rbm_reverb = false
      remove_file(output)
      $rbm_output = make_sample2file(output, channels, data_format, header_type, comment)
      if reverb
        remove_file(revfile)
        $rbm_reverb = make_sample2file(revfile, reverb_channels, data_format, header_type,
                                       "temporary reverb file")
      end
    end
    mus_close($rbm_output) if $rbm_output
    startime = mus_sound_duration(output)
    $rbm_output = continue_sample2file(output)
    beg = Time.now
    body.call(startime)
  rescue
    ws_error("error in %s", get_func_name())
  else
    mus_close($rbm_output) if $rbm_output
    dur = mus_sound_duration(output) - startime
    if scaled_to
      if $IN_SND
        snd = open_sound(output)
        scale_sound_to(scaled_to)
        save_sound(snd)
        close_sound(snd)
      else
        amax = mus_sound_maxamp(output)
        tmpa = []
        1.step(amax.length - 1, 2) do |i| tmpa << amax[i] end
        scaled_to = 1.0 if scaled_to > 1
        scaled_to = 0.0 if scaled_to < 0
        scale = (scaled_to / tmpa.max) - 1
        mus_mix(output, output, seconds2samples(startime),
                seconds2samples(dur),
                seconds2samples(startime),
                make_mixer(channels, *(1..channels * channels).map do scale end))
      end
    end
    if scaled_by
      if $IN_SND
        snd = open_sound(output)
        scale_sound_by(scaled_by)
        save_sound(snd)
        close_sound(snd)
      else
        scale = scaled_by - 1
        mus_mix(output, output, seconds2samples(startime),
                seconds2samples(dur),
                seconds2samples(startime),
                make_mixer(channels, *(1..channels * channels).map do scale end))
      end
    end
    $rbm_output = continue_sample2file(output)
    if reverb
      begin
        mus_close($rbm_reverb)
        $rbm_reverb = make_file2sample(revfile)
        if reverb.kind_of?(Proc)
          reverb.call(startime, dur + decay_time, :verbose, verbose, *reverb_data)
        else
          send(reverb, startime, dur + decay_time, :verbose, verbose, *reverb_data)
        end
        mus_close($rbm_reverb)
      rescue
        ws_error("%s: reverb error", get_func_name())
      end
    end
    if $IN_SND and (snd = find_sound(output))
      save_sound(snd)
      close_sound(snd)
    end
    statistics(output, beg, reverb, revfile) if statistics
    play_sound(output, play)
    output
  ensure
    mus_close($rbm_reverb) if $rbm_reverb
    reverb and $rbm_delete_reverb and (not continue_old_file) and remove_file(revfile)
    mus_close($rbm_output) if $rbm_output
    $rbm_reverb = old_reverb
    $rbm_output = old_output
    set_mus_srate(old_srate)
    $rbm_file_name = old_file_name
    $rbm_srate = old_srate
    $rbm_channels = old_channels
    $rbm_reverb_channels = old_reverb_channels
  end

  def rbm_load(rbm_file_name, *args)
    doc("rbm_load(rbm_file_name, *args)
The file RBM_FILE_NAME may contain instrument calls and other
with_sound()-body calls, ARGS are with_sound()-args.\n") if rbm_file_name == :help
    ws_error("%s: file %s doesn't exist!",
             get_func_name(), rbm_file_name) unless File.exist?(rbm_file_name)
    message("Loading %s", rbm_file_name.inspect) if get_args(args, :verbose, $rbm_verbose)
    with_sound(*args) do load(rbm_file_name) end
  end

  def sound_let(fname = tempnam(), *args, &body)
    doc("sound_let(fname = tempnam(), *args) { |temp_file| ... }
of
sound_let([[fname1, *args, lambda { |fname1| ... }],
           [fname2, *args, lambda { |fname2| ... }], ...]) { |temp_file_array| ... }
") if fname == :help
    fname_ary = []
    if case fname
       when String
         fname_ary << fname
       when Symbol
         args.unshift(fname)
         fname = tempnam()
         fname_ary << fname
       else
         false
       end
      with_sound(:output, fname, :statistics, false, :play, 0, *args) do body.call(fname) end
    elsif fname.kind_of?(Array)
      fname.each do |args|
        tfname = if args[0].kind_of?(String)
                   args.shift
                 else
                   tempnam()
                 end
        tbody = if args[-1].kind_of?(Proc)
                  args.pop
                else
                  ws_error("%s: procedure needed", get_func_name())
                end
        fname_ary << tfname
        with_sound(:output, tfname, :statistics, false, :play, 0, *args) do tbody.call(tfname) end
      end
      body.call(fname_ary)
    else
      ws_error("
Usage: sound_let(fname, *args, &body)
       or
       sound_let([[fname1, *args, lambda { ... }],
                  [fname2, *args, lambda { ... }], ...], &body)")
    end
    fname_ary.each do |f| remove_file(f) end
  end
  
  def with_current_sound(*args, &body)
    ws_error("%s: not in with_sound()", get_func_name()) unless $rbm_output
    output    = get_args(args, :output, tempnam())
    scaled_to = get_args(args, :scaled_to, false)
    scaled_by = get_args(args, :scaled_by, false)
    comment   = get_args(args, :comment, "temporary sound file")
    offset    = get_args(args, :offset, 0)
    current_output = sample2filename($rbm_output)
    with_sound(:output, output,
               :comment, comment,
               :scaled_to, scaled_to,
               :scaled_by, scaled_by,
               :offset, offset,
               :statistics, false,
               :play, 0, &body)
    mus_mix(current_output, output, seconds2samples(offset))
    remove_file(output)
    current_output
  end

  def scaled_to(scale, &body)
    with_current_sound(:scaled_to, scale, &body)
  end

  def scaled_by(scale, &body)
    with_current_sound(:scaled_by, scale, &body)
  end
  
  def with_offset(secs, &body)
    with_current_sound(:offset, secs, &body)
  end

  # with_mix(*args)
  #          *args == *with_sound_args, fname, beg, body_str)
  def with_mix(*args)
    doc("with_mix(*args)
ARGS must be at least a filename and the body_string (no body block
used here).  It can contain several with_sound()-args, a filename, the
beg-time and the body_string.
with_mix :reverb, :nrev, \"foo\", 2.2, %Q{
  fm_violin(0, 2, 440, 0.2)
  ...
}\n") if args[0] == :help
    body_str = args.pop
    beg = if args[-1].kind_of?(Numeric)
            args.pop
          else
            0
          end
    fname = args.pop
    output = fname + ".snd"
    rbm_file = fname + ".rbm"
    ws_error("%s: not in with_sound()", get_func_name()) unless $rbm_output
    current_output = sample2filename($rbm_output)
    snd_time = lambda do
      if File.exist?(output)
        File.mtime(output)
      else
        -1
      end
    end
    rbm_time = lambda do
      str = ""
      File.foreach(rbm_file) do |s| str << s end if File.exist?(rbm_file)
      if str == body_str
        File.mtime(rbm_file)
      else
        io = File.open(rbm_file, "w")
        io << body_str
        io.close
        -1
      end
    end
    snd_t = snd_time.call
    rbm_t = rbm_time.call
    if snd_t == -1 or rbm_t == -1 or snd_t < rbm_t
      rbm_load(rbm_file, :output, output, :statistics, false, :play, 0, *args)
    end
    mus_mix(current_output, output, seconds2samples(beg))
  rescue
    ws_error("%s(%s)", get_func_name(), fname)
  end

  def rbm_mix(filename, *args)
    doc("rbm_mix(filename, *args)
        :input_frame,  0
        :output_frame, 0
        :frames,       mus_sound_frames(filename)
        :scale,        0.0 (0.0...1.0)
        :output,       old_output
Example: rbm_mix(\"tmp\")\n") if filename == :help
    old_output   = sample2filename($rbm_output)
    input_frame  = get_args(args, :input_frame, 0)
    output_frame = get_args(args, :output_frame, 0)
    frames       = get_args(args, :frames, mus_sound_frames(filename))
    scale        = get_args(args, :scale, 0.0) # input 0.0...1.0, mixer uses -1.0...1.0
    output       = get_args(args, :output, old_output)
    ws_error("%s: no output file", get_func_name()) unless output
    chans = $rbm_channels
    mus_close($rbm_output)
    amax = mus_sound_maxamp(filename)
    tmpa = []
    1.step(amax.length - 1, 2) do |i| tmpa << amax[i] end
    bmax = mus_sound_maxamp(output)
    tmpb = []
    1.step(bmax.length - 1, 2) do |i| tmpb << bmax[i] end
    avmax = [tmpa.max, tmpb.max].max
    scale = 1.0 if scale > 1
    scale = 0.0 if scale < 0
    mus_mix(output, filename,
            output_frame, frames, input_frame,
            make_mixer(chans, *(1..chans * chans).map do scale / avmax - 1 end))
    $rbm_output = continue_sample2file(old_output)
    output
  end

  def with_snd(*args, &body)
    doc("with_snd(*args) { |len| ... }
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
	:comment,           $rbm_comment (#$rbm_comment)

If :start and :dur are given, the block may return an out_data vct
which will be set to the channel(s), e.g.:

with_snd(:start, 0, :dur, 9.8,
         :channels, 4, :output, \"noise.snd\") { |len|
  vct_map!(make_vct(len), make_fm_noise(len, 500))
}

If :output is false, the default, the current sound is used, otherwise
a new sound will be opened with values of :channels, :srate, etc.

with_snd(:output, \"bell.snd\") {
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
    comment     = get_args(args, :comment, $rbm_comment)
    play = case play
           when true
             1
           when false, nil
             0
           else
             play.abs
           end
    comment = make_default_comment() unless comment
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
    data = body.call((len or 0))
    if data and loc
      chns.times do |i|
        mix_vct(vct_scale!(vct_copy(data), locsig_ref(loc, i)), beg, snd, i, false)
      end
    end
    scale_sound_to(scaled_to) if scaled_to
    scale_sound_by(scaled_by) if scaled_by
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
  alias fm_play with_snd

  #
  # help methods
  #
  
  def ws_error(*args)
    raise(WSError, format(*args), caller(1)[0])
  end
  
  def play_sound(output, play)
    if $IN_SND
      unless (snd = find_sound(output))
        snd = open_sound(output)
      end
      1.upto(play) do |i| play_and_wait(snd) end
    else
      1.upto(play) do |i| system("#{$rbm_player} #{output}") end
    end
  rescue
    warn get_func_name
  end
  
  def statistics(output, beg, reverb, revfile)
    endtime = Time.now - beg
    samps = mus_sound_samples(output)
    max_amp = mus_sound_maxamp(output)
    sr = mus_srate()
    message("    Sound File: %s", output)
    message("      Duration: %.4f", (samps / sr / $rbm_channels))
    message("  Compute time: %.3f, Compute ratio: %.2f",
            endtime, endtime * (sr / samps) * $rbm_channels)
    out_chan = 64.chr
    0.step(2 * $rbm_channels - 1, 2) do |i|
      message("  Out%s max amp: %.3f (near %.3f secs)",
              out_chan.next!, max_amp[i + 1], max_amp[i] / sr)
    end
    if reverb
      max_amp = mus_sound_maxamp(revfile)
      out_chan = 64.chr
      0.step(2 * $rbm_reverb_channels - 1, 2) do |i|
        message("  Rev%s max amp: %.3f (near %.3f secs)",
                out_chan.next!, max_amp[i + 1], max_amp[i] / sr)
      end
    end
  rescue
    warn get_func_name
  end

  def make_reverb_file_name(snd_name)
    File.split(snd_name)[1].split('.')[0] + ".reverb"
  end

  def make_default_comment
    date = Time.new.localtime.strftime("%a %d-%b-%y %H:%M %Z")
    version = format("by %s at %s using ruby %s (%s) [%s]",
                     Etc.getlogin,
                     Socket.gethostname,
                     RUBY_VERSION,
                     RUBY_RELEASE_DATE,
                     RUBY_PLATFORM)
    format("Written %s %s, rbm of %s", date, version, $rbm_version)
  rescue
    warn get_func_name
  end
  
  def sample2filename(fgen)
    fgen.to_s.split(' ')[1]
  end
  
  def remove_file(file)
    if $IN_SND and (snd = find_sound(file))
      close_sound(snd)
    end
    File.unlink(file) if File.exist?(file)
  rescue
    warn get_func_name
  end

  def tempnam
    if $IN_SND
      snd_tempnam()
    else
      @@file_nr += 1
      dir = (ENV['TMP'] or ENV['TEMP'] or ENV['TMPDIR'] or ".")
      "#{dir}/snd_#{$$}_#{@@file_nr}.snd"
    end
  rescue
    warn get_func_name
  end
end

include WS

# ws.rb ends here
