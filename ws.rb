# ws.rb -- with_sound and friends for Snd/Ruby -*- snd-ruby -*-

# Copyright (C) 2003--2004 Michael Scholz

# Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Apr 08 17:05:03 CEST 2003
# Last: Fri Apr 16 05:59:18 CEST 2004

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

# Commentary:
#
# module WS
#   ws_error(*args)
#   ws_interrupt?
#   make_with_snd(*args) do ... end
#   with_snd(*args) do ... end
#   with_reverb(reverb, reverb_data, snd, chn, rev_amount, *args)
#   make_with_sound(*args) do ... end
#   with_sound(*args) do ... end
#   make_with_dac(*args, &body)
#   with_dac(*args, &body)
#   snd_load(rbm_file, *args)
#   rbm_load(rbm_file, *args)
#   with_temp_sound(snd) do |temp_snd_file_name| ... end
#   make_default_comment
#   remove_file(file)
#   each_sample(start, dur) do |samp| ... end
#   frame2sound_data!(data, samp, frm)
#   sound_data2frame!(data, samp, frm)
#   snd2frame!(readers, frm)
#   process_times
#
# class With_sound
#   initialize(*args, &body)
#
# properties:
#   output       # file name, String
#
# methods:
#   help   (or description, info)
#   with_sound(*args) do ... end
#   scaled_to(scale) do ... end
#   scaled_by(scale) do ... end
#   with_offset(secs) do ... end
#   with_current_sound(*args) do ... end
#   sound_let(*args) do |file_name(s)| ... end
#   rbm_load(rbm_file, *args)
#   with_mix(*args, file[, beg_time], body_string)
#   with_sound_info(instrument_name, start, dur)
#   run_instrument(start, dur, *locsig_args) do |samp| ... end
#   run_reverb(start, dur, chan = 0) do |in_val, samp| ... end
#   rbm_mix(filename, *args)
#   run do ... end
#
# class Instrument
#   my_simp(start, dur, freq, amp, amp_env = [0, 1, 1, 1])
#   make_ws_reader(file, *args)
#   ws_readin(rd)
#   close_ws_reader(file, rd)
#   ws_srate(file)
#   ws_channels(file)
#   ws_duration(file)
#
# Instruments have access to @ws_output and @ws_reverb variables as
# well as @srate, @channels, @reverb_channels etc.  It is no longer
# necessary (but possible) to use the global variables $rbm_output and
# $rbm_reverb.
#
# The classes (Snd_|CLM_)Instrument are not really necessary, but the
# name may be more clear if special instruments are added.
# Instruments of class Snd_Instrument may be as well of class
# With_Snd, the same with CLM_Instrument <-> With_CLM and Instrument
# <-> With_sound.  See dlocsig.rb for additions.
# 
#                With_sound
#                    |
#                    v    
#            --- Instrument ---
#            |                |
#            v                v
#      Snd_Instrument  CLM_Instrument
#       |        |            |
#       v        v            v
#  With_Snd   With_DAC     With_CLM
#
# Instruments can use the generalized run-loop `run_instrument'.
#
# RUN_INSTRUMENT(start, dur, *locsig_args) do |samp| ... (return next sample) end
#
# The body (or block) of `run_instrument' should return the next sample, e.g.:
#
# class Instrument
#   def my_simp(start, dur, freq, amp, amp_env = [0, 1, 1, 1])
#     os = make_oscil(:frequency, freq)
#     en = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
#     run_instrument(start, dur) do
#       env(en) * oscil(os)
#     end
#   end
# end
# 
# in Snd as well as in a Ruby script:
#      with_sound do my_simp(0, 1, 440, 0.2) end
#   or with_sound(:clm, true) do my_simp(0, 1, 440, 0.2) end
#
# in Snd:
#      with_snd do my_simp(0, 1, 440, 0.2) end
#   or with_sound(:clm, false) do my_simp(0, 1, 440, 0.2) end
#
# In addition, `with_dac do my_simp(0, 1, 440, 0.2) end' can use the
# same instrument for dac-output.
#
# Reverbs can use the generalized run-loop `run_reverb'.
#
# RUN_REVERB(start, dur, chan = 0) do |value, samp| ... (return next frame) end
#
# VALUE is the next sample value on location SAMP of reverb file's
# CHANnel.  It replaces
#
#   (beg...len).each do |i|
#     ho = ina(i, $rbm_reverb)
#     ...
#   end
# by
#   run_reverb(start, dur) do |ho, i|
#     ...
#   end
#
# The body should return a frame object of out-channels length.  The
# body is called seconds2sample(dur) times, like run_instrument.
#
# def my_reverb(start, dur, *rest)
#   ...
#   out_frames = make_frame(@channels)
#   run_reverb(start, dur) do |ho, i|
#     ...
#     frame_set!(out_frames, 0, val0)
#     if @channels > 1
#       frame_set!(out_frames, 1, val1)
#     end
#     ...
#     out_frames
#   end
# end
#
# A special case occures in freeverb.rb.
#
#   run_reverb(start, dur, :frames) do |in_frame, i|
#     ...
#   end
#
# IN_FRAME is a frame object of the I-th location in the reverb file
# (or snd), the rest is like above.
#
# The classes Snd_Instrument and CLM_Instrument can be used to define
# special instruments, see FULLMIX in clm-ins.rb.
#
# Class Snd_Instrument instruments can only be used in Snd, that means
# the variables @ws_output and @ws_reverb are snd index numbers.
# `with_snd' calls instruments from this class with higher priority
# than from class Instrument or Object.
#
# Class CLM_Instrument instruments use sample2file output and can be
# used in Snd as well as in Ruby scripts, @ws_output and $rbm_output
# are the same, @ws_reverb and $rbm_reverb are the same too.
# `with_sound' calls instruments from this class with higher priority
# than from class Instrument or Object.

# Usage:
#
# Global variables can be set in ~/.snd-ruby.rb or in other scripts
# before or after loading ws.rb.
#
# with_sound(:play, 3, :statistics, true, :reverb, :jc_reverb) do
#   fm_violin(0, 1, 440, 0.3)
# end
#
# rbm_load("test.rbm", :play, 1, :statistics, true, :verbose, true)
#
# These functions can be called within with_sound or rbm_load:
#   scaled_to(scale, &body)
#   scaled_by(scale, &body)
#   with_offset(secs, &body)
#   with_current_sound(*args, &body)
#   sound_let(*args, &body)
#   with_mix(*args) (does not use a block but a string as "body")
#
# SOUND_LET
#
# sound_let(*args) do |file_name(s)| ... end
# args: [with_sound-args1, with_sound-body1],
#       [with_sound-args2, with_sound-body2], ...
# 
# Each array of ARGS creates a temporary sound file via
# `with_sound(WITH_SOUND-ARGS) do WITH_SOUND-BODY end', which can be
# accessed in the main BODY.  After finishing BODY the temporary sound
# file(s) will be removed.
#
# Examples:
# 
# One with_sound-array and one temporary file name, arbitrary called TMP:
# 
# sound_let([:reverb, :jc_reverb, lambda do fm_violin(0, 1, 220, 0.2) end]) do |tmp|
#   mus_mix(@output, tmp)
# end
# 
# Two with_sound-arrays and two temporary file names, arbitrary
# called TEMP_1 and TEMP_2:
# 
# sound_let([:reverb, :jc_reverb, lambda do fm_violin(0, 1, 220, 0.2) end],
#           [lambda do fm_violin(0.5, 1, 440, 0.3) end]) do |temp_1, temp_2|
#   mus_mix(temp_1, temp_2)
#   mus_mix(@output, temp_1)
# end
#
# WITH_MIX
#
# with_mix(file, body_string)
# with_mix(file, beg_time, body_string)
# with_mix(with_sound-args, file, beg_time, body_string)
#
# The FILE and BODY_STRING are necessary, BEG_TIME and
# WITH_SOUND-ARGS are optional, BEG_TIME defaults to 0.
#
# Creates a text file named FILE.rbm with contents BODY_STRING and a
# sound file named FILE.snd, the result of rbm_load(FILE.rbm).  If
# BODY_STRING is changed, rbm_load(FILE.rbm) is called before mixing
# FILE.snd in the underlying @output of with_sound.
#
# Example:
#
# with_sound() do
#   with_mix(:reverb, :jc_reverb, "foo", 0.2, %Q{
#     fm_violin(0, 1, 440, 0.1)
#   })
# end
#
# with_sound's NOTEHOOK and INFO options:
#
# Every time an instrument starts computing, @with_sound_note_hook is
# called with the instrument name, the start time and the duration.
#
# def my_notehook(name, start, dur)
#   if name =~ /violin/
#     message("%s: start %1.3f, dur %1.3f", name, start, dur)
#   end
# end
#
# with_sound(:notehook, :my_notehook) do
#   fm_violin(0, 1, 440, 0.1)
#   fbell = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
#   abell = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
#   fm_bell(0.5, 2.0, 220, 0.5, abell, fbell, 1)
# end
# 
# installs the @with_sound_note_hook and prints the line
# 
#   `# fm_violin: start 0.000, dur 1.000'
#
# If option :info is true, every instrument-call prints the line
# mentioned above.
#
# with_sound's SAVE_BODY option:
# 
# Works only if proc (the with_sound-body) is located in a source file
# and proc.to_s returns something like
# #<Proc:0x00000000@(/path/to/source/file):2> (older Ruby versions
# return only #<Proc:0x00000000>, so we can't find the source file and
# line number).
#
# Tries to locate the body code and saves it in the comment string of
# the sound file.  Scanning the source code is very simple and may not
# work in every case.

# CLM examples (see clm.html) and their Snd/Ruby counterparts:
#
# ;; CLM examples
#  (with-sound () 
#    (mix (with-sound (:output "hiho.snd") 
#              (fm-violin 0 1 440 .1))
#            :amplitude .5))
# 
#  (with-sound ()
#    (with-mix () "s1" 0
#      (sound-let ((tmp ()
#                    (fm-violin 0 1 440 .1)))
#        (mix tmp))))
# 
#  (with-sound (:verbose t)
#    (with-mix () "s6" 0
#      (sound-let ((tmp ()
#                    (fm-violin 0 1 440 .1))
#                  (tmp1 (:reverb nrev)
#                    (mix "oboe.snd")))
#        (mix tmp1)
#        (mix tmp :amplitude .2 :output-frame *srate*))
#      (fm-violin .5 .1 330 .1)))
# 
#  (with-sound (:verbose t)
#    (sound-let ((tmp ()
#                  (with-mix () "s7" 0
#                    (sound-let ((tmp ()
#                                  (fm-violin 0 1 440 .1))
#                                (tmp1 ()
#                                  (mix "oboe.snd")))
#                     (mix tmp1)
#                     (mix tmp :output-frame *srate*))
#                   (fm-violin .5 .1 330 .1))))
#      (mix tmp :amplitude .5)))
#
=begin
# Snd/Ruby examples
with_sound() do
  rbm_mix(with_sound(:output, "hiho.snd") do
            fm_violin(0, 1, 440, 0.1)
          end.output, :scale, 0.5)
end

with_sound() do
  with_mix "s1", %Q{
  sound_let([lambda do fm_violin(0, 1, 440, 0.1) end]) do |tmp|
    rbm_mix(tmp)
  end
  }
end

with_sound(:verbose, true) do
  with_mix "s6", %Q{
  sound_let([lambda do fm_violin(0, 1, 440, 0.1) end],
            [:reverb, :nrev, lambda do rbm_mix("oboe.snd") end]) do |tmp, tmp1|
    rbm_mix(tmp1)
    rbm_mix(tmp, :scale, 0.2, :output_frame, seconds2samples(1))
  end
  fm_violin(0.5, 0.1, 330, 0.1)
  }
end

with_sound(:verbose, true) do
  sound_let([lambda do
                with_mix "s7", 0, %Q{
                  sound_let([lambda do fm_violin(0, 1, 440, 0.1) end],
                            [lambda do rbm_mix("oboe.snd") end]) do |tmp, tmp1|
                    rbm_mix(tmp1)
                    rbm_mix(tmp, :output_frame, @srate)
                  end
                  fm_violin(0.5, 0.1, 330, 0.1)
                }
              end]) do |tmp0|
    rbm_mix(tmp0, :scale, 0.5)
  end
end
=end

# Code:

require "examp"
require "sndlib" unless provided? "sndlib"
with_silence do
  unless defined? Etc.getlogin
    require "etc"
  end
  unless defined? Socket.gethostname
    require "socket"
  end
end
require "hooks"

$rbm_version          = "16-Apr-2004"
$rbm_output           = false
$rbm_reverb           = false
$rbm_file_name        = "test.snd" unless defined? $rbm_file_name
$rbm_comment          = nil        unless defined? $rbm_comment
$rbm_statistics       = false      unless defined? $rbm_statistics
$rbm_play             = 0          unless defined? $rbm_play
$rbm_player           = "sndplay"  unless defined? $rbm_player
$rbm_reverb_file_name = nil        unless defined? $rbm_reverb_file_name
$rbm_reverb_channels  = 1          unless defined? $rbm_reverb_channels
$rbm_reverb_func      = nil        unless defined? $rbm_reverb_func
$rbm_reverb_data      = []         unless defined? $rbm_reverb_data
$rbm_delete_reverb    = false      unless defined? $rbm_delete_reverb
$rbm_verbose          = $VERBOSE   unless defined? $rbm_verbose
$rbm_info             = false      unless defined? $rbm_info
$rbm_notehook         = nil        unless defined? $rbm_notehook

if provided? "snd"
  $rbm_srate         = default_output_srate  unless defined? $rbm_srate
  $rbm_channels      = default_output_chans  unless defined? $rbm_channels
  $rbm_header_type   = default_output_type   unless defined? $rbm_header_type
  $rbm_data_format   = default_output_format unless defined? $rbm_data_format
  $rbm_locsig_type   = locsig_type           unless defined? $rbm_locsig_type
  $rbm_rt_bufsize    = dac_size              unless defined? $rbm_rt_bufsize
  $rbm_output_device = audio_output_device   unless defined? $rbm_output_device
else
  $rbm_srate         = 22050             unless defined? $rbm_srate
  $rbm_channels      = 1                 unless defined? $rbm_channels
  $rbm_header_type   = Mus_next          unless defined? $rbm_header_type
  $rbm_data_format   = Mus_lshort        unless defined? $rbm_data_format
  $rbm_locsig_type   = Mus_linear        unless defined? $rbm_locsig_type
  $rbm_rt_bufsize    = 512               unless defined? $rbm_rt_bufsize
  $rbm_output_device = Mus_audio_default unless defined? $rbm_output_device
end

module WS
  class WSError < StandardError
  end

  module_function
  def ws_error(*args)
    raise(WSError, verbose_message_string(true, "# ", *args))
  end

  def ws_interrupt?
    throw(:with_sound_interrupt, :interrupted) if c_g?
  end

  def make_with_snd(*args, &body)
    With_Snd.new(*args, &body)
  end

  def with_snd(*args, &body)
    ws = if get_args(args, :clm, false)
           make_with_sound(*args, &body)
         else
           make_with_snd(*args, &body)
         end
    if get_args(args, :help, false)
      rbm_message(ws.help)
    else
      ws.run
    end
    ws
  end

  # with_reverb(:jc_reverb)
  # require 'clm-ins'
  # with_reverb(:jl_reverb, [], 2, false, 0.2)
  def with_reverb(reverb,
                  reverb_data = [],
                  snd = selected_sound(),
                  chn = selected_channel(),
                  rev_amount = 0.05, *args)
    ws = With_Snd.new(:reverb, reverb,
                      :reverb_data, reverb_data,
                      :output, file_name(snd),
                      :save_after, false, *args)
    ws.with_reverb(snd, chn, rev_amount)
    ws
  end
  
  def make_with_sound(*args, &body)
    With_CLM.new(*args, &body)
  end
  
  def with_sound(*args, &body)
    ws = if get_args(args, :clm, true)
           make_with_sound(*args, &body)
         else
           make_with_snd(*args, &body)
         end
    if get_args(args, :help, false)
      rbm_message(ws.help)
    else
      ws.run
    end
    ws
  end

  def make_with_dac(*args, &body)
    With_DAC.new(*args, &body)
  end
  
  def with_dac(*args, &body)
    ws = make_with_dac(*args, &body)
    if get_args(args, :help, false)
      rbm_message(ws.help)
    else
      ws.run
    end
    ws
  end
  
  def snd_load(rbm_file, *args)
    ws = make_with_sound(:clm, false, *args)
    ws.rbm_load(rbm_file)
    ws
  end
  
  def rbm_load(rbm_file, *args)
    ws = make_with_sound(:clm, true, *args)
    ws.rbm_load(rbm_file)
    ws
  end

  def with_temp_sound(snd = false, &body)
    doc("with_temp_sound(snd = false) do |temp_snd_file_name| ... end
Saves SND in a temporary file, which name can be accessed in the body
code.  After finishing the body, the file will be removed.\n") if snd == :help
    t = tempnam
    save_sound_as(t, snd)
    ret = body.call(t)
    remove_file(t)
    ret
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
  end
  
  def remove_file(file)
    if provided?("snd") and (snd = find_sound(file))
      revert_sound(snd)
      close_sound_extend(snd)
    end
    File.unlink(file) if File.exist?(file)
  end

  def each_sample(start, dur)
    beg, ends = times2samples(start, dur)
    ws_interrupt?
    (beg...ends).each do |samp| yield(samp) end
  end

  def frame2sound_data!(data, samp, frm)
    sound_data_chans(data).times do |chn|
      sound_data_set!(data, chn, samp, frame_ref(frm, chn))
    end
  end

  def sound_data2frame!(data, samp, frm)
    sound_data_chans(data).times do |chn|
      frame_set!(frm, chn, sound_data_ref(data, chn, samp))
    end
    frm
  end

  # READERS is an array of sample_reader objects
  def snd2frame!(readers, frm)
    readers.each_with_index do |rd, chn|
      frame_set!(frm, chn, read_sample(rd))
    end
    frm
  end

  def process_times
    if defined? Process.times
      Process.times
    else
      Time.times
    end
  end
end

class With_sound
  include Info
  include WS
  @@file_number = 0

  def initialize(*args, &body)
    @output          = get_args(args, :output, $rbm_file_name)
    @channels        = get_args(args, :channels, $rbm_channels)
    @srate           = get_args(args, :srate, $rbm_srate).to_f
    @header_type     = get_args(args, :header_type, $rbm_header_type)
    @data_format     = get_args(args, :data_format, $rbm_data_format)

    @reverb          = get_args(args, :reverb, $rbm_reverb_func)
    @reverb_data     = get_args(args, :reverb_data, $rbm_reverb_data)
    @reverb_channels = get_args(args, :reverb_channels, $rbm_reverb_channels)
    @revfile         = get_args(args, :reverb_file_name, nil)
    @delete_reverb   = get_args(args, :delete_reverb, $rbm_delete_reverb)

    @decay_time      = get_args(args, :decay_time, 1.0)
    @scaled_to       = get_args(args, :scaled_to, false)
    @scaled_by       = get_args(args, :scaled_by, false)

    @continue        = get_args(args, :continue_old_file, false)
    @notehook        = get_args(args, :notehook, $rbm_notehook)
    @save_body       = get_args(args, :save_body, false)
    @save_after      = get_args(args, :save_after, false)
    @player          = get_args(args, :player, $rbm_player)

    @play            = get_args(args, :play, $rbm_play)
    @statistics      = get_args(args, :statistics, $rbm_statistics)
    @verbose         = get_args(args, :verbose, $rbm_verbose)
    @info            = get_args(args, :info, $rbm_info)
    @comment         = get_args(args, :comment, $rbm_comment)
    @offset          = get_args(args, :offset, 0.0)

    @rtime = @utime = @stime = 0.0
    @body = body
    @revfile = make_reverb_file_name() unless @revfile
    @reverb_amount = 0.05
    # play: either :play, true
    #       or     :play, false
    #       or     :play, nil
    #       or     :play, integer (integer times playing)
    @play = case @play
            when true
              1
            when false, nil
              0
            else
              @play.abs
            end
    if @continue
      @play = 0
      @statistics = false
    end
    # without reverb: either :reverb_channels, 0
    #                 or     :reverb, false
    if @reverb_channels.zero?
      @reverb = $rbm_reverb = false
    end
    @comment = make_default_comment unless @comment.kind_of?(String)
    if @save_body and @body.kind_of?(Proc)
      @comment = format("%s%s%s",
                        @comment,
                        ((@comment.kind_of?(String) and @comment.empty?) ? "" : ";\n"),
                        @body.to_body.chomp)
    end
    @old_srate = mus_srate
    @start = @dur = 0.0
    @locsig_type = $rbm_locsig_type
    @rbm_output = @ws_output = false
    $rbm_reverb = @ws_reverb = false
    # recursive call: :reverb, [:old_reverb, @ws_reverb]
    if (not @reverb)
      @reverb_channels = 0
    elsif @reverb.kind_of?(Array)
      $rbm_reverb = @ws_reverb = @reverb.last
      @reverb = false
    end
    @with_sound_note_hook = Hook.new("@with_sound_note_hook", 3, "\
lambda do |inst_name, start, dur| ... end: called if an instrument has
the run_instrument/run_reverb loop included.

Every time an instrument starts computing, @with_sound_note_hook is
called with the instrument name INST_NAME, the start time START, and
the duration DUR.

def my_notehook(name, start, dur)
  if name =~ /violin/
    message(\"%s: start %1.3f, dur %1.3f\", name, start, dur)
  end
end

with_sound(:notehook, :my_notehook) do
  fm_violin(0, 1, 440, 0.1)
  fbell = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
  abell = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
  fm_bell(0.5, 2.0, 220, 0.5, abell, fbell, 1)
end

installs the @with_sound_note_hook and prints the line

  `# fm_violin: start 0.000, dur 1.000'.")
    if @notehook
      prc = case @notehook
            when Proc
              @notehook
            when Symbol
              method(@notehook).to_proc
            else
              ws_error("%s#%s: need a proc, %s", self.class, get_func_name, @notehook.inspect)
            end
      # @with_sound_note_hook.add_hook!("with-sound-note-hook", &prc)
      # Doesn't work with older Ruby if @notehook is a Symbol/Method,
      # so we go the verbose way.
      @with_sound_note_hook.add_hook!("with-sound-note-hook") do |name, start, dur|
        prc.call(name, start, dur)
      end
    end
    set_help
  end
  attr_reader :output, :with_sound_note_hook
  alias help description
  
  def inspect
    rstr = format(", reverb: %s, reverb-channels: %d", @reverb.to_s, @reverb_channels)
    format("#<%s: output: %s, channels: %d, srate: %d%s>",
           self.class, @output.inspect, @channels, @srate.round, @reverb ? rstr : "")
  end

  def with_sound(*args, &body)
    klass = if get_args(args, :clm, @clm)
              With_CLM
            else
              With_Snd
            end
    com = format("%s#%s: temporary sound, args %s", self.class, get_func_name, args.inspect)
    ws = klass.new(:output,            get_args(args, :output, @output),
                   :comment,           get_args(args, :comment, com),
                   :play,              get_args(args, :play, false),
                   :statistics,        get_args(args, :statistics, false),
                   :save_after,        get_args(args, :save_after, true),
                   :reverb,            get_args(args, :reverb, [:old_reverb, @ws_reverb]),
                   :reverb_data,       get_args(args, :reverb_data, @reverb_data),
                   :reverb_file_name,  get_args(args, :reverb_file_name, @revfile),
                   :reverb_channels,   get_args(args, :reverb_channels, @reverb_channels),
                   :delete_reverb,     get_args(args, :delete_reverb, @delete_reverb),
                   :decay_time,        get_args(args, :decay_time, @decay_time),
                   :continue_old_file, get_args(args, :continue_old_file, @continue),
                   :scaled_to,         get_args(args, :scaled_to, @scaled_to),
                   :scaled_by,         get_args(args, :scaled_by, @scaled_by),
                   :notehook,          get_args(args, :notehook, @notehook),
                   :save_body,         get_args(args, :save_body, @save_body),
                   :channels,          get_args(args, :channels, @channels),
                   :srate,             get_args(args, :srate, @srate).to_f,
                   :header_type,       get_args(args, :header_type, @header_type),
                   :data_format,       get_args(args, :data_format, @data_format),
                   :verbose,           get_args(args, :verbose, @verbose),
                   :info,              get_args(args, :info, @info),
                   :offset,            get_args(args, :offset, 0.0),
                   &body)
    ws.run
    ws
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
  
  def with_current_sound(*args, &body)
    outfile = get_args(args, :output, tempnam)
    offset  = get_args(args, :offset, 0.0)
    scl_to  = get_args(args, :scaled_to, false)
    scl_by  = get_args(args, :scaled_by, false)
    ws = with_sound(:output, outfile,
                    :scaled_to, scl_to,
                    :scaled_by, scl_by,
                    :offset, offset,
                    *args, &body)
    rbm_mix(ws.output)
    remove_file(ws.output)
    ws
  end

  def sound_let(*args, &body)
    outfile_list = []
    args.each do |ws_args|
      if (ws_body = ws_args.pop).kind_of?(Proc)
        outfile_list << with_sound(:output, tempnam(), *ws_args, &ws_body).output
      else
        ws_error("%s#%s: need a procedure, %s", self.class, get_func_name, ws_body.inspect)
      end
    end
    body.call(*outfile_list)
    outfile_list.each do |f| remove_file(f) end
  end

  def rbm_load(rbm_file, *args)
    if File.exist?(rbm_file)
      ws = with_sound(:play, @play,
                      :statistics, @statistics,
                      :comment, @comment,
                      :reverb, @reverb, *args) do
        message("Loading %s", rbm_file.inspect) if @verbose
        eval(File.open(rbm_file).read)
      end
      ws
    else
      ws_error("%s: no such file, %s", get_func_name, rbm_file.inspect)
    end
  end

  def with_mix(*args)
    unless (body_str = args.pop).kind_of?(String)
      ws_error("%s#%s: need a body string, %s", self.class, get_func_name, body_str.inspect)
    end
    beg = if args[-1].kind_of?(Numeric)
            args.pop
          else
            0
          end
    if (fname = args.pop).kind_of?(String)
      out_file = fname + ".snd"
      rbm_file = fname + ".rbm"
    else
      ws_error("%s#%s: need a file name, %s", self.class, get_func_name, fname.inspect)
    end
    snd_time = if File.exist?(out_file)
                 File.mtime(out_file)
               else
                 :load
               end
    old_body = if File.exist?(rbm_file)
                 File.open(rbm_file).read
               else
                 ""
               end
    rbm_time = if old_body == body_str
                 File.mtime(rbm_file)
               else
                 File.open(rbm_file, "w") do |f| f << body_str end
                 :load
               end
    if snd_time == :load or rbm_time == :load or snd_time < rbm_time
      rbm_load(rbm_file, :output, out_file, *args)
    end
    rbm_mix(out_file, :output_frame, seconds2samples(beg))
  end

  def with_sound_info(name, start, dur)
    message("%s: start %1.3f, dur %1.3f", name, start, dur) if @info
    @with_sound_note_hook.call(name, start, dur) if @notehook
  end
  
  def run_instrument(start, dur, *locsig_args, &body)
    with_sound_info(get_func_name(3), start, dur)
    degree         = get_args(locsig_args, :degree, kernel_rand(90.0))
    distance       = get_args(locsig_args, :distance, 1.0)
    @reverb_amount = get_args(locsig_args, :reverb_amount, 0.05)
    make_locsig(:degree, degree,
                :distance, distance,
                :reverb, @reverb_amount,
                :output, (@clm ? @ws_output : false),
                :revout, (@clm ? @ws_reverb : false),
                :channels, @channels,
                :type, @locsig_type)
  end

  # case chan
  # when Integer
  #   # jc_reverb_rb, jl_reverb_rb, nrev_rb
  #   body.call(sample_of_CHAN, current_sample_number)
  # when :frames
  #   # freeverb_rb
  #   body.call(frame_of_reverb_file, current_sample_number)
  # end
  def run_reverb(start, dur, chan)
    with_sound_info(get_func_name(3), start, dur)
    if @verbose
      message("%s on %d in and %d out channel%s",
              get_func_name(3), @reverb_channels, @channels, (@channels > 1 ? "s" : ""))
    end
    unless chan.between?(0, @reverb_channels - 1)
      ws_error("%s#%s: only %d reverb channel(s), but channel %d requested",
               self.class, get_func_name, @reverb_channels, chan)
    end
  end
  
  def run(&body)
    set_mus_srate(@srate)
    before_output
    init_process_time
    if block_given?
      run_body(@scaled_to, @scaled_by, &body)
    else
      run_body(@scaled_to, @scaled_by, &@body)
    end
    after_output
    if @reverb
      mus_close(@ws_reverb) if mus_output?(@ws_reverb)
      old_reverb = @ws_reverb
      # non-run_reverb functions needs it here
      $rbm_reverb = make_file2sample(@revfile)
      @dur += @decay_time
      run_sound_reverb
      mus_close(@ws_reverb) if mus_input?(@ws_reverb)
      $rbm_reverb = @ws_reverb = old_reverb
      after_reverb
    end
    if @statistics
      set_process_time
      statistics
    end
    finish_sound
    1.upto(@play) do play_it end
    set_mus_srate(@old_srate)
  end
  
  protected
  def run_body(scl_to, scl_by, &body)
    frm1 = ws_location
    if (catch(:with_sound_interrupt) do instance_eval(&body) end) == :interrupted
      set_mus_srate(@old_srate)
      finish_sound
      message("%s#%s: body interrupted by user", self.class, get_func_name)
    end
    frm2 = ws_location
    if scl_to
      scaled_to_sound(scl_to, frm1, frm2 - frm1)
    end
    if scl_by
      scaled_by_sound(scl_by, frm1, frm2 - frm1)
    end
  end
  
  def run_sound_reverb
    unless @reverb == :intern or @reverb == :n_rev
      ret = catch(:with_sound_interrupt) do
        case @reverb
        when Proc
          @reverb.call(@start, @dur, *@reverb_data)
        when Symbol
          send(@reverb, @start, @dur, *@reverb_data)
        else
          ws_error("%s#%s: need a proc, %s", self.class, get_func_name, @reverb.inspect)
        end
      end
      if ret == :interrupted
        set_mus_srate(@old_srate)
        finish_sound
        message("%s#%s: reverb interrupted by user", self.class, get_func_name)
      end
    end
  end

  def tempnam
    if provided? "snd"
      snd_tempnam()
    else
      @@file_number += 1
      dir = (ENV.map do |k, v| v if /TMP/ =~ k end.compact.first or "/tmp")
      format("%s/snd_%d_%d.snd", dir, $$, @@file_number)
    end
  end

  def make_reverb_file_name
    path = File.split(@output).first
    file = File.basename(@output, ".*") + ".reverb"
    file = path + "/" + file unless path == "."
    file
  end

  def init_process_time
    @rtime = Time.now
    tms = process_times
    @utime = tms.utime
    @stime = tms.stime
  end
  
  def set_process_time
    tms = process_times
    @rtime = Time.now - @rtime
    @utime = tms.utime - @utime
    @stime = tms.stime - @stime
  end
  
  def before_output
  end

  def after_output
  end

  def ws_location
  end

  def scaled_to_sound(scl_to, from, to)
  end

  def scaled_by_sound(scl_to, from, to)
  end
  
  def statistics(frms, data_fmt, type)
    message("filename: %s", @output.inspect)
    message("   chans: %d, srate: %d", @channels, @srate.round)
    if frms > 0
      message("  length: %1.3f (%d samples)", frms / @srate, frms)
    end
    if data_fmt and type
      message("  format: %s [%s]", mus_data_format_name(data_fmt), mus_header_type_name(type))
    end
    message("    real: %1.3f  (utime %1.3f, stime %1.3f)", @rtime, @utime, @stime)
    if frms > 0
      message("   ratio: %1.2f  (uratio %1.2f)", @rtime * (@srate / frms), @utime * (@srate / frms))
    end
  end
  
  def finish_sound
    @reverb and @delete_reverb and (not @continue) and remove_file(@revfile)
  end

  def play_it
  end
  
  def set_help
    self.description = format("\
# with_sound(*args) do ... end
# 
#   :output,            $rbm_file_name (#{$rbm_file_name.inspect})
#   :channels,          $rbm_channels (#$rbm_channels)
#   :srate,             $rbm_srate (#$rbm_srate)
#   :header_type,       $rbm_header_type (#$rbm_header_type)
#   :data_format,       $rbm_data_format (#$rbm_data_format)
# 
#   :reverb,            $rbm_reverb_func (#{$rbm_reverb_func.inspect})
#   :reverb_data,       $rbm_reverb_data (#{$rbm_reverb_data.inspect})
#   :reverb_channels,   $rbm_reverb_channels (#$rbm_reverb_channels)
#   :revfile,           $rbm_reverb_file_name (#{$rbm_reverb_file_name.inspect})
#   :delete_reverb      $rbm_delete_reverb (#$rbm_delete_reverb)
# 
#   :decay_time,        1.0
#   :scaled_to,         false
#   :scaled_by,         false
#   :continue_old_file, false
#   :notehook,          $rbm_notehook (#{$rbm_notehook.inspect})
#  
#   :play,              $rbm_play (#$rbm_play)
#   :statistics,        $rbm_statistics (#$rbm_statistics)
#   :comment,           $rbm_comment (#{$rbm_comment.inspect})
#   :verbose,           $rbm_verbose (#$rbm_verbose)
#
## next option works only with newer Ruby versions
## save the with_sound-body in sound file's comment
#   :save_body,         false
#
## if an instrument uses run_instrument/run_reverb and INFO is true, a
## message will be printed: `instrument_name: start 0.000, dur 0.000'
#   :info,              $rbm_info (#$rbm_info)
#
%s
#
## special with_dac options:
#   :bufsize,           $rbm_rt_bufsize (#$rbm_rt_bufsize)
#   :device,            $rbm_output_device (#$rbm_output_device)
# 
# Usage: with_sound(:play, 1, :statistics, true) do fm_violin end",
                              @clm ? "\
#   :player,            $rbm_player (#{$rbm_player.inspect})
" : "\
## save sound after computing
#   :save_after,        false")

  end
end

class Instrument < With_sound
  # Actually it isn't necessary to define instruments as methods of
  # class Instrument.
  def my_simp(start = 0, dur = 1, freq = 440, amp = 0.5, amp_env = [0, 1, 1, 1])
    os = make_sum_of_cosines(:frequency, freq, :cosines, 3)
    en = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
    fs = hz2radians(freq)
    pv = make_triangle_wave(:frequency, 6.0, :amplitude, 0.0025 * fs)
    rv = make_rand_interp(:frequency, 8.0, :amplitude, 0.005 * fs)
    run_instrument(start, dur) do
      sum_of_cosines(os, triangle_wave(pv) + rand_interp(rv)) * env(en)
    end
  end

  # simple violin, see snd-7/fm.html
  def fm_v(start = 0, dur = 1, freq = 440, amp = 0.5, fm_index = 1, amp_env = [0, 1, 1, 1])
    frq_scl = hz2radians(freq)
    maxdev = frq_scl * fm_index
    index1 = maxdev * (5.0 / log(freq))
    index2 = maxdev * 3.0 * ((8.5 - log(freq)) / (3.0 + freq / 1000.0))
    index3 = maxdev * (4.0 / sqrt(freq))
    carrier = make_oscil(:frequency, freq)
    fmosc1 = make_oscil(:frequency, freq)
    fmosc2 = make_oscil(:frequency, freq * 3.0)
    fmosc3 = make_oscil(:frequency, freq * 4.0)
    ampf = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
    indf1 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0], :scaler, index1, :duration, dur)
    indf2 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0], :scaler, index2, :duration, dur)
    indf3 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0], :scaler, index3, :duration, dur)
    pervib = make_triangle_wave(:frequency, 0.5, :amplitude, 0.0025 *  frq_scl)
    ranvib = make_rand_interp(:frequency, 16.0, :amplitude, 0.005 * frq_scl)
    run_instrument(start, dur) do
      vib = triangle_wave(pervib) + rand_interp(ranvib)
      env(ampf) * oscil(carrier,
                        vib + \
                        env(indf1) * oscil(fmosc1, vib) + \
                        env(indf2) * oscil(fmosc2, 3.0 * vib) + \
                        env(indf3) * oscil(fmosc3, 4.0 * vib))
    end
  end
end

class Snd_Instrument < Instrument
  # place holder for special Snd instruments, see FULLMIX in
  # clm-ins.rb.

  def get_snd(file)
    snd = if file.kind_of?(Integer)
            file
          elsif file.kind_of?(String)
            if s = find_sound(file)
              s
            else
              open_sound(file)
            end
          end
    if snd.kind_of?(Integer) and sound?(snd)
      snd
    else
      ws_error("%s: no such snd, %s", get_func_name, snd.inspect)
    end
  end
  private :get_snd
  
  # (make-sample-reader (start-samp 0) (snd #f) (chn #f) (dir 1) (edpos #f))
  def make_ws_reader(file, *args)
    start = get_args(args, :start, 0)
    chn   = get_args(args, :channel, 0)
    dir   = get_args(args, :direction, 1)
    make_sample_reader(start, get_snd(file), chn, dir, false)
  end

  # (read-sample reader)
  def ws_readin(rd)
    read_sample(rd)
  end

  def close_ws_reader(file, rd)
    free_sample_reader(rd)
    if snd = if file.kind_of?(Integer)
               file
             else
               find_sound(file)
             end
      close_sound_extend(snd)
    end
  end

  def ws_srate(file)
    srate(get_snd(file)).to_f
  end

  def ws_channels(file)
    channels(get_snd(file))
  end
  
  def ws_duration(file)
    snd = get_snd(file)
    frames(snd) / srate(snd).to_f
  end
end

class CLM_Instrument < Instrument
  # place holder for special Snd instruments, see FULLMIX in
  # clm-ins.rb.

  # (make-readin (:file) (:channel 0) (:start 0) (:direction 1)
  def make_ws_reader(file, *args)
    start = get_args(args, :start, 0)
    chn   = get_args(args, :channel, 0)
    dir   = get_args(args, :direction, 1)
    make_readin(:file, file, :channel, chn, :start, start, :direction, dir)
  end

  # (readin gen)
  def ws_readin(rd)
    readin(rd)
  end

  def close_ws_reader(file, rd)
    mus_close(rd)
  end

  def ws_srate(file)
    mus_sound_srate(file)
  end

  def ws_channels(file)
    mus_sound_chans(file)
  end
  
  def ws_duration(file)
    mus_sound_duration(file)
  end
end

class With_Snd < Snd_Instrument
  def initialize(*args, &body)
    @clm = false
    super
  end
  
  def run_instrument(start, dur, *locsig_args, &body)
    loc = super
    beg = seconds2samples(start + @offset)
    len = seconds2samples(dur)
    out_data = make_vct(len)
    samp = beg - 1
    ws_interrupt?
    vct_map!(out_data, lambda do | | body.call(samp += 1) end)
    ws_interrupt?
    @channels.times do |chn|
      mix_vct(vct_scale!(vct_copy(out_data), locsig_ref(loc, chn)), beg, @ws_output, chn, false)
    end
    ws_interrupt?
    @reverb_channels.times do |chn|
      scl = @reverb_amount * locsig_ref(loc, [chn, @channels - 1].min)
      mix_vct(vct_scale!(vct_copy(out_data), scl), beg, @ws_reverb, chn, false)
    end
  end
  
  def run_reverb(start, dur, chan = 0, &body)
    beg = seconds2samples(start)
    len = seconds2samples(dur)
    out_data = make_sound_data(@channels, len)
    case chan
    when Integer
      super(start, dur, chan)
      ws_interrupt?
      let(make_sample_reader(beg, @ws_reverb, chan)) do |rd|
        len.times do |i|
          frame2sound_data!(out_data, i, body.call(read_sample(rd), i + beg))
        end
        free_sample_reader(rd)
      end
    when :frames
      super(start, dur, 0)
      ws_interrupt?
      frm = make_frame(@reverb_channels)
      readers = make_array(@reverb_channels) do |chn|
        make_sample_reader(beg, @ws_reverb, chn)
      end
      ws_interrupt?
      len.times do |i|
        frame2sound_data!(out_data, i, body.call(snd2frame!(readers, frm), i + beg))
      end
      readers.each do |rd|
        free_sample_reader(rd)
      end
    end
    ws_interrupt?
    v = make_vct(len)
    @channels.times do |chn|
      mix_vct(sound_data2vct(out_data, chn, v), beg, @ws_output, chn, false)
    end
  end
  
  def rbm_mix(filename, *args)
    doc("rbm_mix(filename, *args)
        :input_frame,  0
        :output_frame, 0
        :frames,       mus_sound_frames(filename)
        :scale,        1.0
Example: rbm_mix(\"tmp\")\n") if filename == :help
    input_frame  = get_args(args, :input_frame, 0).round
    output_frame = get_args(args, :output_frame, 0).round
    frms         = get_args(args, :frames, mus_sound_frames(filename)).round
    scale        = get_args(args, :scale, 1.0).to_f
    unless snd = find_sound(filename)
      unless snd = open_sound(filename)
        ws_error("%s#%s: no such file, %s", self.class, get_func_name, filename.inspect)
      end
    end
    [channels(snd), @channels].min.times do |chn|
      scale_channel(scale, input_frame, frms, snd, chn) if scale.nonzero?
      mix_vct(channel2vct(input_frame, frms, snd, chn), output_frame, @ws_output, chn, false)
    end
    revert_sound(snd)
    close_sound_extend(snd)
  end

  def with_reverb(snd, chn, rev_amount)
    @reverb_amount = rev_amount
    select_sound(snd)
    if chn.kind_of?(Integer)
      swap = chn.nonzero?
      @channels = 1
    else
      @channels = channels(snd)
    end
    set_mus_srate(@srate = srate(snd).to_f)
    len = frames(snd, chn)
    @start = 0
    @dur = len / @srate + @decay_time
    @ws_output = snd
    @ws_reverb = false
    init_process_time
    if @reverb == :intern or @reverb == :n_rev
      # apply_controls(snd = false, choice = 0, beg = 0, dur = len)
      # coice 0: sound
      #       1: channel
      #       2: selection
      @reverb_channels = (chn ? 1 : 0)
      after_reverb
    else
      @reverb_channels = @channels
      if rsnd = find_sound(@revfile)
        if channels(rsnd) == @reverb_channels and srate(rsnd) == @srate.round
          @reverb_channels.times do |c| set_frames(0, rsnd, c) end
        else
          remove_file(@revfile)
          rsnd = new_sound(@revfile, @header_type, @data_format, @srate.round, @reverb_channels)
        end
      else
        rsnd = new_sound(@revfile, @header_type, @data_format, @srate.round, @reverb_channels)
      end
      if chn.kind_of?(Integer)
        if swap
          swap_channels(snd, 0, snd, chn)
        end
        vct2channel(vct_scale!(channel2vct(0, len, snd, 0), @reverb_amount), 0, len, rsnd, 0)
      else
        @channels.times do |c|
          vct2channel(vct_scale!(channel2vct(0, len, snd, c), @reverb_amount), 0, len, rsnd, c)
        end
      end
      @ws_reverb = rsnd
      run_sound_reverb
      if swap
        swap_channels(snd, chn, snd, 0)
      end
    end
    @reverb_channels = @channels
    if @statistics
      set_process_time
      statistics
    end
    finish_sound
    1.upto(@play) do play_it end
    set_mus_srate(@old_srate)
  end

  protected
  def before_output
    if @output
      if snd = find_sound(@output)
        unless @continue
          if channels(snd) == @channels and srate(snd) == @srate.round
            set_comment(snd, @comment)
            @channels.times do |chn| set_frames(0, snd, chn) end
          else
            revert_sound(snd)
            close_sound_extend(snd)
            snd = new_sound(@output, @header_type, @data_format, @srate.to_i, @channels, @comment)
          end
        end
      else
        snd = new_sound(@output, @header_type, @data_format, @srate.to_i, @channels, @comment)
      end
    else
      snd = false
    end
    $rbm_output = @ws_output = snd
    # recursive call: :reverb, [:old_reverb, @ws_reverb]
    unless @ws_reverb
      if @reverb_channels.nonzero?
        if rsnd = find_sound(@revfile)
          unless @continue
            if channels(rsnd) == @reverb_channels and srate(rsnd) == @srate.round
              @reverb_channels.times do |chn| set_frames(0, rsnd, chn) end
            else
              remove_file(@revfile)
              rsnd = new_sound(@revfile, @header_type, @data_format, @srate.round, @reverb_channels)
            end
          end
        else
          rsnd = new_sound(@revfile, @header_type, @data_format, @srate.round, @reverb_channels)
        end
      else
        rsnd = false
      end
      $rbm_reverb = @ws_reverb = rsnd
    end
    @start = frames(@ws_output) / @srate
  end

  def after_output
    @dur = frames(@ws_output) / @srate
  end

  def after_reverb
    if @reverb == :intern or @reverb == :n_rev
      amount   = get_args(@reverb_data, :amount, 0.1)
      filter   = get_args(@reverb_data, :filter, 0.5)
      feedback = get_args(@reverb_data, :feedback, 1.09)
      set_reverb_control?(true, @ws_output)
      set_reverb_control_scale(amount, @ws_output)
      set_reverb_control_lowpass(filter, @ws_output)
      set_reverb_control_feedback(feedback, @ws_output)
      # apply_controls(snd = false, choice = 0, beg = 0, dur = len)
      # coice 0: sound
      #       1: channel
      #       2: selection
      apply_controls(@ws_output, @reverb_channels, seconds2samples(@start), seconds2samples(@dur))
      restore_controls
    end
  end

  def scaled_to_sound(scl, beg, len)
    @channels.times do |chn| scale_sound_to(scl, beg, len, @ws_output, chn) end
  end

  def scaled_by_sound(scl, beg, len)
    @channels.times do |chn| scale_channel(scl, beg, len, @ws_output, chn) end
  end

  def statistics
    super(frames(@ws_output), data_format(@ws_output), header_type(@ws_output))
    message(" max out: %s", maxamp(@ws_output, true).to_string)
    if @reverb
      message(" max rev: %s", maxamp(@ws_reverb, true).to_string)
    end
  end

  def finish_sound
    super
    save_sound(@ws_output) if @save_after
    select_sound(@ws_output)
  end

  def play_it
    play(0, @ws_output)
  end

  def ws_location
    frames(@ws_output)
  end

  # with_closed_sound(snd) do |snd_name| ... end
  # returns new snd index
  def with_closed_sound(snd, &body)
    snd_name = file_name(snd)
    save_sound(snd)
    close_sound(snd)
    body.call(snd_name)
    open_sound(snd_name)
  end
end

class With_CLM < CLM_Instrument
  def initialize(*args, &body)
    @clm = true
    super
  end

  def run_instrument(start, dur, *locsig_args, &body)
    loc = super
    each_sample(start + @offset, dur) do |samp|
      locsig(loc, samp, body.call(samp))
    end
  end

  def run_reverb(start, dur, chan = 0, &body)
    @ws_reverb = $rbm_reverb
    case chan
    when Integer
      super(start, dur, chan)
      each_sample(start, dur) do |samp|
        frame2file(@ws_output, samp, body.call(in_any(samp, chan, @ws_reverb), samp))
      end
    when :frames
      super(start, dur, 0)
      frm = make_frame(@reverb_channels)
      each_sample(start, dur) do |samp|
        frame2file(@ws_output, samp, body.call(file2frame(@ws_reverb, samp, frm), samp))
      end
    end
  end

  def rbm_mix(filename, *args)
    doc("rbm_mix(filename, *args)
        :input_frame,  0
        :output_frame, 0
        :frames,       mus_sound_frames(filename)
        :scale,        1.0
Example: rbm_mix(\"tmp\")\n") if filename == :help
    input_frame  = get_args(args, :input_frame, 0).round
    output_frame = get_args(args, :output_frame, 0).round
    frms         = get_args(args, :frames, mus_sound_frames(filename)).round
    scale        = get_args(args, :scale, 1.0).to_f
    with_closed_output do
      if provided? "snd"
        if scale.nonzero?
          unless (snd = find_sound(filename))
            snd = open_sound(filename)
          end
          channels(snd).times do |chn| scale_channel(scale, input_frame, frms, snd, chn) end
          save_sound(snd)
          close_sound_extend(snd)
        end
        mus_mix(@output, filename, output_frame, frms, input_frame)
      else
        mus_mix(@output, filename, output_frame, frms, input_frame,
                make_mixer(@channels, *(1..@channels * @channels).map do scale end))
      end
    end
  end
  
  protected
  def before_output
    if @continue
      @ws_output = continue_sample2file(@output)
      @ws_reverb = continue_sample2file(@revfile) if @reverb_channels.nonzero?
    else
      remove_file(@output)
      @ws_output = make_sample2file(@output, @channels, @data_format, @header_type, @comment)
      # recursive call: :reverb, [:old_reverb, @ws_reverb]
      unless @ws_reverb
        if @reverb_channels.nonzero?
          remove_file(@revfile)
          @ws_reverb = make_sample2file(@revfile, @reverb_channels, @data_format, @header_type)
        end
      end
    end
    with_closed_output do
      @start = mus_sound_duration(@output)
    end
    $rbm_output = @ws_output
    $rbm_reverb = @ws_reverb
  end

  def after_output
    if provided?("snd") and (snd = find_sound(@output))
      save_sound(snd)
      update_sound(snd)
    end
    with_closed_output do
      @dur = mus_sound_duration(@output)
    end
  end
  
  def after_reverb
    mus_close(@ws_reverb)
    if provided? "snd"
      if snd = find_sound(@output)
        update_sound(snd)
      else
        open_sound(@output)
      end
    end
  end

  def scaled_to_sound(scl, beg, len)
    if provided? "snd"
      unless snd = find_sound(@output)
        snd = open_sound(@output)
      end
      @channels.times do |chn| scale_sound_to(scl, beg, len, snd, chn) end
    else
      amax = mus_sound_maxamp(@output)
      tmpa = []
      1.step(amax.length - 1, 2) do |i| tmpa << amax[i] end
      scl = [1.0, scl].min
      scl = [0.0, scl].max
      scale = (scl / tmpa.max) - 1
      mus_mix(@output, @output, beg, len, beg,
              make_mixer(@channels, *(1..@channels * @channels).map do scale end))
    end
  end

  def scaled_by_sound(scl, beg, len)
    if provided? "snd"
      unless (snd = find_sound(@output))
        snd = open_sound(@output)
      end
      @channels.times do |chn| scale_channel(scl, beg, len, snd, chn) end
    else
      scale = scl - 1
      mus_mix(@output, @output, beg, len, beg,
              make_mixer(@channels, *(1..@channels * @channels).map do scale end))
    end
  end
  
  def statistics
    super(mus_sound_frames(@output), mus_sound_data_format(@output), mus_sound_header_type(@output))
    str = ""
    mus_sound_maxamp(@output).each_pair do |s, v|
      str << format("%1.3f (%1.3fs), ", v, s / @srate)
    end
    message(" max out: [%s]", str[0..-3])
    if @reverb
      str = ""
      mus_sound_maxamp(@revfile).each_pair do |s, v|
        str << format("%1.3f (%1.3fs), ", v, s / @srate)
      end
      message(" max rev: [%s]", str[0..-3])
    end
  end
  
  def finish_sound
    super
    mus_close(@ws_output) if @ws_output
    if provided? "snd"
      if @ws_output = find_sound(@output)
        select_sound(@ws_output)
      else
        @ws_output = open_sound(@output)
      end
    end
  end

  def play_it
    if provided? "snd"
      play(0, @ws_output)
    else
      system(format("%s %s", @player, @output))
    end
  end

  def ws_location
    with_closed_output do
      mus_sound_frames(@output)
    end
  end

  def with_closed_output(&body)
    mus_close(@ws_output) if @ws_output
    ret = body.call
    $rbm_output = @ws_output = continue_sample2file(@output)
    ret
  end
end

class With_DAC < Snd_Instrument
  # handles no reverb
  def initialize(*args, &body)
    @clm = false
    super
    @bufsize = get_args(args, :bufsize, $rbm_rt_bufsize)
    @device  = get_args(args, :device, $rbm_output_device)
    @ws_reverb = @ws_output = @reverb = false
    @output = "dac"
    @start_dac = nil
  end

  def run_instrument(start, dur, *args, &body)
    loc = super
    len = seconds2samples(dur)
    bufsize = [len, @bufsize].min
    dac_vct  = make_vct(bufsize)
    dac_data = make_sound_data(@channels, bufsize)
    while (Time.now - @start_dac) < start
      ws_interrupt?
    end
    ws_interrupt?
    -1.step(len - 2, bufsize) do |samp|
      th = Thread.new do
        vct_map!(dac_vct, lambda do | | body.call(samp += 1) end)
        @channels.times do |chn|
          vct2sound_data(vct_scale!(vct_copy(dac_vct), locsig_ref(loc, chn)), dac_data, chn)
        end
      end
      th.alive? and th.join
      Thread.new(dac_data, bufsize) do |d, s|
        mus_audio_write(@ws_output, d, s)
      end
    end
  end

  protected
  def before_output
    @ws_output = mus_audio_open_output(@device, @srate.round, @channels, @data_format,
                                       @bufsize * @channels * 2)
    if @ws_output < 0
      ws_error("%s#%s: can't open DAC (%s)", self.class, get_func_name, @ws_output.inspect)
    end
    @start_dac = Time.now
  end

  def after_output
    mus_audio_close(@ws_output) if @ws_output.kind_of?(Numeric)
    @ws_output = false
  end

  def finish_sound
    after_output
  end
  
  def statistics
    super(0, false, false)
  end
end

include WS

# ws.rb ends here
