# marks.rb -- marks.scm --> marks.rb -*- snd-ruby -*-

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Created: Wed Mar 23 02:08:47 CET 2005
# Changed: Tue Oct 17 21:40:44 CEST 2006

# Commentary:
#
# examples of mark-related functions
#
# module Mark
#  mark_name2id(name)
#  move_syncd_marks(sync, diff)
#  describe_mark(id)
#
#  class Mark_sync
#   initialize
#   start_sync
#   stop_sync
#   click_to_sync(id)
#
#  syncup(ids)
#  fit_selection_between_marks(m1, m2)
#  pad_marks(ids, secs)
#  play_syncd_marks(sync)
#  play_between_marks(mark1 = false, mark2 = false)
#
#  class Mark_report
#   initialize(snd)
#   report_mark_names_play_hook(size)
#   report_mark_names_stop_playing_hook(snd)
#   
#  report_mark_names
#  eval_between_marks(&func)
#  snap_marks
#  define_selection_via_marks(m1, m2)
#  snap_mark_to_beat
#  mark_explode(htype = Mus_next, dformat = Mus_bfloat)
#
#  mark_properties(id)
#  set_mark_properties(id, new_val)
#  mark_property(id, key)
#  set_mark_property(id, key, val)
#  save_mark_properties
#  mark_click_info(id)
#  eval_header(sndf)
#  marks2string(sndf)
#  

# Code:

require "examp"
require "hooks"

module Mark
  # mark_name2id is a global version of find-mark

  add_help(:mark_name2id,
           "mark_name2id(name) is like find-mark but searches all currently accessible channels")
  def mark_name2id(name)
    callcc do |ret|
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          if mark?(m = find_mark(name, snd, chn))
            ret.call(m)
          end
        end
      end
      :no_such_mark
    end
  end

  # move_syncd_marks moves all syncd marks together

  add_help(:move_syncd_marks,
           "move_syncd_marks(sync, diff) moves all marks sharing sync by diff samples")
  def move_syncd_marks(sync, diff)
    (syncd_marks(sync) or []).each do |m| set_mark_sample(m, mark_sample(m) + diff) end
  end

  # describe_mark shows mark history

  add_help(:describe_mark,
           "describe_mark(id) \
returns a description of the movements of mark id over the channel's edit history")
  def describe_mark(id)
    if (mark_setting = Snd.catch do mark_home(id) end.first) == :no_such_mark
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          max_edits = 0
          edits(snd, chn).each do |n| max_edits += n end
          0.upto(max_edits) do |ed|
            if (m = marks(snd, chn, ed)) and m.member?(id)
              mark_setting = [snd, chn]
              break
            end
          end
        end
      end
    end
    if array?(mark_setting)
      snd, chn = mark_setting
      max_edits = 0
      edits(snd, chn).each do |n| max_edits += n end
      descr = [[:mark, id, :sound, snd, short_file_name(snd), :channel, chn]]
      0.upto(max_edits) do |ed|
        if marks(snd, chn, ed).member?(id)
          descr.push(mark_sample(id, ed))
        else
          descr.push(false)
        end
      end
      descr
    else
      Snd.raise(:no_such_mark, id)
    end
  end

  # click marks between start-sync and stop-sync to sync them together
  # (easier than calling mark-sync over and over by hand)
  class Mark_sync
    def initialize
      @mark_sync_number = 0
    end

    def start_sync
      @mark_sync_number += 1
    end

    def stop_sync
      @mark_sync_number = 0
    end

    def click_to_sync(id)
      mark_sync(id, @mark_sync_number)
      false
    end
  end
  # ms = Mark_sync.new
  # $mark_click_hook.add_hook!("marks") do |id| ms.click_to_sync(id) end

  # syncronize sounds at a given mark

  add_help(:syncup,
           "syncup(*ids) \
pads the channels with zeros so that all the marks in ids list occur at the same time")
  def syncup(*args_ids)
    ids = args_ids.flatten
    samps = ids.map do |id| mark?(id) and mark_sample(id) end
    max_samp = samps.max
    ids.zip(samps) do |id, samp|
      if samp < max_samp
        nsamps = max_samp - samp
        snd, chn = mark_home(id)
        insert_samples(0, nsamps, Vct.new(nsamps), snd, chn)
      end
    end
  end

  # fit selection between marks, expanding via granulate (this needs some tweaking...)

  add_help(:fit_selection_between_marks,
           "fit_selection_between_marks(m1, m2) \
fits (and mixes) the current selection (via granulate) between the given marks")
  def fit_selection_between_marks(m1, m2)
    m1_samp = mark_sample(m1)
    m2_samp = mark_sample(m2)
    m1_home = mark_home(m1)
    m2_home = mark_home(m2)
    if m1_home != m2_home
      Snd.display("mark %s is in %s[%s] but mark %s is in %s[%s]?",
                  m1, m1_home[0], m1_home[1], m2, m2_home[0], m2_home[1])
    else
      mark_samps = m2_samp - m1_samp
      selection_samps = selection_frames
      reg_data = region2vct
      reader = make_sample_reader(m1_samp)
      gr = make_granulate(:expansion, mark_samps / selection_samps.to_f)
      inctr = 0
      new_data = Vct.new(mark_samps) do
        next_sample(reader) + granulate(gr,
                                        lambda do |dir|
                                          if inctr >= selection_samps
                                            0.0
                                          else
                                            val = reg_data[inctr]
                                            inctr += dir
                                            val
                                          end
                                        end)
      end
      free_sample_reader(reader)
      vct2channel(new_data, m1_samp, mark_samps, m1_home[0], m1_home[1])
    end
  end

  # pad_marks inserts silence before each in a list of marks

  add_help(:pad_marks,
           "pad_marks(ids, secs) inserts secs seconds of silence before each mark in ids")
  def pad_marks(ids, secs)
    silence_length = (secs * srate()).floor
    silence_samps = Vct.new(silence_length)
    as_one_edit_rb(get_func_name) do
      (ids or []).each do |id|
        samp = [0, mark_sample(id) - 1].max
        snd, chn = mark_home(id)
        insert_samples(samp, silence_length, silence_samps, snd, chn)
      end
    end
  end

  # play_syncd_marks

  add_help(:play_syncd_marks,
           "play_syncd_marks(sync) starts playing from all marks sharing sync")
  def play_syncd_marks(sync)
    chans = 1
    rate = 22050
    (syncd_marks(sync) or []).each do |m|
      snd, chn = mark_home(m)
      new_player = make_player(snd, chn)
      add_player(new_player, mark_sample(m))
      chans = [chans, chn + 1].max
      rate = [rate, srate(snd)].max
    end
    start_playing(chans, rate)
  end

  add_help(:play_between_marks,
           "play_between_marks([mark1=false, [mark2=false]]) \
plays the portion between the marks (searching for plausible default marks)")
  def play_between_marks(mark1 = false, mark2 = false)
    snd = Snd.snd
    chn = Snd.chn
    m1 = if mark1
           mark1
         else
           if ms = marks(snd, chn)
             callcc do |ret|
               ms.each do |m|
                 if mark_sample(m) >= left_sample(snd, chn)
                   ret.call(m)
                 end
               end
               false
             end
           else
             Snd.display("no marks in current window?")
             false
           end
         end
    m2 = if mark?(m1)
           if mark2
             mark2
           else
             if ms = marks(snd, chn)
               callcc do |ret|
                 ms.each do |m|
                   if mark_sample(m) > mark_sample(m1)
                     ret.call(m)
                   end
                 end
               end
             else
               Snd.display("no second mark?")
               false
             end
           end
         else
           false
         end
    if mark?(m1) and mark?(m2)
      pos1 = mark_sample(m1)
      pos2 = mark_sample(m2)
      beg = [pos1, pos2].min
      len = [pos1, pos2].max
      play(beg, mark_home(m1).car, mark_home(m1).cadr, false, len)
    end
  end

  class Mark_report
    def initialize(snd)
      @snd = snd
      @marklist = marks(@snd, 0)
      @samplist = (@marklist or []).map do |m| mark_sample(m) end
      @samp = 0
    end
    
    def report_mark_names_play_hook(size)
      @samp += size
      if array?(@samplist) and @samp >= @samplist.first
        report_in_minibuffer(mark_sample(@marklist.first), @snd)
        @marklist.unshift
        @samplist.unshift
      end
    end

    def report_mark_names_stop_playing_hook(snd)
      report_in_minibuffer("", snd)
      $play_hook.remove_hook("report-mark-names-play")
      $stop_playing_hook.remove_hook("report-mark-names-stop-playing")
    end
  end

  # report_mark_names causes mark names to be posted in the minibuffer as a sound is played

  add_help(:report_mark_names,
           "report_mark_names() causes mark names to be printed as they are passed while playing")
  def report_mark_names
    $start_playing_hook.add_hook!("marks.rb") do |snd|
      rp = Mark_report.new(snd)
      $stop_playing_hook.add_hook!("report-mark-names-stop-playing") do |s|
        rp.report_mark_names_stop_playing_hook(s)
      end
      $play_hook.add_hook!("report-mark-names-play") do |samps|
        rp.report_mark_names_play_hook(samps)
      end
    end
  end

  # eval_between_marks

  add_help(:eval_between_marks,
           "eval_between_marks(&func) \
evaluates func between the leftmost marks; func takes one arg, the original sample")
  def eval_between_marks(func1 = nil, &func2)
    func = if block_given?
             func2
           else
             func1
           end
    if proc?(func)
      snd = Snd.snd
      chn = Snd.chn
      if chn < 0
        chn = 0
      end
      if array?(mlist = marks(snd, chn)) and mlist.length > 1
        left_samp = left_sample(snd, chn)
        winl = []
        mlist.each_with_index do |n, i|
          if mark_sample(n) > left_samp
            winl = mlist[i..-1]
            break
          end
        end
        if array?(winl) and winl.length > 1
          beg = mark_sample(winl[0])
          len = mark_sample(winl[1]) - beg
          old_data = channel2vct(beg, len, snd, chn)
          new_data = Vct.new(len) do |i| func.call(old_data[i]) end
          vct2channel(new_data, beg, len, snd, chn)
        end
      else
        report_in_minibuffer("need 2 marks")
      end
    end
  end
  # bind_key(?m, 0, lambda do | | prompt_in_minibuffer("mark eval:", eval_between_marks) end)

  # snap_marks

  add_help(:snap_marks, "snap_marks() places marks at current selection boundaries")
  def snap_marks
    if selection?
      selection_members.each do |snd, chn|
        pos = selection_position(snd, chn)
        len = selection_frames(snd, chn)
        add_mark(pos, snd, chn)
        add_mark(pos + len, snd, chn)
      end
    end
  end

  # define_selection_via_marks

  add_help(:define_selection_via_marks,
           "define_selection_via_marks(m1, m2) \
defines the current selection to lie between the marks given")
  def define_selection_via_marks(m1, m2)
    m1sc = mark_home(m1)
    m2sc = mark_home(m2)
    if m1sc != m2sc
      Snd.raise(:snd_error, "define_selection_via_marks assumes the marks are in the same channel")
    else
      beg = [mark_sample(m1), mark_sample(m2)].min
      fin = [mark_sample(m1), mark_sample(m2)].max
      snd, chn = m1sc
      if selection?
        set_selection_member?(false, true)
      end
      set_selection_member?(true, snd, chn)
      set_selection_position(beg, snd, chn)
      set_selection_frames(fin - beg + 1, snd, chn)
    end
  end

  # snap_mark_to_beat

  add_help(:snap_mark_to_beat,
           "snap_mark_to_beat() \
ensures that when a mark is dragged, its released position is always on a beat")
  def snap_mark_to_beat
    mark_release = 4
    $mark_hook.add_hook!(get_func_name) do |m, snd, chn, reason|
      if reason == mark_release
        samp = mark_sample(m)
        bps = beats_per_minute(snd, chn) / 60.0
        sr = srate(snd)
        beat = ((samp * bps) / sr).floor.to_f
        lower = ((beat * sr) / bps).floor
        higher = (((beat + 1.0) * sr) / bps).floor
        set_mark_sample(m, if (samp - lower) < (higher - samp)
                             lower
                           else
                             higher
                           end)
        
      end
    end
  end

  # mark_explode
  # write out each section of a file between marks as a separate file

  add_help(:mark_explode,
           "mark_explode([header_type=Mus_next, [data_format=Mus_bfloat]]) \
splits a sound into a bunch of sounds based on mark placements")
  def mark_explode(htype = Mus_next, dformat = Mus_bfloat)
    start = 0
    file_ctr = 0
    snd = Snd.snd
    if marks(snd)
      marks(snd).first.each do |m|
        last = mark_sample(m)
        if last > start
          filename = format("mark-%d.snd", file_ctr)
          file_ctr += 1
          channels(snd).times do |chn|
            set_selection_member?(true, snd, chn)
            set_selection_position(start, snd, chn)
            set_selection_frames(last - start, snd, chn)
          end
          save_selection(filename, :header_type, htype, :data_format, dformat, :srate, srate(snd))
          channels(snd).times do |chn|
            set_selection_member?(false, snd, chn)
          end
        end
        start = last
      end
    end
    update_time_graph(snd)
  end
  
  #
  # === Mark Properties ===
  #
  $all_mark_properties = Array.new

  def mark_properties(id)
    property(id, :mark_property)
  end

  def set_mark_properties(id, new_val)
    set_property(id, :mark_property, new_val)
  end

  def remove_mark_properties(id)
    if hash?(mark_properties(id))
      properties.delete(id)
      $all_mark_properties.delete(id)
    end
  end

  add_help(:mark_property,
           "mark_property(id, key) \
returns the value associated with 'key' in the given mark's property list, or false")
  def mark_property(id, key)
    Snd.raise(:no_such_mark, id) unless mark?(id)
    hash?(h = mark_properties(id)) and h[key]
  end

  add_help(:set_mark_property,
           "set_mark_property(id, key, val) \
sets the value 'val' to 'key' in the given mark's property list")
  def set_mark_property(id, key, val)
    Snd.raise(:no_such_mark, id) unless mark?(id)
    unless hash?(h = mark_properties(id)) and h.store(key, val)
      $all_mark_properties.push(id)
      set_mark_properties(id, {key, val})
    end
  end

  add_help(:remove_mark_property,
           "remove_mark_property(key, id) \
removes the key-value pair in the given mark's property list")
  def remove_mark_property(key, id)
    Snd.raise(:no_such_mark, id) unless mark?(id)
    if hash?(h = mark_properties(id))
      h.delete(key)
    else
      $all_mark_properties.delete(id)
    end
  end
  
=begin  
  $close_hook.add_hook!("remove-mark-properties") do |snd|
    $all_mark_properties.each do |id| (not mark?(id)) and remove_mark_properties(id) end
  end
=end

  add_help(:save_mark_properties,
           "save_mark_properties() \
sets up an $after_save_state_hook function to save any mark-properties")
  def save_mark_properties
    $after_save_state_hook.add_hook!(get_func_name) do |fname|
      File.open(File.expand_path(fname), "a+") do |f|
        f.printf("\n# from %s in %s\n", get_func_name, __FILE__)
        f.printf("require \"marks.rb\"\n")
        (marks or []).each do |snds|
          (snds or []).each do |chns|
            (chns or []).each do |m|
              if mp = mark_properties(m)
                snd, chn = mark_home(m)
                msamp = mark_sample(m)
                f.printf("if sound?(snd = find_sound(%s))\n", file_name(snd).inspect)
                f.printf("  if mark?(m = find_mark(%d, snd, %d))\n", msamp, chn)
                f.printf("    set_mark_properties(m, %s)\n", mp.inspect)
                f.printf("  end\n")
                f.printf("end\n")
              end
            end
          end
        end
      end
    end
  end

  # 
  # === Mark Click Info ===
  # 
  add_help(:mark_click_info,
           "mark_click_info(n) \
is a $mark_click_hook function that describes a mark and its properties")
  def mark_click_info(id)
    Snd.raise(:no_such_mark, id) unless mark?(id)
    info_dialog("Mark info", format("\
     mark id: %d%s
      sample: %d (%1.3f secs)%s%s",
                                    id,
                                    ((s = mark_name(id)) ?
                                     format("\n   mark name: %s", s.inspect) : ""),
                                    mark_sample(id),
                                    mark_sample(id) / srate(mark_home(id)[0]).to_f,
                                    (mark_sync(id).nonzero? ?
                                     format("\n        sync: %d", mark_sync(id)) : ""),
                                    ((props = mark_properties(id)) ?
                                     format("\n  properties: %s", props.inspect) : "")))
    true
  end

  # this code saves mark info in the sound file header, and reads it
  # back in when the sound is later reopened

  def marks2string(sndf)
    str = "require \"marks\"\n"
    (marks(sndf) or []).each_with_index do |chan_marks, chn|
      (chan_marks or []).each do |m|
        str += format("m = add_mark(%s, false, %d, %s, %d)\n",
                      mark_sample(m),
                      chn,
                      mark_name(m).null? ? false : mark_name(m),
                      mark_sync(m))
        if props = mark_properties(m)
          str += format("set_mark_properties(m, %s)\n", props.inspect)
        end
      end
    end
    str
  end
  # $output_comment_hook.add_hook!("marks2string") do |str| marks2string(selected_sound()) end
  # $after_open_hook.add_hook!("marks2string") do |snd|
  #   if string?(str = comment(snd))
  #     Snd.catch do eval(str, TOPLEVEL_BINDING, "(eval-header)", 1) end.first
  #   end
  # end
end

include Mark


# marks.rb ends here
