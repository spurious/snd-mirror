# popup.rb -- Specialize Popup Menus converted from Guile to Ruby.

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Sat Dec 27 17:15:01 CET 2003

# Requires the Motif module (xm.so) or --with-static-xm!

# These hook variables are used
#
#       $after_open_hook
#       $stop_playing_hook
#       $stop_playing_selection_hook

# If you have a $nb_database file (see nb.rb/nb.scm) you can set the
# path in your ~/.snd_ruby.rb file before loading popup.rb to use it.

require "English"
begin
  require "libxm" unless $LOADED_FEATURES.member?("xm")
rescue
  snd_error "popup.rb needs Motif module"
end
require "examp"
require "hooks"

$nb_database ||= nil

def string2compound(str) RXmStringCreateLocalized(str); end

def compound2string(xstr) RXmStringGetLtoR(xstr, "")[1]; end

def get_xtvalue(widget, item) RXtVaGetValues(widget, [item, 0])[1]; end

def for_each_child(w, func = nil)
  doc("for_each_child(w, func)
Applies FUNC to W and each of its children.\n") if w == :help
  func.call(w)
  get_xtvalue(w, RXmNchildren).each do |n| for_each_child(n, func) end if RXtIsComposite(w)
end

def change_label(w, new_label = nil)
  doc("change_label(w, new_label)
changes widget W's label to be NEW_LABEL\n") if w == :help
  RXtVaSetValues(w, [RXmNlabelString, string2compound(new_label)])
end

def current_label(w)
  doc("current_label(w)
returns widget W's label\n") if w == :help
  compound2string(get_xtvalue(w, RXmNlabelString))
end

def make_popup_menu(name, parent = nil, top_args = [], entries = [])
  doc("make_popup_menu(name, parent, top_args, entries)
creates a popup menu\n") if name == :help
  menu = RXmCreatePopupMenu(parent, name, top_args)
  entries.each do |entry|
    widget = RXtCreateManagedWidget(entry[0], entry[1], menu, entry[2])
    if entry.length > 3
      RXtAddCallback(widget, RXmNactivateCallback, entry[3])
      entry[4].call(widget) if entry.length > 4
    end
  end
  menu
end

$selection_popup_menu = lambda do
  every_menu = [RXmNbackground, highlight_color()]
  stopping = stopping1 = false
  stop_widget = stop_widget1 = nil
  selctr = 0
  $stop_playing_selection_hook.add_hook!("popup_stop_selection_hook") do | |
    if stopping
      stopping = false
      change_label(stop_widget, "Play") if RWidget?(stop_widget)
    end
  end
  make_popup_menu("selection_popup",
                  main_widgets()[2],
                  [RXmNpopupEnabled, true, RXmNbackground, highlight_color()],
                  [["Selection", RxmLabelWidgetClass, every_menu],
                   ["sep", RxmSeparatorWidgetClass, every_menu],
                   ["Play", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      if stopping
                        stopping = false
                        change_label(w, "Play")
                        if stopping1
                          stopping1 = false
                          change_label(stop_widget1, "Loop play")
                          $stop_playing_selection_hook.remove_hook!("popup_play_selection")
                        end
                        stop_playing()
                      else
                        change_label(w, "Stop")
                        stop_widget = w
                        stopping = true
                        play_selection()
                      end
                    end],
                   ["Loop play", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      if stopping1
                        stopping1 = false
                        change_label(w, "Loop play")
                        $stop_playing_selection_hook.remove_hook!("popup_play_selection")
                        if stopping
                          stopping = false
                          change_label(stop_widget, "Play")
                        end
                        stop_playing()
                      else
                        change_label(w, "Stop!")
                        stop_widget1 = w
                        stopping1 = true
                        $stop_playing_selection_hook.add_hook!("popup_play_selection") do | |
                          play_selection()
                        end
                        play_selection()
                      end
                    end],
                   ["Delete", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| delete_selection() end],
                   ["Zero", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| scale_selection_by(0.0) end],
                   ["Crop", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      sndlist = []
                      sounds().each do |snd|
                        (channels(snd) - 1).downto(0) do |i|
                          sndlist.unshift([snd, i]) if selection_member?(snd, i)
                        end
                      end
                      sndlist.each do |selection|
                        as_one_edit(lambda do | |
                                      snd = selection[0]
                                      chn = selection[1]
                                      beg = selection_position(snd, chn)
                                      len = selection_frames(snd, chn)
                                      delete_samples(0, beg, snd, chn) if beg > 0
                                      if len < frames(snd, chn)
                                        delete_samples(len + 1, frames(snd, chn) - len, snd, chn)
                                      end
                                    end)
                      end
                    end],
                   ["Save as", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| edit_save_as_dialog() end],
                   ["Copy->New", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      new_file_name = "newf-#{selctr}.snd"
                      selctr += 1
                      save_selection(new_file_name)
                      open_sound(new_file_name)
                    end],
                   ["Cut->New", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      new_file_name = "newf-#{selctr}.snd"
                      selctr += 1
                      save_selection(new_file_name)
                      delete_selection()
                      open_sound(new_file_name)
                    end],
                   ["Snap marks", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      selection_members().each do |snd, chn|
                        pos = selection_position(snd, chn)
                        len = selection_frames(snd, chn)
                        add_mark(pos, snd, chn)
                        add_mark(pos + len, snd, chn)
                      end
                    end],
                   ["Unselect", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| set_selection_member?(false, true) end],
                   ["Revert", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| reverse_selection() end],
                   ["Mix", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| mix_selection(cursor()) end],
                   ["Invert", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| scale_selection_by(-1) end]])
end.call

$graph_popup_snd = nil
$graph_popup_chn = nil

$graph_popup_menu = lambda do
  every_menu = [RXmNbackground, highlight_color()]
  stopping = false
  stop_widget = nil
  $stop_playing_hook.add_hook!("popup_change_label_play2stop") do |snd|
    if stopping
      stopping = false
      change_label(stop_widget, "Play") if RWidget?(stop_widget)
    end
  end
  make_popup_menu("graph_popup",
                  main_widgets()[2],
                  [RXmNpopupEnabled, true, RXmNbackground, highlight_color()],
                  [["Snd", RxmLabelWidgetClass, every_menu],
                   ["sep", RxmSeparatorWidgetClass, every_menu],
                   ["Play", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      if stopping
                        stopping = false
                        change_label(w, "Play")
                        stop_playing()
                      else
                        change_label(w, "Stop")
                        stopping = true
                        play(0, $graph_popup_snd)
                      end
                    end, lambda do |wid| stop_widget = wid end],
                   ["Play channel", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      stopping = true
                      change_label(stop_widget, "Stop")
                      play(0, $graph_popup_snd, $graph_popup_chn)
                    end],
                   ["Play from cursor", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      stopping = true
                      change_label(stop_widget, "Stop")
                      play(cursor($graph_popup_snd, $graph_popup_chn), $graph_popup_snd)
                    end],
                   ["Play previous", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      stopping = true
                      change_label(stop_widget, "Stop")
                      play(0, $graph_popup_snd, $graph_popup_chn, false, false,
                           edit_position() - 1)
                    end],
                   ["Play original", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      stopping = true
                      change_label(stop_widget, "Stop")
                      play(0, $graph_popup_snd, $graph_popup_chn, false, false, 0)
                    end],
                   ["Undo", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| undo_edit(1, $graph_popup_snd, $graph_popup_chn) end],
                   ["Redo", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| redo_edit(1, $graph_popup_snd, $graph_popup_chn) end],
                   ["Revert", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| revert_sound($graph_popup_snd) end],
                   ["Save", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| save_sound($graph_popup_snd) end],
                   ["Save as", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      select_sound($graph_popup_snd)
                      file_save_as_dialog()
                    end],
                   ["Close", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| close_sound($graph_popup_snd) end],
                   ["Mix selection", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      mix_selection(cursor($graph_popup_snd, $graph_popup_chn),
                                    $graph_popup_snd, $graph_popup_chn)
                    end],
                   ["Insert selection", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      insert_selection(cursor($graph_popup_snd, $graph_popup_chn),
                                       $graph_popup_snd, $graph_popup_chn)
                    end],
                   ["Replace with selection", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      snd = $graph_popup_snd
                      chn = $graph_popup_chn
                      beg = cursor(snd, chn)
                      len = selection_frames()
                      sbeg = selection_position()
                      if (not selection_member?(snd, chn)) or
                         ((beg + len) < sbeg) or
                         (beg > (sbeg + len))
                        delete_samples(beg, len, snd, chn)
                        insert_selection(beg, snd, chn)
                      elsif beg < sbeg
                        delete_samples(beg, sbeg - beg, snd, chn)
                      end
                    end],
                   ["Select all", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| select_all($graph_popup_snd, $graph_popup_chn) end],
                   ["Unselect", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| set_selection_member?(false, true) end],
                   ["Equalize panes", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| equalize_panes() end],
                   ["Info", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      ptr = if $nb_database
                              DBM.open($nb_database) rescue warn("DBM.open(#{$nb_database}")
                            else
                              nil
                            end
                      snd = $graph_popup_snd
                      chn = $graph_popup_chn
                      file = file_name(snd)
                      max_a = maxamp(snd, true)
                      comm = comment(snd)
                      loops = mus_sound_loop_info(file)
                      ftime = Time.at(mus_sound_write_date(file))
                      date = ftime.localtime.strftime("%a %d-%b-%y %H:%M %Z")
                      notes = ((ptr and ptr.key?(file)) ? ptr.fetch(file) : "")
                      info_dialog("#{file} info",
                                  format("\
  chans: %d, srate: %d
 length: %.3f (%d samples)
 format: %s [%s]
 maxamp: %s
written: %s
%s%s%s\n%s",
                                         chans(snd), srate(snd),
                                         frames(snd, chn) / srate(snd).to_f,
                                         mus_sound_frames(file),
                                         mus_data_format_name(data_format(snd)),
                                         mus_header_type_name(header_type(snd)),
                                         max_a.map do |x| ("%.3f" % x).to_f end.inspect, date,
                                         comm.empty? ? "" : "comment: #{comm}\n",
                                         loops ? "   loop: #{loops.inspect}\n" : "",
                                         (soundfont_info() or ""),
                                         (notes or "")))
                      ptr.close if ptr
                    end],
                   ["Add mark", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      add_mark(cursor($graph_popup_snd, $graph_popup_chn),
                               $graph_popup_snd, $graph_popup_chn)
                    end],
                   ["Delete mark", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      ms = marks($graph_popup_snd, $graph_popup_chn)
                      id = if ms.empty?
                             false
                           elsif ms.length == 1
                             ms[0]
                           else
                             loc = cursor()
                             find_closest_mark = lambda do |lst, cur_min, cur_id|
                               if (not lst) or lst.empty?
                                 cur_id
                               else
                                 this_id = lst[0]
                                 this_min = (loc - mark_sample(this_id)).abs
                                 if this_min < cur_min
                                   find_closest_mark.call(lst[1..-1], this_min, this_id)
                                 else 
                                   find_closest_mark.call(lst[1..-1], cur_min, cur_id)
                                 end
                               end
                             end
                             find_closest_mark.call(ms[1..-1],
                                                    (loc - mark_sample(ms[0])).abs, ms[0])
                           end
                      delete_mark(id) if id
                    end],
                   ["To next mark", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| forward_mark(1, $graph_popup_snd, $graph_popup_chn) end],
                   ["To last mark", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i|
                      backward_mark(1, $graph_popup_snd, $graph_popup_chn)
                    end],
                   ["sep", RxmSeparatorWidgetClass, every_menu],
                   ["Exit", RxmPushButtonWidgetClass, every_menu,
                    lambda do |w, c, i| exit(0) end]])
end.call

def edit_graph_popup_menu(snd, chn = nil)
  doc("edit_graph_popup_menu(snd, chn)
hides otiose entries, relabel others to reflect current state of SND
and CHN\n") if snd == :help
  eds = edits(snd, chn)
  for_each_child($graph_popup_menu,
                 lambda do |w|
                   case RXtName(w)
                   when "Snd"
                     if chans(snd) > 1
                       change_label(w, format("%s[%d]", short_file_name(snd), chn))
                     else
                       change_label(w, short_file_name(snd))
                     end
                   when "Save", "Undo", "Revert", "Play previous"
                     (eds[0] > 0 ? RXtManageChild(w) : RXtUnmanageChild(w))
                   when "Play channel"
                     (chans(snd) > 1 ? RXtManageChild(w) : RXtUnmanageChild(w))
                   when "Equalize panes"
                     (((chans(snd) > 1) or (sounds().length > 1)) ?
                      RXtManageChild(w) : RXtUnmanageChild(w))
                   when "Redo"
                     (eds[1] > 0 ? RXtManageChild(w) : RXtUnmanageChild(w))
                   when "Mix selection", "Insert selection", "Unselect", "Replace with selection"
                     (selection?() ? RXtManageChild(w) : RXtUnmanageChild(w))
                   when "Play from cursor"
                     (cursor(snd, chn) > 0 ? RXtManageChild(w) : RXtUnmanageChild(w))
                   when "Play original"
                     (eds[0] > 1 ? RXtManageChild(w) : RXtUnmanageChild(w))
                   when "Delete mark", "To next mark", "To last mark"
                     (marks(snd, chn) ? RXtManageChild(w) : RXtUnmanageChild(w))
                   end
                 end)
end

def make_simple_popdown_menu(label, pd_labels = [], parent = nil, cascade_func = nil, args = nil)
  doc("make_simple_popdown_menu(label, pd_labels, parent, cascade_func, args)
creates a simple popdown menu\n") if label == :help
  top = RXmCreatePulldownMenu(parent, label, args)
  top_cascade = RXtCreateManagedWidget(label, RxmCascadeButtonWidgetClass,
                                       parent, args + [RXmNsubMenuId, top])
  children = pd_labels.map do |poplab|
    child = RXtCreateManagedWidget(poplab[0], RxmPushButtonWidgetClass, top, args)
    RXtAddCallback(child, RXmNactivateCallback, poplab[1])
    child
  end
  if cascade_func
    RXtAddCallback(top_cascade, RXmNcascadingCallback,
                   lambda do |w, c, i| cascade_func.call(children) end)
  end
end

$fft_popup_menu = lambda do
  every_menu = [RXmNbackground, highlight_color()]
  fft_popup = RXmCreatePopupMenu(main_widgets()[2], "fft_popup",
                                 every_menu + [RXmNpopupEnabled, true])
  choose_chan = lambda do | |
    if channel_style($graph_popup_snd) == Channels_separate
      $graph_popup_chn
    else
      true
    end
  end
  RXtCreateManagedWidget("Transform", RxmLabelWidgetClass, fft_popup, every_menu)
  RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass, fft_popup, every_menu)
  peaks = RXtCreateManagedWidget("Peaks", RxmPushButtonWidgetClass, fft_popup, every_menu)
  RXtAddCallback(peaks, RXmNactivateCallback,
                 lambda do |w, c, i|
                   set_show_transform_peaks((not show_transform_peaks($graph_popup_snd,
                                                                      $graph_popup_chn)),
                                            $graph_popup_snd, choose_chan.call)
                 end)
  db = RXtCreateManagedWidget("dB", RxmPushButtonWidgetClass, fft_popup, every_menu)
  RXtAddCallback(db, RXmNactivateCallback,
                 lambda do |w, c, i|
                   set_fft_log_magnitude((not fft_log_magnitude($graph_popup_snd,
                                                                $graph_popup_chn)),
                                         $graph_popup_snd, choose_chan.call)
                 end)
  logfreq = RXtCreateManagedWidget("Log freq", RxmPushButtonWidgetClass, fft_popup, every_menu)
  RXtAddCallback(logfreq, RXmNactivateCallback,
                 lambda do |w, c, i|
                   set_fft_log_frequency((not fft_log_frequency($graph_popup_snd,
                                                                $graph_popup_chn)),
                                         $graph_popup_snd, choose_chan.call)
                 end)
  norm = RXtCreateManagedWidget("Normalize", RxmPushButtonWidgetClass, fft_popup, every_menu)
  RXtAddCallback(norm, RXmNactivateCallback,
                 lambda do |w, c, i|
                   if transform_normalization($graph_popup_snd, $graph_popup_chn) ==
                                             Dont_normalize
                     set_transform_normalization(Normalize_by_channel,
                                                 $graph_popup_snd, choose_chan.call)
                   else
                     set_transform_normalization(Dont_normalize,
                                                 $graph_popup_snd, choose_chan.call)
                   end
                 end)
  make_simple_popdown_menu("Graph type",
                           [["once", Graph_once],
                            ["sonogram", Graph_as_sonogram],
                            ["spectrogram", Graph_as_spectrogram]].map do |name, val|
                             [name, lambda do |w, c, i|
                                set_transform_graph_type(val, $graph_popup_snd, choose_chan.call)
                              end]
                           end,
                           fft_popup,
                           lambda do |lst|
                             ctr = 0
                             lst.each do |child|
                               RXtSetSensitive(child,
                                               transform_graph_type($graph_popup_snd,
                                                                    $graph_popup_chn) != ctr)
                               ctr += 1
                             end
                           end, every_menu)
  sizes = [16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 65536, 262144, 1048576]
  make_simple_popdown_menu("Size",
                           sizes.map do |val|
                             [val.to_s,
                              lambda do |w, c, i|
                                set_transform_size(val, $graph_popup_snd, choose_chan.call)
                              end]
                           end,
                           fft_popup,
                           lambda do |lst|
                             lst.each_with_index do |child, idx|
                               RXtSetSensitive(child,
                                               transform_size($graph_popup_snd,
                                                              $graph_popup_chn) != sizes[idx])
                             end
                           end, every_menu)
  windows = [["Rectangular", Rectangular_window],
             ["Hann", Hann_window],
             ["Welch", Welch_window],
             ["Parzen", Parzen_window],
             ["Bartlett", Bartlett_window],
             ["Hamming", Hamming_window],
             ["Blackman2", Blackman2_window],
             ["Blackman3", Blackman3_window],
             ["Blackman4", Blackman4_window],
             ["Exponential", Exponential_window],
             ["Riemann", Riemann_window],
             ["Kaiser", Kaiser_window],
             ["Cauchy", Cauchy_window],
             ["Poisson", Poisson_window],
             ["Gaussian", Gaussian_window],
             ["Tukey", Tukey_window],
             ["Dolph-Chebyshev", Dolph_chebyshev_window]]
  make_simple_popdown_menu("Window",
                           windows.map do |name, val|
                             [name,
                              lambda do |w, c, i|
                                set_fft_window(val, $graph_popup_snd, choose_chan.call)
                              end]
                           end,
                           fft_popup,
                           lambda do |lst|
                             lst.each_with_index do |child, idx|
                               RXtSetSensitive(child,
                                               fft_window($graph_popup_snd,
                                                          $graph_popup_chn) != windows[idx][1])
                             end
                           end, every_menu)
  types = [["Fourier", Fourier_transform],
           ["Wavelet", Wavelet_transform],
           ["Autocorrelate", Autocorrelation],
           ["Cepstrum", Cepstrum],
           ["Walsh", Walsh_transform],
           ["Hadamard", Hadamard_transform],
           ["Haar", Haar_transform]]
  make_simple_popdown_menu("Transform type",
                           types.map do |name, val|
                             [name,
                              lambda do |w, c, i|
                                set_transform_type(val, $graph_popup_snd, choose_chan.call)
                              end]
                           end,
                           fft_popup,
                           lambda do |lst|
                             lst.each_with_index do |child, idx|
                               RXtSetSensitive(child,
                                               transform_type($graph_popup_snd,
                                                              $graph_popup_chn) != types[idx][1])
                             end
                           end, every_menu)
  make_simple_popdown_menu("Wavelet type",
                           begin
                             ary = []
                             ["daub4", "daub6", "daub8", "daub10", "daub12", "daub14", "daub16",
                              "daub18", "daub20", "battle_lemarie", "burt_adelson", "beylkin",
                              "coif2", "coif4", "coif6", "sym2", "sym3", "sym4", "sym5",
                              "sym6"].each_with_index do |name, idx|
                               ary << [name,
                                       lambda do |w, c, i|
                                         set_wavelet_type(idx, $graph_popup_snd, choose_chan.call)
                                       end]
                             end
                             ary
                           end,
                           fft_popup,
                           lambda do |lst|
                             lst.each_with_index do |child, ctr_b|
                               RXtSetSensitive(child, 
                                               wavelet_type($graph_popup_snd, $graph_popup_chn) !=
                                               ctr_b)
                             end
                           end, every_menu)
  color = RXtCreateManagedWidget("Color", RxmPushButtonWidgetClass, fft_popup, every_menu)
  RXtAddCallback(color, RXmNactivateCallback, lambda do |w, c, i| color_dialog() end)
  orient = RXtCreateManagedWidget("Orientation", RxmPushButtonWidgetClass, fft_popup, every_menu)
  RXtAddCallback(orient, RXmNactivateCallback, lambda do |w, c, i| orientation_dialog() end)
  fft_popup
end.call

def edit_fft_popup_menu(snd, chn = nil)
  doc("edit_fft_popup_menu(snd, chn)
changes the fft-related popup menu to reflect the state of SND and CHN\n") if snd == :help
  for_each_child($fft_popup_menu,
                 lambda do |w|
                   case RXtName(w)
                   when "Peaks"
                     change_label(w, (show_transform_peaks(snd, chn) ? "No peaks" : "Peaks"))
                   when "dB"
                     change_label(w, (fft_log_magnitude(snd, chn) ? "Linear" : "dB"))
                   when "Log freq"
                     change_label(w, (fft_log_frequency(snd, chn) ? "Linear freq" : "Log freq"))
                   end
                 end)
end

def add_selection_popup(doc = nil)
  doc("add_selection_popup()
makes the selection-related popup menu\n") if doc == :help
  popups = []
  find_popup = lambda do |snd, chn, dats|
    unless dats.empty?
      cur = dats[0]
      if cur[0] == snd and cur[1] == chn
        cur
      else
        find_popup.call(snd, chn, dats[1..-1])
      end
    else
      false
    end
  end
  add_popup = lambda do |snd|
    (0...chans(snd)).each do |chn|
      unless find_popup.call(snd, chn, popups)
        chn_grf = channel_widgets(snd, chn)[0]
        popups.unshift([snd, chn])
        RXtAddCallback(chn_grf, RXmNpopupHandlerCallback,
                       lambda do |w, c, i|
                         e = Revent(i)
                         xe = Rx_root(e) - RXtTranslateCoords(w, 0, 0)[0]
                         if RButtonPress == Rtype(e)
                           $graph_popup_snd = snd
                           $graph_popup_chn = chn
                           if channel_style(snd) == Channels_combined
                             ye = Ry(e)
                             callcc do |ret|
                               (0...chans(snd)).each do |chn_i|
                                 if(ye < axis_info(snd, chn_i)[14])
                                   $graph_popup_chn = chn_i - 1
                                   ret.call
                                 end
                               end
                               $graph_popup_chn = chans(snd) - 1
                             end
                           end
                           fax = (transform_graph?(snd, chn) ?
                                  axis_info(snd, chn, Transform_graph) : false)
                           lax = (lisp_graph?(snd, chn) ? axis_info(snd, chn, Lisp_graph) : false)
                           if fax and xe >= fax[10] and xe <= fax[12]
                             edit_fft_popup_menu(snd, chn)
                             Rset_menuToPost(i, $fft_popup_menu)
                           else
                             if lax and xe >= lax[10] and xe <= lax[12]
                               false
                             else
                               if selection?()
                                 beg = selection_position(snd, $graph_popup_chn) / srate(snd).to_f
                                 fin = (selection_position(snd, $graph_popup_chn) +
                                        selection_frames(snd, $graph_popup_chn)) / srate(snd).to_f
                               end
                               if selection?() and (xe >= x2position(beg, snd, chn)) and
                                                   (xe <= x2position(fin, snd, chn))
                                 Rset_menuToPost(i, $selection_popup_menu)
                               else
                                 edit_graph_popup_menu($graph_popup_snd, $graph_popup_chn)
                                 Rset_menuToPost(i, $graph_popup_menu)
                               end
                             end
                           end
                         end
                       end)
      end
    end
  end
  $after_open_hook.add_hook!("popup_add_popup") do |snd| add_popup.call(snd) end
  sounds().each do |snd| add_popup.call(snd) end if sounds()
end

def change_menu_color(menu, new_color = nil)
  doc("change_menu_color(menu, new_color)
changes the color of MENU to NEW_COLOR. NEW_COLOR can be the color
name, an xm Pixel, a snd color, or a list of rgb values (as in Snd's
make_color)\n") if menu == :help
  color_pixel = if new_color.class == String
                  shell = RXtDisplay(main_widgets()[1])
                  dpy = RXtDisplay(shell)
                  scr = RDefaultScreen(dpy)
                  cmap = RDefaultColormap(dpy, scr)
                  col = RXColor()
                  if RXallocNamedColor(dpy, cmap, new_color, col, col) == 0
                    warn "can't allocate #{new_color.inspect}"
                  else
                    Rpixel(col)
                  end
                elsif color?(new_color)
                  new_color
                else
                  make_color(new_color[0], new_color[1], new_color[2])
                end
  for_each_child(menu, lambda do |n| RXmChangeColor(n, color_pixel) end)
end

def change_selection_popup_color(new_color)
  doc("change_selection_popup_color(new_color)
changes the selection popup menu's color:
change_selection_popup_color(\"red\")\n") if new_color == :help
  change_menu_color($selection_popup_menu, new_color)
end

def change_fft_popup_color(new_color)
  doc("change_fft_popup_color(new_color)
changes the fft popup menu's color:
change_fft_popup_color(0.5, 0.5, 0.5)\n") if new_color == :help
  change_menu_color($fft_popup_menu, new_color)
end

def change_graph_popup_color(new_color)
  doc("change_graph_popup_color(new_color)
changes the time-domain popup menu's color:
change_graph_popup_color(basic_color())\n") if new_color == :help
  change_menu_color($graph_popup_menu, new_color)
end

def make_popdown_entry(label, parent = nil, func = nil, args = nil, collect = nil, with_one = nil)
  doc("make_popdown_entry(label, parent, func, args, collect, with_one)
makes a new listener popup menu entry\n") if label == :help
  top_one = (with_one ? RXtCreateManagedWidget(label, RxmPushButtonWidgetClass, parent, args) :
             false)
  children = []
  if with_one
    RXtAddCallback(top_one, RXmNactivateCallback,
                   lambda do |w, c, i| func.call(collect.call(sounds())[0]) end)
  end
  top_two = RXmCreatePulldownMenu(parent, label, args)
  top_two_cascade = RXtCreateManagedWidget(label, RxmCascadeButtonWidgetClass, parent,
                                           args + [RXmNsubMenuId, top_two])
  RXtAddCallback(top_two_cascade, RXmNcascadingCallback,
                 lambda do |w, c, i|
                   children.each do |n| RXtUnmanageChild(n) end
                   current_sounds = collect.call(sounds())
                   if children.length < current_sounds.length
                     (children.length...current_sounds.length).each do |i|
                       child = RXtCreateManagedWidget("", RxmPushButtonWidgetClass,
                                                      top_two, args)
                       RXtAddCallback(child, RXmNactivateCallback,
                                      lambda do |w, c, i|
                                        func.call(((current_label(w) == "all") or
                                                   find_sound(current_label(w))))
                                      end)
                       children.unshift(child)
                     end
                   end
                   setup = lambda do |cs, snds|
                     if cs and snds and ((not cs.empty?) or (not snds.empty?))
                       child = cs[0]
                       snd = snds[0]
                       change_label(child, short_file_name(snd))
                       RXtManageChild(child)
                       setup.call(cs[1..-1], snds[1..-1])
                     end
                   end
                   setup.call(children, current_sounds)
                 end)
  [:Popdown, top_one, top_two, top_two_cascade, collect]
end

def add_listener_popup
  listener = (main_widgets()[4] or
              begin
                show_listener()
                set_show_listener(false)
                main_widgets()[4]
              end)
  every_menu = [RXmNbackground, highlight_color()]
  listener_popup = RXmCreatePopupMenu(listener, "listener_popup",
                                      every_menu + [RXmNpopupEnabled, true])
  identity = lambda do |snds| snds end
  edited = lambda do |snds|
    remove_if(lambda do |n|
                callcc do |ret| (0...chans(n)).each do |i| 
                    ret.call(false) unless edits(n, i)[0] == 0
                    true
                  end
                end
              end, snds)
  end
  focused = lambda do |snds| snds.length > 1 ? snds : [] end
  RXtCreateManagedWidget("Listener", RxmLabelWidgetClass, listener_popup, every_menu)
  RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass, listener_popup, every_menu)
  listener_popup_menu = [make_popdown_entry("Play", listener_popup,
                                            lambda do |snd| play(0, snd) end, every_menu,
                                            lambda do |snd| identity.call(snd) end, true),
                         begin
                           help_widget = RXtCreateManagedWidget("Help",
                                                                RxmPushButtonWidgetClass, 
                                                                listener_popup, every_menu)
                           RXtAddCallback(help_widget, RXmNactivateCallback,
                                          lambda do |w, c, i|
                                            selected = listener_selection()
                                            help = (selected and snd_help(selected))
                                            info_dialog(selected, help) if help
                                          end)
                           help_widget
                         end,
                         begin
                           open_widget = RXtCreateManagedWidget("Open",
                                                                RxmPushButtonWidgetClass,
                                                                listener_popup, every_menu)
                           RXtAddCallback(open_widget, RXmNactivateCallback,
                                          lambda do |w, c, i| open_file_dialog() end)
                           open_widget
                         end,
                         make_popdown_entry("Close", listener_popup,
                                            lambda do |snd| close_sound(snd) end, every_menu,
                                            lambda do |snd| identity.call(snd) end, true),
                         make_popdown_entry("Save", listener_popup,
                                            lambda do |snd| save_sound(snd) end, every_menu,
                                            lambda do |snd| edited.call(snd) end, true),
                         make_popdown_entry("Revert", listener_popup,
                                            lambda do |snd| revert_sound(snd) end, every_menu,
                                            lambda do |snd| edited.call(snd) end, true),
                         begin
                           panes_widget = RXtCreateManagedWidget("Equalize panes",
                                                                 RxmPushButtonWidgetClass,
                                                                 listener_popup, every_menu)
                           RXtAddCallback(panes_widget, RXmNactivateCallback,
                                          lambda do |w, c, i| equalize_panes() end)
                           panes_widget
                         end,
                         make_popdown_entry("Focus", listener_popup,
                                            lambda do |us|
                                              pane = sound_widgets(us)[0]
                                              old_resize = auto_resize()
                                              RXtSetValues(main_widgets()[1],
                                                           [RXmNallowShellResize, false])
                                              sounds().each do |them|
                                                RXtUnmanageChild(sound_widgets(them)[0])
                                              end
                                              RXtManageChild(pane)
                                              RXtSetValues(main_widgets()[1],
                                                           [RXmNallowShellResize, old_resize])
                                            end, every_menu,
                                            lambda do |snd| focused.call(snd) end, false),
                         RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass,
                                                listener_popup, every_menu),
                         begin
                           exit_widget = RXtCreateManagedWidget("Exit",
                                                                RxmPushButtonWidgetClass,
                                                                listener_popup, every_menu)
                           RXtAddCallback(exit_widget, RXmNactivateCallback,
                                          lambda do |w, c, i| exit(0) end)
                           exit_widget
                         end]
  RXtAddCallback(listener, RXmNpopupHandlerCallback,
                 lambda do |w, c, i|
                   if RButtonPress == Rtype(Revent(i))
                     listener_popup_menu.each do |n|
                       if array?(n) and n[0] == :Popdown
                         top_one = n[1]
                         top_two = n[2]
                         top_two_cascade = n[3]
                         len = (sounds() ? n[4].call(sounds()).length : 0)
                         RXtUnmanageChild(top_two)
                         RXtUnmanageChild(top_two_cascade)
                         RXtUnmanageChild(top_one) if top_one
                         if(len > 1)
                           RXtManageChild(top_two_cascade)
                           RXtManageChild(top_two)
                         end
                         RXtManageChild(top_one) if top_one and len == 1
                       elsif RWidget?(n)
                         len = (sounds() ? sounds.length : 0)
                         case RXtName(n)
                         when"Equalize panes"
                           len > 1 ? RXtManageChild(n) : RXtUnmanageChild(n)
                         when "Help"
                           listener_selection() ? RXtManageChild(n) : RXtUnmanageChild(n)
                         end
                       end
                     end
                     Rset_menuToPost(i, listener_popup)
                   end
                 end)
  listener_popup
end

add_selection_popup()
$listener_menu = add_listener_popup()

def change_listener_popup_color(new_color)
  change_menu_color($listener_menu, new_color)
end

# popup.rb ends here
