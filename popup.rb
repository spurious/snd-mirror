# popup.rb -- Specialize Popup Menus converted from Guile to Ruby.

# Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Thu Sep 05 22:28:49 CEST 2002
# Last: Wed Feb 11 17:02:59 CET 2004

# Commentary:
#
# Requires Motif module (libxm.so|xm.so) or --with-static-xm!
#
# Tested with Snd 7.2, Motif 2.1, Ruby 1.6.6 and 1.9.0.
#
# $info_comment_hook: lambda do |file, info_string| ...; new_info_string; end
#
# make_snd_popup(name, *rest) do ... end
#
# class Snd_popup_menu < Menu
#   initialize(name, parent, args, where) do ... end
#   before_popup_hook, lambda do |snd, chn, xe| ...; flag; end
#   entry(name, *rest) do |snd, chn, w| ... end
#   label(name, args)
#   separator(single)
#   cascade(name, args) do ... end
#   each_value do |val| ... end
#
#     class Cascade < Snd_popup_menu
#     initialize(name, parent, args)
#     values
#     # listener
#     children(set_cb, rest = false) do |snd| ... end             set_cb.arity == -1 or 0
#     # transform
#     children(set_cb, rest = false) do |snd, chn, val| ... end   set_cb.arity == 3 (snd, chn, val)
#
# Usage:
#
# menu = make_snd_popup("graph") do
#   entry("Play") do |snd, chn, w| play(0, snd) end
#   cascade("Marks") do
#     entry("Add") do |snd, chn, w| add_mark(cursor(snd, chn), snd, chn) end
#     entry("Delete") do |snd, chn, w| delete_mark(marks(snd, chn)[0]) end
#   end
#   entry("Exit") do |snd, chn, w| exit(0) end
# end
# menu.change_menu_color("ivory3")
#
# CHILDREN
#
# Cascading can be simplified by using `children' which can take a
# `set_cb' callback with 0 args (on listener) or 3 args (on channels).
# If set_cb.arity == 0, the body.arity == 1 (current sound).  If
# set_cb.arity == 3, the body.arity == 3 (snd, chn, val).  See
# listener-popup menu below for the former and transform-popup for the
# latter variant.
# 
# children(set_cb, rest = false, &body)
#   case SET_CB.arity
#   when 0
#     # listener popup example below
#     BODY.arity == 1 (snd)
#     REST means "no widget"
#   when 1
#     # transform popup example below
#     BODY.arity == 3 (snd, chn, val)
#     REST means an array of arrays of [name, value]
#   end
#
# make_snd_popup("Listener", ...) do
#   ...
#   cascade("Close") do
#     children(lambda do (sounds() or []) end) do |snd|
#       close_sound_extend(snd)
#     end
#   end
#   ...
# end
#
# make_snd_popup("Transform") do
#   ...
#   cascade("Graph type") do
#     children(lambda do |snd, chn, val|
#                transform_graph_type(snd, chn) != val
#              end, [["once", Graph_once],
#                    ["sonogram", Graph_as_sonogram],
#                    ["spectrogram", Graph_as_spectrogram]]) do |snd, chn, val|
#       set_transform_graph_type(val, snd, choose_chan.call(snd, chn))
#     end
#   end
#   ...
# end
#
# Change the appearance of menu entries with `before_popup_hook', a
# hook per instance.  The example menus below may help.

# Code:

require "examp"
require "hooks"
require "snd-motif"
include Snd_Motif

make_hook("$info_comment_hook", 2, "\
lambda do |file, info_string| ...; new_info_string; end: provides a
way to add more information to INFO_STRING and format the comment of
FILE.  The hook collects the return value of one hook to INFO_STRING
on subsequent calls.  This hook is called in popup.rb and nb.rb.  If
no hook procedure is defined, the normal sound comment is added.

In snd-motif.rb exists format_sound_comment(comment).  It is a very
simple comment formatter.

$info_comment_hook.add_hook!(\"snd-init-hook\") do |file, info|
  info += format_sound_comment(mus_sound_comment(file))
  if s = mus_sound_loop_info(file)
    info += format(\"   loop: %s\n\", s.inspect)
  end
  info
end")

def make_snd_popup(name, *rest, &body)
  doc("make_snd_popup(name, *rest) do ... end
    name                               # menu name
    :parent, main_widgets[Main_pane]   # e.g. listener popup takes main_widgets[Listener]
    :where,  :channels                 # :channels, :widget, :event
    :args,   [RXmNbackground, highlight_color] # Motif arguments\n") if name == :help
  parent = get_args(rest, :parent, main_widgets[Main_pane])
  where  = get_args(rest, :where, :channels)      # :channels, :widget, :event
  args   = get_args(rest, :args, [RXmNbackground, highlight_color])
  Snd_popup_menu.new(name, parent, args, where, &body)
end

class Snd_popup_menu < Menu
  def initialize(name, parent, args, where = nil, &body)
    super(name, parent, args)
    @parent = parent
    @values = []
    @popups = []
    @before_popup_hook = Hook.new("@before_popup_hook", 3, "\
lambda do |snd, chn, xe| ... flag end: called before posting a popup menu.

On channel popups (see popup.rb): SND, CHN, and XE are the selected
sound, channel and x-position on the channel pane.  If it returns
non-nil or non-false, the menu will be posted.

On the listener popup (see popup.rb): SND, CHN, and XE are meaningless
as well as the return value.  The menu will be posted in every case.

On event-handler popups (see nb.rb): SND, CHN, and XE are meaningless.
If it returns non-nil or non-false, the menu will be posted.")
    if where.kind_of?(Symbol)
      # no `create' on Cascade.new
      create(where, &body)
    end
  end
  attr_reader :before_popup_hook
  
  def entry(name, *rest, &body)
    widget_class = get_args(rest, :widget_class, RxmPushButtonWidgetClass)
    args         = get_args(rest, :args, @args)
    proc         = get_args(rest, :proc, nil)
    widget = RXtCreateManagedWidget(name, widget_class, @menu, args)
    if block_given?
      RXtAddCallback(widget, RXmNactivateCallback,
                     lambda do |w, c, i|
                       chn = if snd = selected_sound
                               selected_channel
                             else
                               false
                             end
                       body.call(snd, chn, w)
                     end)
    end
    proc.call(widget) if proc.kind_of?(Proc)
  end

  def label(name, args = @args)
    RXtCreateManagedWidget(name, RxmLabelWidgetClass, @menu, args)
  end

  def separator(single = :single)
    line = (single == :double ? RXmDOUBLE_LINE : RXmSINGLE_LINE)
    RXtCreateManagedWidget("s", RxmSeparatorWidgetClass, @menu, [RXmNseparatorType, line])
  end
  
  def cascade(name, args = @args, &body)
    cas = Cascade.new(name, @menu, args)
    cas.instance_eval(&body) if block_given?
    @values.push(cas.values)
    cas
  end

  def each_value(&body)
    @values.map(&body)
  end

  private
  def make_menu
    @menu = RXmCreatePopupMenu(@parent, @label, [RXmNpopupEnabled, true] + @args)
  end
  
  def create(where, &body)
    make_menu
    unless @label.empty?
      label(@label)
      separator
    end
    instance_eval(&body) if block_given?
    case where
    when :channels
      (sounds() or []).each do |snd|
        set_channel_popup(snd)
      end
      $after_open_hook.add_hook!(format("%s-popup", @label)) do |snd|
        set_channel_popup(snd)
      end
    when :widget
      set_widget_popup
    when :event
      set_event_popup
    end
  end
  
  def set_channel_popup(snd)
    channels(snd).times do |chn|
      unless @popups.detect do |c| c[0] == snd and c[1] == chn end
        chn_grf = channel_widgets(snd, chn)[0]
        @popups.push([snd, chn])
        RXtAddCallback(chn_grf, RXmNpopupHandlerCallback,
                       lambda do |w, c, i|
                         e = Revent(i)
                         if RButtonPress == Rtype(e)
                           if @before_popup_hook.empty?
                             Rset_menuToPost(i, @menu)
                           else
                             xe = Rx_root(e) - RXtTranslateCoords(w, 0, 0)[0]
                             chn = if channel_style(snd) == Channels_combined
                                     ye = Ry(e)
                                     if (cn = (0...channels(snd)).detect do |c|
                                           ye < axis_info(snd, c)[14]
                                         end)
                                       cn - 1
                                     else
                                       channels(snd) - 1
                                     end
                                   else
                                     chn
                                   end
                             select_channel(chn)
                             if @before_popup_hook.call(snd, chn, xe)
                               Rset_menuToPost(i, @menu)
                             end
                           end
                         end
                       end)
      end
    end
  end
  
  # e.g. on listener
  def set_widget_popup
    RXtAddCallback(@parent, RXmNpopupHandlerCallback,
                   lambda do |w, c, i|
                     if RButtonPress == Rtype(Revent(i))
                       @before_popup_hook.call(nil, nil, nil)
                       Rset_menuToPost(i, @menu)
                     end
                   end)
  end

  def set_event_popup
    RXtAddEventHandler(@parent, RButtonPressMask, false,
                       lambda do |w, c, i, f|
                         if Rbutton(i) == 3
                           if @before_popup_hook.empty? or @before_popup_hook.call(nil, nil, nil)
                             RXmMenuPosition(@menu, i)
                             RXtManageChild(@menu)
                           end
                         end
                       end)
  end
  
  class Cascade < Snd_popup_menu
    def initialize(name, parent, args)
      super
      @menu = RXmCreatePulldownMenu(@parent, @label, @args)
      @cascade = RXtCreateManagedWidget(@label, RxmCascadeButtonWidgetClass, @parent,
                                        [RXmNsubMenuId, @menu] + @args)
      @children = []
    end
    attr_reader :values
    
    def children(set_cb, rest = false, &body)
      if set_cb.kind_of?(Proc) and body.kind_of?(Proc)
        case set_cb.arity
        when -1, 0
          add_with_arity_1(rest, set_cb, &body)
        when 3
          add_with_arity_3(rest, set_cb, &body)
        end
      else
        error("%s#%s: set_cb or block missing (%s, %s)",
              self.class, get_func_name, set_cb.inspect, body.inspect)
      end
    end

    private
    # listener: set_cb.arity == 0|-1, body.arity == 1 (snd)
    def add_with_arity_1(no_widget, set_cb, &body)
      if no_widget
        widget = false
      else
        widget = RXtCreateManagedWidget(@label, RxmPushButtonWidgetClass, @parent, @args)
        RXtAddCallback(widget, RXmNactivateCallback,
                       lambda do |w, c, i| body.call(set_cb.call.first) end)
      end
      RXtAddCallback(@cascade, RXmNcascadingCallback,
                     lambda do |w, c, i|
                       @children.each do |child| RXtUnmanageChild(child) end
                       snds = set_cb.call.reverse
                       clen = @children.length
                       slen = snds.length
                       if clen < slen
                         (clen...slen).each do |numb|
                           child = RXtCreateManagedWidget(numb.to_s,
                                                          RxmPushButtonWidgetClass,
                                                          @menu, @args)
                           RXtAddCallback(child, RXmNactivateCallback,
                                          lambda do |w, c, i|
                                            body.call(find_sound(current_label(w)))
                                          end)
                           @children.push(child)
                         end
                       end
                       if slen.nonzero?
                         @children.zip(snds) do |child, snd|
                           break unless snd
                           change_label(child, short_file_name(snd))
                           RXtManageChild(child)
                         end
                       end
                     end)
      @values = [widget, @menu, @cascade, set_cb]
    end
    
    # transform: set_cb.arity and body.arity == 3 (snd, chn, val)
    def add_with_arity_3(list, set_cb, &body)
      list.each do |name, val|
        wid = RXtCreateManagedWidget(name.to_s, RxmPushButtonWidgetClass, @menu, @args)
        RXtAddCallback(wid, RXmNactivateCallback,
                       lambda do |w, c, i|
                         body.call(selected_sound, selected_channel, val)
                       end)
        @values.push(val)
        @children.push(wid)
      end
      RXtAddCallback(@cascade, RXmNcascadingCallback,
                     lambda do |w, c, i|
                       @children.each_with_index do |child, idx|
                         RXtSetSensitive(child, set_cb.call(selected_sound,
                                                            selected_channel,
                                                            @values[idx]))
                       end
                     end)
    end
  end
end

#
# example menus
#
unless defined? $__private_popup_menu__ and $__private_popup_menu__
  #
  # Selection Popup
  #
  make_snd_popup("Selection") do
    stopping = stopping1 = false
    stop_widget = stop_widget1 = nil
    selctr = 0
    $stop_playing_selection_hook.add_hook!("popup-stop-selection-hook") do | |
      if stopping
        stopping = false
        change_label(stop_widget, "Play") if RWidget?(stop_widget)
      end
    end
    entry("Play") do |snd, chn, w|
      if stopping
        stopping = false
        change_label(w, "Play")
        if stopping1
          stopping1 = false
          change_label(stop_widget1, "Loop play")
          $stop_playing_selection_hook.remove_hook!("popup-play-selection")
        end
        stop_playing
      else
        change_label(w, "Stop")
        stop_widget = w
        stopping = true
        play_selection
      end
    end
    entry("Loop play") do |snd, chn, w|
      if stopping1
        stopping1 = false
        change_label(w, "Loop play")
        $stop_playing_selection_hook.remove_hook!("popup-play-selection")
        if stopping
          stopping = false
          change_label(stop_widget, "Play")
        end
        stop_playing
      else
        change_label(w, "Stop!")
        stop_widget1 = w
        stopping1 = true
        $stop_playing_selection_hook.add_hook!("popup-play-selection") do | | play_selection end
        play_selection
      end
    end
    entry("Delete") do |snd, chn, w| delete_selection end
    entry("Zero") do |snd, chn, w| scale_selection_by(0.0) end
    entry("Crop") do |snd, chn, w|
      sndlist = []
      sounds().each do |snd|
        channels(snd).times do |i|
          sndlist.push([snd, i]) if selection_member?(snd, i)
        end
      end
      sndlist.each do |selection|
        snd, chn = selection
        beg = selection_position(snd, chn)
        len = selection_frames(snd, chn)
        as_one_edit(lambda do | |
                      delete_samples(0, beg, snd, chn) if beg > 0
                      if len < frames(snd, chn)
                        delete_samples(len + 1, frames(snd, chn) - len, snd, chn)
                      end
                    end)
      end
    end
    entry("Save as") do |snd, chn, w| edit_save_as_dialog end
    entry("Copy->New") do |snd, chn, w|
      new_file_name = format("newf-%d.snd", selctr)
      selctr += 1
      save_selection(new_file_name)
      open_sound(new_file_name)
    end
    entry("Cut->New") do |snd, chn, w|
      new_file_name = format("newf-%d.snd", selctr)
      selctr += 1
      save_selection(new_file_name)
      delete_selection
      open_sound(new_file_name)
    end
    entry("Snap marks") do |snd, chn, w|
      selection_members.each do |snd, chn|
        pos = selection_position(snd, chn)
        len = selection_frames(snd, chn) - 1
        add_mark(pos, snd, chn)
        add_mark(pos + len, snd, chn)
      end
    end
    entry("Unselect") do |snd, chn, w| set_selection_member?(false, true) end
    entry("Revert") do |snd, chn, w| reverse_selection end
    entry("Mix") do |snd, chn, w| mix_selection(cursor()) end
    entry("Invert") do |snd, chn, w| scale_selection_by(-1) end
    before_popup_hook.add_hook!("selection popup") do |snd, chn, xe|
      fax = if transform_graph?(snd, chn)
              axis_info(snd, chn, Transform_graph)
            else
              false
            end
      lax = if lisp_graph?(snd, chn)
              axis_info(snd, chn, Lisp_graph)
            else
              false
            end
      if fax and xe >= fax[10] and xe <= fax[12]
        false
      else
        if lax and xe >= lax[10] and xe <= lax[12]
          false
        else
          if selection?
            sr = srate(snd).to_f
            beg = selection_position(snd, chn) / sr
            fin = (selection_position(snd, chn) + selection_frames(snd, chn)) / sr
          end
          if selection? and xe >= x2position(beg, snd, chn) and xe <= x2position(fin, snd, chn)
            true
          else
            false
          end
        end
      end
    end
  end

  #
  # Graph Popup
  #
  make_snd_popup("Snd") do
    stopping = false
    stop_widget = nil
    $stop_playing_hook.add_hook!("popup-change-label-play2stop") do |snd|
      if stopping
        stopping = false
        change_label(stop_widget, "Play") if RWidget?(stop_widget)
      end
    end
    entry("Play", :proc, lambda do |w| stop_widget = w end) do |snd, chn, w|
      if stopping
        stopping = false
        change_label(w, "Play")
        stop_playing
      else
        change_label(w, "Stop")
        stopping = true
        play(0, snd)
      end
    end
    entry("Play channel") do |snd, chn, w|
      stopping = true
      change_label(stop_widget, "Stop")
      play(0, snd, chn)
    end
    entry("Play from cursor") do |snd, chn, w|
      stopping = true
      change_label(stop_widget, "Stop")
      play(cursor(snd, chn), snd)
    end
    entry("Play previous") do |snd, chn, w|
      stopping = true
      change_label(stop_widget, "Stop")
      play(0, snd, chn, false, false, edit_position - 1)
    end
    entry("Play original") do |snd, chn, w|
      stopping = true
      change_label(stop_widget, "Stop")
      play(0, snd, chn, false, false, 0)
    end
    entry("Undo") do |snd, chn, w| undo_edit(1, snd, chn) end if defined? undo_edit
    entry("Redo") do |snd, chn, w| redo_edit(1, snd, chn) end if defined? redo_edit
    entry("Revert") do |snd, chn, w| revert_sound(snd) end
    entry("Open") do |snd, chn, w| open_file_dialog end
    entry("Close") do |snd, chn, w| close_sound_extend(snd) end
    entry("Save") do |snd, chn, w| save_sound(snd) end
    entry("Save as") do |snd, chn, w|
      select_sound(snd)
      file_save_as_dialog
    end
    entry("Update") do |snd, chn, w| update_sound(snd) end
    entry("Mix selection") do |snd, chn, w| mix_selection(cursor(snd, chn), snd, chn) end
    entry("Insert selection") do |snd, chn, w| insert_selection(cursor(snd, chn), snd, chn) end
    entry("Replace with selection") do |snd, chn, w|
      beg = cursor(snd, chn)
      len = selection_frames
      sbeg = selection_position
      if (not selection_member?(snd, chn)) or ((beg + len) < sbeg) or (beg > (sbeg + len))
        delete_samples(beg, len, snd, chn)
        insert_selection(beg, snd, chn)
      elsif beg < sbeg
        delete_samples(beg, sbeg - beg, snd, chn)
      end
    end
    entry("Select all") do |snd, chn, w| select_all(snd, chn) end
    entry("Unselect") do |snd, chn, w| set_selection_member?(false, true) end
    entry("Equalize panes") do |snd, chn, w| equalize_panes end
    entry("Info") do |snd, chn, w|
      file = file_name(snd)
      date = Time.at(mus_sound_write_date(file)).localtime.strftime("%a %d-%b-%y %H:%M %Z")
      info_string = format("\
  chans: %d, srate: %d
 length: %1.3f (%d samples)
 format: %s [%s]
 maxamp: %s
written: %s\n", channels(snd), srate(snd), frames(snd) / srate(snd).to_f,
                           frames(snd), mus_data_format_name(data_format(snd)),
                           mus_header_type_name(header_type(snd)),
                           maxamp(snd, true).to_string, date)
      if $info_comment_hook.empty?
        if s = comment(snd)
          info_string += format("comment: %s\n", s)
        end
      else
        $info_comment_hook.run_hook do |prc| info_string = prc.call(file, info_string) end
      end
      if defined? Kernel.xm_nb and Kernel.xm_nb.kind_of?(XM_NB)
        Kernel.xm_nb.popup_nb_hook.run_hook do |prc| info_string = prc.call(snd, info_string) end
      end
      info_dialog(file + " info", info_string)
    end
    entry("Add mark") do |snd, chn, w| add_mark(cursor(snd, chn), snd, chn) end
    entry("Delete mark") do |snd, chn, w|
      if (ms = marks(snd, chn)).nil? or ms.empty?
        false
      elsif ms.length == 1
        delete_mark(ms[0])
      else
        loc = cursor()
        id = ms.first
        cur_min = (loc - mark_sample(ms.first)).abs
        ms.each do |m|
          if (this_min = (loc - mark_sample(m)).abs) < cur_min
            cur_min = this_min
            id = m
          end
        end
        delete_mark(id)
      end
    end
    entry("Delete all marks") do |snd, chn, w| delete_marks(snd, chn) end
    entry("To next mark") do |snd, chn, w| forward_mark(1, snd, chn) end
    entry("To last mark") do |snd, chn, w| backward_mark(1, snd, chn) end
    separator(:double)
    entry("Exit") do |snd, chn, w| exit(0) end
    before_popup_hook.add_hook!("graph popup") do |snd, chn, xe|
      fax = if transform_graph?(snd, chn)
              axis_info(snd, chn, Transform_graph)
            else
              false
            end
      lax = if lisp_graph?(snd, chn)
              axis_info(snd, chn, Lisp_graph)
            else
              false
            end
      flag = if fax and xe >= fax[10] and xe <= fax[12]
               false
             else
               if lax and xe >= lax[10] and xe <= lax[12]
                 # lisp graph
                 true
               else
                 if selection?
                   sr = srate(snd).to_f
                   beg = selection_position(snd, chn) / sr
                   fin = (selection_position(snd, chn) + selection_frames(snd, chn)) / sr
                 end
                 if selection? and
                     xe >= x2position(beg, snd, chn) and
                     xe <= x2position(fin, snd, chn)
                   false
                 else
                   true
                 end
               end
             end
      if flag
        each_entry do |w|
          eds = edits(snd, chn)
          case RXtName(w)
          when "Snd"
            if channels(snd) > 1
              change_label(w, format("%s[%d]", short_file_name(snd), chn))
            else
              change_label(w, short_file_name(snd))
            end
          when "Save", "Undo", "Revert", "Play previous"
            eds[0] > 0 ? RXtManageChild(w) : RXtUnmanageChild(w)
          when "Play channel"
            channels(snd) > 1 ? RXtManageChild(w) : RXtUnmanageChild(w)
          when "Equalize panes"
            [sounds().length, channels(snd)].max > 1 ? RXtManageChild(w) : RXtUnmanageChild(w)
          when "Redo"
            eds[1] > 0 ? RXtManageChild(w) : RXtUnmanageChild(w)
          when "Mix selection", "Insert selection", "Unselect", "Replace with selection"
            selection? ? RXtManageChild(w) : RXtUnmanageChild(w)
          when "Play from cursor"
            cursor(snd, chn) > 0 ? RXtManageChild(w) : RXtUnmanageChild(w)
          when "Play original"
            eds[0] > 1 ? RXtManageChild(w) : RXtUnmanageChild(w)
          when "Delete mark", "To next mark", "To last mark"
            marks(snd, chn) ? RXtManageChild(w) : RXtUnmanageChild(w)
          when "Delete all marks"
            (marks(snd, chn) or []).length > 1 ? RXtManageChild(w) : RXtUnmanageChild(w)
          end
        end
      end
      flag
    end
  end

  #
  # Transform Popup
  #
  make_snd_popup("Transform") do
    choose_chan = lambda do |snd, chn|
      if channel_style(snd) == Channels_separate
        chn
      else
        true
      end
    end
    entry("Peaks") do |snd, chn, w| 
      set_show_transform_peaks(!show_transform_peaks(snd, chn), snd, choose_chan.call(snd, chn))
    end
    entry("dB") do |snd, chn, w|
      set_fft_log_magnitude(!fft_log_magnitude(snd, chn), snd, choose_chan.call(snd, chn))
    end
    entry("Log freq") do |snd, chn, w|
      set_fft_log_frequency(!fft_log_frequency(snd, chn), snd, choose_chan.call(snd, chn))
    end
    entry("Normalize") do |snd, chn, w|
      if transform_normalization(snd, chn) == Dont_normalize
        set_transform_normalization(Normalize_by_channel, snd, choose_chan.call(snd, chn))
      else
        set_transform_normalization(Dont_normalize, snd, choose_chan.call(snd, chn))
      end
    end
    cascade("Graph type") do
      children(lambda do |snd, chn, val|
                 transform_graph_type(snd, chn) != val
               end, [["once", Graph_once],
                     ["sonogram", Graph_as_sonogram],
                     ["spectrogram", Graph_as_spectrogram]]) do |snd, chn, val|
        set_transform_graph_type(val, snd, choose_chan.call(snd, chn))
      end
    end
    cascade("Size") do
      children(lambda do |snd, chn, val|
                 transform_size(snd, chn) != val
               end, [16, 32, 64, 128, 256, 512, 1024, 2048, 4096,
                     8192, 16384, 65536, 262144, 1048576].map do |s|
                 [s.to_s, s]
               end) do |snd, chn, val|
        set_transform_size(val, snd, choose_chan.call(snd, chn))
      end
    end
    cascade("Window") do
      children(lambda do |snd, chn, val|
                 fft_window(snd, chn) != val
               end, [["Rectangular", Rectangular_window],
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
                     ["Dolph-Chebyshev", Dolph_chebyshev_window]]) do |snd, chn, val|
        set_fft_window(val, snd, choose_chan.call(snd, chn))
      end
    end
    cascade("Transform type") do
      children(lambda do |snd, chn, val|
                 transform_type(snd, chn) != val
               end, [["Fourier", Fourier_transform],
                     ["Autocorrelate", Autocorrelation],
                     ["Cepstrum", Cepstrum],
                     ["Walsh", Walsh_transform],
                     ["Hadamard", Hadamard_transform],
                     ["Haar", Haar_transform],
                     ["Wavelet", Wavelet_transform]]) do |snd, chn, val|
        set_transform_type(val, snd, choose_chan.call(snd, chn))
      end
      cascade("Wavelet type") do
        children(lambda do |snd, chn, val|
                   wavelet_type(snd, chn) != val
                 end, ["daub4", "daub6", "daub8", "daub10",
                       "daub12", "daub14", "daub16", "daub18",
                       "daub20", "battle-lemarie", "burt-adelson",
                       "beylkin", "coif2", "coif4", "coif6",
                       "sym2", "sym3", "sym4", "sym5", "sym6"].map_with_index do |v, idx|
                   [v, idx]
                 end) do |snd, chn, val|
          set_wavelet_type(val, snd, choose_chan.call(snd, chn))
        end
      end
    end
    entry("Color") do |snd, chn, w| color_dialog end
    entry("Orientation") do |snd, chn, w| orientation_dialog end
    before_popup_hook.add_hook!("transform popup") do |snd, chn, xe|
      fax = if transform_graph?(snd, chn)
              axis_info(snd, chn, Transform_graph)
            else
              false
            end
      if fax and xe >= fax[10] and xe <= fax[12]
        each_entry do |w|
          case RXtName(w)
          when "Peaks"
            change_label(w, (show_transform_peaks(snd, chn) ? "No peaks" : "Peaks"))
          when "dB"
            change_label(w, (fft_log_magnitude(snd, chn) ? "Linear" : "dB"))
          when "Log freq"
            change_label(w, (fft_log_frequency(snd, chn) ? "Linear freq" : "Log freq"))
          when "Normalize"
            change_label(w, (transform_normalization(snd, chn) == Dont_normalize ?
                             "Normalize" : "Original"))
          end
        end
        true
      else
        false
      end
    end
  end

  #
  # Listener Popup
  #
  make_snd_popup("Listener",
                 :where, :widget,
                 :parent, if RWidget?(w = main_widgets[Listener])
                            w
                          else
                            show_listener
                            set_show_listener(false)
                            main_widgets[Listener]
                          end) do
    identity = lambda do (sounds() or []) end
    edited = lambda do
      if snds = sounds()
        snds.delete_if do |snd|
          (0...channels(snd)).detect do |chn| edits(snd, chn).first.zero? end
        end
      else
        []
      end
    end
    focused = lambda do (snds = (sounds() or [])).length > 1 ? snds : [] end
    cascade("Play") do
      children(identity) do |snd| play(0, snd) end
    end
    entry("Open") do |snd, chn, w| open_file_dialog end
    cascade("Close") do
      children(identity) do |snd| close_sound_extend(snd) end
    end
    cascade("Save") do
      children(edited) do |snd| save_sound(snd) end
    end
    cascade("Revert") do
      children(edited) do |snd| revert_sound(snd) end
    end
    entry("Equalize panes") do |snd, chn, w| equalize_panes end
    cascade("Focus") do
      children(focused, true) do |snd|
        if RWidget?(main_widgets[Notebook])
          set_selected_sound(snd)
        else
          pane = sound_widgets(snd)[0]
          RXtVaSetValues(main_widgets[Top_level], [RXmNallowShellResize, false])
          sounds().each do |them| RXtUnmanageChild(sound_widgets(them)[0]) end
          RXtManageChild(pane)
          RXtVaSetValues(main_widgets[Top_level], [RXmNallowShellResize, auto_resize])
        end
      end
    end
    entry("Help") do |snd, chn, w|
      if help = (selected = listener_selection and snd_help(selected))
        help_dialog(selected, help)
      else
        snd_warning(format("%s: no help found", selected.inspect))
      end
    end
    separator(:double)
    entry("Exit") do |snd, chn, w| exit(0) end
    before_popup_hook.add_hook!("listener popup") do |d1, d2, d3|
      each_value do |val|
        w = val[0]
        cas = val[1..2]
        prc = val[3]
        len = prc.call.length
        if RWidget?(w)
          if len == 1
            RXtManageChild(w)
          else
            RXtUnmanageChild(w)
          end
        end
        if len > 1
          cas.each do |wid| RXtManageChild(wid) end
        else
          cas.each do |wid| RXtUnmanageChild(wid) end
        end
      end
      each_entry do |w|
        if RWidget?(w)
          case name = RXtName(w)
          when "Equalize panes"
            (sounds() or []).length > 1 ? RXtManageChild(w) : RXtUnmanageChild(w)
          when "Help"
            if subject = listener_selection
              change_label(w, format("Help on %s", subject.inspect))
              RXtManageChild(w)
            else
              RXtUnmanageChild(w)
            end
          end
        end
      end
    end
  end
end

# popup.rb ends here
