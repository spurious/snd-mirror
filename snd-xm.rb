# snd-xm.rb -- snd-motif and snd-gtk classes and functions -*- snd-ruby -*-

# Author: Michael Scholz <scholz-micha@@gmx.de>
# Created: Wed Feb 25 05:31:02 CET 2004
# Last: Wed Feb 23 01:32:04 CET 2005

# Commentary:
#
# Requires --with-motif or --with-gtk and module libxm.so or --with-static-xm!
#
# Tested with Snd 7.3, Motif 2.2.2, Gtk+ 2.2.1, Ruby 1.6.6, 1.6.8 and 1.9.0.
#
# module Snd_XM
#  make_snd_menu(name, args) do ... end
#  make_menu(name, parent) do ... end
#  make_popup_menu(name, parent) do ... end
#  make_dialog(label, *rest) do |w, c, i| ... end
#  format_sound_comment(comment)
#  scale_log2linear(lo, val, hi)
#  scale_linear2log(lo, val, hi)
#  scale_log_label(lo, val, hi)
#  semi_scale_label(val)
#  semitones2ratio
#  ratio2semitones
#  get_color(*new_color)
#  create_color(color)
#  yellow_pixel
#  red_pixel
#  white_pixel
#  black_pixel
#  update_label(list)
#  change_label(widget, string, [property: only Motif])
#  find_child(widget, name)
#  each_child(widget) do |w| .... end
#  widget?(obj)
#  is_managed(widget)
#  widget_name(widget)
#  set_scale_value(widget, value, scaler)
#  get_scale_value(widget, info, scaler)
#  raise_dialog(widget)
#  activate_dialog(dialog)
#  set_label_sensitive(widget, name, set_p)
#  set_sensitive(widget, flag)
#  add_main_pane(name, type, *args)
#
#  class Scale_widget
#    initialize(parent)
#    scale
#    label (only Motif)
#    add_scale(title, low, init, high, scale, kind)
#
#  class Dialog
#    initialize(label, ok_cb, reset_cb, clear_cb, help_cb)
#    dialog
#    parent
#    doit_string(*args)
#    dismiss_string(*args)
#    help_string(*args)
#    reset_string(*args)
#    clear_string(*args)
#    add_slider(title, low, init, high, scale, kind, parent) do |w, c, i| ... end
#    add_toggle(label, value) do |val| ... end
#    add_target(labels) do |val| ... end
#
# Snd_Gtk only:
# > module Snd_Gtk
# >  add_event_handler(parent, event, cb_data) do |w, e, d| ... end
# >  add_callback(parent, event, cb_data) do |w, d| ... end
# >  g_list_each(glist) do |val| ... end
#
# Snd_Motif only:
# > module Snd_Motif
# >  string2compound(*args)
# >  compound2string(xstr)
# >  get_xtvalue(widget, item)
# >  current_label(widget)
# >  current_screen
# >  get_pixmap(screen, file)
# >  screen_depth
# >  display_widget_tree(widget, spaces)
# >  add_sound_pane(snd, name, type, *args)
# >  add_channel_pane(snd, chn, name, type, *args)
# > 
# >  class Dialog
# >    add_frame(args)
# >    add_label(label, args)
# >    add_textfield(string, label, columns) do |w, c, i| ... end
# >    add_text(*args)
#
# class Menu
#   initialize(name, menu, args)
#   menu
#   each_entry do |child| ... end
#   change_menu_color(new_color)
# 
# class Snd_main_menu < Menu
#   initialize(name, parent, args) do ... end
#   menu_number
#   entry(klass, *rest) or entry(name) do ... end
#   separator
#   cascade(name, args) do ... end

# Code:

require "examp"

provided?("snd-motif") and (not provided?("xm")) and require("libxm.so")
provided?("snd-gtk")   and (not provided?("xg")) and require("libxg.so")

unless provided? "xm" or provided? "xg"
  error("%s requires --with-motif or --with-gtk and module libxm.so or --with-static-xm", __FILE__)
end

#
# --- functions working with Motif as well as with Gtk ---
#
module Snd_XM
  # main_widgets
  Top_level_application = 0
  Top_level_shell       = 1
  Main_pane_shell       = 2
  Main_sound_pane       = 3
  Listener_pane         = 4
  Notebook_outer_pane   = 5

  # menu_widgets
  Top_menu_bar = 0
  File_menu    = 1
  Edit_menu    = 2
  View_menu    = 3
  Options_menu = 4
  Help_menu    = 5

  # sound_widgets
  Main_pane        = 0
  Name_label       = 1
  Control_panel    = 2
  Minibuffer       = 3
  Play             = 4
  Filter_graph     = 5
  Unite            = 6
  Minibuffer_label = 7
  Name_icon        = 8
  Sync             = 9

  # channel_widgets
  Graph  = 0
  W      = 1
  F      = 2
  Sx     = 3
  Sy     = 4
  Zx     = 5
  Zy     = 6
  Edhist = 7
  Gsy    = 8
  Gzy    = 9

  # dialog_widgets
  Color_dialog        =  0
  Orientation_dialog  =  1
  Enved_dialog        =  2
  Error_dialog        =  3
  Yes_or_no_dialog    =  4
  Transform_dialog    =  5
  File_open_dialog    =  6
  File_save_as_dialog =  7
  View_files_dialog   =  8
  Raw_data_dialog     =  9
  New_file_dialog     = 10
  File_mix_dialog     = 11
  Edit_header_dialog  = 12
  Find_dialog         = 13
  Help_dialog         = 14
  Completion_dialog   = 15
  Mix_panel_dialog    = 16
  Print_dialog        = 17
  Recorder_dialog     = 18
  Region_dialog       = 19
  Info_dialog         = 20
  Track_dialog        = 21

  # MAKE_MENU as well as MAKE_POPUP_MENU may be used in non-Snd
  # scripts.  MAKE_SND_MENU and MAKE_SND_POPUP (see popup.rb) are
  # specialized using them in Snd.
  #
  #  [...]
  #  main_widget = RXmCreateMainWindow(top_level, "main", [])
  #  menu_bar = RXmCreateMenuBar(main_widget, "menu-bar", [])
  #  RXtManageChild(menu_bar)
  #  make_menu("file-button", menu_bar) do
  #    entry("quit") do |w, c, i| exit(0) end
  #  end
  #  make_menu("help-button", menu_bar) do
  #    entry("general-help") do |w, c, i| ... end
  #  end
  def make_menu(name, parent, &body)
    Main_menu.new(name, parent, [], &body)
  end

  #  play = lambda do |w, c, i|
  #    @play = Rset(i)
  #    send_values(!@play)
  #  end
  #  make_popup_menu("popup", main_widget) do
  #    entry("play", :widget_class, RxmToggleButtonWidgetClass, &play)
  #    separator
  #    entry("quit") do |w, c, i| exit(0) end
  #  end
  def make_popup_menu(name, parent, &body)
    Main_popup_menu.new(name, parent, [], &body)
  end

  def make_dialog(label, *rest, &ok_cb)
    reset_cb = get_args(rest, :reset_cb, nil)
    clear_cb = get_args(rest, :clear_cb, nil)
    help_cb  = get_args(rest, :help_cb, nil)
    help_str = get_args(rest, :info, nil)
    unless help_cb.kind_of?(Proc)
      if help_str.kind_of?(String) and !help_str.empty?
        help_cb = lambda do |w, c, i| help_dialog(label, help_str) end
      end
    end
    d = Dialog.new(label, ok_cb, reset_cb, clear_cb, help_cb)
    d.create_dialog
    d
  end

  # simple comment formatter, indents comment text in popup.rb and nb.rb
  # comment: long comment
  #          text ...
  #          and so on
  def format_sound_comment(com)
    if com.empty?
      com
    else
      len = 0
      text_length = if widget?(wid = dialog_widgets[Info_dialog])
                      widget_size(wid).first / 10
                    else
                      56
                    end
      indent_length = "comment: ".length
      str = ""
      format("comment: %s", com).split(/ /).each do |s|
        unless (len += s.length + 1) < text_length
          len = indent_length + s.length
          str << "\n" << " " * indent_length
        end
        str << s << " "
      end
      str << "\n"
    end
  end

  $selection_buttons = [] unless defined? $selection_buttons
  $mark_buttons = [] unless defined? $mark_buttons
  $semi_range = 24 unless defined? $semi_range
  $log_scale_ticks = 500 unless defined? $log_scale_ticks

  Log2 = log(2.0)
  
  def scale_log2linear(lo, val, hi)
    log_lo = log([lo, 1.0].max) / Log2
    log_hi = log(hi) / Log2
    log_val = log(val) / Log2
    $log_scale_ticks.to_f * ((log_val - log_lo) / (log_hi - log_lo))
  end

  def scale_linear2log(lo, val, hi)
    log_lo = log([lo, 1.0].max) / Log2
    log_hi = log(hi) / Log2
    log_val = log_lo + ((val / $log_scale_ticks.to_f) * (log_hi - log_lo))
    2.0 ** log_val
  end

  def scale_log_label(lo, val, hi)
    format("%1.2f", scale_linear2log(lo, val, hi))
  end

  def semi_scale_label(val)
    format("semitones: %d", val - $semi_range)
  end

  def semitones2ratio(val)
    (2.0 ** val) / 12.0
  end

  def ratio2semitones(ratio)
    (12.0 * (log(ratio) / log(2.0))).round
  end

  # get_color("ivory2")
  # get_color(0.93, 0.93, 0.87)
  # get_color(Ivory2) # from rgb.rb
  def get_color(*new_color)
    if new_color[0].kind_of?(String)
      create_color(new_color[0])
    elsif new_color.length == 3
      make_color(*new_color)
    elsif color?(new_color[0])
      new_color[0]
    else
      make_color(0.0, 0.0, 0.0)
    end
  end

  def yellow_pixel
    create_color("yellow")
  end

  def red_pixel
    create_color("red")
  end
  
  def update_label(list)
    if list.kind_of?(Array) and (not widget?(list))
      list.each do |prc| prc.call end
    end
  end

  def find_child(widget, name)
    callcc do |ret|
      each_child(widget) do |child|
        if widget_name(child) == name
          ret.call(child)
        end
      end
      false
    end
  end

  def set_label_sensitive(widget, name, set_p = false)
    if widget?(wid = find_child(widget, name))
      set_sensitive(wid, set_p)
    end
  end
end

#
# --- GTK ---
#
module Snd_Gtk
  def make_snd_menu(name, args = [], &body)
    Snd_main_menu.new(name, nil, args, &body)
  end

  def create_color(color_string)
    tmp = RGdkColor()
    Rgdk_color_parse(color_string, tmp)
    col = Rgdk_color_copy(tmp)
    Rgdk_rgb_find_color(Rgdk_colormap_get_system(), col)
    col
  end

  def white_pixel
    create_color("white")
  end
  
  def black_pixel
    create_color("black")
  end

  def change_label(widget, string)
    if widget
      if RGTK_IS_LABEL(widget)
        Rgtk_label_set_text(RGTK_LABEL(widget), string)
      else
        Rgtk_label_set_text(RGTK_LABEL(Rgtk_bin_get_child(RGTK_BIN(widget))), string)
      end
    end
  end
  
  def each_child(widget, &body)
    if RGTK_IS_WIDGET(widget)
      body.call(widget)
      Rgtk_container_foreach(RGTK_CONTAINER(widget), lambda do |w, d| body.call(RGTK_WIDGET(w)) end)
    end
  end
  alias for_each_child each_child

  def widget?(obj)
    if obj.kind_of?(Array) and obj.length == 2 and obj.last.kind_of?(Numeric)
      RGTK_IS_WIDGET(obj)
    else
      false
    end
  end

  def is_managed(widget)
    if widget?(widget)
      RGTK_WIDGET_REALIZED(widget)
    else
      false
    end
  end

  # you should have set the name before:
  # Rgtk_widget_set_name(widget, "my-name")
  def widget_name(widget)
    Rgtk_widget_get_name(widget)
  end

  def set_scale_value(widget, value, scaler = 1.0)
    Rgtk_adjustment_set_value(RGTK_ADJUSTMENT(widget), value)
    Rgtk_adjustment_value_changed(RGTK_ADJUSTMENT(widget))
  end

  def get_scale_value(widget, info, scaler = 1.0)
    Rvalue(RGTK_ADJUSTMENT(widget))
  end

  def raise_dialog(widget)
    Rgdk_window_raise(Rwindow(widget))
  end

  def activate_dialog(dialog)
    Rgtk_widget_show(dialog)
    raise_dialog(dialog)
  end

  def set_sensitive(widget, flag)
    Rgtk_widget_set_sensitive(widget, flag)
  end

  # This Gtk function works only if not started with -notebook.
  def add_main_pane(name, motif_type_not_needed = nil, *motif_args_not_needed)
    pane = Rgtk_hbox_new(false, 0)
    if RGTK_IS_BOX(main_widgets[Notebook_outer_pane])
      Rgtk_box_pack_start(RGTK_BOX(main_widgets[Notebook_outer_pane]), pane, false, false, 4)
    else
      error("%s doesn't work if Snd-Gtk is started with -notebook", get_func_name)
    end
    Rgtk_widget_show(pane)
    Rgtk_widget_set_name(pane, name)
    pane
  end
  
  # body.arity == 3
  # lambda do |w, e, d| ... end
  # cclosure_new "GClosure* g_cclosure_new(GCallback func,
  #                                        lambda_data func_data,
  #                                        GClosureNotify destroy_data)"
  def add_event_handler(parent, event, cb_data = false, &body)
    Rg_signal_connect_closure_by_id(RGPOINTER(parent),
                                    Rg_signal_lookup(event, RG_OBJECT_TYPE(RGTK_OBJECT(parent))),
                                    0,
                                    Rg_cclosure_new(lambda do |w, e, d|
                                                      body.call(w, e, d)
                                                    end, cb_data, false), false)
  end

  # body.arity == 2
  # lambda do |w, d| ... end
  def add_callback(parent, event, cb_data = false, &body)
    Rg_signal_connect_closure_by_id(RGPOINTER(parent),
                                    Rg_signal_lookup(event, RG_OBJECT_TYPE(RGTK_OBJECT(parent))),
                                    0,
                                    Rg_cclosure_new(lambda do |w, d|
                                                      body.call(w, d)
                                                    end, cb_data, false),
                                    false)
  end
  
  def g_list_each(glist)
    Rg_list_length(glist).times do |i|
      yield(Rg_list_nth_data(glist, i))
    end
  end
  
  class Scale_widget
    include Snd_XM

    def initialize(parent)
      @parent = if RGTK_IS_DIALOG(parent)
                  Rvbox(RGTK_DIALOG(parent))
                else
                  parent
                end
      @scale = nil
    end
    attr_reader :scale

    def add_scale(title, low, init, high, scale, kind)
      rc = Rgtk_table_new(3, 1, false)
      Rgtk_box_pack_start(RGTK_BOX(@parent), rc, false, false, 4)
      Rgtk_widget_show(rc)
      @scale = case kind
               when :log
                 Rgtk_adjustment_new(scale_log2linear(low, init, high),
                                     0, $log_scale_ticks, 1, 10, 1)
               else
                 Rgtk_adjustment_new(init, low, high, 0.0, 0.0, 0.0)
               end
      scl = Rgtk_hscale_new(RGTK_ADJUSTMENT(@scale))
      Rgtk_range_set_update_policy(RGTK_RANGE(RGTK_SCALE(scl)), RGTK_UPDATE_CONTINUOUS)
      Rgtk_scale_set_value_pos(RGTK_SCALE(scl), RGTK_POS_TOP)
      expand_fill = RGTK_EXPAND | RGTK_FILL
      case kind
      when :log
        Rgtk_scale_set_digits(RGTK_SCALE(scl), 0)
        Rgtk_scale_set_draw_value(RGTK_SCALE(scl), false)
        log_lab = Rgtk_label_new("%1.2f" % init)
        Rgtk_misc_set_alignment(RGTK_MISC(log_lab), 0.0, 0.0)
        Rgtk_table_attach(RGTK_TABLE(rc), log_lab, 0, 1, 0, 1, expand_fill, expand_fill, 0, 0)
        Rgtk_widget_show(log_lab)
        add_callback(@scale, "value_changed") do |w, d|
          change_label(log_lab, scale_log_label(low, Rvalue(RGTK_ADJUSTMENT(@scale)), high))
        end
      else
        Rgtk_scale_set_digits(RGTK_SCALE(scl), case scale
                                               when 1000
                                                 3
                                               when 100
                                                 2
                                               when 10
                                                 1
                                               else
                                                 0
                                               end)
        Rgtk_scale_set_draw_value(RGTK_SCALE(scl), true)
      end
      label = Rgtk_label_new(title)
      Rgtk_misc_set_alignment(RGTK_MISC(label), 0.0, 0.0)
      Rgtk_table_attach(RGTK_TABLE(rc), scl, 0, 1, 1, 2, expand_fill, expand_fill, 0, 0)
      Rgtk_widget_show(scl)
      Rgtk_table_attach(RGTK_TABLE(rc), label, 0, 1, 2, 3, expand_fill, expand_fill, 0, 0)
      Rgtk_widget_show(label)
      Rgtk_widget_set_name(scl, title)
    end
  end

  class Dialog
    include Snd_XM

    def initialize(label, ok_cb, reset_cb, clear_cb, help_cb)
      @label = label
      @ok_cb = ok_cb
      @reset_cb = reset_cb
      @clear_cb = clear_cb
      @help_cb = help_cb
      @doit = "DoIt"
      @dismiss = "Dismiss"
      @help = "Help"
      @reset = "Reset"
      @clear = "Clear"
      @dialog = nil
      @parent = nil
      @doit_button = nil
      @dismiss_button = nil
      @help_button = nil
      @reset_button = nil
      @clear_button = nil
    end
    attr_reader :dialog, :parent

    def doit_string(*args)
      change_label(@doit_button, @doit = format(*args))
    end

    def dismiss_string(*args)
      change_label(@dismiss_button, @dismiss = format(*args))
    end

    def help_string(*args)
      change_label(@help_button, @help = format(*args))
    end

    def reset_string(*args)
      change_label(@reset_button, @reset = format(*args))
    end

    def clear_string(*args)
      change_label(@clear_button, @clear = format(*args))
    end

    def create_dialog
      @dialog = Rgtk_dialog_new()
      @doit_button = Rgtk_button_new_with_label(@doit)
      Rgtk_widget_set_name(@doit_button, "doit_button")
      @dismiss_button = Rgtk_button_new_with_label(@dismiss)
      Rgtk_widget_set_name(@dismiss_button, "quit_button")
      @help_button = Rgtk_button_new_with_label(@help)
      Rgtk_widget_set_name(@help_button, "help_button")
      if @reset_cb
        @reset_button = Rgtk_button_new_with_label(@reset)
        Rgtk_widget_set_name(@reset_button, "reset_button")
      end
      if @clear_cb
        @clear_button = Rgtk_button_new_with_label(@clear)
        Rgtk_widget_set_name(@clear_button, "clear_button")
        # doit_again_button_color()
        Rgtk_rc_parse_string(%Q{
style "clear" = "default_button"
{
  bg[NORMAL] = "darkolivegreen1"
  bg[PRELIGHT] = "darkolivegreen1"
}
widget "*.clear_button" style "clear"})
      end
      Rgtk_window_set_title(RGTK_WINDOW(@dialog), @label)
      Rgtk_container_set_border_width(RGTK_CONTAINER(@dialog), 10)
      Rgtk_window_set_default_size(RGTK_WINDOW(@dialog), -1, -1)
      Rgtk_window_set_resizable(RGTK_WINDOW(@dialog), true)
      Rgtk_widget_realize(@dialog)
      add_event_handler(@dialog, "delete_event") do |w, e, d|
        Rgtk_widget_hide(@dialog)
      end
      Rgtk_box_pack_start(RGTK_BOX(Raction_area(RGTK_DIALOG(@dialog))),
                          @doit_button, true, true, 20)
      if @ok_cb.kind_of?(Proc)
        add_callback(@doit_button, "clicked") do |w, d|
          @ok_cb.call(w, d, nil)
        end
      end
      Rgtk_widget_show(@doit_button)
      if @clear_cb
        Rgtk_box_pack_start(RGTK_BOX(Raction_area(RGTK_DIALOG(@dialog))),
                            @clear_button, true, true, 20)
        if @clear_cb.kind_of?(Proc)
          add_callback(@clear_button, "clicked") do |w, d|
            @clear_cb.call(w, d, nil)
          end
        end
        Rgtk_widget_show(@clear_button)
      end
      if @reset_cb
        Rgtk_box_pack_start(RGTK_BOX(Raction_area(RGTK_DIALOG(@dialog))),
                            @reset_button, true, true, 20)
        if @reset_cb.kind_of?(Proc)
          add_callback(@reset_button, "clicked") do |w, d|
            @reset_cb.call(w, d, nil)
          end
        end
        Rgtk_widget_show(@reset_button)
      end
      Rgtk_box_pack_start(RGTK_BOX(Raction_area(RGTK_DIALOG(@dialog))),
                          @dismiss_button, true, true, 20)
      add_callback(@dismiss_button, "clicked") do |w, d|
        Rgtk_widget_hide(@dialog)
      end
      Rgtk_widget_show(@dismiss_button)
      Rgtk_box_pack_start(RGTK_BOX(Raction_area(RGTK_DIALOG(@dialog))),
                          @help_button, true, true, 20)
      if @help_cb.kind_of?(Proc)
        add_callback(@help_button, "clicked") do |w, d|
          @help_cb.call(w, d, nil)
        end
      end
      Rgtk_widget_show(@help_button)
      Rgtk_widget_set_name(@dialog, @label)
      @parent = Rvbox(RGTK_DIALOG(@dialog))
    end

    # kind :log, :linear
    # returns instance of Scale_widget not widget
    # so we can access the widget and label if needed
    # slider = @dialog.add_slider(...)
    # slider.scale --> widget
    # slider.label --> label
    def add_slider(title, low, init, high, scale = 1, kind = :linear, parent = @dialog, &func)
      slider = Scale_widget.new(parent)
      slider.add_scale(title, low, init, high, scale, kind)
      add_callback(slider.scale, "value_changed") do |w, d|
        func.call(w, d, nil)
      end
      slider
    end

    # change_cb.arity == 1
    def add_toggle(label = "truncate at end", value = true, &change_cb)
      wid = Rgtk_check_button_new_with_label(label)
      Rgtk_box_pack_start(RGTK_BOX(@parent), wid, false, false, 4)
      Rgtk_toggle_button_set_active(RGTK_TOGGLE_BUTTON(wid), value)
      Rgtk_widget_show(wid)
      add_callback(wid, "clicked") do |w, d|
        change_cb.call(Rgtk_toggle_button_get_active(RGTK_TOGGLE_BUTTON(w)))
      end
    end

    def add_target(labels = [["entire sound", :sound, true],
                             ["selection", :selection, false],
                             ["between marks", :marks, false]],
                   &target_cb)
      rc = Rgtk_hbox_new(false, 0)
      Rgtk_box_pack_start(RGTK_BOX(@parent), rc, false, false, 4)
      Rgtk_widget_show(rc)
      group = false
      labels.map do |name, type, on|
        wid = Rgtk_radio_button_new_with_label(group, name)
        group = Rgtk_radio_button_get_group(RGTK_RADIO_BUTTON(wid))
        Rgtk_box_pack_start(RGTK_BOX(rc), wid, false, false, 4)
        Rgtk_toggle_button_set_active(RGTK_TOGGLE_BUTTON(wid), on)
        Rgtk_widget_show(wid)
        add_callback(wid, "clicked") do |w, d|
          target_cb.call(type)
        end
        case type
        when :selection
          $selection_buttons.push(wid)
          set_sensitive(wid, false) unless selection?
        when :marks
          $mark_buttons.push(wid)
          set_sensitive(wid, false) unless marks?
        end
      end
    end
  end  
end

#
# --- Motif ---
#
module Snd_Motif
  def make_snd_menu(name, args = [RXmNbackground, basic_color], &body)
    Snd_main_menu.new(name, nil, args, &body)
  end

  def create_color(color)
    col = RXColor()
    dpy = RXtDisplay(main_widgets[Top_level_shell])
    if RXAllocNamedColor(dpy, RDefaultColormap(dpy, RDefaultScreen(dpy)), color, col, col).zero?
      error("can't allocate %s", color.inspect)
    else
      Rpixel(col)
    end
  end

  def white_pixel
    RWhitePixelOfScreen(current_screen)
  end
  
  def black_pixel
    RBlackPixelOfScreen(current_screen)
  end

  def change_label(widget, string, property = RXmNlabelString)
    xs = string2compound(string)
    RXtSetValues(widget, [property, xs])
    RXmStringFree(xs)
  end

  def each_child(widget, &body)
    if RWidget?(widget)
      body.call(widget)
      if (res = get_xtvalue(widget, RXmNchildren)).kind_of?(Array)
        res.each do |wid| each_child(wid) do |w| body.call(w) end if RXtIsComposite(widget) end
      end
    end
  end
  alias for_each_child each_child

  def widget?(obj)
    RWidget?(obj)
  end

  def is_managed(widget)
    RXtIsManaged(widget)
  end

  def widget_name(widget)
    RXtName(widget)
  end

  def set_scale_value(widget, value, scaler = 1.0)
    RXmScaleSetValue(widget, (value * Float(scaler)).round)
  end

  def get_scale_value(widget, info, scaler = 1.0)
    Rvalue(info) / Float(scaler)
  end

  def raise_dialog(widget)
    if RWidget?(widget) and RXtIsManaged(widget)
      parent = RXtParent(widget)
      if RWidget?(parent) and RXtIsSubclass(parent, RxmDialogShellWidgetClass)
        RXtPopup(parent, RXtGrabNone)
      end
    end
  end

  def activate_dialog(dialog)
    RXtIsManaged(dialog) ? raise_dialog(dialog) : RXtManageChild(dialog)
  end

  def set_sensitive(widget, flag)
    RXtSetSensitive(widget, flag)
  end
  
  def add_main_pane(name, type, *args)
    RXtCreateManagedWidget(name,
                           type,
                           (main_widgets[Notebook_outer_pane] or main_widgets[Main_sound_pane]),
                           *args)
  end

  def add_sound_pane(snd, name, type, *args)
    RXtCreateManagedWidget(name, type, sound_widgets(snd)[Main_pane], *args)
  end

  def add_channel_pane(snd, chn, name, type, *args)
    RXtCreateManagedWidget(name, type, RXtParent(RXtParent(channel_widgets(snd, chn)[Edhist])),
                           *args)
  end
  
  def string2compound(*args)
    RXmStringCreateLtoR(format(*args), RXmFONTLIST_DEFAULT_TAG)
  end

  def compound2string(xstr)
    RXmStringGetLtoR(xstr, RXmFONTLIST_DEFAULT_TAG)[1]
  end

  def get_xtvalue(widget, item)
    RXtVaGetValues(widget, [item, 0])[1]
  end

  def current_label(widget)
    compound2string(get_xtvalue(widget, RXmNlabelString))
  end

  def current_screen
    RDefaultScreenOfDisplay(RXtDisplay(main_widgets[Top_level_shell]))
  end

  def get_pixmap(screen, file)
    pix = RXmGetPixmap(screen, file, RBlackPixelOfScreen(screen), RWhitePixelOfScreen(screen))
    if pix == RXmUNSPECIFIED_PIXMAP
      error("can't create pixmap from %s", file.inspect)
    else
      pix
    end
  end
  
  def screen_depth
    RDefaultDepthOfScreen(current_screen)
  end
  
  def display_widget_tree(widget, spaces = "")
    unless name = RXtName(widget) or name.empty?
      name = "<unnamed>"
    end
    rbm_message("%s%s\n", spaces, name)
    if RXtIsComposite(widget) and (res = get_xtvalue(widget, RXmNchildren)).kind_of?(Array)
      res.each do |w| display_widget_tree(w, spaces + "  ") end
    end
  end
  
  class Scale_widget
    include Snd_XM

    def initialize(parent)
      @parent = parent
      @scale = nil
      @label = nil 
    end
    attr_reader :scale, :label

    def add_scale(title, low, init, high, scale, kind)
      xtitle = string2compound(title)
      rc = RXtCreateManagedWidget("rc", RxmRowColumnWidgetClass, @parent,
                                  [RXmNorientation, RXmVERTICAL,
                                   RXmNbackground, highlight_color])
      case kind
      when :log
        @label = RXtCreateManagedWidget(format("%1.2f", init),
                                        RxmLabelWidgetClass, rc,
                                        [RXmNalignment, RXmALIGNMENT_BEGINNING,
                                         RXmNbackground, basic_color])
        @scale = general_scale(rc, title, xtitle)
        RXtVaSetValues(@scale, [RXmNmaximum, $log_scale_ticks,
                                RXmNvalue, scale_log2linear(low, init, high).round])
        RXtAddCallback(@scale, RXmNvalueChangedCallback,
                       lambda do |w, c, i|
                         change_label(@label, scale_log_label(low, Rvalue(i), high))
                       end)
        RXtAddCallback(@scale, RXmNdragCallback,
                       lambda do |w, c, i|
                         change_label(@label, scale_log_label(low, Rvalue(i), high))
                       end)
      when :semi
        @label = RXtCreateManagedWidget(format("semitones: %d", ratio2semitones(init)),
                                        RxmLabelWidgetClass, rc,
                                        [RXmNalignment, RXmALIGNMENT_BEGINNING,
                                         RXmNbackground, basic_color])
        @scale = general_scale(rc, title, xtitle)
        RXtVaSetValues(@scale, [RXmNmaximum, 2 * $semi_range,
                                RXmNvalue, $semi_range + ratio2semitones(init)])
        RXtAddCallback(@scale, RXmNvalueChangedCallback,
                       lambda do |w, c, i| change_label(@label, semi_scale_label(Rvalue(i))) end)
        RXtAddCallback(@scale, RXmNdragCallback,
                       lambda do |w, c, i| change_label(@label, semi_scale_label(Rvalue(i))) end)
      else
        @scale = linear_scale(rc, title, xtitle, low, init, high, scale)
      end
      RXmStringFree(xtitle)
    end

    private
    def linear_scale(parent, title, xtitle, low, init, high, scale)
      RXtCreateManagedWidget(title, RxmScaleWidgetClass, parent,
                             [RXmNorientation, RXmHORIZONTAL,
                              RXmNshowValue, true,
                              RXmNminimum, (low * scale).round,
                              RXmNmaximum, (high * scale).round,
                              RXmNtitleString, xtitle,
                              RXmNbackground, basic_color,
                              RXmNvalue, (init * scale).round,
                              RXmNdecimalPoints, case scale
                                                 when 1000
                                                   3
                                                 when 100
                                                   2
                                                 when 10
                                                   1
                                                 else
                                                   0
                                                 end])
    end

    def general_scale(parent, title, xtitle)
      RXtCreateManagedWidget(title, RxmScaleWidgetClass, parent,
                             [RXmNorientation, RXmHORIZONTAL,
                              RXmNshowValue, false,
                              RXmNminimum, 0,
                              RXmNdecimalPoints, 0,
                              RXmNtitleString, xtitle,
                              RXmNbackground, basic_color])
    end
  end
  
  class Dialog
    include Snd_XM

    def initialize(label, ok_cb, reset_cb, clear_cb, help_cb)
      @label = label
      @ok_cb = ok_cb
      @reset_cb = reset_cb
      @clear_cb = clear_cb
      @help_cb = help_cb
      @doit = "DoIt"
      @dismiss = "Dismiss"
      @help = "Help"
      @reset = "Reset"
      @clear = "Clear"
      @dialog = nil
      @parent = nil
      @reset_button = nil
      @clear_button = nil
    end
    attr_reader :dialog, :parent

    def doit_string(*args)
      change_label(@dialog, @doit = format(*args), RXmNokLabelString)
    end

    def dismiss_string(*args)
      change_label(@dialog, @dismiss = format(*args), RXmNcancelLabelString)
    end

    def help_string(*args)
      change_label(@dialog, @help = format(*args), RXmNhelpLabelString)
    end

    def reset_string(*args)
      change_label(@reset_button, @reset = format(*args))
    end

    def clear_string(*args)
      change_label(@clear_button, @clear = format(*args))
    end
    
    def create_dialog
      xlabel = string2compound(@label)
      xdoit = string2compound(@doit)
      xdismiss = string2compound(@dismiss)
      xhelp = string2compound(@help)
      @dialog = RXmCreateTemplateDialog(main_widgets[Top_level_shell], @label,
                                        [RXmNokLabelString, xdoit,
                                         RXmNcancelLabelString, xdismiss,
                                         RXmNhelpLabelString, xhelp,
                                         RXmNautoUnmanage, false,
                                         RXmNdialogTitle, xlabel,
                                         RXmNresizePolicy, RXmRESIZE_GROW,
                                         RXmNnoResize, false,
                                         RXmNbackground, basic_color,
                                         RXmNtransient, false])
      RXmStringFree(xlabel)
      RXmStringFree(xdoit)
      RXmStringFree(xdismiss)
      RXmStringFree(xhelp)
      [RXmDIALOG_OK_BUTTON,
       RXmDIALOG_CANCEL_BUTTON,
       RXmDIALOG_HELP_BUTTON].zip([doit_button_color,
                                   quit_button_color,
                                   help_button_color]) do |button, color|
        RXtVaSetValues(RXmMessageBoxGetChild(@dialog, button),
                       [RXmNarmColor, pushed_button_color, RXmNbackground, color])
      end
      if defined?(R_XEditResCheckMessages())
        RXtAddEventHandler(RXtParent(@dialog), 0, true,
                           lambda do |w, c, i, f|
                             R_XEditResCheckMessages(w, c, i, f)
                           end)
      end
      RXtAddCallback(@dialog, RXmNokCallback,
                     lambda do |w, c, i| @ok_cb.call(w, c, i) end)
      RXtAddCallback(@dialog, RXmNcancelCallback,
                     lambda do |w, c, i| RXtUnmanageChild(@dialog) end)
      RXtAddCallback(@dialog, RXmNhelpCallback,
                     lambda do |w, c, i| @help_cb.call(w, c, i) end)
      if @clear_cb
        @clear_button = RXtCreateManagedWidget(@clear, RxmPushButtonWidgetClass, @dialog,
                                               [RXmNbackground, doit_again_button_color,
                                                RXmNforeground, black_pixel,
                                                RXmNhighlightColor, black_pixel,
                                                RXmNarmColor, pushed_button_color])
        RXtAddCallback(@clear_button, RXmNactivateCallback,
                       lambda do |w, c, i| @clear_cb.call(w, c, i) end)
      end
      if @reset_cb
        @reset_button = RXtCreateManagedWidget(@reset, RxmPushButtonWidgetClass, @dialog,
                                               [RXmNbackground, reset_button_color,
                                                RXmNforeground, black_pixel,
                                                RXmNhighlightColor, black_pixel,
                                                RXmNarmColor, pushed_button_color])
        RXtAddCallback(@reset_button, RXmNactivateCallback,
                       lambda do |w, c, i| @reset_cb.call(w, c, i) end)
      end
      @parent = RXtCreateManagedWidget("pane", RxmPanedWindowWidgetClass, @dialog,
                                       [RXmNsashHeight, 1,
                                        RXmNsashWidth, 1,
                                        RXmNbackground, basic_color,
                                        RXmNseparatorOn, true,
                                        RXmNalignment, RXmALIGNMENT_BEGINNING,
                                        RXmNorientation, RXmVERTICAL])
    end
    
    # kind :log, :semi, :linear
    # returns instance of Scale_widget not widget
    # so we can access the widget and label if needed
    # slider = @dialog.add_slider(...)
    # slider.scale --> widget
    # slider.label --> label
    def add_slider(title, low, init, high, scale = 1, kind = :linear, parent = @parent, &func)
      slider = Scale_widget.new(parent)
      slider.add_scale(title, low, init, high, scale, kind)
      unless func.kind_of?(Proc) and func.arity == 3
        func = lambda do |w, c, i| func.call end
      end
      RXtAddCallback(slider.scale, RXmNvalueChangedCallback, func)
      slider
    end

    # change_cb.arity == 1
    def add_toggle(label = "truncate at end", value = true, &change_cb)
      button = RXtCreateManagedWidget(label, RxmToggleButtonWidgetClass, @parent,
                                      [RXmNbackground, basic_color,
                                       RXmNalignment, RXmALIGNMENT_BEGINNING,
                                       RXmNset, value,
                                       RXmNselectColor, yellow_pixel])
      RXtAddCallback(button, RXmNvalueChangedCallback,
                     lambda do |w, c, i| change_cb.call(Rset(i)) end)
      h = get_xtvalue(button, RXmNheight)
      h += (h * 0.1).round
      RXtVaSetValues(button, [RXmNpaneMinimum, h, RXmNpaneMaximum, h])
      button
    end
    
    # target_cb.arity == 1
    def add_target(labels = [["entire sound", :sound, true],
                             ["selection", :selection, false],
                             ["between marks", :marks, false]],
                   &target_cb)
      rc = RXtCreateManagedWidget("rc", RxmRowColumnWidgetClass, @parent,
                                  [RXmNorientation, RXmHORIZONTAL,
                                   RXmNbackground, basic_color,
                                   RXmNradioBehavior, true,
                                   RXmNradioAlwaysOne, true,
                                   RXmNbottomAttachment, RXmATTACH_FORM,
                                   RXmNleftAttachment, RXmATTACH_FORM,
                                   RXmNrightAttachment, RXmATTACH_FORM,
                                   RXmNentryClass, RxmToggleButtonWidgetClass,
                                   RXmNisHomogeneous, true])
      wid = nil
      labels.map do |name, type, on|
        wid = RXtCreateManagedWidget(name, RxmToggleButtonWidgetClass, rc,
                                     [RXmNbackground, basic_color,
                                      RXmNalignment, RXmALIGNMENT_BEGINNING,
                                      RXmNset, on,
                                      RXmNselectColor, yellow_pixel,
                                      RXmNindicatorType, RXmONE_OF_MANY_ROUND,
                                      RXmNarmCallback,
                                      [lambda do |w, c, i| target_cb.call(type) end, false]])
        case type
        when :selection
          $selection_buttons.push(wid)
          RXtSetSensitive(wid, false) unless selection?
        when :marks
          $mark_buttons.push(wid)
          RXtSetSensitive(wid, false) unless marks?
        end
      end
      h = get_xtvalue(wid, RXmNheight)
      h += (h * 0.1).round
      RXtVaSetValues(rc, [RXmNpaneMinimum, h, RXmNpaneMaximum, h])
      rc
    end

    def add_frame(args = [])
      RXtCreateManagedWidget("frame", RxmFrameWidgetClass, @parent,
                             [RXmNshadowThickness, 4,
                              RXmNshadowType, RXmSHADOW_ETCHED_OUT] + args)
    end
    
    def add_label(label, args = [])
      RXtCreateManagedWidget(label, RxmLabelWidgetClass, @parent,
                             [RXmNalignment, RXmALIGNMENT_BEGINNING,
                              RXmNbackground, basic_color] + args)
    end

    def add_textfield(string, label = nil, columns = 80, &activate_cb)
      rc = RXtCreateManagedWidget("rc", RxmRowColumnWidgetClass, @parent,
                                  [RXmNorientation, RXmVERTICAL,
                                   RXmNbackground, basic_color])
      if label.kind_of?(String)
        RXtCreateManagedWidget(label, RxmLabelWidgetClass, rc,
                               [RXmNalignment, RXmALIGNMENT_BEGINNING,
                                RXmNbackground, basic_color])
      end
      text_field = RXtCreateManagedWidget("text", RxmTextFieldWidgetClass, rc,
                                          [RXmNvalue, string,
                                           RXmNresizeWidth, false,
                                           RXmNcolumns, columns,
                                           RXmNbackground, basic_color])
      RXtAddCallback(text_field, RXmNactivateCallback, activate_cb)
      RXtAddCallback(text_field, RXmNfocusCallback,
                     lambda do |w, c, i| RXtSetValues(w, [RXmNbackground, text_focus_color]) end)
      RXtAddCallback(text_field, RXmNlosingFocusCallback,
                     lambda do |w, c, i| RXtSetValues(w, [RXmNbackground, basic_color]) end)
      RXtAddEventHandler(text_field, REnterWindowMask, false,
                         lambda do |w, c, i, f| $mouse_enter_text_hook.call(w) end)
      RXtAddEventHandler(text_field, RLeaveWindowMask, false,
                         lambda do |w, c, i, f| $mouse_leave_text_hook.call(w) end)
      text_field
    end

    def add_text(*args)
      rows       = get_args(args, :rows, 16)
      columns    = get_args(args, :columns, 60)
      wordwrap   = get_args(args, :wordwrap, true)
      value      = get_args(args, :value, "")
      horizontal = get_args(args, :scroll_horizontal, false)
      text = RXmCreateScrolledText(@parent, "text",
                                   [RXmNtopAttachment, RXmATTACH_WIDGET,
                                    RXmNeditMode, RXmMULTI_LINE_EDIT,
                                    RXmNrows, rows,
                                    RXmNcolumns, columns,
                                    RXmNwordWrap, wordwrap,
                                    RXmNscrollHorizontal, horizontal,
                                    RXmNvalue, value,
                                    RXmNbackground, basic_color])
      RXtAddCallback(text, RXmNfocusCallback,
                     lambda do |w, c, i| RXtSetValues(w, [RXmNbackground, text_focus_color]) end)
      RXtAddCallback(text, RXmNlosingFocusCallback,
                     lambda do |w, c, i| RXtSetValues(w, [RXmNbackground, basic_color]) end)
      RXtAddEventHandler(text, REnterWindowMask, false,
                         lambda do |w, c, i, f| $mouse_enter_text_hook.call(w) end)
      RXtAddEventHandler(text, RLeaveWindowMask, false,
                         lambda do |w, c, i, f| $mouse_leave_text_hook.call(w) end)
      RXtManageChild(text)
      text
    end
  end
end

module Snd_XM
  if provided? "xm"
    include Snd_Motif
  elsif provided? "xg"
    include Snd_Gtk
  else
    error "neither Motif nor Gtk?"
  end
end

=begin
add_channel_pane(0, 0, "new-pane", RxmDrawingAreaWidgetClass,
                 [RXmNbackground, graph_color, RXmNforeground, data_color])
=end

# SND_MAIN_MENU (for a similar popup menu class see popup.rb)
#
# make_snd_menu(name, args) do ... end
#
# class Menu
#   initialize(name, menu, args)
#   menu
#   each_entry do |child| ... end
#   change_menu_color(new_color)
# 
# class Snd_main_menu < Menu
#   initialize(name, parent, args) do ... end
#   menu_number
#   entry(klass, *rest) or entry(name) do ... end
#   separator
#   cascade(name, args) do ... end
#
# `Snd_main_menu#entry(arg, *rest, &body)': If ARG is of kind Class,
# `entry' calls klass.new(*rest), so you can set initialize values
# (e.g. the label or other args).  If ARG is not of kind Class it is
# taken as a label string and a block must exist.  Classes for the
# menu must have a method `post_dialog' and `inspect'.  `inspect'
# shows the values in the menu label.  See the various examples in
# effects.rb.
#
# class Foo
#   def initialize(label, val1, val2)
#     @label = label
#     @val1 = val1
#     @val2 = val2
#     @dialog = nil
#     ...
#   end
# 
#   def inspect
#     format("%s (%.3f %.3f", @label, @val1, @val2)
#   end
# 
#   def post_dialog
#     ...
#     unless @dialog.kind_of?(Dialog) and RWidget?(@dialog.dialog)
#       ...
#       @dialog = make_dialog(@label,
#                             :info, "Help text",
#                             :reset_cb, lambda do |w, c, i|
#                               ... (reset your values)
#                             end) do |w, c, i|
#         ... (main action)
#       end
#       ...
#     end
#     activate_dialog(@dialog.dialog)
#   end
# end
# 
# make_snd_menu("Foo Menu") do
#   entry(Foo, 3.14, 0.0)
# end

=begin
# example menu using Effects (see effects.rb and new-effects.scm)
require "effects"

make_snd_menu("Effects") do
  cascade("Amplitude Effects") do
    entry(Gain, "Gain")
    entry(Normalize, "Normalize")
    entry(Gate, "Gate")
  end
  cascade("Delay Effects") do
    entry(Echo, "Echo")
    entry(Filtered_echo, "Filtered echo")
    entry(Modulated_echo, "Modulated echo")
  end
  separator
  entry("Octave-down") do down_oct() end
  entry("Remove DC") do
    lastx = lasty = 0.0
    map_chan(lambda do |inval|
               lasty = inval + (0.999 * lasty - lastx)
               lastx = inval
               lasty
             end)
  end
  entry("Spiker") do spike() end
end
=end

class Menu
  include Snd_XM

  def initialize(name, menu, args)
    @label = name
    @menu = menu
    @args = args
  end
  attr_reader :menu

  def inspect
    format("#<%s: label: %s, menu: %s, args: %s>",
           self.class, @label.inspect, @menu.inspect, @args.inspect)
  end

  def entry(name, *rest, &body)
    child = false
    if provided? "xm"
      args         = get_args(rest, :args, @args)
      widget_class = get_args(rest, :widget_class, RxmPushButtonWidgetClass)
      child = RXtCreateManagedWidget(name, widget_class, @menu, args)
      case widget_class
      when RxmPushButtonWidgetClass
        RXtAddCallback(child, RXmNactivateCallback, lambda do |w, c, i| body.call(w, c, i) end)
      when RxmToggleButtonWidgetClass
        RXtAddCallback(child, RXmNvalueChangedCallback, lambda do |w, c, i| body.call(w, c, i) end)
      end
    else
      child = Rgtk_menu_item_new_with_label(name)
      Rgtk_menu_shell_append(RGTK_MENU_SHELL(@menu), child)
      Rgtk_widget_show(child)
      add_callback(child, "activate") do |w, d| body.call(w, d, nil) end
    end
    child
  end

  def label(name, args = @args)
    label = false
    if provided? "xm"
      label = RXtCreateManagedWidget(name, RxmLabelWidgetClass, @menu, args)
    else
      label = Rgtk_menu_item_new_with_label(name)
      Rgtk_menu_shell_append(RGTK_MENU_SHELL(@menu), label)
      Rgtk_widget_show(label)
    end
    label
  end

  def separator(single = :single)
    if provided? "xm"
      line = (single == :double ? RXmDOUBLE_LINE : RXmSINGLE_LINE)
      RXtCreateManagedWidget("s", RxmSeparatorWidgetClass, @menu, [RXmNseparatorType, line])
    else
      sep = Rgtk_menu_item_new()
      Rgtk_menu_shell_append(RGTK_MENU_SHELL(@menu), sep)
      Rgtk_widget_show(sep)
    end
  end

  def each_entry(&body)
    each_child(@menu, &body)
  end

  # $menu.change_menu_color("ivory2")
  # $menu.change_menu_color([0.93, 0.93, 0.87])
  # require 'rgb'
  # $menu.change_menu_color(Ivory2)
  def change_menu_color(new_color)
    color_pixel = get_color(new_color)
    if provided? "xm"
      each_child(@menu) do |child| RXmChangeColor(child, color_pixel) end
    else
      each_child(@menu) do |child| Rgtk_widget_modify_bg(child, RGTK_STATE_NORMAL, color_pixel) end
    end
  end
end

class Snd_main_menu < Menu
  def initialize(name, parent, args, &body)
    if widget? parent
      @menu_number = -1
      super(name, parent, args)
    else
      @menu_number = add_to_main_menu(name, lambda do | | end)
      super(name, main_menu(@menu_number), args)
      instance_eval(&body) if block_given?
    end
  end
  attr_reader :menu_number

  def entry(arg, *rest, &body)
    if arg.class == Class
      menu = arg.new(*rest)
      if menu.respond_to?(:post_dialog)
        if provided? "xm"
          child = RXtCreateManagedWidget(rest[0].to_s, RxmPushButtonWidgetClass, @menu, @args)
          RXtAddCallback(child, RXmNactivateCallback, lambda do |w, c, i| menu.post_dialog end)
        else
          child = Rgtk_menu_item_new_with_label(rest[0].to_s)
          Rgtk_menu_shell_append(RGTK_MENU_SHELL(@menu), child)
          Rgtk_widget_show(child)
          add_callback(child, "activate") do |w, d|
            menu.post_dialog
          end
        end
        child
      else
        error("%s#%s: class %s does not respond to `post_dialog'",
              self.class, get_func_name, arg.class)
      end
    else
      if block_given?
        add_to_menu(@menu_number, arg, lambda do | | body.call end)
      else
        error("%s#%s: no block given", self.class, get_func_name)
      end
    end
  end
  
  def separator
    add_to_menu(@menu_number, false, false)
  end

  def cascade(name, args = @args, &body)
    cas = Cascade.new(name, @menu, args)
    cas.instance_eval(&body) if block_given?
    cas
  end
  
  class Cascade < Snd_main_menu
    def initialize(name, parent, args)
      super
      @children = []
      if provided? "xm"
        @menu = RXmCreatePulldownMenu(parent, @label, @args)
        cascade = RXtCreateManagedWidget(@label, RxmCascadeButtonWidgetClass, parent,
                                         [RXmNsubMenuId, @menu] + @args)
        RXtAddCallback(cascade, RXmNcascadingCallback,
                       lambda do |w, c, i| update_label(@children) end)
      else
        cascade = Rgtk_menu_item_new_with_label(@label)
        Rgtk_menu_shell_append(RGTK_MENU_SHELL(parent), cascade)
        Rgtk_widget_show(cascade)
        @menu = Rgtk_menu_new()
        Rgtk_menu_item_set_submenu(RGTK_MENU_ITEM(cascade), @menu)
        add_callback(cascade, "activate") do |w, d|
          update_label(@children)
        end
      end
    end
    
    def entry(arg, *rest, &body)
      child = false
      if arg.class == Class
        menu = arg.new(*rest)
        if menu.respond_to?(:post_dialog)
          if provided? "xm"
            child = RXtCreateManagedWidget(rest[0].to_s, RxmPushButtonWidgetClass, @menu, @args)
            RXtAddCallback(child, RXmNactivateCallback, lambda do |w, c, i| menu.post_dialog end)
          else
            child = Rgtk_menu_item_new_with_label(rest[0].to_s)
            Rgtk_menu_shell_append(RGTK_MENU_SHELL(@menu), child)
            Rgtk_widget_show(child)
            add_callback(child, "activate") do |w, d|
              menu.post_dialog
            end
          end
          @children.push(lambda do change_label(child, menu.inspect) end)
        else
          error("%s#%s: class %s does not respond to `post_dialog'",
                self.class, get_func_name, arg.class)
        end
      else
        if block_given?
          if provided? "xm"
            child = RXtCreateManagedWidget(arg.to_s, RxmPushButtonWidgetClass, @menu, @args)
            RXtAddCallback(child, RXmNactivateCallback, lambda do |w, c, i| body.call end)
          else
            child = Rgtk_menu_item_new_with_label(arg.to_s)
            Rgtk_menu_shell_append(RGTK_MENU_SHELL(@menu), child)
            Rgtk_widget_show(child)
            add_callback(child, "activate") do |w, d|
              body.call
            end
          end
          change_label(child, arg)
        else
          error("%s#%s: no block given", self.class, get_func_name)
        end
      end
      child
    end
    
    def separator(single = :single)
      if provided? "xm"
        line = (single == :double ? RXmDOUBLE_LINE : RXmSINGLE_LINE)
        RXtCreateManagedWidget("s", RxmSeparatorWidgetClass, @menu, [RXmNseparatorType, line])
      else
        sep = Rgtk_menu_item_new()
        Rgtk_menu_shell_append(RGTK_MENU_SHELL(@menu), sep)
        Rgtk_widget_show(sep)
      end
    end
  end
end

# non-Snd menu functions, may be used outside Snd scripts
class Main_menu < Menu
  def initialize(name, parent, args, &body)
    super(name, parent, args)
    if provided? "xm"
      @menu = RXmCreatePulldownMenu(parent, "pulldown-menu", @args)
      wid = RXtCreateManagedWidget(@label, RxmCascadeButtonWidgetClass, parent,
                                   [RXmNsubMenuId, @menu] + @args)
      RXtVaSetValues(parent, [RXmNmenuHelpWidget, wid]) if name =~ /help/
    else
      wid = Rgtk_menu_item_new_with_label(@label)
      Rgtk_menu_shell_append(RGTK_MENU_SHELL(parent), wid)
      Rgtk_widget_show(wid) 
      @menu = Rgtk_menu_new()
      Rgtk_menu_item_set_submenu(RGTK_MENU_ITEM(wid), @menu)
    end
    instance_eval(&body) if block_given?
  end
end

class Main_popup_menu < Menu
  def initialize(name, parent, args, &body)
    super(name, parent, args)
    @parent = parent
    if provided? "xm"
      @menu = RXmCreatePopupMenu(@parent, "popup-menu", [RXmNpopupEnabled, true] + @args)
      RXtAddEventHandler(@parent, RButtonPressMask, false,
                         lambda do |w, c, i, f|
                           if Rbutton(i) == 3
                             RXmMenuPosition(@menu, i)
                             RXtManageChild(@menu)
                           end
                         end)
    else
=begin
# FIXME: needs some work
      @menu = Rgtk_menu_new()
      add_event_handler(@parent, "button_press_event") do |w, e, d|
        ev = RGDK_EVENT_BUTTON(e)
        if Rbutton(ev) == 3
          Rgtk_widget_show(@menu)
          Rgtk_menu_popup(RGTK_MENU(@menu), false, false, false, false, Rbutton(ev), Rtime(ev))
        end
      end
=end
    end
    unless @label.empty?
      label(@label)
      separator
    end
    instance_eval(&body) if block_given?
  end
end

# snd-xm.rb ends here
