# snd-motif.rb -- functions related to libxm.so

# Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Mon Jan 12 15:31:54 CET 2004
# Last: Thu Feb 12 15:30:04 CET 2004

# Commentary:
#
# Requires Motif module (libxm.so|xm.so) or --with-static-xm!
#
# Tested with Snd 7.2, Motif 2.1, Ruby 1.6.6 and 1.9.0.
#
# module Snd_Motif (see snd-motif.scm)
#  string2compound(*args)
#  compound2string(xstr)
#  get_xtvalue(widget, item)
#  update_label(list)
#  change_label(widget, string, property)
#  current_label(widget)
#  current_screen
#  white_pixel
#  black_pixel
#  yellow_pixel
#  get_color(new_color)
#  create_color(widget, color)
#  get_pixmap(screen, file)
#  screen_depth
#  each_child(widget) do |w| .... end
#  find_child(widget, name)
#  display_widget_tree(widget, spaces)
#  add_channel_pane(snd, chn, name, type, *args)
#  add_sound_pane(snd, name, type, *args)
#  add_main_pane(name, type, *args)
#  raise_dialog(widget)
#  activate_dialog(dialog)
#  set_label_sensitive(widget, name, set_p)
#  format_sound_comment(comment)
#
#  scale_log2linear(lo, val, hi)
#  scale_linear2log(lo, val, hi)
#  scale_log_label(lo, val, hi)
#  semi_scale_label(val)
#  semitones2ratio
#  ratio2semitones
#
#  class Scale_widget
#    initialize(parent)
#    scale
#    label
#    add_scale(title, low, init, high, scale, kind)
#
#  make_dialog(label, *rest) do |w, c, i| ... end
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
#    add_frame(args)
#    add_label(label, args)
#    add_textfield(string, label, columns) do |w, c, i| ... end
#    add_text(*args)
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

# Code:

require "English"
require "examp"

if $LOADED_FEATURES.member?("snd-motif")
  with_silence(LoadError) do require("xm") end unless $LOADED_FEATURES.member?("xm")
  with_silence(LoadError) do require("libxm") end unless $LOADED_FEATURES.member?("xm")
end

unless $LOADED_FEATURES.member?("xm")
  error("%s requires Motif module (libxm.so|xm.so) or --with-static-xm", __FILE__)
end

module Snd_Motif
  Appcontext = 0
  Top_level = 1
  Main_pane = 2
  Sound_pane = 3
  Listener = 4
  Notebook = 5

  Top_menu_bar = 0
  File_menu = 1
  Edit_menu = 2
  View_menu = 3
  Options_menu = 4
  Help_menu = 5

  View_files_dialog = 8
  Help_dialog = 14
  Info_dialog = 20
  
  def string2compound(*args)
    RXmStringCreateLtoR(format(*args), RXmFONTLIST_DEFAULT_TAG)
  end

  def compound2string(xstr)
    RXmStringGetLtoR(xstr, RXmFONTLIST_DEFAULT_TAG)[1]
  end

  def get_xtvalue(widget, item)
    RXtVaGetValues(widget, [item, 0])[1]
  end
  
  def update_label(list)
    list.each do |prc| prc.call end if list.kind_of?(Array) and !RWidget?(list)
  end

  def change_label(widget, string, property = RXmNlabelString)
    xs = string2compound(string)
    RXtSetValues(widget, [property, xs])
    RXmStringFree(xs)
  end

  def current_label(widget)
    compound2string(get_xtvalue(widget, RXmNlabelString))
  end

  def current_screen
    RDefaultScreenOfDisplay(RXtDisplay(main_widgets[Top_level]))
  end

  def white_pixel
    RWhitePixelOfScreen(current_screen)
  end
  
  def black_pixel
    RBlackPixelOfScreen(current_screen)
  end

  def yellow_pixel
    create_color(main_widgets[Top_level], "yellow")
  end

  # get_color("ivory2")
  # get_color([0.93, 0.93, 0.87])
  # get_color(Ivory2) # from rgb.rb
  def get_color(new_color)
    if new_color.kind_of?(String)
      create_color(main_widgets[Top_level], new_color)
    elsif new_color.kind_of?(Array) and new_color.length == 3
      make_color(new_color[0], new_color[1], new_color[2])
    elsif color?(new_color)
      new_color
    else
      make_color(0.0, 0.0, 0.0)
    end
  end

  def create_color(widget, color)
    col = RXColor()
    dpy = RXtDisplay(widget)
    if RXAllocNamedColor(dpy, RDefaultColormap(dpy, RDefaultScreen(dpy)), color, col, col).zero?
      error("can't allocate %s", color.inspect)
    else
      Rpixel(col)
    end
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

  def each_child(widget, &body)
    if RWidget?(widget)
      body.call(widget)
      if (res = get_xtvalue(widget, RXmNchildren)).kind_of?(Array)
        res.each do |wid| each_child(wid) do |w| body.call(w) end if RXtIsComposite(widget) end
      end
    end
  end
  alias for_each_child each_child

  def find_child(widget, name)
    callcc do |ret|
      each_child(widget) do |child| ret.call(child) if RXtName(child) == name end
      false
    end
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

  def add_channel_pane(snd, chn, name, type, *args)
    RXtCreateManagedWidget(name, type, RXtParent(RXtParent(channel_widgets(snd, chn)[7])), *args)
  end

  def add_sound_pane(snd, name, type, *args)
    RXtCreateManagedWidget(name, type, sound_widgets(snd)[0], *args)
  end
  
  def add_main_pane(name, type, *args)
    RXtCreateManagedWidget(name, type, (main_widgets[Notebook] or main_widgets[Sound_pane]), *args)
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

  def set_label_sensitive(widget, name, set_p = false)
    if RWidget?(wid = find_child(widget, name))
      RXtSetSensitive(wid, set_p)
    end
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
      text_length = if RWidget?(wid = dialog_widgets[Info_dialog])
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

  def scale_log2linear(lo, val, hi)
    log2 = log(2.0)
    log_lo = log([lo, 1.0].max) / log2
    log_hi = log(hi) / log2
    log_val = log(val) / log2
    ($log_scale_ticks.to_f * (log_val - log_lo) / (log_hi - log_lo)).round
  end

  def scale_linear2log(lo, val, hi)
    log2 = log(2.0)
    log_lo = log([lo, 1.0].max) / log2
    log_hi = log(hi) / log2
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
  
  class Scale_widget
    include Snd_Motif

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
                                RXmNvalue, scale_log2linear(low, init, high)])
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
  
  class Dialog
    include Snd_Motif

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
      @dialog = RXmCreateTemplateDialog(main_widgets[Top_level], @label,
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

def make_snd_menu(name, args = [RXmNbackground, basic_color], &body)
  Snd_main_menu.new(name, nil, args, &body)
end

class Menu
  include Snd_Motif

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

  def each_entry(&body)
    each_child(@menu, &body)
  end

  # $menu.change_menu_color("ivory2")
  # $menu.change_menu_color([0.93, 0.93, 0.87])
  # require 'rgb'
  # $menu.change_menu_color(Ivory2)
  def change_menu_color(new_color)
    color_pixel = get_color(new_color)
    each_child(@menu) do |child| RXmChangeColor(child, color_pixel) end
  end
end

class Snd_main_menu < Menu
  def initialize(name, parent, args, &body)
    if RWidget?(parent)
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
        child = RXtCreateManagedWidget(rest[0].to_s, RxmPushButtonWidgetClass, @menu, @args)
        RXtAddCallback(child, RXmNactivateCallback, lambda do |w, c, i| menu.post_dialog end)
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
      @menu = RXmCreatePulldownMenu(parent, @label, @args)
      cascade = RXtCreateManagedWidget(@label, RxmCascadeButtonWidgetClass, parent,
                                       [RXmNsubMenuId, @menu] + @args)
      RXtAddCallback(cascade, RXmNcascadingCallback,
                     lambda do |w, c, i| update_label(@children) end)
    end
    
    def entry(arg, *rest, &body)
      if arg.class == Class
        menu = arg.new(*rest)
        if menu.respond_to?(:post_dialog)
          child = RXtCreateManagedWidget(rest[0].to_s, RxmPushButtonWidgetClass, @menu, @args)
          RXtAddCallback(child, RXmNactivateCallback, lambda do |w, c, i| menu.post_dialog end)
          @children.push(lambda do change_label(child, menu.inspect) end)
          child
        else
          error("%s#%s: class %s does not respond to `post_dialog'",
                self.class, get_func_name, arg.class)
        end
      else
        if block_given?
          child = RXtCreateManagedWidget(arg.to_s, RxmPushButtonWidgetClass, @menu, @args)
          RXtAddCallback(child, RXmNactivateCallback, lambda do |w, c, i| body.call end)
          change_label(child, arg)
          child
        else
          error("%s#%s: no block given", self.class, get_func_name)
        end
      end
    end
    
    def separator
      RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass, @menu,
                             [RXmNseparatorType, RXmSINGLE_LINE])
    end
  end
end

# snd-motif.rb ends here
