# nb.rb -- translation of nb.rb

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Sat Dec 27 18:02:11 CET 2003

# User functions
#  make_nb(path)            install mouse hooks
#  remove_nb()              remove mouse hooks
#  write_nb(*args)          write info in database
#  delete_nb(path, file)    delete info in database
#  prune_nb(path)           delete empty entries
#
# If you have libxm:
#  make_nb_motif(path)      install mouse hooks
#
# The latter installs a mouse-button-3 event on info widget.  With
# mouse-button-3 a popup menu will be invoked where you can open an
# edit widget, clear the current entry or delete empty entries in
# database.  The edit widget may be used for editing current file info
# entries.

SND_NB_VERSION = "27-Dec-2003"

require "English"
$HAVE_MOTIF = $LOADED_FEATURES.member?("xm")
begin
  require "libxm" unless $HAVE_MOTIF
rescue LoadError
  $HAVE_MOTIF = false
end
require "examp"
require "hooks"

module NB
    doc "#{self.class} #{self.name} is a translation of nb.scm.

Provide pop-up help in the Files viewer.
If you have `dbm', any data associated with the file in the dbm
database will also be posted.  The database name is defined by
$nb_database (#{$nb_database.inspect}).  You may replace the require
statement with an other database library which you have, e.g. `gdbm'.

The function nb(note[, file=@current_file]) adds NOTE to the info
currently associated with FILE.
To remove a file's info, unb([file=@current_file]).
To clean non-existent file references out of the database,
prune_db()."

  $nb_database ||= "nb"
  Current_file_viewer = 0
  Previous_file_viewer = 1
  Region_viewer = 2
  Files_dialog = 8
  Info_dialog = 20
  
  def info_widget
    dialog_widgets()[Info_dialog]
  end

  def info_widget?
    RWidget?(info_widget)
  end

  def files_widget
    dialog_widgets()[Files_dialog]
  end

  def make_nb(path = $nb_database)
    doc("make_nb([path = $nb_database])
Install mouse enter and leave hooks in Files viewer.\n") if path == :help
    nb = NB_info.new(path)
    $mouse_enter_label_hook.add_hook!("nb_enter") do |t, p, n|
      nb.files_popup_info(t, p, n)
    end
    $mouse_leave_label_hook.add_hook!("nb_leave") do |t, p, n|
      nb.files_popup_quit(t, p, n)
    end
    nb
  end

  if $HAVE_MOTIF
    def make_nb_motif(path = $nb_database)
      doc("make_nb_motif([path = $nb_database])
Install mouse enter and leave hooks in Files viewer
and creates popup on Info viewer.\n") if path == :help
      $mnb ||= Edit_info.new(path)
      $mouse_enter_label_hook.add_hook!("nb_edit_info_enter") do |t, p, n|
        $mnb.mouse_enter_label(t, p, n)
      end
      $mouse_leave_label_hook.add_hook!("nb_edit_info_leave") do |t, p, n|
        $mnb.files_popup_quit(t, p, n)
      end
      $mnb
    end

    # to use in popup.rb
    def show_edit_window(path = $nb_database, file = nil)
      $mnb ||= Edit_info.new(path)
      $mnb.mouse_enter_label(Current_file_viewer, nil, file)
    end
=begin
  # in popup.rb
  ["Edit info", RxmPushButtonWidgetClass, every_menu,
   lambda do |w, c, i| show_edit_window($nb_database, file_name($graph_popup_snd)) end]
=end
  end
  
  def remove_nb(help = nil)
    doc("remove_nb()
Remove mouse enter and leave hooks in Files view.\n") if help == :help
    $mouse_enter_label_hook.remove_hook!("nb_enter")
    $mouse_enter_label_hook.remove_hook!("nb_edit_info_enter")
    $mouse_leave_label_hook.remove_hook!("nb_leave")
    $mouse_leave_label_hook.remove_hook!("nb_edit_info_leave")
  end

  def write_nb(*args)
    doc("write_nb(*args)
     :path,    $nb_database (#$nb_database)
     :file,    nil
Write ARGS in the dbm database of current file.  ARGS means
format(*args).  If you want a special database or snd file you can
give the named options as first options. e.g.

write_nb(:file, file_name(3), :path, 'dbfile', \"%d points in curve\", 3)

If FILE is nil, write to current file.") if get_args(args, :help, false)
    path = get_args(args, :path, $nb_database)
    file = get_args(args, :file, nil)
    while args[0] == :path or args[0] == :file
      args.shift
      args.shift
    end
    NB_info.new(path).nb(format(*args), file)
  end

  def delete_nb(path = $nb_database, file = nil)
    doc("delete_nb([path = $nb_database [, file = nil]])
Remove FILE's info.  If FILE is nil, remove current FILE's info.\n") if path == :help
    NB_info.new(path).unb(file)
  end

  def prune_nb(path = $nb_database)
    doc("prune_nb([path = $nb_database]
Clean non-existent file references out of the database.\n") if path == :help
    NB_info.new(path).prune_db()
  end

  class NB_info
    require "dbm"

    def initialize(path = $nb_database)
      @current_file = sounds() and file_name(false)
      @nb_database = path
    end

    def nb(note, file = nil)
      doc("nb(note[, file=nil])
Add NOTE to the info associated with FILE (defaults to @current_file.\n") if note == :help
      file ||= (@current_file or file_name(false))
      ptr = file_info(file).first
      if ptr
        current_note = (ptr.key?(file) ? ptr.fetch(file) : "")
        ptr.store(file, current_note.empty? ? note : (note + "\n" + current_note))
        ptr.close
      end
      files_popup_info(Current_file_viewer, nil, file)
    end

    def unb(file = nil)
      doc("unb([file=nil])
Removes FILE's info from the nb (dbm) data base.  FILE defaults to
@current_file.\n") if file == :help
      file ||= (@current_file or file_name(false))
      ptr = file_info(file).first
      if ptr
        ptr.delete(file)
        ptr.close
      end
      files_popup_info(Current_file_viewer, nil, file)
    end

    def prune_db(doc = nil)
      doc("prune_db()
Clean up the nb (dbm) data base by removing references to
non-existent files.\n") if doc == :help
      ptr = DBM.open(@nb_database) rescue warn("DBM.open(#{@nb_database}")
      ptr.delete_if do |k, v| k.empty? end if ptr
      ptr.close
    end

    def files_popup_info(type, position = nil, file = nil)
      doc("files_popup_info(type, position, file)
It's intended as a mouse-enter-label hook function.
It causes a description of the file to popup when the mouse crosses
the filename.\n") if type == :help
      file ||= (@current_file or file_name(false))
      ptr, str, notes = file_info(file)
      ptr.close if ptr
      alert_color = make_color(1.0, 1.0, 0.94)
      unless type == Region_viewer
        @current_file = file
        info_dialog(file, str)
        unless info_widget?
          files_position = widget_position(files_widget)
          set_widget_position(info_widget, [files_position[0] + 10, files_position[1] + 10])
        end
        recolor_widget(info_widget, alert_color) if info_widget?
      end
      notes
    end

    def files_popup_quit(type, *args)
      doc("files_popup_quit(type, *args)
It's intended as a mouse-leave-label hook function.
It unhighlights the popped-up info about a file as the mouse leaves
the associated label.\n") if type == :help
      recolor_widget(info_widget, basic_color()) if info_widget?
    end

    def file_info(file)
      ptr = DBM.open(@nb_database) rescue warn("DBM.open(#{@nb_database}")
      notes = ((ptr and ptr.key?(file)) ? ptr.fetch(file) : "")
      chans = mus_sound_chans(file)
      srate = mus_sound_srate(file)
      len = format("%.3f", (mus_sound_samples(file).to_f / (chans * srate)))
      d_format = mus_data_format_name(mus_sound_data_format(file))
      h_type = mus_header_type_name(mus_sound_header_type(file))
      date = Time.at(mus_sound_write_date(file)).localtime.strftime "%a %d-%b-%y %H:%M %Z"
      comm = mus_sound_comment(file)
      loops = mus_sound_loop_info(file)
      str = format("\
  chans: %d, srate: %d
 length: %.3f (%d samples)
 format: %s [%s]
written: %s
%s%s\n%s",
                   chans, srate, len, mus_sound_frames(file), d_format, h_type, date,
                   comm.empty? ? "" : "comment: #{comm}\n",
                   loops ? "   loop: #{loops.inspect}\n" : "", (notes or ""))
      [ptr, str, notes]
    end
    private :file_info
  end

  if $HAVE_MOTIF
    module Motif
      def focus_cb(w, c, i)
        RXtVaSetValues(w, [RXmNbackground, text_focus_color()])
      end
      
      def losing_cb(w, c, i)
        RXtVaSetValues(w, [RXmNbackground, basic_color()])
      end
      
      def cancel_edit_cb(w, c, i)
        RXtUnmanageChild(w)
      end
      
      def clear_text_widget_cb(w, c, i)
        RXmTextSetString(c, "")
      end
      
      def string2compound(*args)
        RXmStringCreateLocalized(format(*args))
      end
      
      # data[0] = "name"
      # data[1] = callback
      def make_menu_item(widget, data)
        if data.first == :sep_one
          RXtVaCreateManagedWidget("sep_one", RxmSeparatorGadgetClass, widget,
                                   [RXmNseparatorType, RXmSINGLE_LINE])
        elsif data.first == :sep_two
          RXtVaCreateManagedWidget("sep_two", RxmSeparatorGadgetClass, widget,
                                   [RXmNseparatorType, RXmDOUBLE_LINE])
        else
          w = RXtVaCreateManagedWidget(data.first, RxmPushButtonGadgetClass, widget,
                                       [RXmNbackground, basic_color()])
          RXtAddCallback(w, RXmNactivateCallback, data.last)
        end
      end
      
      def make_popup_menu(widget, name, *args)
        wid = RXmCreatePopupMenu(widget, "popup",
                                 [RXmNpopupEnabled, true, RXmNbackground, basic_color()])
        RXtVaCreateManagedWidget(name, RxmLabelGadgetClass, wid, [])
        RXtVaCreateManagedWidget("s", RxmSeparatorGadgetClass, wid,
                                 [RXmNseparatorType, RXmDOUBLE_LINE])
        RXtAddEventHandler(widget, RButtonPressMask, false,
                           lambda do |w, c, i, f|
                             if(Rbutton(i) == 3)
                               RXmMenuPosition(c, i)
                               RXtManageChild(c)
                             end
                           end, wid)
        args.each do |data| make_menu_item(wid, data) end
        wid
      end
    end
    
    class Edit_info < NB_info
      include Motif
      F_INFO = "File DB Info"
      
      def initialize(path = $nb_database)
        @popup_widget = @text_widget = @file_widget = @edit_widget = nil
        super
      end
      
      def popup_widget?
        RWidget?(@popup_widget)
      end
      
      def edit_widget?
        RWidget?(@edit_widget)
      end
      
      def make_edit_popup
        unless popup_widget?
          if info_widget?
            @popup_widget = make_popup_menu(info_widget, F_INFO,
                                            ["Edit info", method(:edit_cb).to_proc],
                                            ["Prune DB", lambda do |w, c, i| prune_db() end],
                                            ["Clear current info", lambda do |w, c, i| unb() end],
                                            [:sep_two],
                                            ["Help", method(:help_cb).to_proc])
            RXtAddEventHandler(@popup_widget, RButtonPressMask, false,
                               method(:button_cb).to_proc, :popup_widget)
          end
        end
      end
      
      def mouse_enter_label(type, pos, file)
        show_edit_window(file, files_popup_info(type, pos, file))
      end
      
      def show_edit_window(file, notes)
        popup_widget? or make_edit_popup
        set_edit_widget(file, notes)
      end
      
      def button_cb(w, c, i, f)
        Rbutton(i) == 3 and edit_cb(w, c, i)
      end
      
      def write_string_cb(w, c, i)
        unb()
        nb(RXmTextGetString(c))
      end
      
      def set_edit_widget(file, notes)
        @file_widget and RXtVaSetValues(@file_widget, [RXmNlabelString, string2compound(file)])
        @text_widget and RXtVaSetValues(@text_widget, [RXmNvalue, notes])
      end
      
      def edit_cb(w, c, i)
        unless edit_widget?
          edit = RXmCreateTemplateDialog(main_widgets()[1], "edit",
                                         [RXmNtitle, F_INFO,
                                          RXmNbackground, basic_color(),
                                          RXmNautoUnmanage, false,
                                          RXmNnoResize, false,
                                          RXmNtransient, false,
                                          RXmNokLabelString, string2compound("Submit"),
                                          RXmNcancelLabelString, string2compound("Dismiss"),
                                          RXmNhelpLabelString, string2compound("Help")])
          form = RXtVaCreateManagedWidget("form", RxmFormWidgetClass, edit,
                                          [RXmNbackground, basic_color()])
          info = RXtVaCreateManagedWidget("info", RxmLabelGadgetClass, form,
                                          [RXmNtopAttachment, RXmATTACH_FORM,
                                           RXmNleftAttachment, RXmATTACH_FORM,
                                           RXmNrightAttachment, RXmATTACH_FORM,
                                           RXmNleftOffset, 10,
                                           RXmNalignment, RXmALIGNMENT_BEGINNING])
          text = RXmCreateScrolledText(form, "text",
                                       [RXmNtopAttachment, RXmATTACH_WIDGET,
                                        RXmNtopWidget, info,
                                        RXmNtopOffset, 10,
                                        RXmNeditMode, RXmMULTI_LINE_EDIT,
                                        RXmNrows, 16,
                                        RXmNcolumns, 60,
                                        RXmNwordWrap, true,
                                        RXmNscrollHorizontal, false,
                                        RXmNbackground, basic_color()])
          clear = RXtCreateManagedWidget("Clear", RxmPushButtonWidgetClass, edit,
                                         [RXmNarmColor, pushed_button_color(),
                                          RXmNbackground, reset_button_color(),
                                          RXmNforeground, data_color()])
          RXtAddCallback(clear, RXmNactivateCallback, method(:clear_text_widget_cb).to_proc, text)
          RXtAddCallback(edit, RXmNokCallback, method(:write_string_cb).to_proc, text)
          RXtAddCallback(edit, RXmNcancelCallback, method(:cancel_edit_cb).to_proc)
          RXtAddCallback(edit, RXmNhelpCallback, method(:help_cb).to_proc)
          RXtAddCallback(text, RXmNfocusCallback, method(:focus_cb).to_proc)
          RXtAddCallback(text, RXmNlosingFocusCallback, method(:losing_cb).to_proc)
          # If we have a $mouse_enter_text_hook, e.g. in ~/.snd-ruby.rb
          #   $mouse_enter_text_hook.add_hook!("snd_init_hook") do |w| focus_widget(w) end
          RXtAddEventHandler(text, REnterWindowMask, false,
                             lambda do |w, c, i, f|
                               $mouse_enter_text_hook.call(w) unless $mouse_enter_text_hook.empty?
                             end)
          # Use new button colors (see HISTORY.Snd, 12-Sep-2003).
          [RXmDIALOG_OK_BUTTON,
           RXmDIALOG_CANCEL_BUTTON,
           RXmDIALOG_HELP_BUTTON].zip([doit_button_color(),
                                       quit_button_color(),
                                       help_button_color()]) do |button, color|
            RXtVaSetValues(RXmMessageBoxGetChild(edit, button),
                           [RXmNarmColor, pushed_button_color(), RXmNbackground, color])
          end
          # If we have _XEditResCheckMessage compiled in (--with-editres),
          # we can click in edit widget to get Snd's whole widget tree.
          # (editres: Commands-->Get Tree)
          if defined?(R_XEditResCheckMessages())
            RXtAddEventHandler(RXtParent(edit), 0, true, method(:R_XEditResCheckMessages).to_proc)
          end
          @edit_widget = edit
          @text_widget = text
          @file_widget = info
          RXtManageChild(text)
        end
        file = (@current_file or file_name(false))
        set_edit_widget(file, files_popup_info(Current_file_viewer, nil, file))
        RXtManageChild(@edit_widget)
        RXMapRaised(RXtDisplayOfObject(@edit_widget), RXtWindow(@edit_widget))
      end
      
      def help_cb(w, c, i)
        help_dialog(F_INFO,
                    "Edit info DB of sound files (see snd/nb.scm).

Provides pop-up help in the Files viewer.
If you have `dbm', any data associated with
the file in the dbm database will also be posted.
The database name is defined by $nb_database
(#{$nb_database.inspect}).

o Edit info:    opens the edit widget

o Prune DB:     clears non-existent file references
                out of the database

o Clear:        removes info entry from current
                file

o Submit:       submits info from edit widget
                to file info database\n",
                  ["{Libxm}: graphics module",
                   "{Ruby}: extension language",
                   "{Motif}: Motif extensions via Libxm"])
      end
    end
  end
end

=begin
include NB
$HAVE_MOTIF ? make_nb_motif() : make_nb()
=end

# nb.rb ends here
