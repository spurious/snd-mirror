;;; inf-snd.el -- Inferior Snd Process (Ruby/Guile)

;; Copyright (C) 2002 Michael Scholz

;; Author: Michael Scholz <scholz-micha@gmx.de>
;; Created: Wed Nov 27 20:52:54 CET 2002
;; Last: Thu Dec 19 00:31:04 CET 2002
;; Version: $Revision: 1.3.2.4 $
;; Keywords: processes, snd, ruby, guile

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary:

;; This file defines a snd-in-a-buffer package built on top of
;; comint-mode.  It includes inferior mode for Snd-Ruby
;; (inf-snd-ruby-mode) and Snd-Guile (inf-snd-guile-mode), furthermore
;; both a Snd-Ruby mode (snd-ruby-mode) and a Snd-Guile mode
;; (snd-guile-mode) for editing source files.  It is tested with
;; Snd-Ruby and Snd-Guile, version 6.4, and GNU Emacs, version 21.2.

;; Since this mode is built on top of the general command-interpreter-
;; in-a-buffer mode (comint-mode), it shares a common base
;; functionality, and a common set of bindings, with all modes derived
;; from comint mode.  This makes these modes easier to use.  For
;; documentation on the functionality provided by comint-mode, and the
;; hooks available for customizing it, see the file comint.el.

;; A nice feature may be the command inf-snd-help, which shows the
;; description Snd provides for many functions.  With tab-completion
;; in the minibuffer you can scan all functions at a glance, help
;; strings of Guile functions are also available, but not with
;; tab-completion in the minibuffer.  It should be easy to extent this
;; mode with new commands and key bindings; the example below and the
;; code in this file may show the way.

;; There exist four main modes in this file: the two inferior
;; Snd-process-modes (inf-snd-ruby-mode and inf-snd-guile-mode) and
;; the replacements of ruby-mode (snd-ruby-mode) as well as of
;; scheme-mode (snd-guile-mode).

;; Variables of the inferior Snd-process-modes inf-snd-ruby-mode and
;; inf-snd-guile-mode (defaults):
;;
;; inf-snd-ruby-program-name   "snd-ruby"    Snd-Ruby program name
;; inf-snd-guile-program-name  "snd-guile"   Snd-Guile program name
;; inf-snd-working-directory   "~/"    where Ruby or Guile scripts reside
;; inf-snd-ruby-mode-hook      nil     to customize inf-snd-ruby-mode
;; inf-snd-guile-mode-hook     nil     to customize inf-snd-guile-mode

;; Variables of the editing modes snd-ruby-mode and snd-guile-mode
;; (defaults):
;;
;; snd-ruby-mode-hook          nil     to customize snd-ruby-mode
;; snd-guile-mode-hook         nil     to customize snd-guile-mode

;; You can start inf-snd-ruby-mode interactive either with prefix-key
;; (C-u M-x run-snd-ruby)--you will be ask for program name and
;; optional arguments--or direct (M-x run-snd-ruby).  In the latter
;; case, variable inf-snd-ruby-program-name should be set correctly.
;; The same usage goes for inf-snd-guile-mode.

;; Example for your .emacs file:
;;
;; (autoload 'run-snd-ruby     "inf-snd" "Start inferior Snd-Ruby process" t)
;; (autoload 'run-snd-guile    "inf-snd" "Start inferior Snd-Guile process" t)
;; (autoload 'snd-ruby-mode    "inf-snd" "Load snd-ruby-mode." t)
;; (autoload 'snd-guile-mode   "inf-snd" "Load snd-guile-mode." t)
;;
;; ;; These variables should be set to your needs!
;; (setq inf-snd-working-directory "~/Snd/")
;; (setq inf-snd-ruby-program-name "snd-ruby -notebook")
;; (setq inf-snd-guile-program-name "snd-guile -separate")

;; The hook-variables may be used to set new key bindings and menu
;; entries etc. in your .emacs file, e.g.:
;;
;; (defun foo ()
;;   (interactive)
;;   (inf-snd-send-string "(sounds)"))
;;
;; (add-hook 'inf-snd-ruby-mode-hook
;; 	  (lambda ()
;; 	    (define-key (current-local-map) [menu-bar inf-snd-ruby-mode foo]
;; 	      '("Foo" . foo))
;; 	    (define-key (current-local-map) "\C-c\C-t" 'foo)))
;;
;; To edit source code with special key bindings:
;; 
;; (add-hook 'snd-ruby-mode-hook
;; 	  (lambda ()
;;	    (define-key (current-local-map) "\C-co" 'snd-send-buffer)
;; 	    (define-key (current-local-map) "\C-cb" 'snd-send-block)
;; 	    (define-key (current-local-map) "\C-cr" 'snd-send-region)
;; 	    (define-key (current-local-map) "\C-ce" 'snd-send-definition)))
;;
;; (add-hook 'snd-guile-mode-hook
;; 	  (lambda ()
;;	    (define-key (current-local-map) "\C-co" 'snd-send-buffer)
;; 	    (define-key (current-local-map) "\C-cr" 'snd-send-region)
;; 	    (define-key (current-local-map) "\C-ce" 'snd-send-definition)))

;; You may change the mode in a Snd-Ruby or Snd-Guile source file by
;; M-x snd-ruby-mode or M-x snd-guile-mode.  To determine
;; automatically which mode to set, you can decide to use special
;; file-extensions.  I use file-extension `.rbs' for Snd-Ruby source
;; files and `.cms' for Snd-Guile.
;;
;; (set-default 'auto-mode-alist
;; 	     (append '(("\\.rbs$" . snd-ruby-mode)
;;                     ("\\.cms$" . snd-guile-mode))
;; 		     auto-mode-alist))
;;
;; Or you can use the local mode variable in source files, e.g. by
;; `-*- snd-ruby -*-' or `-*- snd-guile -*-' in first line.

;; Key binding of inf-snd-ruby-mode and inf-snd-guile-mode:
;;
;; C-c C-f   inf-snd-file    	   open file-dialog of Snd
;; C-c C-l   inf-snd-load    	   load script in current working directory
;; C-c C-p   inf-snd-play    	   play current sound file
;; C-c C-u   inf-snd-stop    	   stop playing all sound files
;; C-c C-i   inf-snd-help    	   help on Snd-function
;; C-c C-g   inf-snd-break   	   send C-g! to Snd process
;; C-c C-k   inf-snd-kill    	   kill Snd process only
;; C-c C-q   inf-snd-quit    	   quit Snd-session
;; C-h m     describe-mode   	   describe current major mode

;; Key bindings of snd-ruby-mode and snd-guile-mode editing source files:
;;
;; C-c C-s   snd-run-snd           (Snd-Ruby or Snd-Guile)
;; M-C-x     snd-send-definition
;; C-x C-e   snd-send-last-sexp
;; C-c M-e   snd-send-definition
;; C-c C-e   snd-send-definition-and-go
;; C-c M-r   snd-send-region
;; C-c C-r   snd-send-region-and-go
;; C-c M-o   snd-send-buffer
;; C-c C-o   snd-send-buffer-and-go
;; C-c M-b   snd-send-block        (Ruby only)
;; C-c C-b   snd-send-block-and-go (Ruby only)
;; C-c C-z   snd-switch-to-snd
;; C-c C-l   snd-load-file
;;
;; and in addition:
;; 
;; C-c C-f   snd-file    	   open file-dialog of Snd
;; C-c C-p   snd-play    	   play current sound file
;; C-c C-u   snd-stop    	   stop playing all sound files
;; C-c C-i   snd-help    	   help on Snd-function
;; C-c C-g   snd-break   	   send C-g! to Snd process
;; C-c C-k   snd-quit    	   quit Snd process
;; C-h m     describe-mode   	   describe current major mode

;; Ruby Note (see also documentation string of `snd-send-region'):

;; There are some difficulties in `snd-ruby-mode' with
;; `snd-send-region' and related commands.  A ruby-region must not
;; contain a block spanning several lines, that means something like
;; "do ...  end" or "{ ...  }" must appear on a single line.  This is
;; no good solution but one can load the whole file to define
;; functions and than use blocks with "begin ... end" sending it with
;; `snd-send-block' with point somewhere in or before the block
;; statement or one can send a single line enclosed in parenthesis
;; with `snd-send-last-sexp' with point after the last closing paren.
;; Comments in regions sending to inferior Snd buffer produce errors
;; too.
;; 
;; Working examples:
;; 
;; def foo(beg, len)
;;   amp = .5
;;   dur = 1
;;   beg.upto(len) { |i| play_my_inst(i, dur, [:c4, :e4, :g4].rand, amp) }
;; end
;; 
;; with `snd-send-definition' somewhere in or before the definition.
;; The beg.upto-line must appear on a single line.
;; 
;; begin
;;   first_func
;;   second_func
;;   third_func
;; end
;; 
;; with `snd-send-block' somewhere in or before the begin-block,
;; 
;; (i = 10
;;  j = 440
;;  foo(i, j))
;; 
;; with `snd-send-last-sexp' after the last paren.

;;; Code:

;;;; The inf-snd-ruby-mode and inf-snd-guile-mode.

(require 'comint)
(require 'ruby-mode)
(require 'scheme)
(require 'inf-ruby)
(require 'cmuscheme)

(defconst inf-snd-rcsid "$Id: inf-snd.el,v 1.3.2.4 2002/12/18 23:35:03 mike Exp $")

(defconst inf-snd-version
  (progn
   (string-match "[0-9]+[0-9.]+" inf-snd-rcsid)
   (substring inf-snd-rcsid (match-beginning 0) (match-end 0))))

(defvar inf-snd-ruby-buffer "*Snd-Ruby*"
  "Inferior Snd-Ruby process buffer.")

(defvar inf-snd-ruby-buffer-name "Snd-Ruby"
  "Inferior Snd-Ruby process buffer name.")

(defvar inf-snd-ruby-mode-hook nil
  "User hook variable of `inf-snd-ruby-mode'.
Will be called after `comint-mode-hook' and before starting inferior
Snd-Ruby process.")

(defvar inf-snd-ruby-program-name "snd-ruby"
  "*User variable to set Snd-Ruby-program name and optional arguments.")

(defvar inf-snd-guile-buffer "*Snd-Guile*"
  "Inferior Snd-Guile process buffer.")

(defvar inf-snd-guile-buffer-name "Snd-Guile"
  "Inferior Snd-Guile process buffer name.")

(defvar inf-snd-guile-mode-hook nil
  "User hook variable of `inf-snd-guile-mode'.
Will be called after `comint-mode-hook' and before starting inferior
Snd-Guile process.")

(defvar inf-snd-guile-program-name "snd-guile"
  "*User variable to set Snd-Guile-program name and optional args.")

(defvar inf-snd-working-directory "~/"
  "*User variable where Emacs will find the Ruby or Guile scripts.")

(defvar inf-snd-ruby-flag t
  "Non-nil means extension language Ruby, nil Guile.
Needed to determine which extension language to use.  This variable is
buffer-local.")

(defvar inf-snd-keywords nil
  "Snd keywords providing possibly online help.
\\<inf-snd-ruby-mode-map>\\<inf-snd-guile-mode-map>
Will be used by `inf-snd-help' (\\[inf-snd-help]), taken from
snd-6/snd-strings.h.

With the following Ruby script you can update the value of this
variable from the Snd sources.  It puts the strings in a file and you
can merge it in the Emacs source file of this mode (inf-snd.el).

#!/usr/bin/env ruby -w

# replace with your path to snd-6/snd-strings.h
in_file = (ARGV[0] or \"/usr/gnu/src/snd-6-cvs/snd/snd-strings.h\")
out_file = \"help.strings\"

begin
  unless File.file?(in_file)
    puts \"file #{in_file} does not exist!\"
    exit
  end

  out_fd = File.open(out_file, \"w\")
  i = 0

  out_fd << \"(unless inf-snd-keywords
  (setq inf-snd-keywords
        \'(\"
  
  File.foreach(in_file) { |x|
    if x =~ /(\".*\")+/
      unless $1.empty?
	out_fd << \"\n\t \" if ((i % 3) == 0) and (i != 0)
	out_fd << \" \" unless i == 0
	out_fd << \"(#{$1})\"
	i += 1
      end
    end
  }

  out_fd << \")))

\(provide 'inf-snd)

;;; inf-snd.el ends here\n\"
  out_fd.close
  puts \"#{i} items written to #{out_file}\"
end")

(defvar ruby-font-lock-keywords nil)
(defvar ruby-font-lock-syntax-table nil)
(defvar ruby-font-lock-syntactic-keywords nil)
(defvar font-lock-keywords nil)
(defvar font-lock-syntax-table nil)
(defvar font-lock-syntactic-keywords nil)

(defun inf-snd-set-keys (mode name)
  "Set the key bindings and menu entries for MODE.
Menu name is NAME.  You can extend the key bindings and menu entries
here or via hook variables in .emacs file."
  ;; key bindings
  (define-key (current-local-map) "\C-c\C-f" 'inf-snd-file)
  (define-key (current-local-map) "\C-c\C-l" 'inf-snd-load)
  (define-key (current-local-map) "\C-c\C-p" 'inf-snd-play)
  (define-key (current-local-map) "\C-c\C-u" 'inf-snd-stop)
  (define-key (current-local-map) "\C-c\C-i" 'inf-snd-help)
  (define-key (current-local-map) "\C-c\C-g" 'inf-snd-break)
  (define-key (current-local-map) "\C-c\C-k" 'inf-snd-kill)
  (define-key (current-local-map) "\C-c\C-q" 'inf-snd-quit)
  ;; menu entries in reverse order of appearance
  (define-key (current-local-map) [menu-bar mode]
    (cons name (make-sparse-keymap name)))
  (define-key (current-local-map) [menu-bar mode quit]
    '(menu-item "Quit Snd Session" inf-snd-quit))
  (define-key (current-local-map) [menu-bar mode kill]
    '(menu-item "Kill Snd Process only" inf-snd-kill))
  (define-key (current-local-map) [menu-bar mode break]
    '(menu-item "Send C-g to Snd Process" inf-snd-break))
  (define-key (current-local-map) [menu-bar mode sep-quit] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode desc]
    '(menu-item "Describe Mode" describe-mode))
  (define-key (current-local-map) [menu-bar mode help]
    '(menu-item "Describe Snd Function ..." inf-snd-help))
  (define-key (current-local-map) [menu-bar mode sep-desc] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode stop]
    '(menu-item "Stop Playing" inf-snd-stop))
  (define-key (current-local-map) [menu-bar mode play]
    '(menu-item "Start Playing" inf-snd-play))
  (define-key (current-local-map) [menu-bar mode sep-play] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode load-r]
    '(menu-item "Load Ruby Script ..." inf-snd-load
		:visible inf-snd-ruby-flag))
  (define-key (current-local-map) [menu-bar mode load-g]
    '(menu-item "Load Guile Script ..." inf-snd-load
		:visible (not inf-snd-ruby-flag)))
  (define-key (current-local-map) [menu-bar mode file]
    '(menu-item "Open Snd-File Dialog" inf-snd-file)))

(defun inf-snd-send-string (str &optional no-strip-p)
  "Print STR in buffer and send it to the inferior Snd process.
If NO-STRIP-P is nil, the default, all dashes (-) will be translated
to underlines (_), if `inf-snd-ruby-flag' is true.  If NO-STRIP-P is
non-nil, it won't translate.  See `inf-snd-load' for the latter case."
  (interactive)
  (and (not no-strip-p)
       inf-snd-ruby-flag
       (while (string-match "-" str)
	 (setq str (replace-match "_" t nil str))))
  (let ((buf (if inf-snd-ruby-flag inf-snd-ruby-buffer inf-snd-guile-buffer)))
    (with-current-buffer buf
      (insert str)
      (comint-send-input))))

(defun inf-snd-file ()
  "Open Snd's file-dialog widget."
  (interactive)
  (inf-snd-send-string "(file-dialog)"))

(defun inf-snd-load (file)
  "Load the required Ruby or Guile script.
Asks for FILE interactively in minibuffer."
  (interactive "fLoad Snd Script: ")
  (unless (file-directory-p file)
      (inf-snd-send-string
       (format "(load \"%s\")" (car (file-expand-wildcards file t))) t)))

(defun inf-snd-play ()
  "Play sound which is current in Snd."
  (interactive)
  (inf-snd-send-string "(play)"))

(defun inf-snd-stop ()
  "Stop playing of all sound files."
  (interactive)
  (inf-snd-send-string "(stop-playing)"))

(defun inf-snd-help ()
  "Receive a string in minibuffer and show corresponding help.
This is done via Snd's function snd_help(), putting result at the end
of the inferior Snd process buffer.  If point is near a function name
in inferior Snd process buffer, that function will be used as default
value in minibuffer; tab-completion is activated.  The help strings
are defined in `inf-snd-keywords', where you can find a little Ruby
script to update the value of this variable."
  (interactive)
  (let ((prompt "Snd Help: ")
	(default (thing-at-point 'symbol)))
    (if default
	(progn
	  (if (string-match ">" default)
	      (setq default (replace-match "" nil nil default)))
	  (unless (string= default "")
	    (setq prompt (format "Snd Help (default %s): " default)))))
    (let ((str (completing-read prompt inf-snd-keywords nil nil nil nil default)))
      (unless (string= str "")
	(let ((delimiter (if inf-snd-ruby-flag ?: ?'))
	      (buf (inf-snd-proc-buffer)))
	  (with-current-buffer buf
	    (goto-char (point-max))
	    (if (and (eq (preceding-char) ?>)
		     (eobp))
		(inf-snd-send-string (format "(snd-help %c%s)" delimiter str))
	      (beginning-of-line)
	      (kill-region (point) (point-max))
	      (inf-snd-send-string (format "(snd-help %c%s)" delimiter str))
	      (yank))))))))

(defun inf-snd-break ()
  "Send Break-command to inferior Snd process."
  (interactive)
  (inf-snd-send-string "(c-g!)"))

(defun inf-snd-kill ()
  "Kill current inferior Snd process but not its buffer."
  (interactive)
  (delete-process (get-buffer-process (inf-snd-proc-buffer))))
  
(defun inf-snd-quit ()
  "Kill inferior Snd process and Snd-buffer."
  (interactive)
  (inf-snd-kill)
  (kill-buffer (current-buffer))
  (unless (one-window-p)
    (delete-window (get-buffer-window (inf-snd-proc-buffer)))))

(defun inf-snd-proc-buffer ()
  "Return the current process buffer."
  (if inf-snd-ruby-flag
      inf-snd-ruby-buffer
    inf-snd-guile-buffer))
  
(defun inf-snd-comint-snd-send (proc str)
  "Special function for sending to PROC input STR.
Used by function `comint-input-sender'."
  (if inf-snd-ruby-flag
      (comint-send-string proc (format "(%s)\n" str))
    (comint-send-string proc (concat str "\n"))))

(defun inf-snd-args-to-list (string)
  "Return a list containing the program and optional arguments list.
Argument STRING is the Snd command and optional arguments."
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (inf-snd-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (inf-snd-args-to-list (substring string pos
						 (length string)))))))))

(define-derived-mode inf-snd-ruby-mode comint-mode inf-snd-ruby-buffer-name
  "Inferior mode running Snd-Ruby, derived from `comint-mode'.

Snd is a sound editor created by Bill Schottstaedt
\(bil@ccrma.Stanford.EDU).  You can find it on
ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-6.tar.gz.

You can type in Ruby commands in inferior Snd process buffer which
will be sent via `comint-send-string' to the inferior Snd process.
The return value will be shown in the process buffer, other output
goes to the listener of Snd.

You should set variable `inf-snd-ruby-program-name' and
`inf-snd-working-directory' in your .emacs file to set the appropriate
program name and optional arguments and to direct Snd to the Ruby
scripts directory, you have.

The hook variables `comint-mode-hook' and `inf-snd-ruby-mode-hook'
will be called in that special order after calling the inferior Snd
process.  You can use them e.g. to set additional key bindings.

\\<inf-snd-ruby-mode-map> Interactive start is possible either by
\\[universal-argument] \\[run-snd-ruby], you will be ask for the Snd
program name, or by \\[run-snd-ruby].  Emacs shows an additional menu
entry ``Snd-Ruby'' in the menu bar.

The following key bindings are defined:
\\{inf-snd-ruby-mode-map}"
  (ruby-mode-variables)
  (setq comint-input-filter (function ruby-input-filter))
  (setq comint-get-old-input (function ruby-get-old-input))
  (setq comint-prompt-regexp "^>+")
  (setq comint-input-sender (function inf-snd-comint-snd-send))
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-keywords)
  (make-local-variable 'font-lock-syntax-table)
  (make-local-variable 'font-lock-syntactic-keywords)
  (setq font-lock-defaults '((ruby-font-lock-keywords) nil nil))
  (setq font-lock-keywords ruby-font-lock-keywords)
  (setq font-lock-syntax-table ruby-font-lock-syntax-table)
  (setq font-lock-syntactic-keywords ruby-font-lock-syntactic-keywords)
  (make-local-variable 'default-directory)
  (setq default-directory inf-snd-working-directory)
  (make-local-variable 'inf-snd-ruby-flag)
  (setq inf-snd-ruby-flag t)
  (setq mode-line-process '(":%s"))
  (inf-snd-set-keys 'inf-snd-ruby-mode inf-snd-ruby-buffer-name)
  (pop-to-buffer inf-snd-ruby-buffer)
  (goto-char (point-max))
  (run-hooks 'inf-snd-ruby-mode-hook))

(define-derived-mode inf-snd-guile-mode comint-mode inf-snd-guile-buffer-name
  "Inferior mode running Snd-Guile, derived from `comint-mode'.

Snd is a sound editor created by Bill Schottstaedt
\(bil@ccrma.Stanford.EDU).  You can find it on
ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-6.tar.gz.

You can type in Guile commands in inferior Snd process buffer which
will be sent via `comint-send-string' to the inferior Snd process.
The return value will be shown in the process buffer, other output
goes to the listener of Snd.

You sould set variable `inf-snd-guile-program-name' and
`inf-snd-working-directory' in your .emacs file to set the appropriate
program name and optional arguments and to direct Snd to the Guile
scripts directory, you have.

The hook variables `comint-mode-hook' and `inf-snd-guile-mode-hook'
will be called in that special order after calling the inferior Snd
process.  You can use them e.g. to set additional key bindings.

\\<inf-snd-guile-mode-map> Interactive start is possible either by
\\[universal-argument] \\[run-snd-guile], you will be ask for the Snd
program name, or by \\[run-snd-guile].  Emacs shows an additional menu
entry ``Snd-Guile'' in the menu bar.

The following key bindings are defined:
\\{inf-snd-guile-mode-map}"
  (scheme-mode-variables)
  (setq comint-input-filter (function scheme-input-filter))
  (setq comint-get-old-input (function scheme-get-old-input))
  (setq comint-prompt-regexp "^>+")
  (setq comint-input-sender (function inf-snd-comint-snd-send))
  (make-local-variable 'default-directory)
  (setq default-directory inf-snd-working-directory)
  (make-local-variable 'inf-snd-ruby-flag)
  (setq inf-snd-ruby-flag nil)
  (setq mode-line-process '(":%s"))
  (inf-snd-set-keys 'inf-snd-guile-mode inf-snd-guile-buffer-name)
  (pop-to-buffer inf-snd-guile-buffer)
  (goto-char (point-max))
  (run-hooks 'inf-snd-guile-mode-hook))

(defun run-snd-ruby (cmd)
  "Start inferior Snd-Ruby process.
CMD is used for determine which program to run.  If interactively
called, one will be asked for program name to run."
  (interactive (list (if current-prefix-arg
 			 (read-string "Run Snd Ruby: " inf-snd-ruby-program-name)
 		       inf-snd-ruby-program-name)))
  (unless (comint-check-proc inf-snd-ruby-buffer)
    (let ((cmdlist (inf-snd-args-to-list cmd)))
      (setq inf-snd-ruby-program-name cmd)
      (set-buffer (apply 'make-comint inf-snd-ruby-buffer-name (car cmdlist) nil (cdr cmdlist))))
    (inf-snd-ruby-mode)
    (send-invisible "snd_version"))) ; dummy to force Snd showing a prompt

(defun run-snd-guile (cmd)
  "Start inferior Snd-Guile process.
CMD is used for determine which program to run.  If interactively
called, one will be asked for program name to run."
  (interactive (list (if current-prefix-arg
 			 (read-string "Run Snd Guile: " inf-snd-guile-program-name)
 		       inf-snd-guile-program-name)))
  (unless (comint-check-proc inf-snd-guile-buffer)
    (let ((cmdlist (inf-snd-args-to-list cmd)))
      (setq inf-snd-guile-program-name cmd)
      (set-buffer (apply 'make-comint inf-snd-guile-buffer-name (car cmdlist) nil (cdr cmdlist))))
    (inf-snd-guile-mode)
    (send-invisible "(snd-version)")))

;;;; The snd-ruby-mode and snd-guile-mode

;;; Commentary

;; These two modes are derived from ruby-mode and scheme-mode.  The
;; main changes are the key bindings, which now refer to special
;; Snd-process-buffer-related ones.  I used commands from inf-ruby.el
;; as well as from cmuscheme.el and changed them appropriately.

(defvar snd-ruby-buffer-name "Snd/Ruby"
  "Buffer name of `snd-ruby-mode'.")

(defvar snd-guile-buffer-name "Snd/Guile"
  "Buffer name of `snd-guile-mode'.")

(defvar snd-ruby-mode-hook nil
  "User hook variable.
Called after `ruby-mode-hook' and before starting inferior Snd
process.")

(defvar snd-guile-mode-hook nil
  "User hook variable.
Called after `scheme-mode-hook' and before starting inferior Snd
process.")

(defvar snd-source-modes '(snd-ruby-mode)
  "Used to determine if a buffer contains Snd source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Snd source file by `snd-load-file'.  Used by this command
to determine defaults.  This variable is buffer-local in
`snd-ruby-mode' respective `snd-guile-mode'.")

(defvar snd-inf-ruby-flag t
  "Non-nil means extension language Ruby, nil Guile.
Needed to determine which extension language should be used.  This
variable is buffer-local in `snd-ruby-mode' respective
`snd-guile-mode'.")

(defvar snd-prev-l/c-dir/file nil
  "Cache the (directory . file) pair used in the last `snd-load-file'.
Used for determining the default in the next one.")

(define-derived-mode snd-ruby-mode ruby-mode snd-ruby-buffer-name
  "Major mode for editing Snd-Ruby code.

Editing commands are similar to those of `ruby-mode'.

In addition, you can start an inferior Snd process and some additional
commands will be defined for evaluating expressions.  A menu
``Snd/Ruby'' appears in the menu bar.  Entries in this menu are
disabled if no inferior Snd process exist.

You can use the hook variables `ruby-mode-hook' and
`snd-ruby-mode-hook', which will be called in that order.

The current key bindings are:
\\{snd-ruby-mode-map}"
  (make-local-variable 'snd-inf-ruby-flag)
  (setq snd-inf-ruby-flag t)
  (make-local-variable 'snd-source-modes)
  (setq snd-source-modes '(snd-ruby-mode))
  (snd-set-keys 'snd-ruby-mode snd-ruby-buffer-name)
  (run-hooks 'snd-ruby-mode-hook))

(define-derived-mode snd-guile-mode scheme-mode snd-guile-buffer-name
  "Major mode for editing Snd-Guile code.

Editing commands are similar to those of `scheme-mode'.

In addition, you can start an inferior Snd process and some additional
commands will be defined for evaluating expressions.  A menu
``Snd/Guile'' appears in the menu bar.  Entries in this menu are
disabled if no inferior Snd process exist.

You can use variables `scheme-mode-hook' and `snd-guile-mode-hook',
which will be called in that order.

The current key bindings are:
\\{snd-guile-mode-map}"
  (make-local-variable 'snd-inf-ruby-flag)
  (setq snd-inf-ruby-flag nil)
  (make-local-variable 'snd-source-modes)
  (setq snd-source-modes '(snd-guile-mode))
  (snd-set-keys 'snd-guile-mode snd-guile-buffer-name)
  (run-hooks 'snd-guile-mode-hook))

(defun snd-send-region (start end)
  "Send the current region to the inferior Snd process.
START and END define the region.\\<snd-ruby-mode-map>

There are some difficulties in `snd-ruby-mode'.  A ruby-region must
not contain a block spanning several lines, that means something like
\"do ... end\" or \"{ ...  }\" must appear on a single line.  This is
no good solution but one can load the whole file to define functions
and than use blocks with \"begin ... end\" sending it with
`snd-send-block' with point somewhere in or before the block statement
or one can send a single line enclosed in parenthesis with
`snd-send-last-sexp' with point after the last closing paren.
Comments in regions sending to inferior Snd buffer produce errors too.

Working examples:

def foo(beg, len)
  amp = .5
  dur = 1
  beg.upto(len) { |i| play_my_inst(i, dur, [:c4, :e4, :g4].rand, amp) }
end

with \\[snd-send-definition] somewhere in or before the definition.
The beg.upto-line must appear on a single line.

begin
  first_func
  second_func
  third_func
end

with \\[snd-send-block] somewhere in or before the begin-block,

\(i = 10
  j = 440
  foo(i, j))

with \\[snd-send-last-sexp] after the last paren."
  (interactive "r")
  (let ((str  (concat (buffer-substring-no-properties start end))))
    (if snd-inf-ruby-flag
	(progn
	  (comint-send-string (snd-proc) "(")
	  (while (string-match "\n" str)
	    (setq str (replace-match ";" t nil str)))))
    (comint-send-string (snd-proc) str)
    (if snd-inf-ruby-flag
	(comint-send-string (snd-proc) ")"))
    (comint-send-string (snd-proc) "\n")))

(defun snd-send-region-and-go (start end)
  "Send the current region to the inferior Snd process.
Switch to the process buffer.  START and END define the region."
  (interactive "r")
  (snd-send-region start end)
  (snd-switch-to-snd t))

(defun snd-send-definition ()
  "Send the current definition to the inferior Snd process."
  (interactive)
  (save-excursion
    (if snd-inf-ruby-flag
	(ruby-end-of-defun)
      (end-of-defun))
   (let ((end (point)))
     (if snd-inf-ruby-flag
	 (ruby-beginning-of-defun)
       (beginning-of-defun))
     (snd-send-region (point) end))))

(defun snd-send-definition-and-go ()
  "Send the current definition to the inferior Snd process.
Switch to the process buffer."
  (interactive)
  (snd-send-definition)
  (snd-switch-to-snd t))

(defun snd-send-last-sexp ()
  "Send the previous sexp to the inferior Snd process.
\\<snd-ruby-mode-map>\\<snd-guile-mode-map>
In Ruby one can give the expression in between parentheses: \(puts
\"result is #{foo * 17}\")\\[snd-send-last-sexp]"
  (interactive)
  (snd-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun snd-send-block ()
  "Send the current block to the inferior Ruby-Snd process.
Works only in `snd-ruby-mode'."
  (interactive)
  (save-excursion
    (ruby-end-of-block)
    (end-of-line)
    (let ((end (point)))
      (ruby-beginning-of-block)
      (snd-send-region (point) end))))

(defun snd-send-block-and-go ()
  "Send the current block to the inferior Ruby-Snd process.
Switch to the process buffer.  Works only in `snd-ruby-mode'."
  (interactive)
  (snd-send-block)
  (snd-switch-to-snd t))

(defun snd-send-buffer ()
  "Send the current buffer to the inferior Snd process."
  (interactive)
  (snd-send-region (point-min) (point-max)))

(defun snd-send-buffer-and-go ()
  "Send the current buffer to the inferior Snd process.
Switch to the process buffer."
  (interactive)
  (snd-send-buffer)
  (snd-switch-to-snd t))

(defun snd-switch-to-snd (eob-p)
  "Switch to the Snd process buffer.
Non-nil EOB-P means to position cursor at end of buffer."
  (interactive "P")
  (let ((buf (if snd-inf-ruby-flag inf-snd-ruby-buffer inf-snd-guile-buffer)))
    (if (get-buffer buf)
	(pop-to-buffer buf)
      (error "No current Snd process buffer.  See variable inf-snd-ruby|guile-buffer"))
    (cond (eob-p
	   (push-mark)
	   (goto-char (point-max))))))
  
(defun snd-load-file (filename)
  "Load a Snd script FILENAME into the inferior Snd process."
  (interactive (comint-get-source "Load Snd script file: "
				  snd-prev-l/c-dir/file snd-source-modes t))
  (comint-check-source filename)
  (setq snd-prev-l/c-dir/file (cons (file-name-directory filename)
				     (file-name-nondirectory filename)))
  (comint-send-string (snd-proc) (concat "(load \"" filename "\"\)\n")))

(defun snd-run-snd ()
  "Start inferior Snd-Ruby or Snd-Guile process.
Started from `snd-ruby-mode' or `snd-guile-mode'."
  (interactive)
  (if snd-inf-ruby-flag
      (progn
	(run-snd-ruby inf-snd-ruby-program-name))
    (run-snd-guile inf-snd-guile-program-name)))

(defun snd-save-state ()
  "Synchronizes the inferior Snd process with the edit buffer."
  (setq inf-snd-ruby-flag snd-inf-ruby-flag))
  
(defun snd-file ()
  "Open Snd's file-dialog widget."
  (interactive)
  (snd-save-state)
  (inf-snd-file))

(defun snd-play ()
  "Play sound which is current in Snd."
  (interactive)
  (snd-save-state)
  (inf-snd-play))

(defun snd-stop ()
  "Stop playing of all sounds."
  (interactive)
  (snd-save-state)
  (inf-snd-stop))

(defun snd-help ()
  "Receive a string in minibuffer and show corresponding help.
This is done via Snd's function snd_help(), putting result at the end
of the inferior Snd process buffer.  If point is near a function name
in inferior Snd process buffer, that function will be used as default
value in minibuffer; tab-completion is activated.  The help strings
are defined in `inf-snd-keywords', where you can find a little Ruby
script to update the value of this variable."
  (interactive)
  (snd-save-state)
  (inf-snd-help))

(defun snd-break ()
  "Send Break-command to inferior Snd process."
  (interactive)
  (snd-save-state)
  (inf-snd-break))

(defun snd-quit ()
  "Kill current inferior Snd process."
  (interactive)
  (snd-save-state)
  (save-excursion
    (snd-switch-to-snd t)
    (inf-snd-quit)))

(defun snd-proc ()
  "Return the process buffer."
  (let* ((buf (if snd-inf-ruby-flag inf-snd-ruby-buffer inf-snd-guile-buffer))
	 (proc (get-buffer-process (if (eq major-mode
					   (if snd-inf-ruby-flag
					       'inf-snd-ruby-mode
					     'inf-snd-guile-mode))
				       (current-buffer)
				     buf))))
    (or proc
	(error "No current process.  See variable inf-snd-ruby|guile-buffer"))))

(defun snd-proc-p ()
  "Return non-nil if no process buffer available."
  (save-current-buffer
    (comint-check-proc (if snd-inf-ruby-flag inf-snd-ruby-buffer inf-snd-guile-buffer))))

(defun snd-set-keys (mode name)
  "Set the key bindings and menu entries for MODE.
Menu name is NAME.  You can extend the key bindings and menu entries
here or via hook variables in .emacs file."
  (define-key (current-local-map) "\M-\C-x"  'snd-send-definition)
  (define-key (current-local-map) "\C-x\C-e" 'snd-send-last-sexp)
  (define-key (current-local-map) "\C-c\M-e" 'snd-send-definition)
  (define-key (current-local-map) "\C-c\C-e" 'snd-send-definition-and-go)
  (define-key (current-local-map) "\C-c\M-r" 'snd-send-region)
  (define-key (current-local-map) "\C-c\C-r" 'snd-send-region-and-go)
  (define-key (current-local-map) "\C-c\M-o" 'snd-send-buffer)
  (define-key (current-local-map) "\C-c\C-o" 'snd-send-buffer-and-go)
  (define-key (current-local-map) "\C-c\C-z" 'snd-switch-to-snd)
  (define-key (current-local-map) "\C-c\C-s" 'snd-run-snd)
  (define-key (current-local-map) "\C-c\C-l" 'snd-load-file)
  (define-key (current-local-map) "\C-c\C-f" 'snd-file)
  (define-key (current-local-map) "\C-c\C-p" 'snd-play)
  (define-key (current-local-map) "\C-c\C-u" 'snd-stop)
  (define-key (current-local-map) "\C-c\C-i" 'snd-help)
  (define-key (current-local-map) "\C-c\C-g" 'snd-break)
  (define-key (current-local-map) "\C-c\C-q" 'snd-quit)
  (if snd-inf-ruby-flag
      (progn
	(define-key (current-local-map) "\C-c\M-b" 'snd-send-block)
	(define-key (current-local-map) "\C-c\C-b" 'snd-send-block-and-go)
	(define-key (current-local-map) "\C-cb"    'undefined) ;overwrite inf-ruby-commands
	(define-key (current-local-map) "\C-cr"    'undefined) ;C-c + single letter key
	(define-key (current-local-map) "\C-ce"    'undefined) ;is reserved for user
	(define-key (current-local-map) "\C-c\C-x" 'undefined) ;key bindings
	(define-key (current-local-map) "\C-c\M-x" 'undefined))
    (define-key (current-local-map) "\C-c\C-c" 'undefined) ;no compile
    (define-key (current-local-map) "\C-c\M-c" 'undefined))
  (define-key (current-local-map) [menu-bar mode]
    (cons name (make-sparse-keymap name)))
  (define-key (current-local-map) [menu-bar mode quit]
    '(menu-item "Kill Snd Process" snd-quit
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode break]
    '(menu-item "Send C-g to Snd Process" snd-break
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-quit] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode desc]
    '(menu-item "Describe Mode" describe-mode))
  (define-key (current-local-map) [menu-bar mode help]
    '(menu-item "Describe Snd Function ..." snd-help
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-desc] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode stop]
    '(menu-item "Stop Playing" snd-stop
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode play]
    '(menu-item "Start Playing" snd-play
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-play] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode file]
    '(menu-item "Open Snd-File Dialog" snd-file
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-load] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode start-r]
    '(menu-item "Start Snd-Ruby Process" snd-run-snd
		:enable (not (snd-proc-p))
		:visible snd-inf-ruby-flag))
  (define-key (current-local-map) [menu-bar mode start-g]
    '(menu-item "Start Snd-Guile Process" snd-run-snd
		:enable (not (snd-proc-p))
		:visible (not snd-inf-ruby-flag)))
  (define-key (current-local-map) [menu-bar mode switch]
    '(menu-item "Switch to Snd Process" snd-switch-to-snd
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-proc] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode block-go]
    '(menu-item "Send Block and Go" snd-send-block-and-go
		:enable (snd-proc-p)
		:visible snd-inf-ruby-flag))
  (define-key (current-local-map) [menu-bar mode block]
    '(menu-item "Send Block" snd-send-block
		:enable (snd-proc-p)
		:visible snd-inf-ruby-flag))
  (define-key (current-local-map) [menu-bar mode buffer-go]
    '(menu-item "Send Buffer and Go" snd-send-buffer-and-go
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode buffer]
    '(menu-item "Send Buffer" snd-send-buffer
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode region-go]
    '(menu-item "Send Region and Go" snd-send-region-and-go
		:enable (and (snd-proc-p) mark-active)))
  (define-key (current-local-map) [menu-bar mode region]
    '(menu-item "Send Region" snd-send-region
		:enable (and (snd-proc-p) mark-active)))
  (define-key (current-local-map) [menu-bar mode def-go]
    '(menu-item "Send Definition and Go" snd-send-definition-and-go
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode def]
    '(menu-item "Send Definition" snd-send-definition
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode last-sexp]
    '(menu-item "Send last Sexp" snd-send-last-sexp
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-load] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode load]
    '(menu-item "Load Snd Script ..." snd-load-file
		:enable (snd-proc-p))))

(unless inf-snd-keywords
  (setq inf-snd-keywords
        '(("abort") ("add-mark") ("add-player")
	  ("add-sound-file-extension") ("add-to-main-menu") ("add-to-menu")
	  ("add-transform") ("after-apply-hook") ("after-edit-hook")
	  ("after-graph-hook") ("after-open-hook") ("amp-control")
	  ("append-to-minibuffer") ("apply-controls") ("as-one-edit")
	  ("ask-before-overwrite") ("audio-input-device") ("audio-output-device")
	  ("audio-state-file") ("auto-resize") ("auto-update")
	  ("auto-update-interval") ("autocorrelate") ("autocorrelation")
	  ("axis-info") ("axis-label-font") ("axis-numbers-font")
	  ("backward-graph") ("backward-mark") ("backward-mix")
	  ("bad-header-hook") ("basic-color") ("beats-per-minute")
	  ("before-apply-hook") ("before-transform-hook") ("bind-key")
	  ("bold-button-font") ("bold-peaks-font") ("bomb")
	  ("button-font") ("c-g?") ("c-g!")
	  ("cepstrum") ("change-menu-label") ("change-property")
	  ("change-samples-with-origin") ("channel->vct") ("channel-amp-envs")
	  ("channel-properties") ("channel-style") ("channel-widgets")
	  ("channels") ("channels-combined") ("channels-separate")
	  ("channels-superimposed") ("chans") ("clear-audio-inputs")
	  ("clear-listener") ("clm-channel") ("close-hook")
	  ("close-sound") ("close-sound-file") ("color->list")
	  ("color-cutoff") ("color-dialog") ("color-inverted")
	  ("color?") ("color-scale") ("colormap")
	  ("comment") ("contrast-control") ("contrast-control-amp")
	  ("contrast-control?") ("convolve-arrays") ("convolve-selection-with")
	  ("convolve-with") ("copy-context") ("count-matches")
	  ("current-edit-position") ("current-font") ("cursor")
	  ("cursor-color") ("cursor-context") ("cursor-cross")
	  ("cursor-follows-play") ("cursor-in-middle") ("cursor-in-view")
	  ("cursor-line") ("cursor-on-left") ("cursor-on-right")
	  ("cursor-position") ("cursor-size") ("cursor-style")
	  ("dac-combines-channels") ("dac-hook") ("dac-size")
	  ("data-clipped") ("data-color") ("data-format")
	  ("data-location") ("default-output-chans") ("default-output-format")
	  ("default-output-srate") ("default-output-type") ("define-envelope")
	  ("delete-mark") ("delete-marks") ("delete-sample")
	  ("delete-samples") ("delete-samples-with-origin") ("delete-selection")
	  ("dialog-widgets") ("dismiss-all-dialogs") ("display-edits")
	  ("dont-normalize") ("dot-size") ("draw-axes")
	  ("draw-dot") ("draw-dots") ("draw-line")
	  ("draw-lines") ("draw-mark-hook") ("draw-string")
	  ("drop-hook") ("during-open-hook") ("edit-fragment")
	  ("edit-header-dialog") ("edit-hook") ("edit-position")
	  ("edit-save-as-dialog") ("edit-tree") ("edits")
	  ("emacs-style-save-as") ("env-channel") ("env-selection")
	  ("env-sound") ("enved-active-env") ("enved-add-point")
	  ("enved-amplitude") ("enved-base") ("enved-clip?")
	  ("enved-delete-point") ("enved-dialog") ("enved-exp?")
	  ("enved-filter-order") ("enved-filter") ("enved-hook")
	  ("enved-in-dB") ("enved-move-point") ("enved-power")
	  ("enved-selected-env") ("enved-spectrum") ("enved-srate")
	  ("enved-target") ("enved-wave?") ("enved-waveform-color")
	  ("eps-bottom-margin") ("eps-file") ("eps-left-margin")
	  ("eps-size") ("equalize-panes") ("exit")
	  ("exit-hook") ("expand-control") ("expand-control-hop")
	  ("expand-control-length") ("expand-control?") ("expand-control-ramp")
	  ("fft") ("fft-log-frequency") ("fft-log-magnitude")
	  ("fft-window") ("fft-window-beta") ("file-dialog")
	  ("file-name") ("file-save-as-dialog") ("fill-polygon")
	  ("fill-rectangle") ("filter-control-coeffs") ("filter-control-env")
	  ("filter-control-in-dB") ("filter-control-order") ("filter-control?")
	  ("filter-env-in-hz") ("filter-selection") ("filter-sound")
	  ("filter-waveform-color") ("find") ("find-mark")
	  ("find-mix") ("find-sound") ("finish-progress-report")
	  ("focus-widget") ("foreground-color") ("forget-region")
	  ("forward-graph") ("forward-mark") ("forward-mix")
	  ("fourier-transform") ("frames") ("free-mix-sample-reader")
	  ("free-sample-reader") ("free-track-sample-reader") ("graph")
	  ("graph->ps") ("graph-color") ("graph-cursor")
	  ("graph-data") ("graph-dots") ("graph-dots-and-lines")
	  ("graph-filled") ("graph-hook") ("graph-lines")
	  ("graph-lollipops") ("graph-style") ("graph-once")
	  ("graph-as-wavogram") ("graph-as-sonogram") ("graph-as-spectrogram")
	  ("graphs-horizontal") ("haar-transform") ("hadamard-transform")
	  ("header-type") ("help-dialog") ("help-hook")
	  ("help-text-font") ("hide-widget") ("highlight-color")
	  ("html-dir") ("in") ("initial-graph-hook")
	  ("insert-region") ("insert-sample") ("insert-samples")
	  ("insert-samples-with-origin") ("insert-selection") ("insert-silence")
	  ("insert-sound") ("just-sounds") ("just-sounds-hook")
	  ("key") ("key-binding") ("key-press-hook")
	  ("keyboard-no-action") ("ladspa-dir") ("left-sample")
	  ("lisp-graph?") ("lisp-graph") ("lisp-graph-hook")
	  ("lisp-graph-style") ("listener-color") ("listener-font")
	  ("listener-prompt") ("listener-selection") ("listener-text-color")
	  ("load-font") ("loop-samples") ("main-menu")
	  ("main-widgets") ("make-color") ("make-graph-data")
	  ("make-mix-sample-reader") ("make-player") ("make-region")
	  ("make-region-sample-reader") ("make-sample-reader") ("make-track-sample-reader")
	  ("map-chan") ("map-channel") ("mark-click-hook")
	  ("mark-color") ("mark-context") ("mark-drag-hook")
	  ("mark-home") ("mark-hook") ("mark-name")
	  ("mark?") ("mark-sample") ("mark-sync")
	  ("mark-sync-max") ("marks") ("max-regions")
	  ("max-transform-peaks") ("maxamp") ("memo-sound")
	  ("menu-hook") ("menu-sensitive") ("menu-widgets")
	  ("min-dB") ("minibuffer-history-length") ("mix")
	  ("mix-amp") ("mix-amp-changed-hook") ("mix-amp-env")
	  ("mix-anchor") ("mix-chans") ("mix-color")
	  ("mix-file-dialog") ("mix-frames") ("mix-home")
	  ("mix-locked") ("mix-name") ("mix?")
	  ("mix-panel") ("mix-position") ("mix-position-changed-hook")
	  ("mix-region") ("mix-sample-reader?") ("mix-selection")
	  ("mix-sound") ("mix-speed") ("mix-speed-changed-hook")
	  ("mix-tag-height") ("mix-tag-width") ("mix-tag-y")
	  ("mix-track") ("mix-vct") ("mix-waveform-height")
	  ("mixes") ("mouse-click-hook") ("mouse-drag-hook")
	  ("mouse-enter-graph-hook") ("mouse-enter-label-hook") ("mouse-enter-listener-hook")
	  ("mouse-enter-text-hook") ("mouse-leave-graph-hook") ("mouse-leave-label-hook")
	  ("mouse-leave-listener-hook") ("mouse-leave-text-hook") ("mouse-press-hook")
	  ("mouse-release-hook") ("multichannel-mix-hook") ("mus-error-hook")
	  ("name-click-hook") ("new-sound") ("new-widget-hook")
	  ("next-mix-sample") ("next-sample") ("next-track-sample")
	  ("normalize-by-channel") ("normalize-by-sound") ("normalize-globally")
	  ("open-file-dialog") ("open-hook") ("open-raw-sound")
	  ("open-raw-sound-hook") ("open-sound") ("open-sound-file")
	  ("optimization") ("optimization-hook") ("orientation-dialog")
	  ("output-comment-hook") ("output-name-hook") ("pad-channel")
	  ("peak-env-info") ("peaks") ("peaks-font")
	  ("play") ("play-and-wait") ("play-channel")
	  ("play-hook") ("play-mix") ("play-region")
	  ("play-selection") ("play-track") ("player-home")
	  ("player?") ("position->x") ("position->y")
	  ("position-color") ("preload-directory") ("preload-file")
	  ("previous-files-select-hook") ("previous-files-sort") ("previous-files-sort-procedure")
	  ("previous-sample") ("print-hook") ("print-length")
	  ("progress-report") ("prompt-in-minibuffer") ("property-changed-hook")
	  ("protect-region") ("ptree-channel") ("pushed-button-color")
	  ("ramp-channel") ("read-hook") ("read-only")
	  ("read-peak-env-info-file") ("read-sample") ("read-mix-sample")
	  ("read-track-sample") ("recolor-widget") ("recorder-autoload")
	  ("recorder-buffer-size") ("recorder-dialog") ("recorder-file")
	  ("recorder-gain") ("recorder-in-amp") ("recorder-in-format")
	  ("recorder-in-device") ("recorder-max-duration") ("recorder-out-amp")
	  ("recorder-out-chans") ("recorder-out-format") ("recorder-srate")
	  ("recorder-trigger") ("redo") ("region-chans")
	  ("region-dialog") ("region-frames") ("region-graph-style")
	  ("region-maxamp") ("region?") ("region-sample")
	  ("region-samples") ("region-samples->vct") ("region-srate")
	  ("regions") ("remove-from-menu") ("report-in-minibuffer")
	  ("reset-controls") ("reset-listener-cursor") ("restore-controls")
	  ("restore-marks") ("restore-region") ("reverb-control-decay")
	  ("reverb-control-feedback") ("reverb-control-length") ("reverb-control-lowpass")
	  ("reverb-control?") ("reverb-control-scale") ("reverse-channel")
	  ("reverse-selection") ("reverse-sound") ("revert-sound")
	  ("right-sample") ("run") ("sample")
	  ("sample-reader-at-end?") ("sample-reader-home") ("sample-reader-position")
	  ("sample-reader?") ("samples") ("samples->sound-data")
	  ("samples->vct") ("sash-color") ("save-controls")
	  ("save-dir") ("save-edit-history") ("save-envelopes")
	  ("save-hook") ("save-listener") ("save-macros")
	  ("save-marks") ("save-options") ("save-region")
	  ("save-selection") ("save-sound") ("save-sound-as")
	  ("save-state") ("save-state-file") ("save-state-hook")
	  ("scale-by") ("scale-channel") ("scale-selection-by")
	  ("scale-selection-to") ("scale-sound-by") ("scale-sound-to")
	  ("scale-to") ("scan-chan") ("scan-channel")
	  ("script-arg") ("script-args") ("search-procedure")
	  ("select-all") ("select-channel") ("select-channel-hook")
	  ("select-mix") ("select-mix-hook") ("select-sound")
	  ("select-sound-hook") ("selected-channel") ("selected-data-color")
	  ("selected-graph-color") ("selected-mix") ("selected-mix-color")
	  ("selected-sound") ("selection-chans") ("selection-color")
	  ("selection-context") ("selection-creates-region") ("selection-frames")
	  ("selection-maxamp") ("selection-member?") ("selection?")
	  ("selection-position") ("selection-srate") ("short-file-name")
	  ("show-all-axes") ("show-axes") ("show-backtrace")
	  ("show-controls") ("show-indices") ("show-listener")
	  ("show-marks") ("show-mix-waveforms") ("show-no-axes")
	  ("show-selection-transform") ("show-transform-peaks") ("show-widget")
	  ("show-x-axis") ("show-y-zero") ("sinc-width")
	  ("smooth-channel") ("smooth-selection") ("smooth-sound")
	  ("snd-apropos") ("snd-error") ("snd-error-hook")
	  ("snd-gcs") ("snd-help") ("snd-print")
	  ("snd-spectrum") ("snd-tempnam") ("snd-version")
	  ("snd-warning") ("snd-warning-hook") ("sound-files-in-directory")
	  ("sound-loop-info") ("sound?") ("sound-properties")
	  ("sound-widgets") ("soundfont-info") ("sounds")
	  ("spectro-cutoff") ("spectro-hop") ("spectro-start")
	  ("spectro-x-angle") ("spectro-x-scale") ("spectro-y-angle")
	  ("spectro-y-scale") ("spectro-z-angle") ("spectro-z-scale")
	  ("speed-control") ("speed-control-as-float") ("speed-control-as-ratio")
	  ("speed-control-as-semitone") ("speed-control-style") ("speed-control-tones")
	  ("squelch-update") ("srate") ("src-channel")
	  ("src-selection") ("src-sound") ("start-hook")
	  ("start-playing") ("start-playing-hook") ("start-progress-report")
	  ("stop-dac-hook") ("stop-player") ("stop-playing")
	  ("stop-playing-channel-hook") ("stop-playing-hook") ("stop-playing-region-hook")
	  ("stop-playing-selection-hook") ("swap-channels") ("sync")
	  ("syncd-marks") ("temp-dir") ("text-focus-color")
	  ("time-graph") ("time-graph?") ("time-graph-style")
	  ("time-graph-type") ("tiny-font") ("track-sample-reader?")
	  ("transform-dialog") ("transform-graph") ("transform-graph?")
	  ("transform-graph-style") ("transform-graph-type") ("transform-hook")
	  ("transform-normalization") ("transform-sample") ("transform-samples")
	  ("transform-samples-size") ("transform-samples->vct") ("transform-size")
	  ("transform-type") ("trap-segfault") ("unbind-key")
	  ("undo") ("undo-hook") ("update-hook")
	  ("update-lisp-graph") ("update-sound") ("update-time-graph")
	  ("update-transform-graph") ("use-sinc-interp") ("vct-map")
	  ("vct->channel") ("vct->samples") ("vct->sound-file")
	  ("verbose-cursor") ("view-sound") ("vu-font")
	  ("vu-font-size") ("vu-size") ("walsh-transform")
	  ("wavelet-transform") ("wavelet-type") ("wavo-hop")
	  ("wavo-trace") ("widget-position") ("widget-size")
	  ("widget-text") ("window-height") ("window-width")
	  ("window-x") ("window-y") ("with-background-processes")
	  ("with-gl") ("with-mix-tags") ("with-relative-panes")
	  ("write-peak-env-info-file") ("x->position") ("x-axis-as-percentage")
	  ("x-axis-in-beats") ("x-axis-in-samples") ("x-axis-in-seconds")
	  ("x-axis-style") ("x-bounds") ("x-position-slider")
	  ("xramp-channel") ("x-zoom-slider") ("y->position")
	  ("y-bounds") ("y-position-slider") ("y-zoom-slider")
	  ("yes-or-no?") ("zero-pad") ("zoom-color")
	  ("zoom-focus-active") ("zoom-focus-left") ("zoom-focus-middle")
	  ("zoom-focus-right") ("zoom-focus-style"))))

(provide 'inf-snd)

;;; inf-snd.el ends here
