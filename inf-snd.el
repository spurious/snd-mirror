;;; inf-snd.el -- Inferior Snd Process (Ruby/Guile)

;; Copyright (C) 2002--2004 Michael Scholz

;; Author: Michael Scholz <scholz-micha@gmx.de>
;; Created: Wed Nov 27 20:52:54 CET 2002
;; Last: Sat Oct 30 10:34:27 CEST 2004
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
;; Snd-Ruby and Snd-Guile 7.1 and GNU Emacs 21.2/21.3.50.

;; Since this mode is built on top of the general command-interpreter-
;; in-a-buffer mode (comint-mode), it shares a common base
;; functionality, and a common set of bindings, with all modes derived
;; from comint mode.  This makes these modes easier to use.  For
;; documentation on the functionality provided by comint-mode, and the
;; hooks available for customizing it, see the file comint.el.

;; A nice feature may be the commands `inf-snd-help' and `snd-help',
;; which shows the description which Snd provides for many functions.
;; Using the prefix key `C-u' you get the HTML version of Snd's help.
;; With tab-completion in the minibuffer you can scan all functions at
;; a glance.  It should be easy to extent this mode with new commands
;; and key bindings; the example below and the code in this file may
;; show the way.

;; There exist four main modes in this file: the two inferior
;; Snd-process-modes (inf-snd-ruby-mode and inf-snd-guile-mode) and
;; the replacements of ruby-mode (snd-ruby-mode) as well as of
;; scheme-mode (snd-guile-mode).

;; Variables of the inferior Snd-process-modes inf-snd-ruby-mode and
;; inf-snd-guile-mode (defaults):
;;
;; inf-snd-ruby-program-name   "snd-ruby"    Snd-Ruby program name
;; inf-snd-guile-program-name  "snd-guile"   Snd-Guile program name
;; inf-snd-working-directory   "~/"          where Ruby or Guile scripts reside
;; inf-snd-ruby-mode-hook      nil           to customize inf-snd-ruby-mode
;; inf-snd-guile-mode-hook     nil           to customize inf-snd-guile-mode
;; inf-snd-index-path          "~/"          path to snd-xref.c
;; inf-snd-prompt-char         ">"           listener prompt

;; Variables of the editing modes snd-ruby-mode and snd-guile-mode
;; (defaults):
;;
;; snd-ruby-mode-hook          nil     	     to customize snd-ruby-mode
;; snd-guile-mode-hook         nil     	     to customize snd-guile-mode

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
;; (setq inf-snd-ruby-program-name "snd-ruby -notebook")
;; (setq inf-snd-guile-program-name "snd-guile -separate")
;; (setq inf-snd-working-directory "~/Snd/")
;; (setq inf-snd-index-path "~/Snd/snd-6/")

;; The hook-variables may be used to set new key bindings and menu
;; entries etc in your .emacs file, e.g.:
;;
;; (defun foo ()
;;   (interactive)
;;   (inf-snd-send-string "(sounds)"))
;;
;; (add-hook 'inf-snd-ruby-mode-hook
;; 	  '(lambda ()
;; 	    (define-key (current-local-map) [menu-bar inf-snd-ruby-mode foo]
;; 	      '("Foo" . foo))
;; 	    (define-key (current-local-map) "\C-c\C-t" 'foo)))
;;
;; To edit source code with special key bindings:
;;
;; (add-hook 'snd-ruby-mode-hook
;; 	  '(lambda ()
;;	    (define-key (current-local-map) "\C-co" 'snd-send-buffer)
;; 	    (define-key (current-local-map) "\C-cb" 'snd-send-block)
;; 	    (define-key (current-local-map) "\C-cr" 'snd-send-region)
;; 	    (define-key (current-local-map) "\C-ce" 'snd-send-definition)))
;;
;; (add-hook 'snd-guile-mode-hook
;; 	  '(lambda ()
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
;; C-c C-s   	 inf-snd-run-snd   (Snd-Ruby or Snd-Guile from a dead Snd process buffer)
;; M-C-l 	 inf-snd-load      load script in current working directory
;; C-c C-f   	 inf-snd-file      open view-files-dialog of Snd
;; M-C-p 	 inf-snd-play      play current sound file
;; C-c C-t 	 inf-snd-stop      stop playing all sound files
;; C-c C-i   	 inf-snd-help      help on Snd-function (snd-help)
;; C-u C-c C-i   inf-snd-help-html help on Snd-function (html)
;; C-c C-g   	 inf-snd-break     send C-g! to Snd process
;; C-c C-q   	 inf-snd-quit      send exit to Snd process
;; C-c C-k   	 inf-snd-kill      kill Snd process and buffer
;; C-h m     	 describe-mode     describe current major mode

;; Key bindings of snd-ruby-mode and snd-guile-mode editing source
;; files:
;;
;; C-c C-s   	 snd-run-snd             (Snd-Ruby or Snd-Guile)
;; M-C-x     	 snd-send-definition
;; C-x C-e   	 snd-send-last-sexp
;; C-c M-e   	 snd-send-definition
;; C-c C-e   	 snd-send-definition-and-go
;; C-c M-r   	 snd-send-region
;; C-c C-r   	 snd-send-region-and-go
;; C-c M-o   	 snd-send-buffer
;; C-c C-o   	 snd-send-buffer-and-go
;; C-c M-b   	 snd-send-block          (Ruby only)
;; C-c C-b   	 snd-send-block-and-go   (Ruby only)
;; C-c C-z   	 snd-switch-to-snd
;; C-c C-l   	 snd-load-file
;; C-u C-c C-l 	 snd-load-file-protected (Ruby only)
;;
;; and in addition:
;; 
;; C-c C-f   	 snd-file    	   open view-files-dialog of Snd
;; C-c C-p   	 snd-play    	   play current sound file
;; C-c C-t   	 snd-stop    	   stop playing all sound files
;; C-c C-i   	 snd-help    	   help on Snd-function (snd-help)
;; C-u C-c C-i   snd-help-html 	   help on Snd-function (html)
;; C-c C-g   	 snd-break   	   send C-g! to Snd process
;; C-c C-q   	 snd-quit    	   send exit to Snd process
;; C-c C-k   	 snd-kill    	   kill Snd process and buffer
;; C-h m     	 describe-mode	   describe current major mode

;;; Code:

;;;; The inf-snd-ruby-mode and inf-snd-guile-mode.

(require 'comint)
(require 'ruby-mode)
(require 'scheme)
(require 'inf-ruby)
(require 'cmuscheme)

(defconst inf-snd-version "30-Oct-2004"
  "Version of inf-snd.el.")

(defvar inf-snd-ruby-buffer "*Snd-Ruby*"
  "Inferior Snd-Ruby process buffer.")

(defvar inf-snd-ruby-buffer-name "Snd-Ruby"
  "Inferior Snd-Ruby process buffer name.")

(defvar inf-snd-ruby-mode-hook nil
  "User hook variable of `inf-snd-ruby-mode'.
Will be called after `comint-mode-hook' and before starting
inferior Snd-Ruby process.")

(defvar inf-snd-ruby-program-name "snd-ruby"
  "*User variable to set Snd-Ruby-program name and optional arguments.")

(defvar inf-snd-guile-buffer "*Snd-Guile*"
  "Inferior Snd-Guile process buffer.")

(defvar inf-snd-guile-buffer-name "Snd-Guile"
  "Inferior Snd-Guile process buffer name.")

(defvar inf-snd-prompt-char ">"
  "*User variable to determine Snd's listener prompt.")

(defvar inf-snd-guile-mode-hook nil
  "User hook variable of `inf-snd-guile-mode'.
Will be called after `comint-mode-hook' and before starting
inferior Snd-Guile process.")

(defvar inf-snd-guile-program-name "snd-guile"
  "*User variable to set Snd-Guile-program name and optional args.")

(defvar inf-snd-working-directory "~/"
  "*User variable where Emacs will find the Ruby or Guile scripts.")

(defvar inf-snd-ruby-flag t
  "Non-nil means extension language Ruby, nil Guile.
Needed to determine which extension language to use.  This variable is
buffer-local.")

(defvar inf-snd-index-path "~/"
  "*User variable to path where snd-xref.c is located.")

(defvar inf-snd-ruby-keywords nil
  "Snd keywords providing online help.
\\<inf-snd-ruby-mode-map> Will be used by
`inf-snd-help' (\\[inf-snd-help], \\[universal-argument]
\\[inf-snd-help]) and `snd-help' (\\[snd-help],
\\[universal-argument] \\[snd-help]), taken from
snd-6/snd-xref.c.  The user variable `inf-snd-index-path' should
point to the correct path of snd-xref.c.")

(defvar inf-snd-guile-keywords nil
  "Snd keywords providing online help.
\\<inf-snd-guile-mode-map> Will be used by
`inf-snd-help' (\\[inf-snd-help], \\[universal-argument]
\\[inf-snd-help]) and `snd-help' (\\[snd-help],
\\[universal-argument] \\[snd-help]), taken from
snd-6/snd-xref.c.  The user variable `inf-snd-index-path' should
point to the correct path of snd-xref.c.")

(defvar inf-snd-ruby-keyword-regexp "^  \"\\([_?!0-2a-z ()]+?\\)\"[,}]+?"
  "*Regular expression to find Snd's keywords in snd-xref.c.
Should look in variable help_names for names like
  \"add_mark\",
  \"array2file \",
  \"explode_sf2\",")

(defvar inf-snd-guile-keyword-regexp "^  \"\\([-*>?!a-z ()]+?[0-2]*?\\)\"[,}]+?"
  "*Regular expression to find Snd's keywords in snd-xref.c.
Should look in variable help_names for names like
  \"add-mark (clm)\",
  \"array->file\",
  \"explode-sf2\",")

(defun inf-snd-set-keywords ()
  "Set the keywords for `inf-snd-help'.
The user variable `inf-snd-index-path' should point to the
correct path of snd-xref.c to create valid keywords."
  (let ((fbuf (find-file-noselect (concat (expand-file-name inf-snd-index-path) "snd-xref.c")))
	(regex (if inf-snd-ruby-flag inf-snd-ruby-keyword-regexp inf-snd-guile-keyword-regexp))
	(keys '()))
    (with-current-buffer fbuf
      (goto-char (point-min))
      (setq case-fold-search nil)
      (while (re-search-forward regex nil t)
	(let ((val (match-string 1)))
	  (or (member val keys)
	      (setq keys (cons val keys))))))
    (kill-buffer fbuf)
    keys))

(defun inf-snd-set-keys (mode name)
  "Set the key bindings and menu entries for MODE.
Menu name is NAME.  You can extend the key bindings and menu entries
here or via hook variables in .emacs file."
  ;; key bindings
  (define-key (current-local-map) "\C-c\C-f" 'inf-snd-file)
  (define-key (current-local-map) "\M-\C-l" 'inf-snd-load)
  (define-key (current-local-map) "\M-\C-p" 'inf-snd-play)
  (define-key (current-local-map) "\C-c\C-s" 'inf-snd-run-snd)
  (define-key (current-local-map) "\C-c\C-t" 'inf-snd-stop)
  (define-key (current-local-map) "\C-c\C-i" 'inf-snd-help)
  (define-key (current-local-map) "\C-c\C-g" 'inf-snd-break)
  (define-key (current-local-map) "\C-c\C-k" 'inf-snd-kill)
  (define-key (current-local-map) "\C-c\C-q" 'inf-snd-quit)
  ;; menu entries in reverse order of appearance
  (define-key (current-local-map) [menu-bar mode]
    (cons name (make-sparse-keymap name)))
  (define-key (current-local-map) [menu-bar mode kill]
    '(menu-item "Kill Snd Process and Buffer" inf-snd-kill
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode quit]
    '(menu-item "Send exit to Snd Process" inf-snd-quit
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode break]
    '(menu-item "Send C-g to Snd Process" inf-snd-break
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode start-r]
    '(menu-item "Start Snd-Ruby Process" inf-snd-run-snd
		:enable (not (inf-snd-proc-p))
		:visible inf-snd-ruby-flag))
  (define-key (current-local-map) [menu-bar mode start-g]
    '(menu-item "Start Snd-Guile Process" inf-snd-run-snd
		:enable (not (inf-snd-proc-p))
		:visible (not inf-snd-ruby-flag)))
  (define-key (current-local-map) [menu-bar mode sep-quit] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode desc]
    '(menu-item "Describe Mode" describe-mode))
  (define-key (current-local-map) [menu-bar mode help-html]
    '(menu-item "Describe Snd Function (html) ..." inf-snd-help-html
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode help]
    '(menu-item "Describe Snd Function (snd-help) ..." inf-snd-help
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-desc] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode stop]
    '(menu-item "Stop Playing" inf-snd-stop
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode play]
    '(menu-item "Start Playing" inf-snd-play
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode file]
    '(menu-item "Open Snd-File Dialog" inf-snd-file
		:enable (inf-snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-play] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode load-r]
    '(menu-item "Load Ruby Script ..." inf-snd-load
		:enable (inf-snd-proc-p)
		:visible inf-snd-ruby-flag))
  (define-key (current-local-map) [menu-bar mode load-g]
    '(menu-item "Load Guile Script ..." inf-snd-load
		:enable (inf-snd-proc-p)
		:visible (not inf-snd-ruby-flag))))

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
  (with-current-buffer (inf-snd-proc-buffer)
    (insert str)
    (comint-send-input)))

(defun inf-snd-run-snd ()
  "Start inferior Snd-Ruby or Snd-Guile process.
Started from dead Snd process buffer."
  (interactive)
  (if inf-snd-ruby-flag
      (run-snd-ruby inf-snd-ruby-program-name)
    (run-snd-guile inf-snd-guile-program-name)))

(defun inf-snd-file ()
  "Open Snd's view-files-dialog widget."
  (interactive)
  (inf-snd-send-string "(view-files-dialog)"))

(defun inf-snd-load (file)
  "Load the required Ruby or Guile script.
Asks for FILE interactively in minibuffer."
  (interactive "fLoad Snd Script: ")
  (unless (file-directory-p file)
      (inf-snd-send-string
       (format "(load %S)" (car (file-expand-wildcards file t))) t)))

(defun inf-snd-play ()
  "Play current sound."
  (interactive)
  (inf-snd-send-string "(play)"))

(defun inf-snd-stop ()
  "Stop playing of all sound files."
  (interactive)
  (inf-snd-send-string "(stop-playing)"))

(defun inf-snd-help (&optional html-help)
  "Receive a string in minibuffer and show corresponding help.
\\<inf-snd-ruby-mode-map>\\<inf-snd-guile-mode-map> This is done
via Snd's function snd_help() or html() if HTML-HELP is non-nil,
i.e. it's called by \\[universal-argument] \\[inf-snd-help],
putting result at the end of the inferior Snd process buffer.  If
point is near a function name in inferior Snd process buffer,
that function will be used as default value in minibuffer;
tab-completion is activated.  `inf-snd-ruby-keywords' and
`inf-snd-guile-keywords' hold the help strings, the user variable
`inf-snd-index-path' should point to the correct path of
snd-xref.c."
  (interactive "P")
  (let ((prompt (format "Snd%s Help: " (if html-help " HTML" "")))
	(default (thing-at-point 'symbol)))
    (if default
	(progn
	  (if (string= inf-snd-prompt-char (substring default 0 (length inf-snd-prompt-char)))
	      (setq default (substring default (length inf-snd-prompt-char))))
	  (unless (string= default "")
	    (setq prompt (format "%s(default %s): " prompt default)))))
    (let ((str (completing-read prompt
				(if inf-snd-ruby-flag
				    inf-snd-ruby-keywords
				  inf-snd-guile-keywords) nil nil nil nil default)))
      (unless (string= str "")
	(unless html-help
	  (while (string-match " " str)
	    (setq str (replace-match "" t nil str))))
	(let ((inf-str (if html-help
			   (format "(html \"%s\")" str)
			 (if inf-snd-ruby-flag
			     (format "printf(\"%%s\\n\", snd-help(\"%s\"))" str)
			   (format "(snd-help \"%s\")" str)))))
	  (with-current-buffer (inf-snd-proc-buffer)
	    (goto-char (point-max))
	    (if (and (string= (char-to-string (preceding-char)) inf-snd-prompt-char)
		     (eobp))
		(inf-snd-send-string inf-str)
	      (beginning-of-line)
	      (kill-region (point) (point-max))
	      (inf-snd-send-string inf-str)
	      (yank))))))))

(defun inf-snd-help-html ()
  "Start html help."
  (interactive)
  (inf-snd-help t))

(defun inf-snd-break ()
  "Send Break-command to inferior Snd process."
  (interactive)
  (inf-snd-send-string "(c-g!)"))

(defun inf-snd-quit ()
  "Send exit to inferior Snd process."
  (interactive)
  (get-buffer-process (inf-snd-proc-buffer))
  (goto-char (point-max))
  (inf-snd-send-string "(exit 0)"))

(defun inf-snd-kill ()
  "Kill current inferior Snd process and buffer."
  (interactive)
  (delete-process (get-buffer-process (inf-snd-proc-buffer)))
  (kill-buffer (current-buffer))
  (unless (one-window-p)
    (delete-window (get-buffer-window (inf-snd-proc-buffer)))))

(defun inf-snd-proc-buffer ()
  "Return the current process buffer."
  (if inf-snd-ruby-flag inf-snd-ruby-buffer inf-snd-guile-buffer))

(defun inf-snd-proc-p ()
  "Return non-nil if no process buffer available."
  (save-current-buffer
    (comint-check-proc (if inf-snd-ruby-flag inf-snd-ruby-buffer inf-snd-guile-buffer))))
  
(defun inf-snd-comint-snd-send (proc str)
  "Special function for sending to PROC input STR.
Used by function `comint-input-sender'."
  (comint-send-string proc
		      (if (> (length str) 0)
			  (if inf-snd-ruby-flag
			      (format "((val = (%s)).kind_of?(String) ? val : val.inspect)\n" str)
			    (format "%s\n" str))
			(if inf-snd-ruby-flag
			    "\n"
			  "#f\n"))))

(defun inf-snd-comint-prepend-comment-if-exception (string)
  "Look for `Exception' in STRING in the current output.
If `Exception' exists, prepends all lines with the comment sign
`#'.  This function could be on
`comint-preoutput-filter-functions'."
  (if (string-match "^Exception `" string)
      (let ((strs (split-string string "\n+"))
	    (str ""))
	(dolist (s strs)
	  (setq str (concat str (format "# %s\n" s))))
	(if (string-match (concat "# " inf-snd-prompt-char "\n") str)
	    (setq str (replace-match inf-snd-prompt-char t nil str)))
	str)
    string))

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

(defvar ruby-font-lock-keywords nil)
(defvar font-lock-keywords nil)
(defvar ruby-font-lock-syntax-table nil)
(defvar font-lock-syntax-table nil)
(defvar ruby-font-lock-syntactic-keywords nil)
(defvar font-lock-syntactic-keywords nil)

(define-derived-mode inf-snd-ruby-mode comint-mode inf-snd-ruby-buffer-name
  "Inferior mode running Snd-Ruby, derived from `comint-mode'.

Snd is a sound editor created by Bill Schottstaedt
\(bil@ccrma.Stanford.EDU).  You can find it on
ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-7.tar.gz.

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
  (setq comint-preoutput-filter-functions '(inf-snd-comint-prepend-comment-if-exception))
  (setq comint-input-filter (function ruby-input-filter))
  (setq comint-get-old-input (function ruby-get-old-input))
  (setq comint-prompt-regexp (concat "^\\(" inf-snd-prompt-char "\\)+"))
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
  (setq inf-snd-ruby-keywords (inf-snd-set-keywords))
  (setq mode-line-process '(":%s"))
  (inf-snd-set-keys 'inf-snd-ruby-mode inf-snd-ruby-buffer-name)
  (pop-to-buffer inf-snd-ruby-buffer)
  (goto-char (point-max))
  (run-hooks 'inf-snd-ruby-mode-hook))

(define-derived-mode inf-snd-guile-mode comint-mode inf-snd-guile-buffer-name
  "Inferior mode running Snd-Guile, derived from `comint-mode'.

Snd is a sound editor created by Bill Schottstaedt
\(bil@ccrma.Stanford.EDU).  You can find it on
ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-7.tar.gz.

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
  (setq comint-prompt-regexp (concat "^\\(" inf-snd-prompt-char "\\)+"))
  (setq comint-input-sender (function inf-snd-comint-snd-send))
  (make-local-variable 'default-directory)
  (setq default-directory inf-snd-working-directory)
  (make-local-variable 'inf-snd-ruby-flag)
  (setq inf-snd-ruby-flag nil)
  (setq inf-snd-guile-keywords (inf-snd-set-keywords))
  (setq mode-line-process '(":%s"))
  (inf-snd-set-keys 'inf-snd-guile-mode inf-snd-guile-buffer-name)
  (pop-to-buffer inf-snd-guile-buffer)
  (goto-char (point-max))
  (run-hooks 'inf-snd-guile-mode-hook))

;; Only for run-snd-ruby run-snd-guile to force Snd showing a prompt.
(defun snd-send-invisible (str)
  "Send a STR to the process running in the current buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (cond ((not proc)
	   (error "Current buffer has no process"))
	  ((stringp str)
	   (comint-snapshot-last-prompt)
	   (funcall comint-input-sender proc str)))))

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
    (snd-send-invisible "snd_version"))) ; dummy to force Snd showing a prompt

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
    (snd-send-invisible "(snd-version)")))

;;;; The snd-ruby-mode and snd-guile-mode

;;; Commentary

;; These two modes are derived from ruby-mode and scheme-mode.  The
;; main changes are the key bindings, which now refer to special
;; Snd-process-buffer-related ones.  I took commands from inf-ruby.el
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

In addition, you can start an inferior Snd process and some
additional commands will be defined for evaluating expressions.
A menu ``Snd/Ruby'' appears in the menu bar.  Entries in this
menu are disabled if no inferior Snd process exist.

You can use the hook variables `ruby-mode-hook' and
`snd-ruby-mode-hook', which will be called in that order.

The current key bindings are:
\\{snd-ruby-mode-map}"
  (make-local-variable 'snd-inf-ruby-flag)
  (setq snd-inf-ruby-flag t)
  (make-local-variable 'snd-source-modes)
  (setq snd-source-modes '(snd-ruby-mode))
  (unless inf-snd-ruby-keywords
    (setq inf-snd-ruby-keywords (inf-snd-set-keywords)))
  (snd-set-keys 'snd-ruby-mode snd-ruby-buffer-name)
  (run-hooks 'snd-ruby-mode-hook))

(define-derived-mode snd-guile-mode scheme-mode snd-guile-buffer-name
  "Major mode for editing Snd-Guile code.

Editing commands are similar to those of `scheme-mode'.

In addition, you can start an inferior Snd process and some
additional commands will be defined for evaluating expressions.
A menu ``Snd/Guile'' appears in the menu bar.  Entries in this
menu are disabled if no inferior Snd process exist.

You can use variables `scheme-mode-hook' and
`snd-guile-mode-hook', which will be called in that order.

The current key bindings are:
\\{snd-guile-mode-map}"
  (make-local-variable 'snd-inf-ruby-flag)
  (setq snd-inf-ruby-flag nil)
  (make-local-variable 'snd-source-modes)
  (setq snd-source-modes '(snd-guile-mode))
  (unless inf-snd-guile-keywords
    (setq inf-snd-guile-keywords (inf-snd-set-keywords)))
  (snd-set-keys 'snd-guile-mode snd-guile-buffer-name)
  (run-hooks 'snd-guile-mode-hook))

(defun snd-send-region (start end)
  "Send the current region to the inferior Snd process.
START and END define the region."
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (if snd-inf-ruby-flag
	(let ((tfile (make-temp-name (expand-file-name "inf-snd" temporary-file-directory))))
	  (with-temp-file tfile
	    (insert str))
	  (comint-send-string (snd-proc) (format "eval(File.open(%S).read).inspect\n" tfile))
	  (delete-file tfile))
      (comint-send-string (snd-proc) str)
      (comint-send-string (snd-proc) "\n"))))

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

(defun snd-send-last-sexp (&optional cnt)
  "Send the previous or CNT sexp to the inferior Snd process."
  (interactive "p")
  (snd-send-region (save-excursion
		     (if snd-inf-ruby-flag
			 (ruby-backward-sexp cnt)
		       (backward-sexp cnt))
		     (point))
		   (point)))

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
  "If inferior Snd process exists, switch to process buffer, else start Snd.
Non-nil EOB-P positions cursor at end of buffer."
  (interactive "P")
  (let ((buf (if snd-inf-ruby-flag inf-snd-ruby-buffer inf-snd-guile-buffer)))
    (if (get-buffer buf)
	(pop-to-buffer buf)
      (snd-run-snd))
    (if eob-p
	(push-mark)
      (goto-char (point-max)))))

(defun snd-run-snd ()
  "If inferior Snd process exists, switch to process buffer, else start Snd.
Started from `snd-ruby-mode' or `snd-guile-mode'."
  (interactive)
  (if (snd-proc-p)
      (snd-switch-to-snd t)
    (if snd-inf-ruby-flag
	(run-snd-ruby inf-snd-ruby-program-name)
      (run-snd-guile inf-snd-guile-program-name))))

(defun snd-load-file-protected (filename)
  "Load a Snd script FILENAME into the inferior Snd process.
In `snd-ruby-mode' the script will be loaded in an anonymous
module, thus protecting the namespace."
  (interactive (comint-get-source "Load Snd script file: "
				  snd-prev-l/c-dir/file snd-source-modes t))
  (comint-check-source filename)
  (setq snd-prev-l/c-dir/file (cons (file-name-directory filename)
				     (file-name-nondirectory filename)))
  (let ((opts (if snd-inf-ruby-flag ", true" "")))
    (comint-send-string (snd-proc) (concat "(load \"" filename"\"" opts "\)\n"))))

(defun snd-load-file (filename)
  "Load a Snd script FILENAME into the inferior Snd process."
  (interactive (comint-get-source "Load Snd script file: "
				  snd-prev-l/c-dir/file snd-source-modes t))
  (comint-check-source filename)
  (setq snd-prev-l/c-dir/file (cons (file-name-directory filename)
				     (file-name-nondirectory filename)))
  (comint-send-string (snd-proc) (concat "(load \"" filename"\"\)\n")))

(defun snd-save-state ()
  "Synchronizes the inferior Snd process with the edit buffer."
  (and (snd-proc)
       (setq inf-snd-ruby-flag snd-inf-ruby-flag)))
  
(defun snd-file ()
  "Open Snd's view-files-dialog widget."
  (interactive)
  (snd-save-state)
  (inf-snd-file))

(defun snd-play ()
  "Play current sound."
  (interactive)
  (snd-save-state)
  (inf-snd-play))

(defun snd-stop ()
  "Stop playing of all sounds."
  (interactive)
  (snd-save-state)
  (inf-snd-stop))

(defun snd-help (&optional html-help)
  "Receive a string in minibuffer and show corresponding help.
\\<inf-snd-ruby-mode-map>\\<inf-snd-guile-mode-map> This is done
via Snd's function snd_help() or html() if HTML-HELP is non-nil,
i.e. it's called by \\[universal-argument] \\[snd-help], putting
result at the end of the inferior Snd process buffer.  If point
is near a function name in inferior Snd process buffer, that
function will be used as default value in minibuffer;
tab-completion is activated.  `inf-snd-ruby-keywords' and
`inf-snd-guile-keywords' hold the help strings, the user variable
`inf-snd-index-path' should point to the correct path of
snd-xref.c."
  (interactive "P")
  (snd-save-state)
  (inf-snd-help html-help))

(defun snd-help-html ()
  "Start html help."
  (interactive)
  (snd-help t))

(defun snd-break ()
  "Send Break-command to inferior Snd process."
  (interactive)
  (snd-save-state)
  (inf-snd-break))

(defun snd-quit ()
  "Send exit to current inferior Snd process."
  (interactive)
  (snd-save-state) 
  (save-excursion
    (snd-switch-to-snd t)
    (inf-snd-quit)))

(defun snd-kill ()
  "Kill current inferior Snd process buffer."
  (interactive)
  (snd-save-state)
  (save-excursion
    (snd-switch-to-snd t)
    (inf-snd-kill)))

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
  (define-key (current-local-map) "\C-c\C-t" 'snd-stop)
  (define-key (current-local-map) "\C-c\C-i" 'snd-help)
  (define-key (current-local-map) "\C-c\C-g" 'snd-break)
  (define-key (current-local-map) "\C-c\C-k" 'snd-kill)
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
  (define-key (current-local-map) [menu-bar mode kill]
    '(menu-item "Kill Snd Process and Buffer" snd-kill
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode quit]
    '(menu-item "Send exit to Snd Process" snd-quit
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode break]
    '(menu-item "Send C-g to Snd Process" snd-break
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-quit] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode desc]
    '(menu-item "Describe Mode" describe-mode))
  (define-key (current-local-map) [menu-bar mode help-html]
    '(menu-item "Describe Snd Function (html) ..." snd-help-html
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode help]
    '(menu-item "Describe Snd Function (snd-help) ..." snd-help
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode sep-desc] '(menu-item "--"))
  (define-key (current-local-map) [menu-bar mode stop]
    '(menu-item "Stop Playing" snd-stop
		:enable (snd-proc-p)))
  (define-key (current-local-map) [menu-bar mode play]
    '(menu-item "Start Playing" snd-play
		:enable (snd-proc-p)))
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
  (define-key (current-local-map) [menu-bar mode load-sec]
    '(menu-item "Load Ruby Script (protected) ..." snd-load-file-protected
		:enable (snd-proc-p)
		:visible snd-inf-ruby-flag))
  (define-key (current-local-map) [menu-bar mode load-r]
    '(menu-item "Load Ruby Script ..." snd-load-file
		:enable (snd-proc-p)
		:visible snd-inf-ruby-flag))
  (define-key (current-local-map) [menu-bar mode load-g]
    '(menu-item "Load Guile Script ..." snd-load-file
		:enable (snd-proc-p)
		:visible (not snd-inf-ruby-flag))))

(provide 'inf-snd)

;;; inf-snd.el ends here
