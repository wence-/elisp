;;; cua-emul.el --- CUA style buffer-switching

;; Copyright (C) 2002 lawrence mitchell <wence@gmx.li>

;; Time-stamp: <2002-06-16 20:47:03 lawrence>

;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-04-26
;; Keywords: buffer-switching convenience
;; Version: $Id: cua-emul.el,v 1.3 2002/06/16 19:47:14 lawrence Exp $

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; This file provides some emulation of CUA style buffer manipulation, in
;; the form of a minor mode.  In particular it provides the following (I
;;  think useful) functions:
;; `cua-emul-next-buffer' -- Switch to the next buffer in the buffer
;;                           list, like CUA style ctrl-tab.
;; `cua-emul-previous-buffer' -- Switch to the previous buffer in the
;;                               buffer list, like CUA style
;;                               ctrl-shift-tab.
;; `cua-emul-kill-buffer' -- Kill and maybe save the current buffer,
;;                           like CUA ctrl-f4.
;; `cua-emul-kill-frame' -- Kill the current frame, or
;;                          `save-buffers-kill-emacs' if the current
;;                          frame is the only one, like CUA alt-f4.
;;
;; To use this file, put it somewhere in your load-path, optionally
;; byte compile it and then add the following to your .emacs:
;; (require  'cua-emul)
;; (turn-on-cua-emul-mode)
;;
;; If you just want the functionality of the above mentioned commands,
;; but don't want the whole minor mode, the bits you have to pull out
;; are:
;; `cua-emul-delete-from-list', both the variable and the function
;; `cua-emul-invisible-buffers', and whichever of the above mentioned
;; commands you want.
;;
;; Note, by default, if you have any keybindings on keys cua-emul-mode
;; uses, they will not be overriden, if you want to force cua-emul-mode
;; to override keybindings, set the variable `cua-emul-force' to a
;; non-nil value.  If you do set `cua-emul-force', you may also want to
;; set `cua-emul-save-and-restore-keys' to a non-nil value so that
;; overriden keybindings are restored when `cua-emul-mode' is turned
;; off.
;;
;; If you like CUA keybindings a lot you might also want to look at
;;`pc-selection-mode', `pc-bindings-mode', (both included with Emacs
;; 21) and `cua-mode' by Kim F.  Storm -- see <URL:http://www.cua.dk/>.


;;; History:
;; $Id: cua-emul.el,v 1.3 2002/06/16 19:47:14 lawrence Exp $
;;
;; $Log: cua-emul.el,v $
;; Revision 1.3  2002/06/16 19:47:14  lawrence
;; Minor cosmetic changes.
;;
;; New functions -- `cua-emul-restore-keys' and `cua-emul-save-keys'.
;; Allow saving and restoring of overriden keys.
;; New variables -- `cua-emul-overriden-key-alist' and
;; `cua-emul-save-and-restore-keys'.  Used by the above functions.
;;
;; New function -- `cua-emul-version'.
;;
;; `cua-emul-{next|previous}-buffer' completely re-worked, cleaner and
;; quicker (especially previous-buffer).
;;
;; Made `cua-emul-previous-buffer' somewhat faster by removing redundant
;; calls to `cua-emul-delete-from-list'.  This is especially noticable
;; when a large (100+) number of buffers are open.
;;
;; New variable -- `cua-emul-key-alist'.  Alist associating keys to
;; commands.  This is so that we can use the new functions:
;; `cua-emul-set-keys' and `cua-emul-unset-keys'.  These two functions
;; are there purely to make the function `cua-emul-mode' look nicer, ;-)
;; Well, not quite, they also ease  maintainability, since to add
;; extra keybindings, one need only change the value of the variable
;; `cua-emul-key-alist'.
;;
;; Added support for customize.
;; New functions -- `turn-on-cua-emul-mode' and `turn-off-cua-emul-mode',
;; relatively self-explanatory.
;;
;; New functions -- `cua-emul-set-key' and `cua-emul-unset-key'.  Make
;; rebinding keys semi-optional.
;; Make the keybindings variables rather than hard-coding them.
;;

;;; TODO:
;; If overriding existing key definitions, remember what they were so
;; that when/if we disable cua emul mode, the previous keybindings go
;; back into place.  Is this at all feasible?
;;
;; Make keybindings Emacs/XEmacs and version specific.

;;; Code:


;;; Customize stuff

(defgroup cua-emul nil
  "CUA style buffer-switching."
  :group 'convenience
  :prefix "cua-emul-")


(defcustom cua-emul-mode nil
  "Toggle CUA Emul Mode.

This minor mode provides some minimal CUA style buffer manipulation
keybindings, namely:
`cua-emul-next-buffer' -- switch to the next buffer, bound to ctrl-tab;

`cua-emul-previous-buffer' -- switch to the previous buffer, bound to
                              ctrl-shift-tab;

`cua-emul-kill-buffer' -- kill and optionally save the current
                          buffer, bound to ctrl-f4;

`cua-emul-kill-frame' -- kill the current frame, if this is the only
                         frame, call `save-buffers-kill-emacs', bound
                         to meta-f4.

You can customize the keybindings used by setting the variables
`cua-emul-next-buffer-key', `cua-emul-previous-buffer-key',
`cua-emul-kill-buffer-key' and `cua-emul-kill-frame-key'
appropriately.

Setting this variable directly does not take effect;
use either \\[customize] or the function `cua-emul-mode'."
  :set (lambda (symbol value)
	 (cua-emul-mode (or value 0)))
  :initialize 'custom-initialize-default
  :group 'cua-emul
  :require 'cua-emul
  :type 'boolean)


;;; User variables

(defcustom cua-emul-force nil
  "*If non-nil, CUA emul mode will unconditionally rebind keys.

The keys rebound are those defined by the variables:
`cua-emul-next-buffer-key', `cua-emul-previous-buffer-key',
`cua-emul-kill-buffer-key' and `cua-emul-kill-frame-key'."
  :group 'cua-emul
  :require 'cua-emul
  :type 'boolean)

(defcustom cua-emul-mode-hook nil
  "Hook to be run when entering, leaving `cua-emul-mode'."
  :type 'hook
  :group 'cua-emul)

(defcustom cua-emul-next-buffer-key [(control tab)]
  "*Keybinding for `cua-emul-next-buffer'.
If you leave this as an empty string or as the symbol nil, the key
will be unset.

This has to be a valid keybinding in your Emacs version.
The default value is ctrl-tab."
  :set (lambda (symbol value)		; yes this is ugly, but I can't think of
					; a better way of doing it.
	 (if (or (null value) (equal value ""))
	     (cua-emul-unset-key cua-emul-next-buffer-key
				 'cua-emul-next-buffer nil)
	   (cua-emul-set-key value 'cua-emul-next-buffer t)))
  :initialize 'custom-initialize-default
  :type 'sexp
  :group 'cua-emul)

(defcustom cua-emul-previous-buffer-key [(control shift tab)]
  "*Keybinding for `cua-emul-previous-buffer'.
If you leave this as an empty string or as the symbol nil, the key
will be unset.

This has to be a valid keybinding in your Emacs version.
The default value is ctrl-shift-tab."
  :set (lambda (symbol value)
	 (if (or (null value) (equal value ""))
	     (cua-emul-unset-key cua-emul-previous-buffer-key
				 'cua-emul-previous-buffer nil)
	   (cua-emul-set-key value 'cua-emul-previous-buffer t)))
  :initialize 'custom-initialize-default
  :type 'sexp
  :group 'cua-emul)

(defcustom cua-emul-kill-buffer-key [(control f4)]
  "*Keybinding for `cua-emul-kill-buffer'.
If you leave this as an empty string or as the symbol nil, the key
will be unset.

This has to be a valid keybinding in your Emacs version.
The default value is ctrl-f4."
  :set (lambda (symbol value)
	 (if (or (null value) (equal value ""))
	     (cua-emul-unset-key cua-emul-kill-buffer-key
				 'cua-emul-kill-buffer nil)
	   (cua-emul-set-key value 'cua-emul-kill-buffer t)))
  :initialize 'custom-initialize-default
  :type 'sexp
  :group 'cua-emul)

(defcustom cua-emul-kill-frame-key [(meta f4)]
  "*Keybinding for `cua-emul-kill-frame'.
If you leave this as an empty string or as the symbol nil, the key
will be unset.

This has to be a valid keybinding in your Emacs version.
The default value is meta-f4."
  :set (lambda (symbol value)
	 (if (or (null value) (equal value ""))
	     (cua-emul-unset-key cua-emul-kill-frame-key
				 'cua-emul-kill-frame nil)
	   (cua-emul-set-key value 'cua-emul-kill-frame t)))
  :initialize 'custom-initialize-default
  :type 'sexp
  :group 'cua-emul)

(defcustom cua-emul-invisible-buffers
  '("KILL" "*Compile-Log*" "*Completions*" ".newsrc-dribble" "buffer")
  "*List of buffer names you don't want to see when buffer-switching."
  :type 'alist
  :group 'cua-emul)

(defcustom cua-emul-save-and-restore-keys nil
  "Whether `cua-emul-mode' should save and restore existing keybindings.

If this is non-nil, we will save and restore them."
  :type 'boolean
  :group 'cua-emul)

;;; Internal Variables

(defconst cua-emul-key-alist
  '((cua-emul-next-buffer-key . cua-emul-next-buffer)
    (cua-emul-previous-buffer-key . cua-emul-previous-buffer)
    (cua-emul-kill-buffer-key . cua-emul-kill-buffer)
    (cua-emul-kill-frame-key . cua-emul-kill-frame))
    "Alist of keys and their associated commands.

Don't touch this variable unless you understand how the functions
`cua-emul-set-keys' and `cua-emul-unset-keys' work.  You might break
something.")

(defvar cua-emul-overriden-key-alist nil
  "Alist of keys overriden by `cua-emul-mode'.

We will try and restore these when disabling it.")

(defconst cua-emul-version
  "$Id: cua-emul.el,v 1.3 2002/06/16 19:47:14 lawrence Exp $"
  "CUA Emul Mode version number.")

;;; Internal Functions

(defun cua-emul-invisible-buffers ()
  "Convert the variable `cua-emul-invisible-buffers' to a list of buffers.

Then add all buffers whose name begins with \" \" to the list."
  ;; taken from the emacs wiki <URL:http://www.emacswiki.org/>
  (delete nil
          (append
           (mapcar 'get-buffer cua-emul-invisible-buffers)
           (mapcar (lambda (this-buffer)
                     (if (string-match "^ " (buffer-name this-buffer))
                         this-buffer))
                   (buffer-list)))))

(defun cua-emul-delete-from-list (members list)
  "Delete MEMBERS from LIST.

Return modified list."
  (while members
    (setq list (delete (car members) list)
          members (cdr members)))
  list)

(defun cua-emul-set-key (key command &optional force)
  "Bind KEY globally to COMMAND if KEY is currently unbound.

If optional third argument FORCE is non-nil, override any existing key
definition.
Calls `global-set-key' (which see)."
  (if (and (null force) (key-binding key))
      (message "%s not rebound from `%s' to `%s'."
	       key (key-binding key) command)
    (global-set-key key command)))

(defun cua-emul-unset-key (key command &optional force)
  "Unbind KEY globally if it is currently bound to COMMAND.

If optional third argument FORCE is non-nil, unconditionally unbind KEY.
Calls `global-unset-key' (which see)."
  (if (or force (eq (key-binding key) command))
	(global-unset-key key)
    (message "%s not unbound" key)))

(defun cua-emul-restore-keys ()
  "Restore the keybindings we overrode."
  (let ((alist cua-emul-overriden-key-alist))
    (while alist
      (global-set-key (caar alist) (cdar alist))
      (setq alist (cdr alist))))
  (setq cua-emul-overriden-key-alist nil) )

(defun cua-emul-save-keys ()
  "Save the keybindings we have overriden."
  (let ((alist cua-emul-key-alist))
    (while alist
      (let ((key (symbol-value (caar alist))))
	(if (key-binding key)
	    (add-to-list 'cua-emul-overriden-key-alist
			 (cons key (key-binding key)))))
      (setq alist (cdr alist)))))
	    
(defun cua-emul-set-keys (alist &optional force)
  ;; hmmm...does this docstring make sense to you?
  "Bind the cars (keys) of the conses of ALIST to the cdrs (commands).

For example, if ALIST's value was:
\(([(control c)] . ignore))
control-c would be bound to `ignore'.

A key is only bound if it is currently undefined, unless the optional
second argument FORCE is non-nil, in which case any existing key
definitions are overriden.
See also `cua-emul-set-key'."
  (if cua-emul-save-and-restore-keys
      (cua-emul-save-keys))
  (while alist
    (cua-emul-set-key (symbol-value (caar alist)) (cdar alist) force)
    (setq alist (cdr alist))))
 
(defun cua-emul-unset-keys (alist &optional force)
  ;; and does this one...?
  "Unbind the cars (keys) of the conses of ALIST.

A key is only unbound if it is currently bound to the cdr of its cons
cell, unless the optional second argument FORCE is non-nil, in which
case it is unconditionally unbound.
See also `cua-emul-unset-key'."
  (while alist
    (cua-emul-unset-key (symbol-value (caar alist)) (cdar alist) force)
    (setq alist (cdr alist)))
  (if cua-emul-save-and-restore-keys
      (cua-emul-restore-keys)))


;;; User functions

(defun cua-emul-mode (&optional arg force)
  "Enable or disable CUA emul mode.

If called interactively with no prefix argument, toggle current condition
of the mode.
If ARG is positive unconditionally enable the mode, if ARG is negative
turn off the mode.

If FORCE is non-nil, override any existing keybindings that might
happen to use the ones we want to.
See the variable `cua-emul-mode' for more information."
  (interactive "P")
  (let ((key-alist cua-emul-key-alist) 	; alist of keys and commands
	(force-flag			; non-nil if we want to force
	 (or force cua-emul-force)))	; setting/unsetting of keys.
    (setq cua-emul-mode
	  (if (null arg)
	      (not cua-emul-mode)
	    (> (prefix-numeric-value arg) 0)))
    (if (interactive-p)
	(if cua-emul-mode
	    (message "CUA emul mode enabled.")
	  (message "CUA emul mode disabled.")))
    (if cua-emul-mode
	(cua-emul-set-keys key-alist force-flag)
      (cua-emul-unset-keys key-alist force-flag)))
  (run-hooks 'cua-emul-mode-hook))

(defun cua-emul-version (&optional arg)
  "Print Cua Emul's version number in the minibuffer.

If optional ARG is non-nil, insert in current buffer."
  (interactive "*P")
  (if arg
      (insert "\n" cua-emul-version "\n")
    (message cua-emul-version)))

(defun turn-on-cua-emul-mode ()
  "Unconditionally turn on CUA emul mode."
  (interactive)
  (cua-emul-mode 1))

(defun turn-off-cua-emul-mode ()
  "Unconditionally turn off CUA emul mode."
  (interactive)
  (cua-emul-mode -1))

(defun cua-emul-next-buffer ()
  "Switch to the next buffer in `buffer-list'.

This function emulates the CUA style ctrl-shift-tab."
  (interactive)
  (bury-buffer (car (buffer-list)))
  (let ((target-buffer (car (cua-emul-delete-from-list
			     (cua-emul-invisible-buffers) (buffer-list)))))
    (switch-to-buffer target-buffer)))

(defun cua-emul-previous-buffer ()
  "Switch to the previous buffer in `buffer-list'.

This function emulates the CUA style ctrl-shift-tab."
  (interactive)
  (let* ((buffer-list (cua-emul-delete-from-list
		       (cua-emul-invisible-buffers) (buffer-list)))
	 (target-buffer (nth (1- (length buffer-list)) buffer-list)))
    (switch-to-buffer target-buffer)))

(defun cua-emul-kill-buffer ()
  "Maybe save, then kill current buffer."
  (interactive)
  (if (or (null buffer-file-name)
	  buffer-read-only)
      (kill-this-buffer)
    (if (buffer-modified-p)
	(if (yes-or-no-p "Save buffer before closing? ")
	    (save-buffer)
	  (set-buffer-modified-p nil)))
    (kill-this-buffer)))

(defun cua-emul-kill-frame ()
  "Kill the current frame.

If this is the only frame, check to see if Gnus is running, kill it if
it is, and then kill Emacs."
  (interactive)
  (if (cdr (frame-list))    ; non nil if there is more than one frame.
      (delete-frame)
    (and (featurep 'gnus) (gnus-alive-p) (gnus-group-exit))
    (save-buffers-kill-emacs)))

(provide 'cua-emul)

;;; cua-emul.el ends here
