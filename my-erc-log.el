;;; erc-log.el --- Logging facilities for ERC.

;; Author: Lawrence Mitchell <wence@gmx.li>
;; Keywords: IRC, chat, client, Internet, logging

;; Contributions from: (on #emacs)
;; antifuchs
;; Zwax

;; Created 2003-04-26
;; Logging code taken from erc.el and modified to use markers.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; This file implements log file writing support for the Emacs IRC
;; Client.

;; How it works:
;;
;; If logging is enabled, at some point, `erc-save-buffer-in-logs'
;; will be called.  The "end" of the buffer is taken from
;; `erc-insert-marker', while `erc-last-saved-position' holds the
;; position the buffer was last saved at (as a marker, or if the
;; buffer hasn't been saved before, as the number 1 (point-min)).

;; The region between `erc-last-saved-position' and
;; `erc-insert-marker' is saved to the current buffer's logfile, and
;; `erc-last-saved-position' is updated to reflect this.

;;; History:
;; 2003-04-26: logging code pulled out of erc.el.  Switched to using
;; markers.

;;; TODO:
;; * Erc needs a generalised make-safe-file-name function, so that
;;   generated file names don't contain any invalid file characters.
;;
;; * Really, we need to lock the logfiles somehow, so that if a user
;;   is running multiple emacsen and/or on the same channel as more
;;   than one user, only one process writes to the logfile.  This is
;;   especially needed for those logfiles with no nick in them, as
;;   these would become corrupted.
;;   For a single emacs process, the problem could be solved using a
;;   variable which contained the names of buffers already being
;;   logged.  This would require that logging be buffer-local,
;;   possibly not a bad thing anyway, since many people don't want to
;;   log the server buffer.
;;   For multiple emacsen the problem is trickier.  On some systems,
;;   on could use the function `lock-buffer' and `unlock-buffer'.
;;   However, file locking isn't implemented on all platforms, for
;;   example, there is none on w32 systems.
;;   A third possibility might be to fake lockfiles.  However, this
;;   might lead to problems if an emacs crashes, as the lockfile
;;   would be left lying around.
;;
;; * Should one be able to enable logging on a per-buffer basis?  I
;;   think yes, since often people don't want to log server buffers
;;   or dcc chats etc...
;;   This would also remove the need to overload the semantics of
;;   `erc-log-channels-directory'.
;;   If logging should be enabled on a per buffer basis, there are a
;;   number of different ways it could be implemented.
;; ** A boolean variable, that is buffer local.
;; ** A regexp that matches buffer names.
;; ** A function that gets called with the buffer name (and possibly
;;    other useful variables) and figures out whether said buffer
;;    should be logged.
;; ** A combination of all of the above.
;;
;; Currently, logging is buffer local, allowing either a boolean
;; variable, or a function to be used.  (regexps likely being too
;; slow for large numbers of buffers).

;;; Code:

(require 'erc)

(defgroup erc-log nil
  "Logging facilities for ERC."
  :group 'erc)

(defcustom erc-generate-log-file-name-function 'erc-generate-log-file-name-long
  "*A function to generate a log filename.
The function must take five arguments: BUFFER, TARGET, NICK, SERVER and PORT.
BUFFER is the buffer to be saved,
TARGET is the name of the channel, or the target of the query,
NICK is the current nick,
SERVER and PORT are the parameters used to connect BUFFERs `erc-process'."
  :group 'erc-log
  :type '(choice (const erc-generate-log-file-name-long)
		 (const erc-generate-log-file-name-short)
		 (const erc-generate-log-file-name-with-date)
		 (symbol)))

(defcustom erc-save-buffer-on-part nil
  "*Save the channel buffer content using `erc-save-buffer-in-logs' on PART."
  :group 'erc-log
  :type 'boolean)

(defcustom erc-truncate-buffer-on-save nil
  "Truncate any ERC (channel, query, server) buffer when it is saved."
  :group 'erc-log
  :type 'boolean)

(defcustom erc-enable-logging t
  "If non-nil, ERC will log IRC conversations.
This can either be a boolean value nil or t.  Or a function.
If the value is a function, it will be called with one argument, the
name of the current ERC buffer.  One possible function, which saves
all but server buffers is `erc-log-all-but-server-buffers'.

This variable is buffer local setting it via \\[customize] sets the
default value.

Log files are stored in `erc-log-channels-directory'."
  :group 'erc-log
  :type '(choice boolean
                 function))
(make-variable-buffer-local 'erc-enable-logging)

(defcustom erc-log-channels-directory "~/log/"
  "The directory to place log files for channels.
Leave blank to disable logging.  If not nil, all the channel
buffers are logged in separate files in that directory.  The
directory should not end with a trailing slash."
  :group 'erc-log
  :type '(choice (const nil)))

(defcustom erc-log-insert-log-on-open t
  "*Insert log file contents into the buffer if a log file exists."
  :group 'erc-log
  :type 'boolean)

;; (defconst erc-w32-invalid-file-characters
;;   (if (memq system-type '(windows-nt ms-dos cygwin))
;;       (let ((regexp file-name-invalid-regexp))
;; 	(when (string-match "^.*\\?\\(:.*\\)" regexp)
;; 	  (setq regexp (match-string 1 regexp))
;; 	  (when (string-match "\\[\\(.*\\)]" regexp)
;; 	    (setq regexp (concat "["
;; 				 (regexp-quote
;; 				  (concat ":"
;; 					  (match-string 1 regexp)))
;; 				 "]"))))
;; 	regexp)
;;       nil)
;;   "Regular expression matching invalid file-name characters on w32 systems.")

(define-erc-module log nil
  "Automatically logs things you receive on IRC into files.
Files are stored in `erc-log-channels-directory'; file name
format is defined through a formatting function on
`erc-generate-log-file-name-function'.

Since automatic logging is not always a Good Thing (especially if
people say things in different coding systems), you can turn logging
behaviour on and off with the variable `erc-enable-logging', which can
also be a predicate function. To only log when you are not set away, use:

\(setq erc-enable-logging
      (lambda (buffer)
	(with-current-buffer buffer
	  (not away))))"
  ;; enable
  ((add-hook 'erc-insert-post-hook
	     'erc-save-buffer-in-logs)
   (add-hook 'erc-send-post-hook
	     'erc-save-buffer-in-logs))
  ;; disable
  ((remove-hook 'erc-insert-post-hook
		'erc-save-buffer-in-logs)
   (remove-hook 'erc-send-post-hook
		'erc-save-buffer-in-logs)))
  
(defun erc-log-all-but-server-buffers (buffer)
  "Returns t if logging should be enabled in BUFFER.
Returns nil iff `erc-server-buffer-p' returns t."
  (save-window-excursion
    (set-buffer buffer)
    (not (erc-server-buffer-p))))

;;;###autoload
(defun erc-logging-enabled (&optional buffer)
  "Return non-nil if logging is enabled for BUFFER.
If BUFFER is nil, the value of `current-buffer' is used.
Logging is enabled if `erc-log-channels-directory' is non-nil, the directory
is writeable (it will be created as necessary) and
`erc-enable-logging' returns a non-nil value."
  (and erc-log-channels-directory
       (erc-directory-writable-p erc-log-channels-directory)
       (if (functionp erc-enable-logging)
           (funcall erc-enable-logging (or buffer (current-buffer)))
           erc-enable-logging)))

(defun erc-current-logfile (&optional buffer)
  "Return the logfile to use for BUFFER.
If BUFFER is nil, the value of `current-buffer' is used.
This is determined by `erc-generate-log-file-name-function'.
The result is converted to lowercase, as IRC is case-insensitive"
  (downcase (expand-file-name
	     (funcall erc-generate-log-file-name-function
		      (or buffer (current-buffer))
                      (erc-default-target) (erc-current-nick)
		      erc-session-server erc-session-port))))

(defun erc-generate-log-file-name-with-date (buffer &rest ignore)
  "This function computes a short log file name.
The name of the log file is composed of BUFFER and the current date.
This function is a possible value for `erc-generate-log-file-name-function'."
  (let ((filename (llm-erc-make-filename
                   (concat (buffer-name buffer)
                           (format-time-string "-%Y-%m-%d.txt")))))
  (concat erc-log-channels-directory
          filename)))

(defun erc-generate-log-file-name-short (buffer &rest ignore)
  "This function computes a short log file name.
In fact, it only uses the buffer name of the BUFFER argument, so
you can affect that using `rename-buffer' and the-like.  This
function is a possible value for
`erc-generate-log-file-name-function'."
  (concat erc-log-channels-directory "/"
          (llm-erc-make-filename
           (concat (buffer-name buffer) ".txt"))))

(defun erc-generate-log-file-name-long (buffer target nick server port)
  "Generates a log-file name in the way ERC always did it.
This results in a file name of the form #channel!nick@server:port.txt.
This function is a possible value for `erc-generate-log-file-name-function'."
  (let ((file (concat
	       (if target (concat target "!"))
	       nick "@" server ":" (cond ((stringp port) port)
					 ((numberp port)
					  (number-to-string port))) ".txt")))
    ;; we need a make-safe-file-name function.
    (concat erc-log-channels-directory "/"
            (llm-erc-make-filename file))))

;;;###autoload
(defun erc-save-buffer-in-logs (&optional buffer)
  "Append BUFFER contents to the log file, if logging is enabled.
If BUFFER is not provided, current buffer is used.
Logging is enabled if `erc-logging-enabbled' returns non-nil.

This is normally done on exit, to save the unsaved portion of the
buffer, since only the text that runs off the buffer limit is logged
automatically.

You can save every individual message by putting this function on
`erc-insert-post-hook'."
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (when (erc-logging-enabled buffer)
    (let ((file (erc-current-logfile buffer)))
      (with-current-buffer buffer
	(save-restriction
	  (widen)
	  (setq buffer-file-name nil)
          ;; early on in the initalisation, don't try and write the log out
          (when (> erc-insert-marker (1+ erc-last-saved-position))
            (write-region (1+ (marker-position erc-last-saved-position))
                          (marker-position erc-insert-marker)
			    file t 'nomessage)
            (if (and erc-truncate-buffer-on-save (interactive-p))
                (progn
                  (erase-buffer)
                  (move-marker erc-last-saved-position (point-max))
                  (erc-display-prompt))
		(move-marker erc-last-saved-position
                             ;; If we place erc-last-saved-position
                             ;; at erc-insert-marker, because text
                             ;; gets inserted /before/
                             ;; erc-insert-marker, the log file will
                             ;; not be saved (erc-last-saved-position
                             ;; will always be equal to
                             ;; erc-insert-marker).
                             (1- (marker-position erc-insert-marker)))))
          (set-buffer-modified-p nil)
          ;; do we need to do this? was file, but that stops you from c-x
          ;; c-f-ing an old log when you have an active query with someone.
          ;; we don't use nil, because some of the other save hook
          ;; functions make assumptions as to the type of that var
          (setq buffer-file-name ""))))))

(provide 'erc-log)

;;; erc-log.el ends here
