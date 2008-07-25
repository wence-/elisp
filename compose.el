;;; compose.el --- Minimal composition mode for mutt mail messages.

;; This file is NOT part of Emacs.

;; Copyright (C) 2006, 2007 Lawrence Mitchell <wence@gmx.li>
;; Filename: compose.el
;; Version: 1
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2006-11-09
;; Keywords: mail

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:

;; Installation:

;;; History:

;;; TODO:

;;; Code:
(eval-when-compile
  (require 'cl))

(defvar compose-cite-function 'cite-cite
  "*Function for citing an original message.")

(defconst compose-file-is-mail-message-regexp
  (rx "mutt-"
      (one-or-more (any (?a . ?z) (?0 . ?9)))
      "-"
      (zero-or-more
       (and (one-or-more (any (?0 . ?9)))
            "-"
            (one-or-more (any (?0 . ?9)))))
      string-end)
  "Regexp matching filename to indicate that this is a mail message.")

(defvar compose-cite-prefix-regexp
  (rx (or
       (one-or-more
        (and (zero-or-more (syntax whitespace))
             (any "}>|:+]")))
       (repeat 3 5 ?\s)))
  "*Regexp matching the longest possible citation prefix on a line.")

(defvar compose-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-a") 'compose-goto-beginning-of-header-field-or-line)
    (define-key map (kbd "C-c C-c") 'compose-send-and-exit)
    (define-key map (kbd "C-c C-k") 'compose-exit-dont-send)
    (define-key map (kbd "C-c C-a") 'compose-attach-file)
    (define-key map (kbd "C-c C-e") 'compose-elide-region)
    (define-key map (kbd "C-c C-t") 'compose-cite-region)
    (define-key map (kbd "C-c C-i") 'compose-uncite-region)
    (define-key map (kbd "C-c C-q") 'cite-quote-region)
    (define-key map (kbd "C-c C-o") 'cite-unquote-region)
    (define-key map (kbd "M-RET") 'compose-break-paragraph)
    map)
  "Keybindings for `compose-mode'.")


(defun compose-cite-region (start end &optional arg)
  "Cite the region between START and END.

If optional prefix ARG is non-nil, insert that many citation marks."
  (interactive "r\np")
  (compose-cite-or-uncite-region start end arg t))

(defun compose-uncite-region (start end &optional arg)
  "Uncite the region between START and END.

If optional prefix ARG is non-nil, remove that many citation marks."
  (interactive "r\np")
  (compose-cite-or-uncite-region start end arg nil))

(defun compose-cite-or-uncite-region (start end &optional arg citep)
  "Cite or uncite the region between START and END.

If CITEP is non-nil, cite the region, otherwise uncite it.
If ARG is non-nil, insert or remove that many cite marks."
  (if citep
      (cite-cite-region start end arg nil)
    (cite-uncite-region start end arg nil)))

(define-derived-mode compose-mode text-mode "Compose"
  "Mail composition mode for Mutt mail messages."
  (unless current-prefix-arg
    (compose-clean-message))
  (compose-setup-fill-variables)
  (setq fill-paragraph-function 'fill-paragraph)
  (set (make-local-variable 'font-lock-defaults)
       '(compose-font-lock-keywords t))
  (when (boundp 'cite-prefix)
    (set (make-local-variable 'comment-start) (concat cite-prefix " "))
    (set (make-local-variable 'comment-start-skip)
	 (concat "^" (regexp-quote cite-prefix) " [ \t]*")))
  (compose-goto-end-of-headers)
  (forward-line 1))


(defun compose-font-lock-make-header-matcher (regexp)
  "Make a font-lock pattern that matches REGEXP.

The pattern is limited to match only in the message headers."
  (let ((form
	 `(lambda (limit)
	    (let ((start (point)))
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(if (re-search-forward
		     (concat "^" (regexp-quote mail-header-separator) "$")
		     nil t)
		    (setq limit (min limit (match-beginning 0))))
		(goto-char start))
	      (and (< start limit)
		   (re-search-forward ,regexp limit t))))))
    (if (featurep 'bytecomp)
	(byte-compile form)
      form)))

(defvar compose-font-lock-keywords
  (let ((content "[ \t]*\\(.+\\(\n[ \t].*\\)*\\)\n?"))
    `((,(compose-font-lock-make-header-matcher
	 (concat "^\\([^:]+:\\)" content))
       (1 'compose-header-name)
       (2 'compose-header-contents nil t))
      ,@(if (and mail-header-separator
		 (not (equal mail-header-separator "")))
	    `((,(concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
	       1 'compose-separator))
	  nil)
      ((lambda (limit)
	 (re-search-forward (concat "^\\("
				    compose-cite-prefix-regexp
				    "\\).*")
			    limit t))
       (0 'compose-cited-text))
      ("^-- \n\\(\n\\|.\\)*\\'"
       . 'compose-signature)
      ))
  "Expressions to highlight in compose mode.")

(defface compose-separator
  '((((class color)
      (background dark))
     (:foreground "blue3"))
    (((class color)
      (background light))
     (:foreground "brown"))
    (t
     (:bold t)))
  "Face used for displaying the separator.")

(defface compose-header-name
  '((((class color)
      (background dark))
     (:foreground "DarkGreen"))
    (((class color)
      (background light))
     (:foreground "cornflower blue"))
    (t
     (:bold t)))
  "Face used for displaying header names.")

(defface compose-header-contents
  '((((class color)
      (background dark))
     (:foreground "#b00000"))
    (((class color)
      (background light))
     (:foreground "steel blue"))
    (t
     (:bold t :italic t)))
  "Face used for displaying header contents.")

(defface compose-signature
  '((((class color)
      (background dar))
     (:foreground "#c00000"))
    (((class color)
      (background light))
     (:foreground "slate blue"))
    (t
     (:italic t)))
  "Face used for display the signature.")

(defface compose-cited-text
  '((((class color)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background light))
     (:foreground "red"))
    (t
     (:bold t)))
  "Face used for displaying cited text.")


(defun compose-clean-message ()
  "Cleanup the raw message as it arrives from Mutt and cite it properly."
  (goto-char (point-min))
  (search-forward "\n\n")
  (let ((replyp (looking-at "^--start--$")))
    (save-excursion
      (forward-line 7)
      (when (not (looking-at "\n\n"))
        (insert "\n\n")))
    (unless (or (eobp) (not replyp))
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    (forward-line -1)
    (insert mail-header-separator "\n")
    (forward-line 1)
    (push-mark (point-max) t nil)
    (let ((quoted-p  (save-excursion
                       (forward-line 7)
                       (skip-chars-forward "\n")
                       (looking-at ">"))))
      (macrolet ((flet-simple (bindings &rest body)
                   "Establish a local binding for NAME with ARGLIST and execute BODY.

Works like `flet' but the original definition of NAME is available as
a variable to be funcalled under OLDNAME.
\(fn ((NAME OLDNAME (ARGLIST...) &rest FN-BODY)) &rest BODY)"
                   (let ((name (first (first bindings)))
                         (old-name (second (first bindings)))
                         (arglist (third (first bindings)))
                         (fn-body (nthcdr 3 (first bindings)))
                         (fn-exists (make-symbol "fn-exists")))
                     `(let ((,old-name (symbol-function ',name))
                            (,fn-exists (fboundp ',name)))
                        (unwind-protect
                            (progn
                              (fset ',name (lambda ,arglist
                                             ,@fn-body))
                              ,@body)
                          (if ,fn-exists
                              (fset ',name ,old-name)
                            (fmakunbound ',name)))))))
        (flet-simple ((cite-cite-region %orig (start end &optional arg prefix)
                         (funcall %orig start end arg (if quoted-p "" nil))))
         (when replyp
           (funcall compose-cite-function)))))
    (compose-maybe-insert-sig (funcall compose-make-sig-function))))

(defvar compose-make-sig-function (lambda () (llm-make-sig 'mail))
  "*Make a signature.")

(defun compose-goto-beginning-of-header-field-or-line (&optional arg)
  "Move to the beginning of the header field, or the beginning of the
line.

If ARG is non-nil, definitely go to the beginning of the line."
  (interactive "P")
  (if (or arg (not (compose-point-in-header-p)))
      (move-beginning-of-line nil)
    (let ((p (point)))
      (forward-line 0)
      (search-forward ": " nil t)
      (if (= p (point))
          (move-beginning-of-line nil)))))

(defun compose-goto-end-of-headers ()
  "Go to the beginning of the message body."
  (goto-char (point-min))
  (re-search-forward (format "^%s$" mail-header-separator) nil t)
  (forward-line 0))

(defun compose-attach-file (file description)
  "Insert an \"Attach: FILE DESCRIPTION\" line to be picked up by Mutt."
  (interactive "fAttach file: \nsDescription: ")
  (if (string= file "")
      (message "No file specified")
    (save-excursion
      (save-restriction
        (compose-goto-end-of-headers)
        (insert (format "Attach: %s %s\n" (file-truename file) description))))))

(defun compose-maybe-insert-sig (signature)
  "Maybe insert SIGNATURE.

If SIGNATURE already exists in the buffer, don't insert it again."
  (save-excursion
    (save-restriction
      (unless (re-search-forward (format "^-- \n%s" signature) nil t)
        (goto-char (point-max))
        (insert (format "\n\n-- \n%s" signature))))))

(defun compose-attachment-mentioned-p ()
  "Return non-nil if the body of the message seems to mention an attachment.

Looks for the word \"attach\" and returns the sentence it appears in,
or nil if it doesn't appear."
  (save-excursion
    (compose-goto-end-of-headers)
    (when (search-forward "attach" nil t)
      (buffer-substring-no-properties
       (progn (backward-sentence) (point))
       (progn (forward-sentence) (point))))))

(defun compose-attachment-header-exists-p ()
  "Return non-nil if a header specifying an attachment exists."
  (save-excursion
    (compose-goto-end-of-headers)
    (re-search-backward "^Attach: " nil t)))

(defun compose-send-and-exit ()
  "Save the buffer and mark it as no longer in use using `server-edit'."
  (interactive)
  (let* ((attachment-mentioned (compose-attachment-mentioned-p))
         (attachment-header-exists-p (compose-attachment-header-exists-p))
         (continue-p (if (and (buffer-modified-p)
                              attachment-mentioned
                              (not attachment-header-exists-p))
                         (yes-or-no-p
                          (format
                           "An attachment is referred to in the email%s%s%s%s"
                           "\n\n\"" attachment-mentioned
                           "\"\n\nBut no file is attached.  "
                           "Continue anyway? "))
                       t))
         (buf (current-buffer)))
    (when continue-p
      (when (buffer-modified-p)
        (compose-goto-end-of-headers)
        (delete-region (point) (line-end-position))
        (basic-save-buffer))
      (server-edit)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (set-buffer-modified-p nil)
          (kill-buffer buf))))))

(defun compose-exit-dont-send ()
  "Mark the message as unmodified and exit, i.e. don't send."
  (interactive)
  (set-buffer-modified-p nil)
  (compose-send-and-exit))


(defun compose-break-paragraph (&optional arg dummy)
  (interactive)
  (catch 'done
    (when (bolp)
      (insert "\n\n\n\n")
      (forward-line -2)
      (throw 'done t))
    (let ((pfx ""))
      (save-excursion
        (forward-line 0)
        (when (looking-at compose-cite-prefix-regexp)
          (setq pfx (match-string 0))))
      (insert "\n\n\n\n")
      (forward-line 0)
      (insert pfx)
      (unless (or (string= pfx "")
                  (string= pfx "    ")
                  (looking-at "\\s-"))
        (insert " "))
      (fill-paragraph arg)
      (forward-line -2))))

(defun compose-setup-fill-variables ()
  "Setup message fill variables."
  (set (make-local-variable 'fill-paragraph-function)
       'fill-paragraph)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'adaptive-fill-regexp)
  (unless (boundp 'adaptive-fill-first-line-regexp)
    (setq adaptive-fill-first-line-regexp nil))
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (let ((quote-prefix-regexp
	 (concat "\\(" compose-cite-prefix-regexp "\\)[ \t]*")))
    (setq paragraph-start
	  (concat
	   (regexp-quote mail-header-separator) "$\\|"
	   "[ \t]*$\\|"			; blank lines
	   "-- $\\|"			; signature delimiter
	   "---+$\\|"              ; delimiters for forwarded messages
	   page-delimiter "$\\|"	; spoiler warnings
	   ".*wrote:$\\|"		; attribution lines
	   quote-prefix-regexp "$"	; empty lines in quoted text
	   ))
    (setq paragraph-separate paragraph-start)
    (setq adaptive-fill-regexp
	  (concat quote-prefix-regexp "\\|" adaptive-fill-regexp))
    (setq adaptive-fill-first-line-regexp
	  (concat quote-prefix-regexp "\\|"
		  adaptive-fill-first-line-regexp)))
  (make-local-variable 'auto-fill-inhibit-regexp)
  (setq auto-fill-inhibit-regexp nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'compose-do-auto-fill)
  (when auto-fill-function
    (setq auto-fill-function normal-auto-fill-function)))

(defun compose-do-auto-fill ()
  "Like `do-auto-fill', but don't fill in message header."
  (unless (compose-point-in-header-p)
    (do-auto-fill)))

(defun compose-point-in-header-p ()
  "Return non-nil if point is in the header."
  (save-excursion
    (let ((p (point)))
      (compose-goto-end-of-headers)
      (< p (point)))))

(defun compose-elide-region (start end)
  "Remove the region between START and END replacing it with an elision marker."
  (interactive "r")
  (kill-region start end)
  (insert "\n[...]\n\n"))

(add-to-list 'auto-mode-alist
             `(,compose-file-is-mail-message-regexp . compose-mode))

(provide 'compose)

;;; compose.el ends here
