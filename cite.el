;;;  cite.el --- Citing engine for Gnus -*- fill-column: 78 -*-
;; $Id: cite.el,v 1.14 2002/10/06 21:05:07 lawrence Exp $

;; This file is NOT part of Emacs.

;; Copyright (C) 2002 lawrence mitchell <wence@gmx.li>
;; Filename: cite.el
;; Version: $Revision: 1.14 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-06-15
;; Keywords: mail news

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; This is yet another citing engine for Emacs.  It's to trivial-cite
;; what trivial-cite is to supercite (i.e. stripped down).  I wrote it
;; because, well, I wanted to see if I could.
;; Doesn't do any fancy guessing of cite-prefixes, just tacks a ">" on
;; the front of the quoted article.
;; See the docstrings of `cite-cite' and `cite-parse-headers' for information
;; on extending cite.

;;; Installation:
;; To use this package, you have to make it the default citing function:
;; First make sure cite.el is somewhere on your `load-path', then add:
;; (autoload 'cite-cite "cite" "A simple cite function for Emacs" nil)
;; to your .emacs.
;;
;; If you are a Gnus user:
;; In your ~/.gnus add
;; (setq message-cite-function 'cite-cite)
;; to make message call `cite-cite' to cite articles.
;; Since `cite-cite' also generates an attribution, you probably also want to
;; do:
;; (setq news-reply-header-hook nil)
;; or at least make sure that `news-reply-header-hook' doesn't call a function
;; which creates an attribution line.
;;
;; If you are a VM user:
;; If you use Emacs' mail-mode to compose replies, add the following to your
;; ~/.vm
;; (setq mail-citation-hook 'cite-cite)

;;; History:
;;
;; $Log: cite.el,v $
;; Revision 1.14  2002/10/06 21:05:07  lawrence
;; Many changes added over summer.  See the file ChangeLog for details.
;;
;; Revision 1.13  2002/06/21 23:45:44  lawrence
;; New functions -- `cite-parse-date' header parsing function that
;;                   returns the date header in the form yyyy-mm-dd.
;;                   `cite-mail-or-news-attribution'.  Produce a
;;                   different attribution for mail or news.
;; Fixed minor error in `cite-parse-groups'.
;;
;; Revision 1.12  2002/06/20 23:06:37  lawrence
;; New function -- `cite-assoc'.  Used in `cite-simple-attribution'.
;; New variable -- `cite-remove-trailing-lines'.
;; Changed blank line regexp from "^[ \t\n]*$" to "^[ \t]*$".
;;
;; Revision 1.11  2002/06/19 17:10:38  lawrence
;; Changed ordering of code.
;; Removed require statements:
;; Changed
;; (eval-when-compile
;;   (require 'gnus-util)
;;   (require 'ietf-drums))
;; Into
;; (eval-and-compile
;;   (autoload 'gnus-extract-address-components "gnus-util")
;;   (autoload 'ietf-drums-unfold-fws "ietf-drums"))
;; As we only need the one function from each.  And by the time cite
;; actually gets used, these will probably already be loaded.
;;
;; Revision 1.10  2002/06/19 16:30:45  lawrence
;; New function -- `cite-find-sig'
;; This fixes the undo boundary problem in removing the signature.  We
;; first find the signature and save its position, and then, at the very
;; last, remove it.
;;
;; Revision 1.9  2002/06/17 21:08:22  lawrence
;; New function -- `cite-remove-trailing-lines'.
;; New variables -- `cite-remove-sig'
;;                  `cite-make-attribution'
;;
;; Revision 1.8  2002/06/17 16:52:46  lawrence
;; Removed redundant variable `cite-attribution-function'.
;; Added copyright notice.
;;
;; Revision 1.7  2002/06/17 00:11:24  lawrence
;; New functions -- `cite-parse-subject' and `cite-parse-groups'.
;; Improved commented documentation in places.
;;
;; Revision 1.6  2002/06/16 20:21:34  lawrence
;; Cleaned up header search regexp in `cite-parse-headers'.
;;
;; Revision 1.5  2002/06/16 19:40:01  lawrence
;; Minor change to `cite-parse-headers'.
;;
;; Revision 1.4  2002/06/16 19:06:52  lawrence
;; More minor changes.
;;
;; Revision 1.3  2002/06/16 19:05:26  lawrence
;; Minor cosmetic changes.
;;
;; Revision 1.2  2002/06/16 19:03:30  lawrence
;; New function -- `cite-version'.  Remove need for `replace-in-string'.
;;

;;; TODO:
;; Try and refill overly long lines? --- done? see `cite-rewrap-long-lines'.
;;
;; Add better installation instructions.
;;
;; Add a cite-before-run-hook and cite-after-run-hook?
;;
;; Remove need for Gnus specific functions.

;;; Code:

;;; Stuff we need
;; shut the byte-compiler up
(eval-and-compile
  ;; make sure these functions exist.
  (defalias 'cite-point-at-bol (if (fboundp 'point-at-bol)
                                   'point-at-bol
                                 'line-beginning-position))
  (defalias 'cite-point-at-eol (if (fboundp 'point-at-eol)
                                   'point-at-eol
                                 'line-end-position)))

;;; User variables.

(defvar cite-prefix ">"
  "*Prefix used for citing lines.

If a line is not already cited, a SPC is also added.
The default value, \">\", is the one recommended by Son-of-RFC1036, you may
provoke people by using a non-standard option here.")

(defvar cite-prefix-regexp "[>|:}+]"    ; These seem to be the most common.
  "Regexp matching a cite prefix.")

(defvar cite-re-regexp "Re\\|Aw"
  "*Regexp matching a \"Reply prefix\".")

(defvar cite-sig-sep-regexp "^-- ?$"
  "*Regular expression matching a sig-dash.

You definately want to anchor this to the beginning and end of the line, so
that you wouldn't match something elsewhere -- like that.")

(defvar cite-remove-sig t
  "*If non-nil `cite-cite' should remove the signature.

This is the recommended setting since it is generally considered bad form to
quote the signature.  Even if you have this set to t, you can easily reinsert
the sig, by calling `cite-reinsert-sig'.")

(defvar cite-remove-trailing-lines nil
  "*If non-nil, cite will remove trailing blank lines.

A line is considered to be blank if it matches \"^[ \\t]*$\".")

(defvar cite-rewrap-long-lines nil
  "*If non-nil, cite will attempt to rewrap long lines.

The function, `cite-rewrap-long-lines', which does the rewrapping, is a long
way from being perfect, so you may not want to call this from `cite-cite', but
rather interactively if you find a particularly bad paragraph that needs
reformatting.")

(defvar cite-make-attribution t
  "*If non-nil `cite-cite' will add an attribution line above the cited text.

See also `cite-make-attribution-function'.")

(defvar cite-make-attribution-function 'cite-simple-attribution
  "*Function to call to make an attribution line.

This is a function called with no arguments, it can access the values of
various headers parsed by `cite-parse-headers', and stored in
`cite-parsed-headers'.")

;;;; Version information.
(defconst cite-version
  "$Id: cite.el,v 1.14 2002/10/06 21:05:07 lawrence Exp $"
  "Cite's version number.")

(defconst cite-maintainer "Lawrence Mitchell <wence@gmx.li>"
  "Email address of Cite's maintainer.")

;;; Internal variables.

(defvar cite-removed-sig nil
  "The signature we have just removed.

Sometimes we might want to comment on the signature, by storing it in a
variable, it is easy to restore it.")

(defvar cite-removed-sig-pos nil
  "Where in the buffer the string contained in `cite-removed-sig' was.

The position is saved as two `point-markers' in a cons cell.")

(defvar cite-parsed-headers nil
  "Alist of parsed headers and their associated values.")

(defconst cite-rfc2822-address-regexp
  (concat "\\([!#-'*+/-9=?A-Z^-~-]+\\(\\.[!#-'*+/-9=?A-Z^-~-]+\\)*\\|"
          "\"\\([]\001-\010\013\014\016-\037!#-[^-\177]\\|"
          "\\\\[\001-\011\013\014\016-\177]\\)*\""
          "\\)@[!#-'*+/-9=?A-Z^-~-]+\\(\\.[!#-'*+/-9=?A-Z^-~-]+\\)*")
  "Regexp to match a RFC2822 compliant email address.

This does not match some valid addresses, notably:
obs-* patterns, domain-literal or internal whitespace.
Yes, this does also match Message-IDs, but that's because Message-IDs
have the same syntax, almost.
This was concocted by Paul Jarc <prj@po.cwru.edu> in gnu.emacs.gnus.")

;;; User functions.

;;;; Main entry point to cite.
(defun cite-cite ()
  "Cite: this function is the one called to cite an article.

Yet another citing engine for Gnus, even more minimalist than trivial cite.

If you add extra functions to the citing engine and call them from here, be
careful that you preserve the order, and, if you're going to change the
position of point, wrap them in a
\(save-excursion
   (save-restriction
     ...))."
  (save-excursion
    (save-restriction
      ;; narrow to the newly yanked region (i.e. the article we want to
      ;; quote).  Message puts (point) at the top of the region and sets a
      ;; "hidden" mark (i.e. the mark is not active) at the end, which we
      ;; access with (mark t).
      (narrow-to-region (point) (mark t))
      (cite-parse-headers)
      (cite-clean-up-cites (point-min) (point-max))
      ;; Find the signature.  We don't remove it yet, since we want the
      ;; removal of the signature to be first on the undo list.
      (cite-find-sig)
      ;; Remove trailing lines.
      (if cite-remove-trailing-lines
          (cite-remove-trailing-lines (point-min) (point-max)))
      (cite-remove-cite-if-line-empty (point-min) (point-max))
      (cite-cite-region (point-min) (point-max))
      (if cite-rewrap-long-lines
          (cite-rewrap-long-lines (point-min) (point-max)))
      ;; This might have to be played with if using format=flowed, but
      ;; Emacs can't handle composing it yet, so it's not a problem.
      (cite-remove-trailing-blanks)
      ;; insert the attribution.
      (if cite-make-attribution
          (insert (funcall cite-make-attribution-function)))
      ;; Remove the signature.
      (undo-boundary)
      (if cite-remove-sig
          (cite-remove-sig)))))

;;;; Version information.
(defun cite-version (&optional arg)
  "Echo cite's version in the minibuffer.

If optional ARG is non-nil, insert at point."
  (interactive "P")
  (if arg
      (insert "\n" cite-version "\n")
    (message "%s" cite-version)))

;;;; Attribution creation.
(defun cite-simple-attribution ()
  "Produce a very small attribution string.

Substitute \"An unnamed person wrote:\\n\\n\" if no email/name is available."
  (let ((email (cite-get-header "email"))
	(name  (cite-get-header "name")))
    (if (and (null name) (null email))
	"An unnamed person wrote:\n\n"
      (concat (or name email) " wrote:\n\n"))))

(defun cite-mail-or-news-attribution ()
  "Produce a different attribution for mail and news.

The test for whether this is a news article is done using the function
`cite-news-p'."
  (let* ((email  (cite-get-header "email"))
         (name   (cite-get-header "name"))
         (date   (cite-get-header "date"))
         (news   (cite-news-p))
         (attrib (if (and (null name) (null email))
                     "an unnamed person wrote:\n\n"
                   (concat (or name email) " wrote:\n\n"))))
    (if news
        attrib
      (concat "On " date ", " attrib))))

;;; Internal functions.

;;;; Header parsing.
;; basically stolen from tc.el but modified slightly and cleaned up.
(defun cite-parse-headers ()
  "Parse the headers of the current article for useful information.

Since we narrow to the headers, we also delete them, as we don't want them
included in the followup.

See the `cite-parse-...' functions for examples of how to extract information
from headers.  The functions you write should take one argument, the header
contents, mess about with it as you wish, and then add the manipulated data to
the variable `cite-parsed-headers'."
  ;; make sure we're starting with a fresh set of headers.
  (setq cite-parsed-headers nil)
  (save-excursion
    (save-restriction
      (let ((point (point)))
        (search-forward "\n\n" nil t)   ; find the end of the header section
        (narrow-to-region point (point))
        (goto-char (point-min))
        (cite-unfold-fws)               ; unfold headers
        (while (not (eobp))
          ;; Header fields take the form:
          ;; TITLE: CONTENTS
          ;; We strip out TITLE and CONTENTS into two variables, and then pass
          ;; them off to different functions to parse them.
          (if (looking-at "^\\([^:]+\\):[ \t]*\\([^ \t]?.*\\)$")
              ;; We use (downcase ...) in case the header field has
              ;; "non-standard" capitalisation.
              (let ((name      (downcase (buffer-substring-no-properties
                                          (match-beginning 1) (match-end 1))))
                    (contents  (buffer-substring-no-properties
                                (match-beginning 2) (match-end 2))))
                ;; Add match conditions here if you want to parse extra
                ;; headers, `name' will be all lower-case.  The functions you
                ;; write to extract information should take one argument, the
                ;; contents of the header field.
                (cond ((string= name "from")
                       (cite-parse-from contents))
                      ((string= name "newsgroups")
                       (cite-parse-groups contents))
                      ((string= name "subject")
                       (cite-parse-subject contents))
                      ((string= name "date")
                       (cite-parse-date contents))
                      ((string= name "message-id")
                       (cite-parse-mid contents)))))
          (forward-line 1))
        ;; Delete the current (narrowed) buffer.  This removes headers
        ;; from the followup.
        (delete-region (point-min) (point-max))))))

(defun cite-parse-from (string)
  "Extract the real name and/or email address from STRING."
  (setq string (cite-extract-address-components string))
  (let ((name (car string))
        (addr (cadr string)))
    ;; deal with b0rked TMDA addresses
    ;; TMDA addresses have one of the following forms (where the "-" might be
    ;; replaced by "+" if the MTA is sendmail):
    ;; `dated':
    ;;   foo-dated-989108708.a13f22@bar.com
    ;; `sender':
    ;;   foo-sender-e78f34d@bar.com
    ;; `keyword':
    ;;   foo-keyword-baz.78fk90@bar.com
    (and  (string-match (concat
                         "[-+]\\(:?dated\\|keyword\\|sender\\)[-+]"
                         "[0-9a-zA-Z]+\\(:?\\.[0-9a-zA-Z]+\\)?@")
                        addr)
          (setq addr (replace-match "@" nil t addr)))
    (cond ((string= addr "graham@affordable-leather.co.ukDELETETHIS")
           (setq name "Graham"))
          ((and name (string-match "Kai Gro.johann" name))
           (setq name "Kai Grossjohann")))
    (cite-add-parsed-header "name" name)
    (cite-add-parsed-header "email" addr)))

(defun cite-parse-mid (string)
  "Extract the message-id from STRING.

Return it in the form <news:message-id>."
  (and (string-match "^<" string)
       (setq string (replace-match "<news:" nil nil string)))
  (cite-add-parsed-header "mid" string))

(defun cite-parse-date (string)
  "Extract the date in YYYY-MM-DD form from STRING."
  (cite-add-parsed-header "date" (cite-format-date string)))

(defun cite-parse-subject (string)
  "Extract the subject from STRING.

Remove \"Re:\" strings first if they occur at the beginning."
  (let ((case-fold-search t))
    (if (string-match (concat "^\\(?:" cite-re-regexp "\\):[ \t]*") string)
        (setq string (replace-match "" nil nil string)))
    (cite-add-parsed-header "subject" string)))

(defun cite-parse-groups (string)
  "Extract the newsgroups from STRING."
  (and (string-match ",\\([^ \t]\\)" string) ; ensure space between group names
       (setq string (replace-match ", \\1" nil nil string)))
  (cite-add-parsed-header "newsgroups" string))

;;;; Article cleanup.
(defun cite-clean-up-cites (start end)
  "Make cite marks in region between START and END uniform.

e.g.
Before:  \"}|> : foo.\"
After:   \">>>> foo.\""
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      ;; cache regexp, this saves calling `concat' for every line.
      (let ((regexp (concat " \\{0,2\\}" cite-prefix-regexp)))
        (while (not (eobp))
          (cond (;; Eat spaces (up to a maximum of two) if they are followed by a cite
                 ;; mark.  Replace the cite mark matched with `cite-prefix'.
                 (looking-at regexp)
                 (delete-region (match-beginning 0) (match-end 0))
                 (insert cite-prefix))
                (;; We've now normalised all the cite marks, if we're looking
                 ;; at a non-space, followed by another non-space (ie, not and
                 ;; end-of-line), insert a space.
                 (and (not (or (= (preceding-char) ?\ )
                               (looking-at "^")
                               (looking-at "$")))
                      (looking-at "[^ \t]"))
                 (insert " "))
                (t
                 (forward-line 1))))))))

(defun cite-remove-trailing-blanks ()
  "Remove whitespace from the end of lines."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (skip-chars-backward " \t")
      (delete-region (point) (cite-point-at-eol))
      (forward-line 1))))

(defun cite-remove-trailing-lines (start end)
  "Remove trailing lines from the region between START and END.

A trailing line is one that matches \"^[ \\t]+$\", and is followed in the
buffer only by further blank lines."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-max))
      (let ((finished nil))
        (while (not finished)
          (if (looking-at "^[ \t]*$")
            (forward-line -1)
            (setq finished t)))
        (forward-line 1)
        (delete-region (point) (point-max))))))

(defun cite-line-empty-p ()
  "Return t if a line is \"empty\".

An empty line is one matching:
\(concat \"^\" cite-prefix \"+[ \\t]*$\")."
  (looking-at (concat "^" cite-prefix "+[ \t]*$")))

(defun cite-remove-cite-if-line-empty (start end)
  "Remove cite marks from a line in the region between START and END.

A cite mark is only removed if the current line is an empty.
\"Emptiness\" is checked for via the function `cite-line-empty-p', which see."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
        (if (cite-line-empty-p)
            (delete-region (cite-point-at-bol) (cite-point-at-eol)))
        (forward-line 1)))))

(defun cite-rewrap-long-lines (start end)
  "Try to reformat long lines between START and END.

This only really works with text, as code is not rewrapped well, we try to
preserve whitespace (other than line breaks) but this is far from perfect."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((cite-depth (cite-count-cite-marks))
              (point      (point))
              paragraph-cite-prefix)
          (if (= cite-depth 0)
              (forward-line 1)
            (while (= cite-depth (cite-count-cite-marks))
              (forward-line 1))
            (setq paragraph-cite-prefix
                  (mapconcat #'identity (make-vector cite-depth cite-prefix) ""))
            (let ((fill-prefix (concat paragraph-cite-prefix " ")))
              (fill-region-as-paragraph point (point) nil t))))))))

;;;; Signature removal.
(defun cite-reinsert-sig ()
  "Reinsert the signature removed by function `cite-remove-sig'.

It will already be quoted, since we remove the signature after quoting the
article."
  (interactive)
  (if cite-removed-sig
      (let ((start (marker-position (car cite-removed-sig-pos))))
        (save-excursion
          (goto-char start)
          (insert cite-removed-sig)))))

(defun cite-find-sig ()
  "Find the signature and save its postion as two markers.

The signature is defined as everything from the first occurance of
`cite-sig-sep-regexp' until the end of the buffer."
  (save-excursion
    (setq cite-removed-sig-pos nil)
    (goto-char (point-min))
    (if (re-search-forward cite-sig-sep-regexp nil t)
        (let ((start (and (forward-line 0) (point-marker)))
              (end (and (goto-char (point-max)) (point-marker))))
          (setq cite-removed-sig-pos (cons start end))))))

(defun cite-remove-sig ()
  "Remove a signature.

This removes everything from the first occurance of `cite-sig-sep-regexp' to
the end of the buffer.  This function doesn't actually search for the
signature, we have already done that with `cite-find-sig'."
  (save-excursion
    (setq cite-removed-sig nil)
    (if cite-removed-sig-pos
        (let ((start (marker-position (car cite-removed-sig-pos)))
              (end   (marker-position (cdr cite-removed-sig-pos))))
          (setq cite-removed-sig
                (buffer-substring-no-properties start end))
          (delete-region start end)))))

;;;; Article citing.
(defun cite-cite-region (start end)
  "Prefix the region between START and END with `cite-prefix'.

A \" \" is added if the current line is not already cited."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at cite-prefix-regexp)
            (if (string-match "[^ \t\n]" (buffer-substring-no-properties
                                          (cite-point-at-bol)
                                          (cite-point-at-eol)))
                (insert cite-prefix))
          (if (string-match "[^ \t\n]" (buffer-substring-no-properties
                                        (cite-point-at-bol)
                                        (cite-point-at-eol)))
              (insert cite-prefix " ")))
        (forward-line 1)))))

(defun cite-uncite-region (start end &optional arg)
  "Remove cites from the region between START and END.

With optional numeric prefix ARG, remove that many cite marks."
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((arg (or arg 1))
              (regexp (concat "[ \t]*" cite-prefix-regexp "[ \t]?"))
              (i 0))
          (while (< i arg)
            (and (looking-at regexp)
                 (delete-region (match-beginning 0) (match-end 0)))))
        (forward-line 1)))))

;;;; Support functions.
;; some of these functions are replicated in gnus, but we don't want to force
;; non-gnus users to load gnus files if not needed.
(defun cite-unfold-fws ()
  "Unfold folding whitespace in the current buffer.

From ietf-drums.el"
  (save-excursion
    (while (re-search-forward "[ 	]*\n[ 	]+" nil t)
      (replace-match " " t t))))

(defun cite-format-date (date)
  "Format DATE as a YYYY-MM-DD string.

Adapted from gnus-util.el"
  (format-time-string "%Y-%m-%d"
                      (condition-case nil
                          (apply 'encode-time
                                 (parse-time-string
                                  (timezone-make-date-arpa-standard date)))
                        (error '(0 0)))))


(defun cite-extract-address-components (string)
  "Extract address components of an email in STRING.

Adapted slightly from gnus-util.el"
  (let (name address)
    (when (string-match cite-rfc2822-address-regexp string)
      (setq address (substring string (match-beginning 0) (match-end 0))))
    (and address
         (string-match (concat "[ \t]*<" (regexp-quote address) ">") string)
         (and (setq name (substring string 0 (match-beginning 0)))
              (string-match "^\"\\(.*\\)\"" name)
              (setq name (replace-match "\\1" nil nil name))))
    (or name
        (and (string-match "(.+)" string)
             (setq name (substring string (1+ (match-beginning 0))
                                   (1- (match-end 0)))))
        (and (string-match "()" string)
             (setq name address)))
    (list (if (string= "" name) nil name) (or address string))))

(defun cite-news-p ()
  "Say whether the current buffer contains a news message.

From message.el"
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (narrow-to-region (point-min)
                        (save-excursion
                          (or (re-search-forward (concat
                                                  "^"
                                                  (regexp-quote mail-header-separator)
                                                  "$")
                               nil t)
                              (goto-char (point-max)))))
      (let ((case-fold-search t))
        (and (save-excursion (re-search-forward "^newsgroups:" nil t))
             (not (re-search-forward "^posted-to:" nil t)))))))

(defun cite-get-header (key)
  "Return the value of the fieldname KEY from `cite-parsed-headers'.

The test is done with `assoc'.
This is equivalent to:
\(cadr (assoc key cite-parsed-headers))."
  (cadr (assoc key cite-parsed-headers)))

(defun cite-add-parsed-header (field value)
  "Add a list cell of (FIELD VALUE) to `cite-parsed-headers'.

FIELD should be a unique identifier string (e.g. \"mid\" for message-id).
Note, we don't do error checking to see if the identifier string already
exists, so it's up to you to make sure it doesn't.

This just does:
\(add-to-list 'cite-parsed-headers `(,field ,value))"
  (add-to-list 'cite-parsed-headers `(,field ,value)))

(defun cite-count-cite-marks ()
  "Count the number of cite marks at the beginning of a line.

Call this after calling `cite-clean-up-cites', as we search for `cite-prefix',
rather than `cite-prefix-regexp'."
  (save-excursion
    (let ((count 0))
      (forward-line 0)
      (while (looking-at cite-prefix)
        (setq count (1+ count))
        (forward-char 1))
      count)))

(provide 'cite)

;;; cite.el ends here
