;;; cite.el --- Citing engine for Gnus -*- fill-column: 78 -*-
;; $Id: cite.el,v 1.9 2002/06/17 21:08:22 lawrence Exp $

;; Copyright (C) 2002 lawrence mitchell <wence@gmx.li>

;; Filename: cite.el
;; Version: $Revision: 1.9 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-06-15
;; Keywords: citing mail news

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
;; This is yet another citing engine for Gnus.  It's to trivial-cite
;; what trivial-cite is to supercite (i.e. stripped down).  I wrote it
;; because, well, I wanted to see if I could.
;; Doesn't do any fancy guessing of cite-prefixes, just tacks a ">" on
;; the front of the quoted article.
;; See the docstrings of `cite-cite' and `cite-parse-headers' for information
;; on extending cite.

;;; Installation:
;; To use this package, you have to make it the default citing function:
;; First make sure cite.el is somewhere on your `load-path', then add:
;; (autoload 'cite-cite "cite" "A simple cite function for Gnus" nil)
;; to your .emacs.
;;
;; In your .gnus add
;; (setq message-cite-function 'cite-cite)
;; to make message call `cite-cite' to cite articles.
;; Since `cite-cite' also generates an attribution, you probably also want to
;; do:
;; (setq news-reply-header-hook nil)
;; or at least make sure that `news-reply-header-hook' doesn't call a function
;; which creates an attribution line.

;;; History:
;;
;; $Log: cite.el,v $
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
;; Try and refill overly long lines?
;; Maybe remove empty lines from end of article?
;; Fix the undo boundary for the reinsertion of a removed .sig.

;;; Code:

(eval-when-compile
  (require 'gnus-util)
  (require 'ietf-drums))

;;; User variables

(defvar cite-prefix ">"
  "*Prefix used for citing paragraphs.")

(defvar cite-quote-prefix "| "
  "*Prefix used for \"quoting\" paragraphs.")

(defvar cite-prefix-regexp "[>|:}+]"
  "Regexp matching a cite prefix.")

(defvar cite-sig-sep-regexp "^-- ?$"
  "*Regular expression matching a sig-dash.")

(defvar cite-remove-sig t
  "*If non-nil `cite-cite' should remove the signature.

This is the recommended setting since it is generally considered bad form to
quote the signature.  Even if you have this set to t, you can easily reinsert
the sig, by calling `cite-reinsert-sig'.")

(defvar cite-make-attribution t
  "*If non-nil `cite-cite' will add an attribution line above the cited text.

See also `cite-make-attribution-function'.")
  
(defvar cite-make-attribution-function 'cite-simple-attribution
  "*Function to call to make an attribution line.

This is a function called with no arguments, it can access the values of
various headers parsed by `cite-parse-headers', and stored in
`cite-parsed-headers'.")


;;; Internal variables

(defvar cite-removed-sig nil
  "The signature we have just removed.

Sometimes we might want to comment on the signature, by storing it in a
variable, it is easy to restore it.")

(defvar cite-removed-sig-pos nil
  "Where in the buffer the string contained in `cite-removed-sig' was.")

(defvar cite-parsed-headers nil
  "Alist of parsed headers and their associated values.")

(defconst cite-version
  "$Id: cite.el,v 1.9 2002/06/17 21:08:22 lawrence Exp $"
  "Cite's version number.")

;;; Internal functions

(defun cite-remove-trailing-blanks ()
  "Remove whitespace from the end of lines."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (re-search-forward "[ \t]+$" (line-end-position) t)
          (replace-match ""))
      (forward-line 1))))

(defun cite-remove-trailing-lines (beg end)
  "Remove trailing lines from the region between BEG and END.

A trailing line is one that matches \"^[ \\t\\n]$\".  Mulitple trailing
lines are replaced with just one."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-max))
      (let ((final nil))
      (while (not final)
        (if (looking-at "^[ \t\n]*$")
            (forward-line -1)
          (setq final t)))
      (forward-line 1)
      (delete-region (point) (point-max))
      (insert "\n")))))

;; basically stolen from tc.el but modified and (I think) cleaned up.
(defun cite-parse-headers ()
  "Parse the headers of the current article for useful information.

Since we narrow to the headers, we also delete them, as we don't want them
included in the followup."
  ;; make sure we're starting with a fresh set of headers.
  (setq cite-parsed-headers nil)
  (save-excursion
    (save-restriction
      (let ((point (point)))
        (re-search-forward "\n\n" nil t) ; find the end of the header section
        (narrow-to-region point (point))
        (ietf-drums-unfold-fws)          ; unfold headers
        (goto-char (point-min))
        (while (not (eobp))
          ;; Header fields take the form:
          ;; TITLE: CONTENTS
          ;; We strip out TITLE and CONTENTS into two variables, and then pass
          ;; them off to different functions to parse them.
          (if (looking-at "^\\([^:]+\\):[ \t]*\\([^ \t]?.*\\)")
              (let ((name (buffer-substring-no-properties
                           (match-beginning 1) (match-end 1)))
                    (contents (buffer-substring-no-properties
                               (match-beginning 2) (match-end 2))))
                ;; Add match conditions here if you want to parse
                ;; extra headers.  The functions you write to extract
                ;; information should take one argument, the contents
                ;; of the header field.
                (cond ((string= name "From")
                       (cite-parse-from contents))
                      ;; ((string= name "Newsgroups")
                      ;;  (cite-parse-groups contents))
                      ;; ((string= name "Subject")
                      ;;  (cite-parse-subject contents))
                      ((string-match "Message-id" name) ; no standard
                                                        ; capitalisation
                       (cite-parse-mid contents)))))
          (forward-line 1))
        ;; Delete the current (narrowed) buffer.  This removes headers
        ;; from the followup.
        (delete-region (point-min) (point-max))))))

;; Here are some examples of functions used to extract information from
;; headers.  The functions you write should take one argument, the header
;; contents, mess about with it as you wish, and then add the manipulated data
;; to the variable `cite-parsed-headers'.
(defun cite-parse-from (string)
  "Extract the real name or email address from STRING.

Uses the function `gnus-extract-address-components' to do the hard work."
  (setq string (gnus-extract-address-components string))
  (let ((name (car string))
        (addr (cadr string)))
    (add-to-list 'cite-parsed-headers `("real-name" ,name))
    (add-to-list 'cite-parsed-headers `("email-addr" ,addr))))

(defun cite-parse-mid (string)
  "Extract the message-id from STRING.

Return it in the form <news:message-id>."
  (and (string-match "^<" string)
       (setq string (replace-match "<news:" nil nil string)))
  (add-to-list 'cite-parsed-headers `("mid" ,string)))

(defun cite-parse-subject (string)
  "Extract the subject from STRING."
  (add-to-list 'cite-parsed-headers `("subject" ,string)))

(defun cite-parse-groups (string)
  "Extract the newsgroups from STRING."
  (and (string-match ",\\([^ \t]\\)" string)
       (setq string (replace-match ", \\1" nil nil string)))
  (add-to-list 'cite-parsed-headers `("mid" ,string)))


;;; Pseudo-User functions

(defun cite-clean-up-cites (beg end)
  "Make cite marks in region between BEG and END uniform.

e.g.
Before: }|> : foo
After: >>>> foo."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        ;; Eat spaces (up to a maximum of two) if they are followed by a cite
        ;; mark.
        (if (looking-at (concat " \\{1,2\\}"cite-prefix-regexp))
            (delete-region (match-beginning 0) (1- (match-end 0)))
          ;; Normalise cite marks, replacing anything matched by
          ;; `cite-prefix-regexp' with `cite-prefix'.
          (if (looking-at cite-prefix-regexp)
              (replace-match cite-prefix)
            ;; Not at a cite mark.  If we're either at the beginning of a line
            ;; or the previous character is a space, do nothing.
            (if (or (eq (preceding-char) ?\ )
                    (bolp))
                nil
              ;; We've now got to the end of the cite marks.  If the current
              ;; character is a non-space, insert a space, else do nothing.
              (if (looking-at "[^ \t]")
                  (insert " ")))
            (forward-line 1)))))))


(defun cite-remove-sig ()
  "Remove a .sig.

This removes everything from the first occurance of `cite-sig-sep-regexp' to
the end of the buffer."
  (save-excursion
    (save-restriction
      (setq cite-removed-sig nil
            cite-removed-sig-pos nil)
      (goto-char (point-min))
      (if (re-search-forward cite-sig-sep-regexp nil t)
          (progn
            ;; delete the sig-sep
            (delete-region (line-beginning-position) (line-beginning-position 2))
            (setq cite-removed-sig-pos (point-marker)
                  cite-removed-sig (buffer-substring-no-properties
                                    (point) (point-max)))
            (delete-region (point) (point-max)))))))

(defun cite-reinsert-sig ()
  "Reinsert the .sig removed by function `cite-remove-sig'.

Prefix it with `cite-prefix'."
  (if cite-removed-sig
    (let ((mark (marker-position cite-removed-sig-pos)))
      (save-excursion
        (save-restriction
          (goto-char mark)
          (insert cite-removed-sig)
          (narrow-to-region mark (point))
          (goto-char (point-min))
          (while (not (eobp))
            (insert cite-prefix " ")
            (forward-line 1)))))))

(defun cite-cite-region (beg end)
  "Prefix the region between BEG and END with `cite-prefix'.

A \" \" is added if the current line is not already cited."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at cite-prefix-regexp)
            (if (string-match "[^ \t\n]" (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)))
                (insert cite-prefix))
          (if (string-match "[^ \t\n]" (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))
              (insert cite-prefix " ")))
        (forward-line 1)))))

(defun cite-quote-region (beg end)
  "Prefix the region between BEG and END with `cite-quote-prefix'."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (insert cite-quote-prefix)
        (forward-line 1)))))

;;; User functions

(defun cite-version (&optional arg)
  "Echo cite's version in the minibuffer.

If optional ARG is non-nil, insert at point."
  (interactive "*P")
  (if arg
      (insert "\n" cite-version "\n")
    (message "%s" cite-version)))

(defun cite-simple-attribution ()
    "Produce a very small attribution string.

Substitute \"An unnamed person wrote:\\n\\n\" if no email/name is available."
  (let ((email (assoc "email-addr" cite-parsed-headers))
	(name (assoc "real-name" cite-parsed-headers)))
    (if (and (null name) (null email))
	"An unnamed person wrote:\n\n"
      (concat (cadr (or name email)) " wrote:\n\n"))))

(defun cite-uncite-region (beg end &optional arg)
  "Remove cites from the region between BEG and END.

With optional numeric prefix ARG, remove that many cite marks."
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((i 1)
              (arg (if arg arg 1)))
          (while (<= i arg)
            (if (looking-at cite-prefix-regexp)
                (delete-char 1))
            (if (looking-at "[ \t]")
                (delete-char 1))
            (setq i (1+ i))))
        (forward-line 1)))))

;; Main entry point into cite.  This is the function used to actually
;; create the cited reply.
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
      ;; narrow to the newly yanked region (i.e. the just article we want to
      ;; quote)
      (narrow-to-region (point) (mark t))
      (cite-parse-headers)
      (cite-clean-up-cites (point-min) (point-max))
      ;; This might have to be played with if using format=flowed, but
      ;; Emacs can't handle composing it yet, so it's not a problem.
      (cite-remove-trailing-blanks)
      (if cite-remove-sig
          (cite-remove-sig))
      ;; Remove trailing lines and replace with a single one.
      ;; (cite-remove-trailing-lines (point-min) (point-max))
      (cite-cite-region (point-min) (point-max))
      (goto-char (point-min))
      (if cite-make-attribution
          (insert (funcall cite-make-attribution-function))))))

(provide 'cite)

;;; cite.el ends here
