;;; cite.el --- Citing engine for Gnus -*- fill-column: 78 -*-

;;; Commentary:
;; This is yet another citing engine for Gnus.  It's to trivial-cite
;; what trivial-cite is to supercite (i.e. stripped down).  I wrote it
;; because, well, I wanted to see if I could.
;; Doesn't do any fancy guessing of cite-prefixes, just tacks a ">" on
;; the front of the quoted article.
;; See the docstrings of `cite-cite' and `cite-parse-headers' for information
;; on extending cite.

;;; History:
;; $Id: cite.el,v 1.4 2002/06/16 19:06:52 lawrence Exp $
;; $Log: cite.el,v $
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
;; Actually hook `cite-reinsert-sig' into the undo list. (done in 1.1.1)
;;

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

(defvar cite-attribution-function nil
  "*Function to call when creating an attribution.

This is a function called with no arguments, it can access the values of
various headers parsed by `cite-parse-headers', and stored in
`cite-parsed-headers'.")

(defvar cite-make-attribution-function 'cite-simple-attribution
  "*Function to call to make an attribution line.")

;;; Internal variables

(defvar cite-removed-sig nil
  "The signature we have just removed.

Sometimes we might want to comment on the signature, by storing it in a
variable, it is easy to restore it.")

(defvar cite-removed-sig-pos nil
  "Where in the buffer the string contained in `cite-removed-sig' was.")

(defvar cite-parsed-headers nil
  "Alist of parsed headers and their associated values.")

(defconst cite-version "$Id: cite.el,v 1.4 2002/06/16 19:06:52 lawrence Exp $"
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

;; basically stolen from tc.el
(defun cite-parse-headers ()
  "Parse the headers of the current article for useful information.

Since we narrow to the headers, we also delete them, as we don't want them
included in the followup."
  (setq cite-parsed-headers nil)
  (save-excursion
    (save-restriction
      (let ((point (point)))
        (re-search-forward "\n\n" nil t) ; find the end of the header section
        (narrow-to-region point (point))
        (ietf-drums-unfold-fws)          ; unfold headers
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "\\\([!-9;-~]+\\\):[ ]*\\\([^ ]?.*\\\)")
              (let ((name (buffer-substring-no-properties
                           (match-beginning 1) (match-end 1)))
                    (contents (buffer-substring-no-properties
                               (match-beginning 2) (match-end 2))))
                ;; add match conditions here if you want to parse
                ;; extra headers.  The functions you write to extract
                ;; information should take one argument, the contents
                ;; of the header field.
                (cond ((string= name "From")
                       (cite-parse-from contents))
                      ((string-match "Message-id" name)
                       (cite-parse-mid contents)))))
          (forward-line 1))
        ;; Delete the current (narrowed) buffer.  This removes headers
        ;; from the followup.
        (delete-region (point-min) (point-max))))))


(defun cite-parse-from (string)
  "Extract the real name or email address from STRING.

Uses function `gnus-extract-address-components' to do the hard work."
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
        (if (looking-at (concat " \\{1,2\\}"cite-prefix-regexp))
            ;; delete the spaces.
            (delete-region (match-beginning 0) (1- (match-end 0)))
          (if (looking-at cite-prefix-regexp)
              (progn
                (insert cite-prefix)
                (delete-char 1))
            (if (or (eq (preceding-char) ?\ )
                    (bolp))
                nil
              (if (looking-at "[^ \t]") ; if there's no space between
                                        ; the cites and the article
                  (insert " ")))        ; insert one.
            (forward-line 1)))))))


(defun cite-remove-sig ()
  "Remove a .sig.

This removes everything from the last occurance of `cite-sig-sep-regexp' to
the end of the buffer."
  (save-excursion
    (save-restriction
      (setq cite-removed-sig nil
            cite-removed-sig-pos nil)
      (goto-char (point-max))
      (if (re-search-backward cite-sig-sep-regexp nil t)
          (progn
            (delete-region (line-beginning-position) (line-beginning-position 2))
            (setq cite-removed-sig-pos (point-marker)
                  cite-removed-sig (buffer-substring-no-properties
                                    (point) (point-max)))
            (delete-region (point) (point-max)))))))

(defun cite-reinsert-sig ()
  "Reinsert the .sig removed by `cite-remove-sig'.

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
        (let ((i 1))
          (if arg
              (while (< i arg)
                (if (looking-at cite-prefix)
                    (delete-char 1))
                (setq i (1+ i))))
            (if (looking-at cite-prefix)
                (delete-char 1)))
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
     ...)).

As it currently stands, we narrow to the freshly yanked text, parse the
headers (which also deletes them).  Then normalise cite marks, remove trailing
whitespace, remove the sig, cite the article, an finally, insert an
attribution."
  (save-excursion
    (save-restriction
      ;; narrow to the newly yanked region (i.e. the just quoted text)
      (narrow-to-region (point) (mark t))
      (cite-parse-headers)
      (cite-clean-up-cites (point-min) (point-max))
      ;; This would have to be played with if using format=flowed, but
      ;; Emacs can't handle composing it yet, so it's not a problem.
      (cite-remove-trailing-blanks)
      (undo-boundary)
      (cite-remove-sig)
      (cite-cite-region (point-min) (point-max))
      (insert (funcall cite-make-attribution-function)))))

(provide 'cite)

;;; cite.el ends here
