;;; eng-complete.el --- Completion for the english language.
;; $Id: eng-complete.el,v 1.3 2004/04/27 22:51:58 wence Exp $

;; Copyright (C) 2002, 2003 lawrence mitchell <wence@gmx.li>

;; Filename: eng-complete.el
;; Version: $Revision: 1.3 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-06-09
;; Keywords: completion convenience laziness

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
;; If you use Emacs `lisp-complete-symbol' a lot, or use tab
;; completion in a shell, you may find yourself trying to complete
;; words using tab, this package provides a way of doing that.  Yes,
;; how lazy am I!!


;;; History:
;;

;;; Code:

(defconst eng-complete-version
  "$Id: eng-complete.el,v 1.3 2004/04/27 22:51:58 wence Exp $"
  "Eng-complete's version number.")

(defvar eng-obarray (make-vector 29 nil)
  "Obarray holding list of completions.")

;; (let ((words (with-temp-buffer
;;                (insert-file-contents-literally "~/elisp/words")
;;                (split-string (buffer-string) "\n"))))
;;   (mapc #'(lambda (string)
;;             (intern string eng-obarray))
;;         words))

(defun eng-complete-symbol ()
  "Perform completion on the English word at point.

Yes, how lazy am I."
  (interactive)
  (let* ((end (point))
         (beg (progn (backward-word 1)
                     (point)))
         (pattern (buffer-substring-no-properties beg end))
         (completion (try-completion pattern eng-obarray))
         caps)
    (let ((case-fold-search nil))
      (goto-char beg)
      (if (looking-at "[A-Z]")
          (setq caps t)))
    (cond ((eq completion t))
          ((null completion)
           (message "Can't find completion for \"%s\"" pattern)
           (ding))
          ((not (string= pattern completion))
           (delete-region beg end)
           (if caps
               (setq completion (capitalize completion)))
           (insert completion))
          (t
           (message "Making completion list...")
           (let ((list (all-completions pattern eng-obarray)))
             (setq list (sort list 'string<))
             (let (new)
               (while list
                 (setq new (cons (if (fboundp (intern (car list)))
                                     (list (car list) " <f>")
                                   (car list))
                                 new))
                 (setq list (cdr list)))
               (setq list (nreverse new)))
             (with-output-to-temp-buffer "*Completions*"
               (display-completion-list list)))
           (message "Making completion list...%s" "done"))))
  (skip-chars-forward "^ \t\n"))


(provide 'eng-complete)

;;; eng-complete.el ends here
