;;; eng-complete.el --- Completion for the english language.
;; $Id: eng-complete.el,v 1.1 2003/05/21 00:12:38 wence Exp $

;; Copyright (C) 2002, 2003 lawrence mitchell <wence@gmx.li>

;; Filename: eng-complete.el
;; Version: $Revision: 1.1 $
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
;; $Log: eng-complete.el,v $
;; Revision 1.1  2003/05/21 00:12:38  wence
;; Initial commit
;;
;; Revision 1.11  2003/04/21 23:34:28  lawrence
;; Fix docstring type.
;;
;; Revision 1.10  2003/04/21 23:29:21  lawrence
;; Update copyright.
;;
;; Revision 1.9  2003/04/21 20:59:38  lawrence
;; Modified initialisation of eng-obarray.
;;
;; Revision 1.8  2003/03/28 19:56:52  lawrence
;; Made path to word list absolute.
;;
;; Revision 1.7  2003/03/08 22:15:03  lawrence
;; Removed hardcoded list of words.  Now read in from a separate file.
;;
;; Revision 1.6  2002/12/07 14:53:29  lawrence
;; Corrected typo.
;;
;; Revision 1.5  2002/10/24 20:53:28  lawrence
;; Made significantly shorter.
;;
;; Revision 1.4  2002/10/06 21:05:44  lawrence
;; See ChangeLog.
;;
;; Revision 1.3  2002/06/17 17:56:51  lawrence
;; Added copyright notice.
;;
;; Revision 1.2  2002/06/16 19:51:06  lawrence
;; New variable `eng-complete-version'.
;; Cleaned up code and added docstrings where appropriate.
;;

;;; Code:

(defconst eng-complete-version
  "$Id: eng-complete.el,v 1.1 2003/05/21 00:12:38 wence Exp $"
  "Eng-complete's version number.")

(defvar eng-obarray (make-vector 29 nil)
  "Obarray holding list of completions.")

(let ((words (with-temp-buffer
               (insert-file-contents-literally "~/elisp/words")
               (split-string (buffer-string) "\n"))))
  (mapc #'(lambda (string)
            (intern string eng-obarray))
        words))

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
