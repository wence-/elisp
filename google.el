;;; google.el --- Googling for stuff
;;; $Id: google.el,v 1.4 2002/10/06 21:06:06 lawrence Exp $

;; This file is NOT part of Emacs.

;; Copyright (C) 2002 lawrence mitchell <wence@gmx.li>
;; Filename: google.el
;; Version: $Revision: 1.4 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-05-15
;; Keywords: convenience searching

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
;; This file provides functionality I'm sure most of us have
;; implemented, starting web searches from within Emacs.
;; It was inspired and modified from a few functions in Ted O'Connor's
;; .emacs <URL:http://oconnor.cx/emacs.html>

;;; History:
;;
;; $Log: google.el,v $
;; Revision 1.4  2002/10/06 21:06:06  lawrence
;; Integrated summer changes, see ChangeLog for details.
;;
;; Revision 1.3  2002/06/17 17:57:39  lawrence
;; Added copyright notice.
;;
;; Revision 1.2  2002/06/16 19:53:06  lawrence
;; Provided a working `replace-in-string' even for those Emacsen that
;; don't have one by default.
;;
;; Changed prefix to google-
;;


;;; Code:


(defun google (string &optional type option)
  "Google for STRING.

TYPE is the type of search (a symbol), one of:
   search --- search www.google.com.
   groups --- search groups.google.com.
   direct --- search the Google directory.
   images --- search images.google.com.

OPTION is an option for groups.google.com searches only (a symbol), one of:
   selm --- search for a message-id.
   group --- go to a group."
  (interactive "sGoogle: ")
  (let ((dir (eq type 'direct))
        (type (symbol-name (if (or (null type) (eq type 'direct))
                               'search
                             type)))
        (option (and option (symbol-name option))))
    (browse-url (concat "http://www.google.com/"
                        type
                        "?"
                        (or option "q")
                        "="
                        (google-make-sendable-string string)
                        (and dir "&cat=gwd/Top")))))


(defun google-make-sendable-string (string)
  "Make STRING sendable as part of a Google URL.

This converts each character in STRING to its hex representation
preceded by a \"%\".

e.g.
\(google-make-sendable-string \"foo\")
    => \"%66%6f%6f\"."
  (mapconcat #'(lambda (c)
                 (format "%%%02X" c))
             (string-to-list string) ""))

(defun google-groups (string)
  "Search for STRING on groups.google.com."
  (interactive "sGoogle Groups: ")
  (google string 'groups))


(defun google-groups-group ()
  "Prompt for a newsgroup to go to on groups.google.com.

Defaults to the newsgroup at point."
  (interactive)
  (let* ((group (thing-at-point 'url))
         (group (and group
                    (string-match "^http://" group)
                    (replace-match "" nil t group))))
    (google
     (read-string (if group
                      (format "Which newsgroup (default %s): " group)
                    "Which newsgroup: ")
                  nil nil group)
     'groups 'group)))


(defun google-groups-message-id (start end)
  "Google Groups for the message-id between START and END."
  (interactive "r")
  (google (buffer-substring-no-properties start end) 'groups 'selm))


(defun google-region (start end)
  "Google for text from START to END."
  (interactive "r")
  (google (buffer-substring-no-properties start end) 'search))


(defun google-sentence ()
  "Google for sentence at point."
  (interactive)
  (google (thing-at-point 'sentence) 'search))


(defun google-word ()
  "Google for word at point."
  (interactive)
  (google (thing-at-point 'word) 'search))


(provide 'google)

;;; google.el ends here
