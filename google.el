;;; google.el --- Googling for stuff
;; $Id: google.el,v 1.3 2002/06/17 17:57:39 lawrence Exp $

;; Copyright (C) 2002 lawrence mitchell <wence@gmx.li>

;; Filename: google.el
;; Version: $Revision: 1.3 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-05-15
;; Keywords: convenience searching

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
;; This file provides functionality I'm sure most of us have
;; implemented, starting web searches from within Emacs.
;; It was inspired and modified from a few functions in Ted O'Connor's
;; .emacs <URL:http://oconnor.cx/emacs.html>

;;; History:
;;
;; $Log: google.el,v $
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

;; provide a working `replace-in-string'
(eval-and-compile
  (cond
   ((fboundp 'replace-in-string)
    (defalias 'google-replace-in-string 'replace-in-string))
   ((fboundp 'replace-regexp-in-string)
    (defun google-replace-in-string  (string regexp newtext &optional literal)
      (replace-regexp-in-string regexp newtext string nil literal)))
   (t
    (defun google-replace-in-string (string regexp newtext &optional literal)
      (let ((start 0) tail)
	(while (string-match regexp string start)
	  (setq tail (- (length string) (match-end 0)))
	  (setq string (replace-match newtext nil literal string))
	  (setq start (- (length string) tail))))
      string))))


(defun google (string &optional type option)
  "Google for STRING.

TYPE is the type of search (a symbol), one of:
   www --- search www.google.com.
   groups --- search groups.google.com.

OPTION is an option for groups.google.com searches only (a symbol), one of:
   mid --- search for a message-id.
   group --- go to a group."
  (interactive "sGoogle: ")
  (browse-url (concat "http://"
		      (if type (symbol-name type)"www")".google.com/"
		      (cond ((eq type 'www)
			      "search?hl=en&q")
			    ((eq type 'groups)
			     (concat "groups?hl=en"
				     (cond ((eq option 'mid)
					    "&selm")
					   ((eq option 'group)
					    "&group")
					   (t "&q"))))
			    (t "search?hl=en&q"))
		      "="
		      (google-replace-in-string string " " "+") "")))


(defun google-groups (string)
  "Search for STRING on groups.google.com."
  (interactive "sGoogle Groups: ")
  (google string 'groups))


(defun google-groups-group ()
  "Prompt for a newsgroup to go to on groups.google.com.

Defaults to the newsgroup at point."
  (interactive)
  (let ((group
	 (condition-case nil
	     (google-replace-in-string (thing-at-point 'url) "^http://" "")
	   (error nil))))
    (google (read-string (format "Which newsgroup (default is %s): " group)
			 nil nil group) 'groups 'group)))


(defun google-groups-message-id (start end)
  "Google Groups for the message-id between START and END."
  (interactive "r")
  (google (buffer-substring-no-properties start end) 'groups 'mid))


(defun google-region (start end)
  "Google for text from START to END."
  (interactive "r")
  (google (buffer-substring-no-properties start end) 'www))


(defun google-sentence ()
  "Google for sentence at point."
  (interactive)
  (google (thing-at-point 'sentence) 'www))


(defun google-word ()
  "Google for word at point."
  (interactive)
  (google (thing-at-point 'word) 'www))



(provide 'google)

;;; google.el ends here
