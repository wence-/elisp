;;; google.el --- Googling for stuff
;; Time-stamp: <2002-05-20 20:10:41 wence>

;;; Commentary:
;; This file provides functionality I'm sure most of us have
;; implemented, starting web searches from within Emacs.
;; It was inspired and modified from a few functions in Ted O'Connor's
;; .emacs <URL:http://oconnor.cx/emacs.html>

;;; History:
;; Revision 1.1.1:  2002/05/20 20:10:32 wence
;; Provided a working `replace-in-string' even for those Emacsen that
;; don't have one by default.
;;
;; Revision 1.1:  2002/04/27  0:23:54 wence
;; Changed prefix to google-
;;
;; Revision 1.0:  2002/04/25 wence
;; Initial revision.

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
