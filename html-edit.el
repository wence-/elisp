;;; html-edit.el --- HTML editing functions.  Very specific to my webpages.
;; $Id: html-edit.el,v 1.3 2002/12/17 17:04:49 lawrence Exp $

;; Copyright (C) 2002 lawrence mitchell <wence@gmx.li>
;; Filename: html-edit.el
;; Version: $Revision: 1.3 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-09-04
;; Keywords: convenience html editing

;;; Commentary:

;;; History:
;; $Log: html-edit.el,v $
;; Revision 1.3  2002/12/17 17:04:49  lawrence
;; Minor cosmetic changes.
;;
;; Revision 1.2  2002/10/24 20:26:25  lawrence
;; Tidied up.
;;

;;; Code:

(defun he-insert-diary-entry ()
  "Insert a diary entry in the current page."
  (interactive)
  (goto-char (point-min))
  (erroring-search 're-search-forward
    "^[ \t]+<!-- Diary entries start here. -->"
    "Can't find start of diary entries")
  (forward-line 1)
  (newline)
  (forward-line -1)
  (let ((point (point)))
    (save-restriction
      (insert (concat "<div class=\"diaryheading\">\n"
                      "&lt;"
                      (format-time-string "%Y-%m-%d %H:%M:%S %z")
                      "&nbsp;&nbsp;" user-login-name "&gt;\n"
                      "</div>\n"
                      "<p class=\"diaryentry\">\n\n"
                      "</p>"))
      (narrow-to-region point (point))
      (forward-char -5)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (indent-according-to-mode)
          (forward-line)))))
  (indent-according-to-mode))

(defun he-preview-buffer ()
  "Preview the current html buffer in your favourite browser."
  (interactive)
  (save-window-excursion
    (save-buffer)
    (browse-url-of-buffer)))
(defun he-make-new-page ()
  "Create a new html page from a template."
  (interactive)
  (let ((file (concat (read-string "Filename: ") ".html"))
        (title (read-string "Title: "))
        (heading (read-string "Heading: ")))
    (find-file (concat "~/web/" file))
    (insert-file-contents "~/web/template.html")
    (and (search-forward "title goes here")
         (replace-match title))
    (and (search-forward "heading goes here")
         (replace-match heading))))

(defun he-archive-diary (&optional keep-number)
  "Archive off all but KEEP-NUMBER (or 10) diary entries."
  (interactive "nNumber of diary entries to keep: ")
  (save-window-excursion
    (find-file "~/web/index.html")      ; find diary page
    (save-excursion
      (goto-char (point-min))
      (let ((count 0)                   ; current diary entry
            archive          ; the diary entires to archive (a string)
            entries)                    ; individual entries
        (while (and (<= count keep-number)
                    ;; search for a diary entry
                    (re-search-forward "^[ \t]*<div class=\"diaryheading\">$" nil t))
          (incf count))
        (when (= count (1+ keep-number)) ; only start archiving if we
                                        ; have reached the keep-limit
          (forward-line 0)
          (let ((point (point)))
            (erroring-search 're-search-forward
              "^[ \t]*<h1>\n[ \t]*Randomness\n[ \t]*</h1>"
              "Can't find end of diary section")
            (forward-line -2)
            (forward-char -1)
            ;; set the archive string
            (setq archive (buffer-substring-no-properties point (point)))
            ;; ...and delete the diary entires from this buffer
            (delete-region point (point)))
          (delete-char 1)               ; delete extraeneous newline
          (save-buffer)                 ; save diary page
          ;; construct archive data.  We create an alist with an entry
          ;; for each archived diary entry,
          (with-temp-buffer
            (insert archive)
            (setq archive nil)          ; reuse variable
            (goto-char (point-min))
            (let (date header)
              ;; search for the diary heading
              (while (re-search-forward
                      (concat
                       "^[ \t]*<div class=\"diaryheading\">\n[ \t]*&lt;"
                       "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\).*"
                       "\n[ \t]*</div>")
                      nil t)
                ;; date section
                (setq date (match-string-no-properties 1))
                ;; entire matched string
                (setq header (match-string-no-properties 0))
                (forward-line 1)
                (let ((point (point)))
                  ;; search for the end of this diary entry.
                  (re-search-forward "^[ \t]*</p>$")
                  ;; add it to the list.
                  (add-to-list
                   'archive
                   `(,date
                     ,header
                     ,(buffer-substring-no-properties point (point))))))))
          (find-file "~/web/archives.html") ; open archive page
          (goto-char (point-min))
          (erroring-search 're-search-forward
            "^[ \t]*<!-- Diary links start here. -->"
            "Can't find links section of archive page")
          (newline-and-indent)
          ;; simplify archive list.  This concats entries made on
          ;; the same day into one for link-tagging purposes.  The
          ;; exact date/time is not lost.
          (setq archive (he-simplify-archive-list archive))
          ;; loop to actually insert the entries.
          ;; the data structure (the variable archive) is such that
          ;; the newest entry to be archived is last in the list.
          ;; Hence we just insert above existing entries, to make
          ;; sure that the newest entries are at the top.
          (while archive
            (save-excursion
              (let* ( ;; deal with entries one at a time
                     (archive (car archive))
                     ;; this goes in the menu sidebar as the link.
                     (link-name (car archive))
                     ;; the w3c says that the ``name'' attribute of
                     ;; <a></a> is deprecated, and that it should be
                     ;; replaced by ``id''.  However, ``id'' may not
                     ;; begin with a numeric character.  Hence, we
                     ;; must construct an anchor tag different from
                     ;; the link name.
                     (link-tag (concat "date:"link-name))
                     ;; the actual diary entry
                     (content (cadr archive))
                     ;; whether or not a link already exists for this
                     ;; date (does `link-tag' already exist?
                     link-exists)
                ;; try and find an existing link for this date.
                (save-excursion
                  (goto-char (point-min))
                  (if (re-search-forward (concat "\"#"link-tag"\"") nil t)
                      (setq link-exists t)))
                (if link-exists
                    ;; if the link already exists, adding the entry is
                    ;; easy, we just search for the anchor with the
                    ;; correct link-tag, and insert the diary entry
                    (progn
                      (re-search-forward (concat "id=\""link-tag"\""))
                      (forward-line 1)
                      (insert "\n" content"\n"))
                    ;; the link doesn't already exist.  note we've
                    ;; already found where the links should go, so we
                    ;; just insert the requisite link.
                    (insert "<p><a href=\"#" link-tag"\">" link-name "</a></p>\n")
                    ;; Now search for the top of the diary entries themselves.
                    (erroring-search 're-search-forward
                      "^[ \t]*<!-- Diary archives start here. -->$"
                      "Can't find diary archive start")
                    (forward-line 1)
                    ;; insert the diary entry, preceded by its anchor
                    (insert "<a id=\""link-tag"\" />\n"
                            content "\n"))))
            ;; and loop
            (setq archive (cdr archive)))
          (save-buffer))))))

(defun he-simplify-archive-list (list)
  "Simplify LIST.

Simplification consists of the following:
  Given a list foo:
  (setf foo '((\"1\" \"a\" \"b\")
              (\"1\" \"c\" \"d\")
              (\"2\" \"e\" \"f\")))
  If the `car' of successive conses matches, the rest of the list is
  `concat'ed together with linefeeds between each element.
  Hence, calling this function on foo above would result in:
    ((\"1\" \"c\\nd\\na\\nb\")
     (\"2\" \"e\\nf\"))"
  (let (date contents result)
    (while list
      (let ((list (car list)))
        (setq date (car list)
              contents (concat (cadr list) "\n" (caddr list))))
      (setq list (cdr list))
      (let ((continue t))
        (while (and continue list)
          (let ((next-date (caar list)))
            (if (string= date next-date)
                (let ((next-contents (concat (cadar list) "\n" (caddar list))))
                  (setq contents (concat next-contents "\n"contents)
                        list (cdr list)))
                (setq continue nil)))))
      (add-to-list 'result `(,date ,contents) t))
    result))

(defmacro erroring-search (fn string error-message)
  "Call FN to search for STRING.

If STRING not found display ERROR-MESSAGE."
  `(condition-case err
    (funcall ,fn ,string)
    (error (error ,error-message))))

(put 'erroring-search 'lisp-indent-function 1)

(provide 'html-edit)

;;; html-edit.el ends here
