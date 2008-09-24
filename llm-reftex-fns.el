;;; llm-reftex-fns.el --- Useful functions for extracting data from reftex and doing things with it

;; Copyright (C) 2007  Lawrence Mitchell

;; Author: Lawrence Mitchell <wence@gmx.li>
;; Keywords: tex, convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(require 'reftex)


(defalias 'reftex-abbreviate-title 'identity)

(defun llm-reftex-get-field (cite-key field)
  "Return the contents of FIELD in the entry referenced by CITE-KEY."
  (reftex-access-scan-info)
  (modify-syntax-entry ?\- "w" reftex-syntax-table-for-bib)
  (let ((files (reftex-get-bibfile-list)))
    (condition-case nil
        (save-excursion
          (let ((ret (reftex-get-bib-field
                      field
                      (reftex-parse-bibtex-entry
                       (reftex-pop-to-bibtex-entry cite-key files
                                                   nil nil nil t)))))
            (when (string= ret "")
              (setq ret nil))
            ret))
      (error (error "The entry for %s was not found" cite-key)))))

(defsubst llm-reftex-get-cite-key ()
  (reftex-this-word "^{}%\n\r, \t"))

(defun llm-reftex-browse-to-doi (&optional doi)
  "Get the citation at point and browse to its DOI."
  (interactive)
  (unless doi
    (let* ((entry (llm-reftex-get-cite-key)))
      (setq doi (llm-reftex-get-field entry "doi"))))
  (if doi
      (browse-url (format "http://dx.doi.org/%s" doi))
    (error "%s has no doi field" (llm-reftex-get-cite-key))))
    
(defun llm-reftex-show-paper (&optional item)
  "Get the citation at point and show the paper it corresponds to."
  (interactive)
  (let* ((initial-dir
          "/Users/wence/Documents/work/physics/phd/references/papers/")
         (directory (if (eq system-type 'darwin)
                        initial-dir
                      (expand-file-name "~/work/phd/references/papers/")))
         (cite-key (llm-reftex-get-cite-key)))
    (unless item
      (setq item (llm-reftex-get-field cite-key "local-url")))
    (when (null item)
      (error "%s has no paper associated with it" cite-key))
    (when (string-match "file://localhost" item)
      (setq item (replace-match "" nil t item)))
    (when (string-match initial-dir item)
      (setq item (replace-match directory nil t item)))
    (if (file-exists-p item)
        (call-process (if (eq system-type 'darwin) "open" "xpdf")
                      nil 0 nil item)
      (error "Associated paper %s does not exist in system for %s"
             item cite-key))))

(defvar llm-reftex-actions
  (list (cons "local-url" 'llm-reftex-show-paper)
        (cons "doi" 'llm-reftex-browse-to-doi)
        (cons "url" 'browse-url)
        (cons "title" 'google)))

(defun llm-reftex-get-info ()
  "Try successively less specific ways of getting data about a paper."
  (interactive)
  (let ((key (llm-reftex-get-cite-key)))
    (loop for (field . fn) in llm-reftex-actions
          for entry = (llm-reftex-get-field key field)
              then (llm-reftex-get-field key field)
          when entry
            do (funcall fn entry)
            (return))))
(provide 'llm-reftex-fns)
;;; llm-reftex-fns.el ends here
