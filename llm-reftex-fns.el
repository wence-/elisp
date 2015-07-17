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
    
(defun llm-reftex-show-paper (&optional key no-error)
  "Get the citation at point and show the paper it corresponds to."
  (interactive)
  (let* ((cite-key (or key (llm-reftex-get-cite-key)))
         (paper (llm-reftex-make-paper cite-key)))
    (when (and (null paper) (not no-error))
      (error "Associated paper %s does not exist in system for %s"
             (file-name-nondirectory paper) cite-key))
    (if paper
        (progn (call-process (if (eq system-type 'darwin) "open" "evince")
                             nil 0 nil (expand-file-name paper))
               t)
      nil)))

(defvar llm-reftex-actions
  (list (cons "doi" 'llm-reftex-browse-to-doi)
        (cons "url" 'browse-url)
        (cons "title" 'google)))

(defvar llm-reftex-paper-root "~/work/phd/references/papers/")

(defun llm-reftex-make-paper (key)
  (when (string-match "\\([^:]+\\):\\(.*\\)"  key)
    (setq key (format "%s/%s"(downcase (match-string 1 key))
                      (match-string 2 key)))
    (let ((p (format "%s%s.pdf" llm-reftex-paper-root key)))
      (if (file-exists-p p)
          p
        nil))))
                   
(defun llm-reftex-get-info ()
  "Try successively less specific ways of getting data about a paper."
  (interactive)
  (let ((key (llm-reftex-get-cite-key)))
    (unless (llm-reftex-show-paper key t)
      (loop for (field . fn) in llm-reftex-actions
            for entry = (llm-reftex-get-field key field)
            then (llm-reftex-get-field key field)
            when entry
            do (funcall fn entry)
            (return)))))

(require 'bibtex)
(defun bibtex-generate-autokey ()
  (let* ((bibtex-autokey-name-case-convert-function 'identity)
         (bibtex-autokey-year-length 4)
         (names (bibtex-autokey-get-names))
         (year (bibtex-autokey-get-year))
         key)
    (setq key (format "%s:%s" names year))
    (let ((ret key))
      (loop for c from ?a to ?z
            while (try-completion ret (bibtex-parse-keys nil t))
            do (setq ret (format "%s%c" key c)))
      ret)))

(setq reftex-default-bibliography '("~/docs/work/bibliography/references.bib"))

(defun llm-reftex-format-ascii-citation ()
  (interactive)
  (let ((reftex-cite-format
         "%3a. %t (%y)")
        (reftex-cite-punctuation '(", " " and " " et al.")))
    (reftex-citation)))

(provide 'llm-reftex-fns)
;;; llm-reftex-fns.el ends here
