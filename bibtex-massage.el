;;; bibtex-massage.el --- Format latex bibliographies in APS style into ones suitable for EPL

;;; Commentary:
;; Hacky code to reformat latex bibliographies suitable for APS
;; journals (such as Phys Rev Lett) into ones suitable for Europhysics
;; Letters.

;;; Code:
(defun llm-extract-fields (field-name)
  "Extract the contents of all \\bibinfo commands with name FIELD-NAME."
  (save-excursion
    (let ((ret nil))
      ;; Find the field
      (while (re-search-forward (format "\\\\bibinfo{%s}" field-name) nil t)
        (let ((num-open 0)
              (point nil))
          ;; look for opening character of "contents"
          (when (looking-at "{")
            (forward-char 1)
            (incf num-open))
          ;; found it
          (setq point (point))
          ;; Keep track of how many opening characters (#\{) we've
          ;; found and finish when we've closed them all.
          (while (> num-open 0)
            (forward-char 1)
            (when (looking-at "{")
              (incf num-open))
            (when (looking-at "}")
              (decf num-open)))
          ;; Save the contents
          (push (buffer-substring-no-properties point (point)) ret)))
      ret)))

(defun llm-massage-name-list (list)
  "Turn a LIST of marked up author names into a string."
  (setq list (loop for name in list
                   ;; Looking for:
                   ;; \bibfnamefont{A. N.}~\bibnamefont{Surname}
                   ;;                    ^ could be space
                   if (string-match (rx "\\bibfnamefont{"
                                        (group (1+ (not (any "}"))))
                                        "}"
                                        (0+ (or (syntax whitespace) "~"))
                                        "\\bibnamefont{"
                                        (group (1+ (not (any "}"))))
                                        "}")
                                    name)
                   ;; Turn it into:
                   ;; Surname A. N.
                   collect (format "%s %s" (match-string 2 name)
                                   (match-string 1 name))))
  ;; If author list has more than two authors, we need to put commas
  ;; between the names.
  ;; But not between the penultimate two, because "and" goes between
  ;; them.
  ;; At this point the list of authors is reversed, so we check to see
  ;; if the list has a cddr to see if we need to put some commas in.
  (when (cddr list)
    (setcdr (cdr list) (mapcar (lambda (x)
                                 (concat x ", "))
                               (cddr list))))
  ;; Add a separating "and" if there are more than two authors.
  (when (cdr list)
    (push "\\and" (cdr list)))
  ;; Reverse and turn into a string
  (mapconcat 'identity (nreverse list) " "))


(defun llm-massage-aps-bibliography-to-epl (s e)
  "Convert the APS bibliography between S and E into one for EPL."
  (interactive "r")
  (save-restriction
    (narrow-to-region s e)
    (goto-char (point-min))
    (let ((result nil)
          (single-out-buffer "*single-output*"))
      (while (progn
               ;; Move to the beginning of the next bibitem
               (skip-chars-forward " \n")
               ;; Can't find an item if we've reached the end of the buffer
               (not (eobp)))
        ;; Massage the current bibitem
        (llm-massage-aps-bibitem-to-epl single-out-buffer)
        ;; Grab the output
        (push (with-current-buffer single-out-buffer
                (buffer-string))
              result))
      ;; Show the nicely formatted new bibliography
      (with-output-to-temp-buffer "*reformatted-bibitems*"
        (princ (mapconcat 'identity (nreverse result) "\n"))))))
      
(defun llm-massage-aps-bibitem-to-epl (&optional output-buffer)
"Massage a single bibitem into EPL format.

If OUTPUT-BUFFER is non-nil, send the output there, otherwise send it
to *epl-format*."
(interactive)
(save-restriction
  (let ((start
         ;; Find the start
         (progn (search-forward "\\bibitem" nil t)
                (forward-line 0)
                (point)))
        ;; Find the end
        (end (progn (search-forward ".\n\n" nil t)
                    (forward-line -1)
                    (point)))
        result)
    (narrow-to-region start end)
    (goto-char (point-min))
    (let (cite-key authors journal volume pages
                   booktitle year articlep bookp editors publisher
                   address)
      ;; Try and match a cite-key
      (if (looking-at (rx "\\bibitem["
                          (1+ (not (any "]")))
                          "]{"
                          (group (1+ (not (any "}"))))
                          "}"))
          (setq cite-key (match-string 1))
        (error "No valid cite key found.\n"))
      (goto-char (point-min))
      ;; Find the list of authors
      (setq authors (llm-massage-name-list
                     (llm-extract-fields "author")))
      ;; And those of the editors
      (setq editors (llm-massage-name-list
                     (llm-extract-fields "editor")))
      ;; Is this an article?
      (when (save-excursion (search-forward "\\bibinfo{journal}" nil t))
        (setq articlep t)
        (setq journal (llm-extract-fields "journal")))
      ;; Or is it from a book?
      (when (save-excursion (re-search-forward
                             "\\\\bibinfo{\\(?:book\\)?title}" nil t))
        (setq bookp t)
        (setq booktitle (or (llm-extract-fields "title")
                            (llm-extract-fields "booktitle"))))
      ;; Extract other relevant info
      (setq year (llm-extract-fields "year"))
      (setq pages (llm-extract-fields "pages"))
      (setq volume (llm-extract-fields "volume"))
      (setq publisher (llm-extract-fields "publisher"))
      (setq address (llm-extract-fields "address"))
      ;; If the publisher and address are separate, concat them
      ;; together as PUBLISHER, ADDRESS
      (if (and address publisher (not (string= (car address) "")))
          (setcar publisher (concat (car publisher) ", " (car
                                                          address))))
      ;; If we have a range of pages, turn them into a cons of
      ;; (START . END)
      (if (and (first pages)
               (string-match "\\([0-9]+\\)--\\([0-9]+\\)"  (first pages)))
          (setq pages (cons (match-string 1 (first pages))
                            (match-string 2 (first pages))))
        (setq pages (first pages)))
      (with-temp-buffer
        (macrolet (  ;; Insert \NAME{THING}\n only if THING is not
                   ;; null.  If THING is a list, we check if its
                   ;; `car' for nullness.  In this sense, null means
                   ;; that THING is `string=' to "" or nil.
                   (insert-if-not-null (name thing)
                                       `(let ((stringified-thing
                                               (if (listp ,thing)
                                                   (first ,thing)
                                                 ,thing)))
                                          (unless (or (string= stringified-thing "")
                                                      (null stringified-thing))
                                            (insert "  \\" ,name "{"
                                                    stringified-thing
                                                    "}\n"))))
                   ;; Insert \NAME{THING}\n unconditionally.
                   ;; If THING is a list, insert its `car',
                   ;; otherwise just insert THING.
                   (insert-thing (name thing)
                                 `(let ((stringified-thing
                                         (if (listp ,thing)
                                             (first ,thing)
                                           ,thing)))
                                    (unless (null stringified-thing)
                                      (insert "  \\" ,name "{"
                                              stringified-thing "}\n")))))
          (insert "\\bibitem{" cite-key "}\n")
          ;; Format for an article
          (when articlep
            (insert-thing "Name" authors)
            (insert-thing "Review" journal)
            (insert-if-not-null "Vol" volume)
            (insert-if-not-null "Year" year)
            (insert-if-not-null "Page" pages))
          ;; For a book
          (when bookp
            ;; If the book has no authors, only editors, insert that
            ;; first
            (if (string= authors "")
                (insert-thing "Editor" editors)
              ;; Otherwise insert the authors first
              (insert-thing "Name" authors))
            (insert-thing "Book" booktitle)
            ;; If we haven't already inserted the editors, do so now.
            (unless (string= authors "")
              (insert-if-not-null "Editor" editors))
            (insert-if-not-null "Vol" volume)
            (insert-if-not-null "Publ" publisher)
            (insert-if-not-null "Year" year)
            ;; Insert either a range of pages
            (if (and pages (listp pages))
                (insert "  \\Pages{" (car pages) "}{" (cdr pages) "}\n")
              ;; Or a single page number
              (insert-if-not-null "Page" pages))))
        ;; Print the results somewhere
        (with-output-to-temp-buffer (or output-buffer "*epl-format*")
          (princ (buffer-string))))))
  ;; Go to the end of the current bibitem.
  (goto-char (point-max))))


(provide 'bibtex-massage)

;;; bibtex-massage.el ends here
