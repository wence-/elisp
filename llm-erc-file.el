;; Creating "safe" filenames in ERC
(defvar llm-erc-unwanted-filename-chars
  '((?/ . ?-)
    (?! . ?-))
  "*Alist of characters you don't want to appear in a filename, and
their replacements.

Each cell is of the form:
\(UNWANTED . REPLACEMENT).")

(defvar llm-erc-invalid-filname-chars
  (if (memq system-type '(windows-nt cygwin ms-dos))
      "[|<>\"?*\000-\031:]"
      "[\000]"))

(defvar llm-erc-replacement-filename-char
  ?-
  "*Character to replace invalid characters in a filename with.
Invalid characters are ones in `llm-erc-invalid-filename-chars'.
Should you want to replace other characters, use
`llm-erc-unwanted-filename-chars' instead.")

;; (llm-erc-make-filename "foo/bar:")
(defun llm-erc-make-filename (file)
  "Make a safe and wanted filename for FILE.

This calls `llm-erc-make-safe-filename' and
`llm-erc-make-wanted-filename' on FILE.

FILE should not be an absolute path.  In particular, do not use this
function on filenames like \"F:/tmp/foo/bar/baz.log\", since the colon
will be replaced."
  (llm-erc-make-wanted-filename
   (llm-erc-make-safe-filename file)))

(defun llm-erc-make-safe-filename (file)
  "Make a safe filename for FILE.

This replaces all occurances of `llm-erc-invalid-filname-chars' in
FILE with `llm-erc-replacement-filename-char'."
  (while (string-match llm-erc-invalid-filname-chars file)
    (setq file (replace-match (char-to-string
                               llm-erc-replacement-filename-char)
                              nil t file)))
  file)

(defun llm-erc-make-wanted-filename (file)
  "Make a wanted filename for FILE.

This replaces all occurances of the car of each cons cell of
`llm-erc-unwanted-filename-chars' with the cdr of said cell."
  (dolist (char llm-erc-unwanted-filename-chars)
    (while (string-match (char-to-string (car char)) file)
      (setq file (replace-match (char-to-string (cdr char)) nil t file))))
  file)

(provide 'llm-erc-file)
