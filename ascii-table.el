(defun ascii-pad-to-column (column string)
  "Pad to COLUMN with spaces, then insert STRING."
  (while (< (current-column) column)
    (insert " "))
  (insert string))

(defun ascii-table ()
  "Display an ASCII Table."
  (interactive)
  (switch-to-buffer (get-buffer-create "*ASCII Table*"))
  (erase-buffer)
  (insert (concat
           " Cha | Dec | Oct  | Hex  || Cha | Dec | Oct  | Hex\n"
           "-----+-----+------+------++-----+-----+------+------\n"))
  (let ((char 0))
    (while (<= char 63)
      (insert (format " %2s" (single-key-description char)))
      (ascii-pad-to-column 5 "|")
      (insert (format " %2d" char))
      (ascii-pad-to-column 11 "|")
      (insert (format " %04o" char))
      (ascii-pad-to-column 18 "|")
      (insert (format " 0x%02x" char))
      (ascii-pad-to-column 25 "||")
      (let ((char1 (+ 64 char)))
        (insert (format " %2s" (single-key-description char1)))
        (ascii-pad-to-column 32 "|")
        (insert (format " %2d" char1))
        (ascii-pad-to-column 38 "|")
        (insert (format " %04o" char1))
        (ascii-pad-to-column 45 "|")
        (insert (format " 0x%02x\n" char1)))
      (setq char (1+ char)))))


