;;; ascii-table.el --- Print an ASCII Table.
;; $Id: ascii-table.el,v 1.7 2004/02/27 21:30:21 wence Exp $

;;; Commentary:
;; Vital for any coder :)
;; The function `ascii-table' switches to a new buffer and then prints
;; the ascii-table showing Emacs' internal representations of the
;; files, and their respective decimal, octal and hexadecimal codes.


;;; History:
;;

;;; Code:
(require 'cl)
(defconst ascii-version
  "$Id: ascii-table.el,v 1.7 2004/02/27 21:30:21 wence Exp $"
  "ascii-table's version number.")

(defun ascii-pad-to-column (column string)
  "Pad to COLUMN with spaces, then insert STRING."
  (loop while (< (current-column) column)
        do (insert " ")
        finally (insert string)))

(defun ascii-table ()
  "Display an ASCII Table."
  (interactive)
  (switch-to-buffer (get-buffer-create "*ASCII Table*"))
  (erase-buffer)
  (insert " Cha | Dec | Oct  | Hex  || Cha | Dec | Oct  | Hex  || Cha | Dec | Oct  | Hex\n"
          "-----+-----+------+------++-----+-----+------+------++-----+-----+------+------\n")
  (loop for i from 0 to 42
        do
        (ascii-insert-char i)
        (ascii-insert-char (+ i 43))
        (ascii-insert-char (+ i 86) t)))

(defun ascii-insert-char (char &optional newline)
  (if (= char 128)
      (insert "\n")
    (let ((col (cond ((< char 43)
                      5)
                     ((< char 86)
                      32)
                     (t
                      59))))
      (insert (format " %2s" (single-key-description char)))
      (ascii-pad-to-column col "|")
      (insert (format " %2d" char))
      (ascii-pad-to-column (setq col (+ col 6)) "|")
      (insert (format " %04o" char))
      (ascii-pad-to-column (setq col (+ col 7)) "|")
      (insert (format " 0x%02x" char))
      (if newline
          (insert "\n")
        (ascii-pad-to-column (setq col (+ col 7)) "||")))))

(provide 'ascii-table)

;;; ascii-table.el ends here
