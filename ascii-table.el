;;; ascii-table.el --- Print an ASCII Table.
;; $Id: ascii-table.el,v 1.5 2002/10/04 09:40:41 lawrence Exp $

;;; Commentary:
;; Vital for any coder :)
;; The function `ascii-table' switches to a new buffer and then prints
;; the ascii-table showing Emacs' internal representations of the
;; files, and their respective decimal, octal and hexadecimal codes.


;;; History:
;; $Log: ascii-table.el,v $
;; Revision 1.5  2002/10/04 09:40:41  lawrence
;; Cleaned up, moved to 3-column display.
;;
;; Revision 1.4  2002/06/17 17:54:21  lawrence
;; Added Commentary.
;;
;; Revision 1.3  2002/06/16 19:42:51  lawrence
;; Minor cosmetic changes.
;;
;; Revision 1.2  2002/06/16 19:42:31  lawrence
;; New variable `ascii-version'.
;; Added commentary.
;;

;;; Code:
(require 'cl)
(defconst ascii-version
  "$Id: ascii-table.el,v 1.5 2002/10/04 09:40:41 lawrence Exp $"
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
        for j = (+ i 43)
        for k = (+ j 43)
        do
        (ascii-insert-char i)
        (ascii-insert-char j)
        (ascii-insert-char k)))

(defun ascii-insert-char (char)
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
      (if (> char 85)
          (insert "\n")
        (ascii-pad-to-column (setq col (+ col 7)) "||")))))


(provide 'ascii-table)

;;; ascii-table.el ends here
