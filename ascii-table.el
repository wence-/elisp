;;; ascii-table.el --- Print an ASCII Table.
;; $Id: ascii-table.el,v 1.4 2002/06/17 17:54:21 lawrence Exp $

;;; Commentary:
;; Vital for any coder :)
;; The function `ascii-table' switches to a new buffer and then prints
;; the ascii-table showing Emacs' internal representations of the
;; files, and their respective decimal, octal and hexadecimal codes.


;;; History:
;; $Log: ascii-table.el,v $
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

(defconst ascii-version
  "$Id: ascii-table.el,v 1.4 2002/06/17 17:54:21 lawrence Exp $"
  "ascii-table's version number.")

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



(provide 'ascii-table)

;;; ascii-table.el ends here
