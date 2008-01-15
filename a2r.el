;;; a2r.el --- Convert between Arabic numbers and Roman numerals.
;; $Id: a2r.el,v 1.4 2004/02/27 21:30:24 wence Exp $

;; Copyright (C) 2002, 2003 lawrence mitchell <wence@gmx.li>
;; Filename: a2r.el
;; Version: $Revision: 1.4 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-08-03
;; Keywords: numbers fun

;;; Commentary:
;; 


;;; History:
;;

;;; Code:

(defconst roman<->arabic
  '((1000 .   "M") (900  .  "CM") (500  .   "D") (400  .  "CD")
    (100  .   "C") (90   .  "XC") (50   .   "L") (40   .  "XL")
    (10   .   "X") (9    .  "IX") (5    .   "V") (4    .  "IV")
    (1    .   "I"))
  "List of maps between Arabic numbers and their Roman numeral equivalents.")

(defun arabic-to-roman (num &optional arg)
  "Convert Arabic number NUM to its Roman numeral representation.

Obviously, NUM must be greater than zero.  Don't blame me, blame the
Romans, I mean \"What have the Romans ever done for /us/?\" (with
apologies to Monty Python).
If optional prefix ARG is non-nil, insert in current buffer."
  (interactive "nNumber: \nP")
  (if (< num 0)
      (message (concat "Error, NUM must be a positive number, the Romans had no"
                       "notion of\n"
                       "zero or negative numbers."))
    (let ((map roman<->arabic)
          res)
      (while (and map (> num 0))
        (if (or (= num (caar map))
                (> num (caar map)))
            (setq res (concat res (cdar map))
                  num (- num (caar map)))
          (setq map (cdr map))))
      (if arg
          (insert "\n" res "\n")
        (message "%s" res)))))

(defun roman-to-arabic (string &optional arg)
  "Convert STRING of Roman numerals to an Arabic number.

If STRING contains a letter which isn't a valid Roman numeral, the rest
of the string from that point onwards is ignored.

Hence:
MMD == 2500
and
MMDFLXXVI == 2500.
If optional ARG is non-nil, insert in current buffer."
  (interactive "sRoman numeral: \nP")
  (let ((res 0)
        (map roman<->arabic))
    (while (and map (not (string= string "")))
      (if (string-match (concat "^" (cdar map)) string)
          (setq res (+ res (caar map))
                string (replace-match "" nil t string))
        (setq map (cdr map))))
    (if arg
        (insert "\n" (number-to-string res) "\n")
      (message "%d" res))))

(provide 'a2r)

;;; a2r.el ends here
