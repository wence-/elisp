;;; cuf-mode.el --- Major mode for editing CUDA Fortra

;; This file is NOT part of Emacs.

;; Copyright (C) 2011 Lawrence Mitchell <wence@gmx.li>
;; Filename: cuf-mode.el
;; Created: 2011-11-18

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; Extra fontification for CUDA Fortran based on f90 support.

;;; Code:

(eval-when-compile
  (require 'rx))

(require 'f90)

(defconst cuf-keywords
  (rx bow (group (or "threadIdx" "blockIdx" "blockDim"
                 "gridDim" "shared" "device" "constant"))
      eow))

(defconst cuf-procedure-modifiers
  (rx bow (group "attributes") eow (0+ space)
      "(" (0+ space) bow (group (or "host" "device" "global"))
      eow
      (0+ space) ")"))

(defconst cuf-device-call
  (rx (group "<<<" (0+ space) (+ word)
                 (0+ (and (0+ space) "," (0+ space) (+ word)))
                 (0+ space) ">>>")))

(defvar cuf-font-lock-keywords
  (append
   f90-font-lock-keywords
   (list
   `(,cuf-procedure-modifiers (1 font-lock-keyword-face) (2 font-lock-constant-face))
   `(,cuf-device-call (1 font-lock-constant-face))
   `(,cuf-keywords (1 font-lock-keyword-face)))))

(defvar cuf-font-lock-keywords-1
  (append
   f90-font-lock-keywords-4
   (list
   `(,cuf-procedure-modifiers (1 font-lock-keyword-face) (2 font-lock-constant-face))
   `(,cuf-device-call (1 font-lock-constant-face))
   `(,cuf-keywords (1 font-lock-keyword-face)))))

(define-derived-mode cuf-mode f90-mode "CUF"
  "Major mode for editing CUDA Fortran."
  (set (make-local-variable 'font-lock-defaults)
       '((cuf-font-lock-keywords cuf-font-lock-keywords-1)
         nil t)))

(provide 'cuf-mode)

;;; cuf-mode.el ends here
