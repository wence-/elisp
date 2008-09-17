;;; gnuplot-mode.el --- Major mode for interacting with gnuplot

;; Copyright (C) 2008  Lawrence Mitchell

;; Author: Lawrence Mitchell <lawrence.mitchell@ed.ac.uk>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'comint)

(define-derived-mode inferior-gnuplot-mode comint-mode "Inf Gnuplot"
  "Major mode for interacting with gnuplot."
  (setq comint-prompt-regexp
        (rx bol (or "gnuplot>"
                    ">"
                    "Help topic:"
                    (group "Subtopic of " (1+ word) ":"))
            (0+ space)))
  (gnuplot-mode-setup-comint-variables)
  (setq mode-line-process '(":%s")))

(defvar gnuplot-buffer nil)

(defun gnuplot-make-inf-process ()
  (interactive)
  (let (b)
    (unless (comint-check-proc gnuplot-buffer)
      (setq b (apply 'make-comint "gnuplot" (list "gnuplot")))
      (set-buffer b)
      (inferior-gnuplot-mode))
  (pop-to-buffer b)
  (setq gnuplot-buffer (buffer-name b))))

(defun gnuplot-start-process ()
  (save-window-excursion
    (gnuplot-make-inf-process)))
(defun gnuplot-mode-setup-comint-variables ()
  )

(defun gnuplot-get-process ()
  "Return the current gnuplot process or nil if none is running."
  (get-buffer-process (if (eq major-mode 'inferior-gnuplot-mode)
                          (current-buffer)
                        gnuplot-buffer)))
(defun gnuplot-process ()
  "Return the current gnuplot process, starting one if necessary.
See variable `gnuplot-buffer'."
  (unless (and gnuplot-buffer
               (get-buffer gnuplot-buffer)
               (comint-check-proc gnuplot-buffer))
    (gnuplot-start-process))
  (or (gnuplot-get-process)
      (error "No current process.  See variable `gnuplot-buffer'")))
(defvar gnuplot-mode-syntax-table
  (let ((ta (make-syntax-table)))
    (modify-syntax-entry ?# "<" ta)
    (modify-syntax-entry ?\n ">" ta)
    (modify-syntax-entry ?\" "\"" ta)
    (modify-syntax-entry ?\' "\"" ta)
    (modify-syntax-entry ?\( "()  " ta)
    (modify-syntax-entry ?\) ")(  " ta)
    (modify-syntax-entry ?\[ "(]  " ta)
    (modify-syntax-entry ?\] ")[  " ta)
    ta))

(define-derived-mode %gnuplot-mode fundamental-mode "Gnuplot"
  "Major mode for editing gnuplot plot files."
  (set (make-local-variable 'font-lock-defaults) gnuplot-mode-font-lock-defaults)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) nil)
  (set (make-local-variable 'indent-line-function) 'gnuplot-indent-line)
  (set-syntax-table gnuplot-mode-syntax-table))

(defvar %gnuplot-mode-commands-regexp
  (rx bow
          (or                           ; Commands
           "call" "cd" "clear" "exit" "fit" "help" "history" "if"
           "load" "pause" "plot" "print" "pwd" "quit" "replot" "reread"
           "reset" "save" "set" "shell" "show" "splot" "system" "test"
           "unset" "update")
          eow))
(defvar gnuplot-mode-font-lock-keywords
  `((,%gnuplot-mode-commands-regexp
     . font-lock-keyword-face)
    (,(rx (group
           bow
           (zero-or-one "no")           ;option negation
           (or                          ; options
            "angles" "arrow" "autoscale" "bars" "bmargin" "border" "boxwidth"
            "cbdata" "cbdtics" "cblabel" "cbmtics" "cbrange" "cbtics" "clabel"
            "clip" "cntrparam" "contour" "decimalsign" "dgrid3d" "dummy"
            "encoding" "fontpath" "format" "functions" "grid" "hidden3d"
            "historysize" "isosamples" "key" "label" "lmargin" "loadpath"
            "locale" "logscale" "mapping" "margin" "mouse" "multiplot" "mx2tics"
            "mxtics" "my2tics" "mytics" "mztics" "offsets" "origin" "output"
            "palette" "parametric" "pm3d" "pointsize" "polar" "rmargin" "rrange"
            "samples" "size" "style" "surface" "terminal" "tics" "ticscale"
            "ticslevel" "timefmt" "timestamp" "title" "tmargin" "trange"
            "urange" "variables" "view" "vrange" "x2data" "x2dtics" "x2label"
            "x2mtics" "x2range" "x2tics" "x2zeroaxis" "xdata" "xdtics" "xlabel"
            "xmtics" "xrange" "xtics" "xzeroaxis" "y2data" "y2dtics" "y2label"
            "y2mtics" "y2range" "y2tics" "y2zeroaxis" "ydata" "ydtics" "ylabel"
            "ymtics" "yrange" "ytics" "yzeroaxis" "zdata" "zdtics" "zero"
            "zeroaxis" "zlabel" "zmtics" "zrange" "ztics")
           eow))
     . font-lock-type-face)
    (,(rx bow
          (or                           ;Plot modifiers
           "linewidth" "linetype" "pointsize" "points" "lines"
           "linestyle")
          eow)
     . font-lock-variable-name-face)))

(defvar gnuplot-mode-font-lock-defaults
  '(gnuplot-mode-font-lock-keywords nil t))

(defvar gnuplot-basic-indent 4)
(defun gnuplot-indent-line ()
  (interactive)
  (let* ((pos (- (point-max) (point)))
         (continuation-p
          (save-excursion (end-of-line 0)
                          (eq (char-before) ?\\)))
         (command-p (save-excursion
                      (forward-line (if continuation-p -1 0))
                      (looking-at %gnuplot-mode-commands-regexp)))
         (prev-indent (save-excursion
                        (forward-line -1)
                        (back-to-indentation)
                        (current-column)))
         (indent 0))
    (cond ((and continuation-p command-p)
           (incf indent gnuplot-basic-indent))
          (continuation-p
           (incf indent prev-indent)))
    (back-to-indentation)
    (delete-region (point-at-bol) (point))
    (indent-to indent)
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

(defun gnuplot-send-buffer-to-gnuplot ()
  (interactive)
  (comint-send-string (gnuplot-process)
                      (format "cd \"%s\"\n"
                              (file-name-directory (or (buffer-file-name)
                                                       default-directory))))
  (comint-send-region (gnuplot-process) (point-min) (point-max))
  (comint-send-string (gnuplot-process) "\n"))

(provide 'gnuplot-mode)
;;; gnuplot-mode.el ends here
