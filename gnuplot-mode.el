;;; gnuplot-mode.el --- Major mode for interacting with gnuplot

;; This file is NOT part of Emacs.

;; Copyright (C) 2008  Lawrence Mitchell
;; Filename: gnuplot-mode.el
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Version: 1
;; Created: 2008-09-17
;; Keywords:

;; COPYRIGHT NOTICE

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
(require 'rx)

(define-derived-mode inferior-gnuplot-mode comint-mode "Inf Gnuplot"
  "Major mode for interacting with gnuplot."
  (setq comint-prompt-regexp
        (rx bol (or "gnuplot>"
                    ">"
                    "Help topic:"
                    (group "Subtopic of " (1+ word) ":"))
            (0+ space)))
  (gnuplot-setup-comint-variables)
  (setq mode-line-process '(":%s")))

(define-key inferior-gnuplot-mode-map (kbd "TAB") 'gnuplot-indent-or-complete)

(defun %gnuplot-watch-for-cd (s)
  "Watch for a change of directory in gnuplot input.

Sets `default-directory' in `gnuplot-buffer' accordingly."
  (let (dir)
    (when (string-match (rx string-start "cd"
                            (1+ (syntax whitespace))
                            "\""
                            (group (1+ (not (any ?\"))))
                            "\"")
                        s)
      (setq dir (match-string-no-properties 1 s))
      (with-current-buffer gnuplot-buffer
        (setq default-directory dir)))))
(defvar gnuplot-buffer nil
  "Buffer associated with the inferior gnuplot process (if not nil).")

;; comint-preoutput-filter-functions
(defun gnuplot-make-inf-process ()
  "Create a new inferior gnuplot process if one does not exist.

See also `gnuplot-buffer'."
  (interactive)
  (let (b)
    (unless (comint-check-proc gnuplot-buffer)
      (setq b (apply 'make-comint "gnuplot" (list "gnuplot")))
      (set-buffer b)
      (inferior-gnuplot-mode))
    (pop-to-buffer b)
    (setq gnuplot-buffer (buffer-name b))))

(defun gnuplot-start-process ()
  "Start an inferior gnuplot process in the background."
  (save-window-excursion
    (gnuplot-make-inf-process)))

(defun gnuplot-setup-comint-variables ()
  "Setup local variables for comint interaction with gnuplot."
  (set (make-local-variable 'indent-line-function) 'gnuplot-indent-line)
  (progn (make-variable-buffer-local 'comint-input-filter-functions)
         (add-hook 'comint-input-filter-functions '%gnuplot-watch-for-cd)))


(defun gnuplot-get-process ()
  "Return the current inferior gnuplot process or nil if none is running."
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

(defun gnuplot-indent-or-complete ()
  "Indent, if point doesn't move, try and complete."
  (interactive)
  (let ((p (point))
        (comint-completion-addsuffix (cons "/" "")))
    (call-interactively 'indent-for-tab-command)
    (when (= p (point))
      (cond ((nth 3 (syntax-ppss (point)))
             (comint-dynamic-complete-filename))
            (t nil)))))

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
    ta) )

(defvar gnuplot-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") 'gnuplot-plot-file)
    (define-key m (kbd "C-c C-b") 'gnuplot-send-buffer-to-gnuplot)
    (define-key m (kbd "C-c C-r") 'gnuplot-send-region-to-gnuplot)
    (define-key m (kbd "RET") 'newline-and-indent)
    (define-key m (kbd "TAB") 'gnuplot-indent-or-complete)
    m))

(define-derived-mode gnuplot-mode fundamental-mode "Gnuplot"
  "Major mode for editing gnuplot plot files.

\\{gnuplot-mode-map}"
  :syntax-table gnuplot-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       gnuplot-font-lock-defaults)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) nil)
  (set (make-local-variable 'indent-line-function) 'gnuplot-indent-line)
  (set-syntax-table gnuplot-mode-syntax-table))



(defvar %gnuplot-commands-regexp
  (rx bow
          (or                           ; Commands
           "call" "cd" "clear" "exit" "fit" "help" "history" "if"
           "load" "pause" "plot" "print" "pwd" "quit" "replot" "reread"
           "reset" "save" "set" "shell" "show" "splot" "system" "test"
           "unset" "update")
          eow))

(defvar %gnuplot-options-regexp
  (rx (group
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
       eow)))

(defvar %gnuplot-modifiers-regexp
  (rx bow
      (or                           ;Plot modifiers
       "linewidth" "linetype" "pointsize" "points" "lines" "linestyle")
      eow))

(defvar gnuplot-font-lock-keywords
  `((,%gnuplot-commands-regexp
     . font-lock-keyword-face)
    (,%gnuplot-options-regexp
     . font-lock-type-face)
    (,%gnuplot-modifiers-regexp
     . font-lock-variable-name-face)))

(defvar gnuplot-font-lock-defaults
  '(gnuplot-font-lock-keywords nil t))

(defvar gnuplot-basic-indent 5
  "Indentation level in `gnuplot-mode' buffers.")

(defun gnuplot-indent-line ()
  "Indent the current line in a gnuplot buffer.

See `gnuplot-basic-indent' to control how large the indentation is."
  (interactive)
  (let* ((pos (- (point-max) (point)))
         (continuation-p
          (save-excursion (end-of-line 0)
                          (eq (char-before) ?\\)))
         (command-p (save-excursion
                      (forward-line (if continuation-p -1 0))
                      (looking-at %gnuplot-commands-regexp)))
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

(defun %gnuplot-send-string (s)
  "Send S to the inferior gnuplot process."
  (let ((p (gnuplot-process)))
  (comint-send-string p s)
  (comint-send-string p "\n")))

(defun %gnuplot-setwd ()
  "Change to the current directory in the inferior gnuplot process."
  (%gnuplot-send-string
   (format "cd \"%s\"\n"
           (file-name-directory (or (buffer-file-name) default-directory)))))

(defun gnuplot-send-buffer-to-gnuplot ()
  "Send the current buffer to an inferior gnuplot process."
  (interactive)
  (gnuplot-send-region-to-gnuplot (point-min) (point-max)))

(defun gnuplot-send-region-to-gnuplot (b e)
  "Send the region between B and E to an inferior gnuplot process."
  (interactive "r")
  (%gnuplot-setwd)
  (comint-send-region (gnuplot-process) b e)
  (comint-send-string (gnuplot-process) "\n"))

(defun gnuplot-plot-file ()
  "Load the current file in an inferior gnuplot process."
  (interactive)
  (let ((b (current-buffer)))
    (when (and (buffer-modified-p b)
               (y-or-n-p (format "%s modified; save? " (buffer-name b))))
      (save-buffer)))
  (%gnuplot-setwd)
  (%gnuplot-send-string (format "load \"%s\"\n" (buffer-file-name)))
  (gnuplot-maybe-generate-pdf))

(defun gnuplot-maybe-generate-pdf ()
  "If the current gnuplot file generates an EPS figure, also epstopdf it."
  (save-excursion
    (goto-char (point-min))
    (let (output)
      (while (re-search-forward
              (rx "set"
                  (1+ (syntax whitespace))
                  "output"
                  (1+ (syntax whitespace))
                  "\""
                  (group (1+ (not (any ?\"))))
                  "\"")
              nil t)
        (when (not (nth 4 (syntax-ppss)))
          (setq output (match-string-no-properties 1))))
      (when (and output
                 (string-match (rx ".eps" string-end) output))
        (setq output (expand-file-name output))
        (run-at-time 15 nil (lambda (o)
                              (call-process "epstopdf" nil 0 nil o)
                              (message "gnuplot: PDF figure generated"))
                     output)))))
                     
(when (require 'info-look nil t)
  (info-lookup-maybe-add-help
   :mode 'gnuplot-mode :topic 'symbol
   :regexp "[a-zA-Z][_a-zA-Z0-9]*"
   :doc-spec '(("(gnuplot)Concept_Index" nil "[_a-zA-Z0-9]+")
               ("(gnuplot)Options_Index" nil "[_a-zA-Z0-9]+")
               ("(gnuplot)Function_Index" nil "[_a-zA-Z0-9]+")
               ("(gnuplot)Terminal_Index" nil "[_a-zA-Z0-9]+"))))

(provide 'gnuplot-mode)
;;; gnuplot-mode.el ends here
