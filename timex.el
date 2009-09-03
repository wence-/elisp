;;; timex.el --- Parse and display timeclock data from timex
;; Copyright (C) 2009 Lawrence Mitchell <wence@gmx.li>
;; File: timex.el
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2009-09-01
;; Version: 1
;; Keywords: calendar data

;;; Commentary:
;; Parse data from timex scheduling and timeclock files and display
;; the result in a nice way.  Better than perl.

;;; Code:

(defvar timex-schedule-dir "~ag/html-local/schedules"
  "Directory containing schedule files.")

(defvar timex-user user-login-name
  "Username of this timex user.")

(defvar timex-days-dir "~/.timex"
  "Directory containing timeclock data.")

(defun timex-parse-schedule (&optional user month year)
  "Parse the schedule for USER in MONTH and YEAR into a list.

If USER is unspecified user `timex-user'.  If MONTH and/or YEAR are
unspecified, use those returned by `current-time'.

Return a list of (cons TOTAL-HOURS
                       ((PROJECT1 . HOURS1)
                        ...))."
  (let* ((month (downcase (format-time-string
                           "%b"
                           (when month (encode-time
                                        1 1 1 1 month 2009)))))
         (year (format-time-string
                "%y"
                (when year (encode-time
                            1 1 1 1 1 year))))
         (user (or user timex-user))
         (sched-file (format "%s/%s%s/%s"
                             timex-schedule-dir
                             month year user))
         (total 0)
         ret)
    (when (file-exists-p sched-file)
      (with-temp-buffer
        (insert-file-contents-literally sched-file)
        (search-forward "=============" nil t)
        (save-excursion
          (while (search-forward ":" nil t)
            (replace-match " " nil t)))
        (forward-line 3)
        (save-restriction
          (narrow-to-region (point)
                            (save-excursion
                              (search-forward "\n\n" nil t)
                              (match-beginning 0)))
          (while (not (eobp))
            (let* ((proj (read (current-buffer)))
                   (task (read (current-buffer)))
                   (hours (read (current-buffer))))
              (push (cons (format "%s:%s" proj task) hours) ret)
              (incf total hours)
              (forward-line 1)))))
      (cons total (sort ret (lambda (a b)
                              (string-lessp (car a) (car b))))))))

(defun timex-parse-file (file)
  "Parse a single timeclock FILE.

Return a list of (PROJECT . HOURS) pairs."
  (let (ret)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (switch-to-buffer (current-buffer))
      (save-restriction
        (while (looking-at "#")
          (forward-line 1))
        (narrow-to-region (point) (point-max))
        (save-excursion
          (while (search-forward ":" nil t)
            (replace-match " " nil t)))
        (while (not (eobp))
          (let* ((hours (+ (read (current-buffer))
                           (/ (read (current-buffer))
                              60.0)))
                 (proj (read (current-buffer)))
                 (task (read (current-buffer))))
            (push (list proj task hours) ret)
            (forward-line 1)))))
    ret))

(defun timex-parse-files (&optional month year files)
  "Parse timeclock data from MONTH and YEAR into a list.

If FILES is non-nil, just parse the list specified there.
If FILES is nil, use the result of `timex-month-files'.
If MONTH and/or YEAR are nil use the value from `current-time'.

Return a list of (cons TOTAL-HOURS
                       ((PROJECT1 . HOURS1)
                        ...))."
  (let ((files (or files (timex-month-files month year))))
    (loop for f in files
          with result = nil
          with total = 0
          do (incf total
                   (loop for (proj task hours) in (timex-parse-file f)
                         if (assoc (format "%s:%s" proj task)
                                   result)
                         do (incf (cdr (assoc (format "%s:%s" proj task)
                                              result))
                                  hours)
                         else do (push (cons (format "%s:%s" proj task)
                                             hours)
                                       result)
                         sum hours into total
                         finally (return total)))
          finally (return (cons total
                                (sort result (lambda (a b)
                                               (string-lessp (car a)
                                                             (car b)))))))))
(defun timex-read-date ()
  "Read a freeform date from a string and return a value like `decode-time'.

Uses `parse-time-string' internally, but uses defaults from
`current-time' rather than nil values for unknown entries."
  (condition-case err
      (let ((time (parse-time-string
                   (read-string "Date (somewhat freeform):"
                                (format-time-string "%e %b %y"))))
            (ctime (decode-time)))
        (loop for elem in time
              for i = 0 then (1+ i)
              unless (null elem)
              do (setf (nth i ctime) elem))
        ctime)
    (decode-time)))

(defsubst timex-leap-year-p (year)
  "Return t if YEAR is a Gregorian leap year.
A negative year is interpreted as BC; -1 being 1 BC, and so on."
  ;; 1 BC = 0 AD, 2 BC acts like 1 AD, etc.
  (if (< year 0) (setq year (1- (abs year))))
  (and (zerop (% year 4))
       (or (not (zerop (% year 100)))
           (zerop (% year 400)))))

(defsubst timex-last-day-of-month (month year)
  "The last day in MONTH during YEAR."
  (if (and (= month 2) (calendar-leap-year-p year))
      29
    (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))

(defun timex-month-files (&optional month year)
  "Return all timeclock files in MONTH and YEAR.

If either MONTH or YEAR are nil, use the values from `current-time'.

Return a list of all files containing timeclock data."
  (let* ((time (decode-time))
         (month (or month (nth 4 time)))
         (year (or year (nth 5 time)))
         (first-day 1)
         (last-day (timex-last-day-of-month month year)))
    (loop for day from first-day to last-day
          for f = #1=(format "%s/%s-%02d-%02d" timex-days-dir year month day)
                then #1#
          when (file-exists-p f)
          collect f)))

(defun timex-number-of-weeks-in-month (&optional month year)
  "Return the (fractional) number of working weeks in MONTH and YEAR.

If either MONTH or YEAR are nil, use the valuse from `current-time'."
  (let* ((time (decode-time))
         (month (or month (nth 4 time)))
         (year (or year (nth 5 time)))
         (first-day 1)
         (last-day (timex-last-day-of-month month year)))
    (setf (nth 4 time) month)
    (setf (nth 5 time) year)
    (/ (loop for day from first-day to last-day
             when (memq (nth 6 (decode-time (apply 'encode-time
                                                   (progn
                                                     (setf (nth 3 time) day)
                                                     time))))
                        '(1 2 3 4 5))
             sum 1) 5.0)))

(defun timex-week-files (&optional when)
  "Return list of one week's timeclock files.

If WHEN is non-nil it should specify a week relative to the current
one.  For example, if when is -1, return the list of timeclock files
from last week's work."
  (let ((time (decode-time))
        dow week-start week-end)
    (when when
      (incf (nth 3 time) (* when 7)))
    (setq dow (nth 6 time))
    (setq week-start (- (nth 3 time) dow -1))
    (setq week-end (+ (nth 3 time) (- 5 dow)))
    (loop for i from week-start to week-end
          for f = #1=(format-time-string
                  (format "%s/%s" timex-days-dir "%Y-%m-%d")
                  (progn (setf (nth 3 time) i)
                         (apply 'encode-time time)))
          then #1#
          when (file-exists-p f)
          collect f)))

(defun timex-format-line (data schedule weeks-in-month)
  "Pretty print a single task in DATA along with hours from SCHEDULE."
  (destructuring-bind (project . time) data
    (format "%25s %8.1f %8.1f %8.1f\n" project time
            #1=(/ (or (cdr (assoc project schedule))
                      0) weeks-in-month)
            (- #1# time))))

(defmacro with-timex-results-buffer (buf &rest body)
  "Execute forms in BODY with BUF temporarily current.

Like `with-current-buffer', but displays BUF (using
`display-buffer') and sets the major mode to `timex-results-mode'."
  `(with-current-buffer ,buf
     ,@body
     (timex-results-mode)
     (display-buffer ,buf)))

(put 'with-timex-results-buffer 'lisp-indent-function 1)
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(with-timex-results-buffer\\)\\>" 
                           1 font-lock-keyword-face)))

(defun timex-pretty-print (buf data schedule total scheduled-total
                           &optional weeks-in-month)
  "Pretty print into BUF timeclock DATA.

Also prints hours from SCHEDULE along with TOTAL worked and
SCHEDULED-TOTAL hours."
  (with-current-buffer buf
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (format "%25s   Worked   Target   Hours left\n" "Project"))
    (insert (make-string 56 ?=) "\n")
    (unless weeks-in-month
      (setq weeks-in-month 1))
    (insert (mapconcat #'identity (loop for item in data
                                        collect
                                        (timex-format-line
                                         item schedule weeks-in-month))
                       ""))
    (loop for (project . time) in schedule
          when (null (assoc project data))
          do (insert (format "%25s %8.1f %8.1f %8.1f\n"
                             project 0 (/ time weeks-in-month)
                             (/ time weeks-in-month))))
    (insert (make-string 56 ?=) "\n")
    (insert (format "%25s %8.1f %8.1f %8.1f"
                    "TOTALS" total (/ (or scheduled-total 0) weeks-in-month)
                    (- (/ (or scheduled-total 0) weeks-in-month) total)))))

(defun timex-pretty-month (&optional user month year)
  "Pretty print a month of timeclock data.

If USER is nil, use `timex-user'.  If MONTH and/or YEAR are nil, use
the value from `current-time'.

Pops up a buffer \"*timex*\" containing the prettified result."
  (let* ((data (timex-parse-files month year))
         (total (car data))
         (schedule (timex-parse-schedule user month year))
         (sched-total (car schedule))
         (buf (get-buffer-create "*timex-cal-month*")))
    (setq schedule (cdr schedule))
    (setq data (cdr data))
    (timex-pretty-print buf data schedule total sched-total)
    (with-timex-results-buffer buf
      (goto-char (point-min))
      (save-excursion
        (insert (format-time-string "Timeclock data for %B %Y\n\n"
                                    (encode-time 1 1 1 1 month year)))))))

(defun timex-pretty-week (&optional user relative-week)
  "Pretty print USER's timeclock hours for a week.

If RELATIVE-WEEK is non-nil print the specified week.  For example, if
RELATIVE-WEEK is -1 print last week's hours."
  (let* ((data (timex-parse-files nil nil
                                  (timex-week-files relative-week)))
         (total (car data))
         (time (decode-time (apply 'encode-time
                                   (let ((tmp (decode-time)))
                                     (incf (nth 3 tmp)
                                           (* (or relative-week 0) 7))
                                     tmp))))
         (schedule (timex-parse-schedule user (nth 4 time) (nth 5 time)))
         (sched-total (car schedule))
         (buf (get-buffer-create "*timex-week*")))
    (setq schedule (cdr schedule))
    (setq data (cdr data))
    (timex-pretty-print buf data schedule total sched-total
                        (timex-number-of-weeks-in-month (nth 4 time)
                                                        (nth 5 time)))
    (with-timex-results-buffer buf
      (goto-char (point-min))
      (save-excursion
        (insert
         (format-time-string "Timeclock data for WEF %Y-%m-%d\n\n"
                             (progn
                               (incf (nth 3 time)
                                     (- 5 (nth 6 time)))
                               (setf (nth 6 time) 5)
                               (apply 'encode-time time))))))))

(defun timex-pretty-day (&optional date)
  "Pretty print the timeclock hours from DATE.

If DATE is nil, use today.  DATE should be a value like that returned
from `decode-time'."
  (let* ((data (timex-parse-files
                nil nil
                (list (format-time-string
                       (format "%s/%s" timex-days-dir "%Y-%m-%d")
                       (and date (apply 'encode-time date))))))
         (total (car data))
         (buf (get-buffer-create "*timex-day*")))
    (setq data (cdr data))
    (with-timex-results-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (save-excursion
        (insert (format-time-string "Timeclock data for %a, %b %-e %Y\n\n"
                                    (and date (apply 'encode-time date))))
        (insert (format "%25s    Hours\n" "Project"))
        (insert (make-string 34 ?=) "\n")
        (insert (format "%25s %8.1f\n" "TOTAL"
                        (loop for (proj . hours) in data
                              do (insert
                                  (format "%25s %8.1f\n" proj hours))
                              sum hours into total
                              finally
                              (progn (insert (make-string 34 ?=) "\n")
                                     (return total)))))))))

(defun timex-print-month (&optional promptp)
  "Display a month of timeclock data.

With prefix arg, or if PROMPTP is non-nil, prompt for month to display."
  (interactive "P")
  (let* ((time (decode-time))
         (month (nth 4 time))
         (year (nth 5 time)))
    (when promptp
      (setq month (read-number "Which month? " month))
      (setq year (read-number "Which year? " year)))
    (timex-pretty-month timex-user month year)))

(defun timex-print-week (&optional promptp)
  "Display a week of timeclock data.

With prefix arg, or if PROMPTP is non-nil, prompt for week to display."
  (interactive "P")
  (timex-pretty-week timex-user
                     (when promptp
                       (read-number "Which week, relative to current? " 0))))

(defun timex-print-day (&optional promptp)
  "Display a day of timeclock data.

With prefix arg, or if PROMPTP is non-nil, prompt for the day to display."
  (interactive "P")
  (timex-pretty-day (when promptp (timex-read-date))))

(defmacro defun-timex-call-with-arg (sym)
  (let ((name (intern (format "timex-print-specified-%s" sym)))
        (oname (intern-soft (format "timex-print-%s" sym))))
    (when oname
      `(defun ,name ()
         ,(format "Display a %s of timeclock data.

Prompt for the %s to display, see also `%s'." sym sym oname)
         (interactive)
         (,oname 1)))))

(defun-timex-call-with-arg day)
(defun-timex-call-with-arg week)
(defun-timex-call-with-arg month)

(defun timex-set-user (user)
  "Set `timex-user' to USER."
  (interactive "sTimex username: ")
  (unless (string= user "")
    (setq timex-user user)))

(defun timex-quit ()
  "Quit the current paste buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

(defvar timex-help
  (concat "Commands:\n\n"
          "`\\[timex-set-user]' -- timex-set-user.\n"
          "       Set the username for finding schedules.\n\n"
          "`\\[timex-print-day]' -- timex-print-day\n"
          "       Print hours worked today.\n\n"
          "`\\[timex-print-week]' -- timex-print-week.\n"
          "       Print hours worked in the current week.\n\n"
          "`\\[timex-print-month]' -- timex-print-month\n"
          "       Print hours worked in the current month.\n\n"
          "`\\[timex-print-specified-day]' -- timex-print-specified-day\n"
          "         Print hours worked on a specified day.\n\n"
          "`\\[timex-print-specified-week]' -- timex-print-specified-week\n"
          "         Print hours worked in a specified week\n"
          "         (relative to the current week)\n\n"
          "`\\[timex-print-specified-month]' -- timex-print-specified-month\n"
          "         Print hours worked in a specified month.\n\n"
          "`\\[timex-quit]' -- timex-quit\n"
          "       Quit the timex interface.\n"))

(defvar timex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'timex-set-user)
    (define-key map "m" 'timex-print-month)
    (define-key map "w" 'timex-print-week)
    (define-key map "d" 'timex-print-day)
    (define-key map (kbd "o m") 'timex-print-specified-month)
    (define-key map (kbd "o w") 'timex-print-specified-week)
    (define-key map (kbd "o d") 'timex-print-specified-day)
    (define-key map (kbd "q") 'timex-quit)
    map)
  "Keymap for `timex-mode'.")

(define-derived-mode timex-mode fundamental-mode "Timex"
  "Major mode for interacting with timeclock records from timex."
  (insert "Top-level interface to timex\n\n"
          (substitute-command-keys timex-help))
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defvar timex-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'timex-quit)
    map)
  "Keymap for `timex-results-mode'.")

(define-derived-mode timex-results-mode fundamental-mode "Timex data"
  "Major mode for viewing data from timeclock records."
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun timex ()
  "Top-level interface to timex."
  (interactive)
  (switch-to-buffer (get-buffer-create "*timex*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (timex-mode))

(provide 'timex)

;;; timex.el ends here
