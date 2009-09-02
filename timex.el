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

(defun timex-schedule (&optional user month year)
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
         (last-day (calendar-last-day-of-month month year)))
    (loop for day from first-day to last-day
          for f = #1=(format "%s/%s-%02d-%02d" timex-days-dir year month day)
                then #1#
          when (file-exists-p f)
          collect f)))


(defun timex-parse-files (&optional month year files)
  "Parse the list of timeclock FILES from MONTH and YEAR into a list.

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
                                             

(defun timex-format-line (data schedule)
  "Pretty print a single task in DATA along with hours from SCHEDULE."
  (destructuring-bind (project . time) data
    (format "%25s %8.1f %8.1f\n" project time
            (or (cdr (assoc project schedule))
                0))))

(defun timex-pretty-calendar-month (&optional user month year)
  "Pretty print a month of timeclock data.

If USER is nil, use `timex-user'.  If MONTH and/or YEAR are nil, use
the value from `current-time'.

Pops up a buffer \"*timex*\" containing the prettified result."
  (let* ((data (timex-parse-files month year))
         (total (car data))
         (schedule (timex-schedule user month year))
         (sched-total (car schedule))
         (buf (get-buffer-create "*timex-cal-month*")))
    (setq schedule (cdr schedule))
    (setq data (cdr data))
    (timex-pretty-print buf data schedule total sched-total)
    (with-current-buffer buf
      (goto-char (point-min))
      (save-excursion
        (insert (format-time-string "Timeclock data for %B %Y\n\n"
                                    (encode-time 1 1 1 1 month year)))))
              
    (display-buffer buf)))

(defun timex-pretty-print (buf data schedule total scheduled-total)
  "Pretty print into BUF timeclock DATA.

Also prints hours from SCHEDULE along with TOTAL worked and
SCHEDULED-TOTAL hours."
  (with-current-buffer buf
    (erase-buffer)
    (insert (format "%25s   Cumul.   Target\n" "Project"))
    (insert (make-string 43 ?=) "\n")
    (insert (mapconcat #'identity (loop for item in data
                                        collect
                                        (timex-format-line item schedule))
                       ""))
    (loop for (project . time) in schedule
          when (null (assoc project data))
          do (insert (format "%25s %8.1f %8.1f\n"
                             project 0 time)))
    (insert (make-string 43 ?=) "\n")
    (insert (format "%25s %8.1f %8.1f" "TOTALS" total (or scheduled-total 0)))))

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
         (schedule (timex-schedule user (nth 4 time) (nth 5 time)))
         (sched-total (car schedule))
         (buf (get-buffer-create "*timex-week*")))
    (setq schedule (cdr schedule))
    (setq data (cdr data))
    (timex-pretty-print buf data schedule total sched-total)
    (with-current-buffer buf
      (goto-char (point-min))
      (save-excursion
        (insert
         (format-time-string "Timeclock data for WEF %Y-%m-%d\n\n"
                             (progn
                               (incf (nth 3 time)
                                     (- 5 (nth 6 time)))
                               (setf (nth 6 time) 5)
                               (apply 'encode-time time))))))
    (display-buffer buf)))

(defun timex-set-user (user)
  "Set `timex-user' to USER."
  (interactive "sTimex username: ")
  (setq timex-user user))

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
    (timex-pretty-calendar-month timex-user month year)))

(defun timex-print-week (&optional promptp)
  "Display a week of timeclock data.

With prefix arg, or if PROMPTP is non-nil, prompt for week to display."
  (interactive "P")
  (timex-pretty-week timex-user
                     (when promptp
                       (read-number "Which week, relative to current? " 0))))

(defun timex-quit ()
  "Quit the current paste buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

(defvar timex-help
  (concat "Commands:\n\n"
          "`u' -- set `timex-user'.\n\n"
          "`m' -- timex-print-month\n"
          "       Print hours worked in the current month.\n\n"
          "`w' -- timex-print-week.\n"
          "       Print hours worked in the current week.\n\n"
          "`o m' -- timex-print-month\n"
          "         Print hours worked in a specified month.\n\n"
          "`o w' -- timex-print-week\n"
          "         Print hours worked in a specified week\n"
          "         (relative to the current week)\n\n"
          "`q' -- timex-quit\n"
          "       Quit the timex interface.\n"))

(defvar timex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'timex-set-user)
    (define-key map "m" 'timex-print-month)
    (define-key map "w" 'timex-print-week)
    (define-key map (kbd "o m") (lambda () (interactive)
                                  (timex-print-month 1)))
    (define-key map (kbd "o w") (lambda () (interactive)
                                  (timex-print-week 1)))
    (define-key map (kbd "q") 'timex-quit)
    map)
  "Keymap for `timex-mode'.")

(define-derived-mode timex-mode fundamental-mode "Timex"
  "Major mode for viewing timeclock records from timex."
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun timex ()
  "Top-level interface to timex."
  (interactive)
  (switch-to-buffer (get-buffer-create "*timex*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Top-level interface to timex\n\n"
          timex-help)
  (timex-mode))


(provide 'timex)

;;; timex.el ends here
