;;; timex.el --- Parse and display timeclock data from timex
;; Copyright (C) 2009-2011 Lawrence Mitchell <wence@gmx.li>
;; File: timex.el
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2009-09-01
;; Version: 1
;; Keywords: calendar data

;;; Commentary:
;; Parse data from timex scheduling and timeclock files and display
;; the result in a nice way.  Better than perl.
;; Does not currently DTRT for weekly reports when the week spans two
;; schedules.
;;; Code:

(eval-when-compile
  (require 'cl))

(defvar timex-schedule-dir "~ag/html-local/schedules"
  "Directory containing schedule files.")

(defvar timex-user user-login-name
  "Username of this timex user.")

(defvar timex-days-dir "~/.timex"
  "Directory containing timeclock data.")

(defvar timex-january-scheduled-with-december-p nil
  "Is January scheduled along with December?

If this variable is non-nil, it is assumed that hours for December and
January are scheduled in one go and live in the December scheduling
file.")

(defvar timex-created-buffers nil
  "List of buffers created by timex.

Deleted when calling `timex-quit'.")

(defvar timex-saved-window-configuration nil
  "Window configuration when timex interface was started.

Restored when exiting interface with `timex-quit'.")

(defstruct (timex-schedule-data (:conc-name timex.))
  (hours-scheduled 0 :type int)
  (working-days 0 :type int)
  (projects (make-hash-table :test 'equal) :type hash-table))

(defmacro define-timex-time-accessor (name place)
  "Define an inline function `timex-NAME' to access PLACE from `decode-time'.

Additionally, define a setf-expander for `timex-NAME' so that
\(setf (timex-name time-spec) value) works."
  (let ((fn-name (intern (format "timex-%s" name))))
    `(progn (defsubst ,fn-name (time-spec)
              ,(format "Return the %ss value from TIME-SPEC.

TIME-SPEC should be a value from `decode-time'." (upcase (symbol-name name)))
              (nth ,place time-spec))
            (defsetf ,fn-name (x) (store)
              `(setcar (nthcdr ,,place ,x) ,store))
            ',fn-name)))

(define-timex-time-accessor second 0)
(define-timex-time-accessor minute 1)
(define-timex-time-accessor hour 2)
(define-timex-time-accessor day 3)
(define-timex-time-accessor month 4)
(define-timex-time-accessor year 5)
(define-timex-time-accessor dow 6)
(define-timex-time-accessor dst 7)
(define-timex-time-accessor zone 8)

(defun timex-find-schedule-file (&optional time-spec user)
  "Find a schedule file in `timex-schedule-dir'.

If TIME-SPEC (a specification from `decode-time') is non-nil,
find a file from that time, otherwise use the value of
`current-time'.

If USER is nil, use `timex-user'."
  ;; Schedule files are stored in
  ;; `timex-schedule-dir'/MMMYY/USER
  ;; MMM is a downcased three letter month abbreviation
  ;; YY is the two digit year.
  (let* ((time-spec (or (copy-sequence time-spec) (decode-time)))
         (user (if (and (string= timex-user "lmitche4")
                        (= (timex-month time-spec) 8)
                        (= (timex-year time-spec) 2009))
                   "LM"
                 (or user timex-user)))
         (file nil)
         (provisional nil)
         (new-file nil))
    ;; Special case for january, which is conflated with december as
    ;; far as scheduling goes.
    (when (and timex-january-scheduled-with-december-p
               (= (timex-month time-spec) 1))
      (decf (timex-month time-spec)))
    (setq file (format "%s/%s/%s" timex-schedule-dir
                       (downcase (format-time-string
                                  "%b%y"
                                  (apply 'encode-time time-spec)))
                       user))
    (setq provisional (format "%s/provisional/%s" timex-schedule-dir user))
    (setq new-file (format "%s/schedules/%s"
                           timex-days-dir (downcase
                                           (format-time-string
                                            "%b%y"
                                            (apply 'encode-time time-spec)))))
    (cond ((file-exists-p file)
           file)
          ((and (file-exists-p provisional)
                (with-temp-buffer
                  (insert-file-contents-literally provisional)
                  (search-forward (format-time-string
                                   "%B %Y" (apply 'encode-time time-spec))
                                  (point-at-eol) t)))
           provisional)
          ((file-exists-p new-file)
           new-file)
          (t (error "Unable to find %s schedule file for %s"
                    (format-time-string "%B %Y"
                                        (apply 'encode-time time-spec))
                    user)))))
                         

(defsubst timex-weekday-p (time-spec &optional day)
  "Return non-nil if TIME-SPEC denotes a weekday.

If DAY is non-nil, set the day value of TIME-SPEC to that value before
asking if the day is weekday."
  (let ((time-spec (copy-sequence time-spec)))
    (when day
      (setf (timex-day time-spec) day))
    (memq (timex-dow (decode-time (apply 'encode-time time-spec)))
          '(1 2 3 4 5))))

(defmacro timex-increment-time (time-spec place-accessor &optional amount)
  (let ((time (make-symbol "time"))
        (inc (make-symbol "inc")))
    `(let ((,time ,time-spec)
           (,inc (or ,amount 1)))
       (incf (,place-accessor ,time) ,inc)
       (decode-time (apply 'encode-time ,time)))))

(defun timex-number-of-working-days-in-dec-and-jan (year)
  "Return working days in december YEAR and january YEAR+1.

Assumes 23 Dec til 5 Jan inclusive are taken as holidays."
  (let ((time-spec (decode-time))
        ret)
    (setf (timex-year time-spec) year)
    (setf (timex-month time-spec) 12)
    (list (loop for day from 1 to 22
                when (timex-weekday-p time-spec day)
                sum 1)
          (progn
            (setf (timex-month time-spec) 1)
            (setf time-spec (timex-increment-time time-spec timex-year))
            (loop for day from 6 to (timex-last-day-of-month time-spec)
                  when (timex-weekday-p time-spec day)
                  sum 1)))))
                    
        
(defun timex-parse-schedule (&optional time-spec user)
  "Parse the schedule matching TIME-SPEC for USER.

The schedule file is found by `timex-find-schedule-file'.

Return a `timex-schedule-data' struct."
  (let* ((sched-file (timex-find-schedule-file time-spec user))
         (decp (= (timex-month time-spec) 12))
         (janp (memq (timex-month time-spec) '(1 13)))
         (dec-jan-fixup (timex-number-of-working-days-in-dec-and-jan
                         (- (timex-year time-spec) (if janp 1 0))))
         (ret (make-timex-schedule-data)))
    (setq dec-jan-fixup (/ (float (car dec-jan-fixup))
                           (apply '+ dec-jan-fixup)))
    (when (file-exists-p sched-file)
      (with-temp-buffer
        ;; Format is
        ;; "Schedule from A to B"
        ;; =============================================
        ;; Project:               Task          report-to
        ;;
        ;; PROJECT: TASK HOURS NAME
        ;; PROJECT: TASK HOURS NAME
        ;; ...
        ;;
        ;; Project descriptions/Notes:
        ;; More comments here.
        ;; We extract the PROJECT: TASK HOURS lines.
        (insert-file-contents-literally sched-file)
        (search-forward "=============" nil t)
        (save-excursion
          ;; Replace ":" for easy READing.
          (while (search-forward ":" nil t)
            (replace-match " " nil t)))
        ;; Skip to first schedule entry.
        (forward-line 3)
        (save-restriction
          (narrow-to-region (point)
                            (save-excursion
                              ;; Find end of entries
                              (search-forward "\n\n" nil t)
                              (match-beginning 0)))
          (while (not (eobp))
            (let* ((proj (read (current-buffer)))
                   (task (read (current-buffer)))
                   (hours (read (current-buffer))))
              (when (and timex-january-scheduled-with-december-p
                         (or decp janp))
                (setq hours (* (if decp dec-jan-fixup
                                 (- 1 dec-jan-fixup))
                               hours)))
              (puthash (format "%s:%s" proj task) hours (timex.projects ret))
              (incf (timex.hours-scheduled ret) hours)
              (forward-line 1)))))
      ret)))

(defun timex-schedules-for-date-range (d1 d2 &optional user)
  "Return schedules files for range of dates between D1 and D2 (inclusive).

If USER is non-nil, return schedules for that USER, otherwise use
`timex-user'.  See `timex-parse-schedule' for more details."
  (let ((dates (timex-split-date-range d1 d2)))
    (loop for (start end) in dates
          collect (cons (list (timex-month end) (timex-year end))
                        (timex-parse-schedule end user)))))

(defun timex-parse-file (file)
  "Parse a single timeclock FILE.

Return a list of (PROJECT TASK HOURS) tuples."
  (let (ret)
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (delete-trailing-whitespace)
        (save-restriction
          ;; Skip comments
          (while (looking-at "#")
            (forward-line 1))
          ;; Remove empty lines at end.
          (narrow-to-region (point) (save-excursion
                                      (goto-char (point-max))
                                      (skip-chars-backward " \n")
                                      (point)))
          (save-excursion
            ;; Format of entry is HH:MM PROJECT:TASK
            ;; Turn this into HH MM PROJECT TASK for easy READing.
            (while (search-forward ":" nil t)
              (replace-match " " nil t)))
          (while (not (eobp))
            (let* ((hours (+ (read (current-buffer))
                             (/ (read (current-buffer))
                                60.0)))
                   (proj (read (current-buffer)))
                   ;; Pull in all the subtasks, these are later ignored.
                   (task (save-restriction
                           (narrow-to-region (point) (point-at-eol))
                           (loop while (not (eobp))
                                 collect (read (current-buffer))))))
              (push (list proj task hours) ret)
              (forward-line 1))))))
    ret))

(defsubst timex-flatten-task (task)
  ;; Ignore subtasks.
  (format "%s" (if (atom task) task (car task))))

(defsubst timex-project (proj task)
  (format "%s:%s" proj (timex-flatten-task task)))

(defun timex-parse-files (files)
  "Parse timeclock data in FILES.

Return a list of (cons TOTAL-HOURS
                       ((PROJECT1 . HOURS1)
                        ...))."
  (loop for f in files
        with result = nil
        with total = 0
        do (incf total
                 (loop for (proj task hours) in (timex-parse-file f)
                       ;; We've already seen this project
                       if (assoc (timex-project proj task) result)
                       ;; Increment the number of worked hours
                       do (incf (cdr (assoc (timex-project proj task) result))
                                hours)
                       ;; otherwise tart a new entry
                       else do (push (cons (timex-project proj task) hours)
                                     result)
                       ;; keep track of total
                       sum hours into total
                       finally (return total)))
        finally (return (cons total
                              (sort result (lambda (a b)
                                             (string-lessp (car a)
                                                           (car b))))))))
(defun timex-read-date (&optional prompt)
  "Read a freeform date from a string and return a value like `decode-time'.

Uses `parse-time-string' internally, but uses defaults from
`current-time' rather than nil values for unknown entries.

If PROMPT is non-nil, use that, otherwise use default:
\"Date (somewhat freeform): \"."
  (condition-case err
      (let ((time (parse-time-string
                   (read-string (or prompt "Date (somewhat freeform): ")
                                (format-time-string "%e %b %y"))))
            (ctime (decode-time)))
        (loop for elem in time
              for i = 0 then (1+ i)
              unless (null elem)
              do (setf (nth i ctime) elem))
        (decode-time (apply 'encode-time ctime)))
    (decode-time)))

(defsubst timex-leap-year-p (year)
  "Return t if YEAR is a Gregorian leap year.
A negative year is interpreted as BC; -1 being 1 BC, and so on."
  ;; 1 BC = 0 AD, 2 BC acts like 1 AD, etc.
  (if (< year 0) (setq year (1- (abs year))))
  (and (zerop (% year 4))
       (or (not (zerop (% year 100)))
           (zerop (% year 400)))))

(defsubst timex-last-day-of-month (time-spec)
  "The last day in the month given in TIME-SPEC."
  (let ((month (timex-month time-spec))
        (year (timex-year time-spec)))
    (if (and (= month 2) (timex-leap-year-p year))
        29
      (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month)))))

(defun timex-files-in-date-range (d1 d2)
  "Return all the files in the date range D1 to D2 (inclusive).

Dates should be specified like the return value from `decode-time'."
  (setf d1 (copy-sequence d1))
  (setf d2 (copy-sequence d2))
  (flet ((file-name (d1)
          (let (unarchived archived)
            (setq unarchived
                  (format "%s/%s" timex-days-dir "%Y-%m-%d"))
            (setq archived
                  (format "%s/%s" timex-days-dir "%Y/%m-%d"))
            (setq unarchived (format-time-string unarchived
                                                 (apply 'encode-time d1)))
            (setq archived (format-time-string archived
                                               (apply 'encode-time d1)))
            (incf (timex-day d1))
            (if (file-exists-p unarchived)
                unarchived
              archived))))
    (or (loop while (or (time-less-p (apply 'encode-time d1)
                                     (apply 'encode-time d2))
                        (equal d1 d2))
              for f = (file-name d1) then (file-name d1)
              when (file-exists-p f)
              collect f)
        (list nil))))

(defun timex-month-files (&optional time-spec)
  "Return all timeclock files in the month matching TIME-SPEC.

If TIME-SPEC is nil, use the value of `current-time'.

Return a list of all files containing timeclock data."
  (let* ((d1 (or (copy-sequence time-spec) (decode-time)))
         (d2 (copy-sequence d1)))
    (setf (timex-day d1) 1)
    (setf (timex-day d2) (timex-last-day-of-month d2))
    (timex-files-in-date-range d1 d2)))

(defun timex-number-of-weeks-in-month (&optional time-spec)
  "Return the number of working weeks in the month given in TIME-SPEC.

If TIME-SPEC is nil, use the value from `current-time'."
  (setf time-spec (or (copy-sequence time-spec) (decode-time)))
  (let* ((ndays (loop for day from 1 to (timex-last-day-of-month time-spec)
                      ;; DOW \in [0, ..., 6].  0 is sunday, 1 monday, etc.
                      when (timex-weekday-p time-spec day)
                      sum 1))
         (janp (and timex-january-scheduled-with-december-p
                    (= (timex-month time-spec) 1)))
         (decp (and timex-january-scheduled-with-december-p
                    (= (timex-month time-spec) 12)))
         (fixup (timex-number-of-working-days-in-dec-and-jan
                 (- (timex-year time-spec) (if janp 1 0))))
         (nweeks (/ ndays 5.0)))
    (cond (janp
           (* nweeks (/ (float (cadr fixup)) ndays)))
          (decp
           (* nweeks (/ (float (car fixup)) ndays)))
          (t nweeks))))

(defun timex-week-files (&optional time-spec)
  "Return list of one week's timeclock files.

If TIME-SPEC is non-nil, it should be a value like `decode-time'
specifying a date.  The week is taken to be the week containing that
date."
  (let* ((d1 (or (copy-sequence time-spec) (decode-time)))
         (d2 (copy-sequence d1))
         (dow (timex-dow d1)))
    ;; Fix up relative to what day today is.
    (setf (timex-day d1) (- (timex-day d1) dow -1))
    (setf (timex-day d2) (+ (timex-day d2) (- 5 dow)))
    (timex-files-in-date-range d1 d2)))

(defsubst timex-padleft (str len)
  "Pad STR to a minimum of LEN characters."
  (format (format "%%%ds" len) str))

(defun timex-format-line (data schedule weeks-in-month len)
  "Pretty print a single task in DATA along with hours from SCHEDULE.

LEN is the minimum width of the project string."
  (destructuring-bind (project . time) data
    ;; Heuristic for how many hours we should have worked.
    ;; Assumes uniform distribution of hours per week.
    (let ((target-hours (/ (gethash project (timex.projects schedule) 0)
                           weeks-in-month)))
      (format "%s %8.1f %8.1f %8.1f\n"
              (timex-padleft project len) time
              target-hours
              ;; Did we over- or under-shoot?
              (- target-hours time)))))

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

(defun timex-pretty-print (buf data schedule total
                           &optional weeks-in-month keep-contents)
  "Pretty print into BUF timeclock DATA.

Also prints hours from SCHEDULE along with TOTAL worked and
the total number of scheduled hours.

If KEEP-CONTENTS is non-nil, don't erase the buffer before starting."
  (with-current-buffer buf
    (setq buffer-read-only nil)
    (unless keep-contents (erase-buffer))
    (let* ((max-namelen (loop for p being the hash-keys of
                                 (timex.projects schedule)
                              maximize (length p)))
           (heading (format "%s   Worked   Target   Hours left\n"
                            (timex-padleft "Project" max-namelen)))
           (len (length heading))
           target-hours)
      (insert heading)
      ;; Could probably use nice drawing characters.
      (insert (make-string len ?=) "\n")
      (unless weeks-in-month
        ;; Fix divisor for target hours up correctly.  If
        ;; `weeks-in-month' is nil, assume we're printing a month of
        ;; data so don't divide by anything.
        (setq weeks-in-month 1))
      (setq target-hours (/ (timex.hours-scheduled schedule) weeks-in-month))
      (loop for item in data
            do (insert (timex-format-line item schedule weeks-in-month
                                          max-namelen)))
      (maphash (lambda (project time)
                 (when (null (assoc project data))
                   (insert (format "%s %8.1f %8.1f %8.1f\n"
                                   (timex-padleft project max-namelen)
                                   0 (/ time weeks-in-month)
                                   (/ time weeks-in-month)))))
               (timex.projects schedule))
      (insert (make-string len ?=) "\n")
      (insert (format "%s %8.1f %8.1f %8.1f"
                      (timex-padleft "TOTALS" max-namelen)
                      total target-hours (- target-hours total))))))

(defun timex-split-date-range (d1 d2)
  "Split the date range D1 to D2 (inclusive) into separate months."
  (let ((d1 (copy-sequence d1))
        (d2 (copy-sequence d2))
        ret)
    (if (= (timex-month d1) (timex-month d2))
        ;; Only one month, just return the end points.
        (push (list d1 d2) ret)
      (push (list (copy-sequence d1)
                  ;; Start point and end of the first month.
                  (progn
                    (setf (timex-day d1) (timex-last-day-of-month d1))
                    (copy-sequence d1)))
            ret)
      ;; go to the beginning of the next month
      (setf (timex-day d1) 1)
      (setf d1 (timex-increment-time d1 timex-month))
      (setf (timex-second d1) (timex-second d2))
      (setf (timex-minute d1) (timex-minute d2))
      (setf (timex-hour d1) (timex-hour d2))
      (let ((intermediates
             ;; Collect the intermediate months
             (loop while (and (time-less-p
                               ;; Don't pick up an intermediate month
                               ;; if the end of the month will go past
                               ;; the end of d2.
                               (apply 'encode-time
                                      (let ((d (copy-sequence d1)))
                                        (setf (timex-day d)
                                              (timex-last-day-of-month d))
                                        d))
                               (apply 'encode-time d2)))
                   collect (prog1 (list (copy-sequence d1)
                                        (progn
                                          (setf (timex-day d1)
                                                (timex-last-day-of-month d1))
                                          (copy-sequence d1)))
                             (setf (timex-day d1) 1)
                             (setf d1 (timex-increment-time
                                       d1 timex-month))))))
        (when intermediates
          ;; If we got any intermediate months shove them onto the
          ;; return value.  Need to reverse the intermediates because
          ;; we're putting everything on backwards.
          (setf ret (nconc (reverse intermediates) ret))))
      ;; Get the last month.
      (push (list d1 d2) ret))
    (reverse ret)))
                  
(defun timex-pretty-date-range (d1 d2 &optional user)
  "Pretty print all timeclock data between D1 and D2 (inclusive).

If USER is nil, use `timex-user'."
  (let ((buf (get-buffer-create "*timex-data*")))
    (push buf timex-created-buffers)
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Just insert the data for each month separately.
    (loop for (d1 d2) in (reverse (timex-split-date-range d1 d2))
          do (let* ((data (timex-parse-files (timex-files-in-date-range d1 d2)))
                    (total (car data))
                    (schedule (timex-parse-schedule d1 user)))
               (setq data (cdr data))
               (timex-pretty-print buf data schedule total nil t)
               (with-timex-results-buffer buf
                 ;; Separator between entries
                 (insert "\n\n\n")
                 (goto-char (point-min))
                 (save-excursion
                   (insert (format
                            "Timeclock data for period %s to %s\n"
                            (format-time-string "%e %b %Y"
                                                (apply 'encode-time d1))
                            (format-time-string "%e %b %Y"
                                                (apply 'encode-time d2))))))))))

(defun timex-pretty-month (&optional time-spec user)
  "Pretty print a month of timeclock data.

If USER is nil, use `timex-user'.  If TIME-SPEC is nil, use the
value from `current-time'.

Pops up a buffer \"*timex*\" containing the prettified result."
  (let* ((data (timex-parse-files (timex-month-files time-spec)))
         (total (car data))
         (schedule (timex-parse-schedule time-spec user))
         (buf (get-buffer-create "*timex-cal-month*")))
    (push buf timex-created-buffers)
    (setq data (cdr data))
    (timex-pretty-print buf data schedule total)
    (with-timex-results-buffer buf
      (goto-char (point-min))
      (save-excursion
        (insert (format-time-string "Timeclock data for %B %Y\n\n"
                                    (apply 'encode-time time-spec)))))))

(defun timex-pretty-week (&optional user time-spec)
  "Pretty print USER's timeclock hours for a week.

If TIME-SPEC is non-nil, print hours for the week specified."
  (let* ((data (timex-parse-files (timex-week-files time-spec)))
         (total (car data))
         (time-spec (or (copy-sequence time-spec) (decode-time)))
         (schedule (timex-parse-schedule time-spec user))
         (buf (get-buffer-create "*timex-week*")))
    (push buf timex-created-buffers)
    (setq data (cdr data))
    (timex-pretty-print buf data schedule total
                        (timex-number-of-weeks-in-month time-spec))
    (with-timex-results-buffer buf
      (goto-char (point-min))
      (save-excursion
        (insert
         (format-time-string "Timeclock data for WEF %Y-%m-%d\n\n"
                             (apply 'encode-time
                                    (timex-increment-time
                                     time-spec timex-day 
                                     (- 5 (timex-dow time-spec))))))))))

(defun timex-pretty-day (&optional time-spec)
  "Pretty print the timeclock hours from TIME-SPEC.

If TIME-SPEC is nil, use today.  TIME-SPEC should be a value like that returned
from `decode-time'."
  (let* ((time-spec (or (copy-sequence time-spec) (decode-time)))
         (data (timex-parse-files
                (list (format-time-string
                       (format "%s/%s" timex-days-dir "%Y-%m-%d")
                       (apply 'encode-time time-spec)))))
         (max-namelen (loop for (proj . hours) in (cdr data)
                              maximize (length proj)))
         (total (car data))
         (buf (get-buffer-create "*timex-day*"))
         (header (format "%s    Hours\n" (timex-padleft "Project" max-namelen)))
         (len (length header)))
    (push buf timex-created-buffers)
    (setq data (cdr data))
    (with-timex-results-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      ;; We don't use `timex-pretty-print' because we're not
      ;; interested in targets and so only print the actual hours
      ;; worked.
      (save-excursion
        (insert (format-time-string "Timeclock data for %a, %b %-e %Y\n\n"
                                    (apply 'encode-time time-spec)))
        (insert header)
        (insert (make-string len ?=) "\n")
        (insert (format "%s %8.1f\n" (timex-padleft "TOTAL" max-namelen)
                        (loop for (proj . hours) in data
                              do (insert
                                  (format "%s %8.1f\n" (timex-padleft
                                                        proj
                                                        max-namelen) hours))
                              sum hours into total
                              finally
                              (progn (insert (make-string len ?=) "\n")
                                     (return total)))))))))

(defun timex-print-date-range ()
  "Print timeclock data for a specified date range.

Prompts for dates with `timex-read-date'."
  (interactive)
  (timex-pretty-date-range (timex-read-date "Start date: ")
                           (timex-read-date "Finish date: ")))

(defun timex-print-month (&optional promptp)
  "Display a month of timeclock data.

With prefix arg, or if PROMPTP is non-nil, prompt for month to display."
  (interactive "P")
  (let* ((time (if promptp (timex-read-date) (decode-time))))
    (timex-pretty-month time timex-user)))

(defun timex-print-week (&optional promptp)
  "Display a week of timeclock data.

With prefix arg, or if PROMPTP is non-nil, prompt for week to display."
  (interactive "P")
  (timex-pretty-week timex-user (when promptp (timex-read-date))))

(defun timex-print-day (&optional promptp)
  "Display a day of timeclock data.

With prefix arg, or if PROMPTP is non-nil, prompt for the day to display."
  (interactive "P")
  (timex-pretty-day (when promptp (timex-read-date))))

(defun timex-view-schedule (&optional promptp)
  "View a timex schedule file.

If PROMPTP is non-nil, ask for the schedule to display, otherwise use
the current schedule.  See also `timex-find-schedule-file'."
  (interactive "P")
  (let* ((file (timex-find-schedule-file (and promptp (timex-read-date))))
         (buf (get-buffer-create "*timex-schedule*")))
    (push buf timex-created-buffers)
    (with-timex-results-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-file-contents-literally file))))

;; Yes, I'm very lazy.
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
  "Quit the timex interface, deleting all associated buffers."
  (interactive)
  (mapc (lambda (b)
          (when (buffer-live-p b)
            (with-current-buffer b
              (set-buffer-modified-p nil)
              (kill-this-buffer))))
        timex-created-buffers)
  (when (window-configuration-p timex-saved-window-configuration)
    (set-window-configuration timex-saved-window-configuration))
  (setq timex-created-buffers nil)
  (setq timex-saved-window-configuration nil))

(defvar timex-help
  (concat "Commands:\n\n"
          "`\\[timex-set-user]' -- timex-set-user.\n"
          "       Set the username for finding schedules.\n\n"
          "`\\[timex-view-schedule]' -- timex-view-schedule.\n"
          "       View the schedule for this month.\n"
          "       With prefix arg, ask for the month to display.\n\n"
          "`\\[timex-print-day]' -- timex-print-day\n"
          "       Print hours worked today.\n\n"
          "`\\[timex-print-week]' -- timex-print-week.\n"
          "       Print hours worked in the current week.\n\n"
          "`\\[timex-print-month]' -- timex-print-month\n"
          "       Print hours worked in the current month.\n\n"
          "`\\[timex-print-date-range]' -- timex-print-date-range\n"
          "       Print hours worked in a specified date range.\n\n"
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
    (define-key map "v" 'timex-view-schedule)
    (define-key map "m" 'timex-print-month)
    (define-key map "w" 'timex-print-week)
    (define-key map "r" 'timex-print-date-range)
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
  (setq timex-saved-window-configuration (current-window-configuration))
  (switch-to-buffer (get-buffer-create "*timex*"))
  (push (current-buffer) timex-created-buffers)
  (setq buffer-read-only nil)
  (erase-buffer)
  (timex-mode))

(provide 'timex)

;;; timex.el ends here
