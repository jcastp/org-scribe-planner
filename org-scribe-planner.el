;;; org-scribe-planner.el --- Writing planning tool for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Javier Castilla
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, writing, planning, productivity
;; URL: https://codeberg.org/jcastp/org-scribe-planner

;;; Commentary:

;; A comprehensive writing planning tool inspired by pacemaker.press that helps
;; writers calculate, visualize, and manage their writing goals through an
;; interactive interface integrated with Org-mode.
;;
;; Features:
;; - Calculate writing schedules given 2 of 3 variables (total words, daily words, days)
;; - Beautiful calendar visualizations with day-by-day breakdown
;; - Flexible spare days management (holidays, weekends, breaks)
;; - Plan modification and recalculation
;; - Save/load functionality using Org-mode properties
;; - Weekly summaries and milestone tracking
;; - Integration with org-agenda

;;; Code:

(require 'org)
(require 'org-element)
(require 'calendar)
(require 'cl-lib)

;;; Customization

(defgroup org-scribe-planner nil
  "Writing planning and tracking tools for Org-mode."
  :group 'org
  :prefix "org-scribe-planner-")

(defcustom org-scribe-planner-directory (expand-file-name "writing-projects" org-directory)
  "Directory where writing project files are stored."
  :type 'directory
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-default-spare-days 0
  "Default number of spare/break days in a writing plan."
  :type 'integer
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-calendar-buffer "*Writing Plan Calendar*"
  "Name of the buffer for calendar visualization."
  :type 'string
  :group 'org-scribe-planner)

;;; Data Structures

(cl-defstruct org-scribe-plan
  "Structure representing a writing plan."
  (title nil :type string)
  (total-words nil :type (or null number))
  (daily-words nil :type (or null number))
  (days nil :type (or null number))
  (start-date nil :type string)  ; Format: "YYYY-MM-DD"
  (end-date nil :type string)
  (spare-days nil :type list)    ; List of dates in "YYYY-MM-DD" format
  (current-words 0 :type number)
  (org-heading-marker nil)       ; Marker to org heading
  (daily-word-counts nil :type list)) ; Alist of (date . word-count) pairs

;;; Core Calculation Functions

(defun org-scribe-planner--calculate-missing-variable (plan)
  "Calculate the missing variable in PLAN.
Given 2 of 3 variables (total-words, daily-words, days), calculate the third.
Updates PLAN in place and returns the calculated variable as a symbol."
  (with-slots (total-words daily-words days spare-days) plan
    (let ((num-spare-days (if spare-days (length spare-days) 0))
          (non-nil-count 0)
          (missing-var nil))

      ;; Count how many variables are set (must be numbers, not nil)
      (dolist (var (list total-words daily-words days))
        (when (numberp var) (setq non-nil-count (1+ non-nil-count))))

      (cond
       ;; Exactly 2 variables set - calculate the third
       ((= non-nil-count 2)
        (cond
         ;; Calculate total-words
         ((not (numberp total-words))
          (setf (org-scribe-plan-total-words plan)
                (ceiling (* daily-words (- days num-spare-days))))
          (setq missing-var 'total-words))

         ;; Calculate daily-words
         ((not (numberp daily-words))
          (let ((working-days (- days num-spare-days)))
            (if (<= working-days 0)
                (error "Not enough working days (days: %d, spare: %d)" days num-spare-days)
              (setf (org-scribe-plan-daily-words plan)
                    (ceiling (/ (float total-words) working-days)))
              (setq missing-var 'daily-words))))

         ;; Calculate days
         ((not (numberp days))
          (if (<= daily-words 0)
              (error "Daily words must be greater than 0")
            (setf (org-scribe-plan-days plan)
                  (+ (ceiling (/ (float total-words) daily-words))
                     num-spare-days))
            (setq missing-var 'days)))))

       ;; All 3 variables set - verify consistency and adjust if needed
       ((= non-nil-count 3)
        (let ((working-days (- days num-spare-days)))
          (when (<= working-days 0)
            (error "Not enough working days (days: %d, spare: %d)" days num-spare-days))

          ;; Check if the values are consistent
          (let ((calculated-total (* daily-words working-days)))
            (unless (= calculated-total total-words)
              (message "Warning: Values are inconsistent. Recalculating daily-words.")
              (setf (org-scribe-plan-daily-words plan)
                    (ceiling (/ (float total-words) working-days)))
              (setq missing-var 'daily-words)))))

       ;; Less than 2 variables set
       ((< non-nil-count 2)
        (error "At least 2 variables must be set to calculate a plan. Found: total-words=%s, daily-words=%s, days=%s"
               total-words daily-words days)))

      missing-var)))

(defun org-scribe-planner--calculate-dates (plan)
  "Calculate start-date and end-date for PLAN based on days.
If start-date is set, calculates end-date.
If neither is set, uses today as start-date."
  (with-slots (start-date end-date days spare-days) plan
    (unless days
      (error "Days must be calculated before calculating dates"))

    ;; Set start-date to today if not set
    (unless start-date
      (setf (org-scribe-plan-start-date plan)
            (format-time-string "%Y-%m-%d")))

    ;; Calculate end-date
    (let* ((start (org-scribe-planner--parse-date start-date))
           (end (time-add start (days-to-time (1- days)))))
      (setf (org-scribe-plan-end-date plan)
            (format-time-string "%Y-%m-%d" end)))))

(defun org-scribe-planner--parse-date (date-string)
  "Parse DATE-STRING in YYYY-MM-DD format to Emacs time."
  (apply #'encode-time (parse-time-string (concat date-string " 00:00:00"))))

(defun org-scribe-planner--date-to-string (time)
  "Convert Emacs TIME to YYYY-MM-DD string."
  (format-time-string "%Y-%m-%d" time))

(defun org-scribe-planner--add-days (date-string days)
  "Add DAYS to DATE-STRING and return new date string in YYYY-MM-DD format."
  (let* ((date-time (org-scribe-planner--parse-date date-string))
         (new-time (time-add date-time (days-to-time days))))
    (format-time-string "%Y-%m-%d" new-time)))

(defun org-scribe-planner--days-between (start-date-string end-date-string)
  "Calculate the number of days between START-DATE-STRING and END-DATE-STRING."
  (let* ((start-time (org-scribe-planner--parse-date start-date-string))
         (end-time (org-scribe-planner--parse-date end-date-string))
         (diff (time-subtract end-time start-time))
         (days (/ (float-time diff) 86400)))
    (round (1+ days))))  ; Add 1 to include both start and end dates

(defun org-scribe-planner--is-spare-day (date spare-days)
  "Check if DATE is in the SPARE-DAYS list."
  (member date spare-days))

(defun org-scribe-planner--generate-day-schedule (plan)
  "Generate a list of writing days with cumulative word counts for PLAN.
Returns a list of plists with :date, :words, :cumulative, :is-spare-day."
  (with-slots (start-date end-date daily-words spare-days) plan
    (let ((current-date (org-scribe-planner--parse-date start-date))
          (end (org-scribe-planner--parse-date end-date))
          (cumulative 0)
          (schedule nil))

      (while (not (time-less-p end current-date))
        (let* ((date-str (org-scribe-planner--date-to-string current-date))
               (is-spare (org-scribe-planner--is-spare-day date-str spare-days))
               (words (if is-spare 0 daily-words)))

          (unless is-spare
            (setq cumulative (+ cumulative words)))

          (push (list :date date-str
                     :words words
                     :cumulative cumulative
                     :is-spare-day is-spare)
                schedule)

          (setq current-date (time-add current-date (days-to-time 1)))))

      (nreverse schedule))))

;;; Interactive Commands

;;;###autoload
(defun org-scribe-planner-new-plan ()
  "Create a new writing plan interactively."
  (interactive)
  (let ((plan (make-org-scribe-plan)))

    ;; Get plan title
    (setf (org-scribe-plan-title plan)
          (read-string "Project title: "))

    ;; Ask which variables to set and read them directly into the plan
    (let ((var-choice (completing-read
                      "What do you know? "
                      '("Total words + Days → Calculate daily words"
                        "Total words + Daily words → Calculate days needed"
                        "Daily words + Days → Calculate total words")
                      nil t)))

      (cond
       ;; Total words + Days → Calculate daily words
       ((string-match-p "Calculate daily words" var-choice)
        (setf (org-scribe-plan-total-words plan)
              (read-number "Total words to write: "))
        (setf (org-scribe-plan-days plan)
              (read-number "Days available: ")))

       ;; Total words + Daily words → Calculate days needed
       ((string-match-p "Calculate days needed" var-choice)
        (setf (org-scribe-plan-total-words plan)
              (read-number "Total words to write: "))
        (setf (org-scribe-plan-daily-words plan)
              (read-number "Words per day you can write: ")))

       ;; Daily words + Days → Calculate total words
       ((string-match-p "Calculate total words" var-choice)
        (setf (org-scribe-plan-daily-words plan)
              (read-number "Words per day you can write: "))
        (setf (org-scribe-plan-days plan)
              (read-number "Days available: ")))

       ;; Fallback if nothing matched (shouldn't happen, but just in case)
       (t
        (error "Invalid choice: %s" var-choice))))

    ;; Ask about start date and set immediately (default to today)
    (let ((start-date-input (read-string "Start date (YYYY-MM-DD, default today): ")))
      (setf (org-scribe-plan-start-date plan)
            (if (and start-date-input (not (string-empty-p start-date-input)))
                start-date-input
              (format-time-string "%Y-%m-%d"))))

    ;; Calculate missing variable and dates FIRST (before asking about spare days)
    (condition-case err
        (progn
          ;; Initial calculation without spare days
          ;; Remember which variable we calculated so we can recalculate it if spare days are added
          (let* ((calculated-var (org-scribe-planner--calculate-missing-variable plan))
                 (initial-value (when calculated-var
                                  (cl-getf (list 'total-words (org-scribe-plan-total-words plan)
                                                'daily-words (org-scribe-plan-daily-words plan)
                                                'days (org-scribe-plan-days plan))
                                          calculated-var))))
            (when calculated-var
              (message "Calculated %s: %s" calculated-var initial-value))

            ;; Calculate dates (now we have start-date and all variables)
            (org-scribe-planner--calculate-dates plan)

            ;; NOW ask about spare days (we have all the info needed for weekend calculation)
            (when (y-or-n-p "Do you want to set spare/break days? ")
              (org-scribe-planner--configure-spare-days plan)

              ;; If spare days were added, recalculate the variable we originally calculated
              (when (org-scribe-plan-spare-days plan)
                (let ((num-spare (length (org-scribe-plan-spare-days plan))))
                  (message "Added %d spare day(s), recalculating..." num-spare)

                  ;; Recalculate the same variable we calculated before
                  (cond
                   ;; If we calculated days, recalculate days (add spare days to total)
                   ((eq calculated-var 'days)
                    (setf (org-scribe-plan-days plan) nil)
                    (org-scribe-planner--calculate-missing-variable plan)
                    (org-scribe-planner--calculate-dates plan)
                    (message "Recalculated: %d total days needed (including %d spare days)"
                            (org-scribe-plan-days plan) num-spare))

                   ;; If we calculated total-words, recalculate total-words
                   ((eq calculated-var 'total-words)
                    (setf (org-scribe-plan-total-words plan) nil)
                    (org-scribe-planner--calculate-missing-variable plan)
                    (message "Recalculated: %d total words achievable (%d working days)"
                            (org-scribe-plan-total-words plan)
                            (- (org-scribe-plan-days plan) num-spare)))

                   ;; If we calculated daily-words (or nothing), recalculate daily-words
                   (t
                    (setf (org-scribe-plan-daily-words plan) nil)
                    (org-scribe-planner--calculate-missing-variable plan)
                    (message "Recalculated: %d words/day (%d working days)"
                            (org-scribe-plan-daily-words plan)
                            (- (org-scribe-plan-days plan) num-spare))))))))

          ;; Save to org file
          (org-scribe-planner--save-plan plan)

          ;; Display the plan
          (org-scribe-planner-show-calendar plan))
      (error
       (message "Error creating plan: %s" (error-message-string err))))))

(defun org-scribe-planner--configure-spare-days (plan)
  "Interactively configure spare days for PLAN."
  (let ((spare-days (or (org-scribe-plan-spare-days plan) nil))
        (continue t))

    (while continue
      (let ((method (completing-read
                    "Add spare days by: "
                    '("Specific date" "Date range" "All weekends" "All Saturdays" "All Sundays" "Done")
                    nil t)))
        (cond
         ((equal method "Specific date")
          (let ((date (read-string "Enter date (YYYY-MM-DD): ")))
            (when (and date (not (string-empty-p date)))
              (push date spare-days)
              (message "Added %s as spare day" date))))

         ((equal method "Date range")
          (let ((start (read-string "Start date (YYYY-MM-DD): "))
                (end (read-string "End date (YYYY-MM-DD): ")))
            (when (and start end (not (string-empty-p start)) (not (string-empty-p end)))
              (setq spare-days (append spare-days
                                      (org-scribe-planner--generate-date-range start end)))
              (message "Added dates from %s to %s" start end))))

         ((equal method "All weekends")
          (setq spare-days (append spare-days
                                  (org-scribe-planner--get-weekends plan)))
          (message "Added all weekends"))

         ((equal method "All Saturdays")
          (setq spare-days (append spare-days
                                  (org-scribe-planner--get-day-of-week plan 6)))
          (message "Added all Saturdays"))

         ((equal method "All Sundays")
          (setq spare-days (append spare-days
                                  (org-scribe-planner--get-day-of-week plan 0)))
          (message "Added all Sundays"))

         ((equal method "Done")
          (setq continue nil)))))

    (setf (org-scribe-plan-spare-days plan) (delete-dups spare-days))))

(defun org-scribe-planner--generate-date-range (start-date end-date)
  "Generate a list of dates from START-DATE to END-DATE."
  (let ((current (org-scribe-planner--parse-date start-date))
        (end (org-scribe-planner--parse-date end-date))
        (dates nil))
    (while (not (time-less-p end current))
      (push (org-scribe-planner--date-to-string current) dates)
      (setq current (time-add current (days-to-time 1))))
    (nreverse dates)))

(defun org-scribe-planner--get-weekends (plan)
  "Get all weekend dates (Saturday and Sunday) for PLAN."
  (append (org-scribe-planner--get-day-of-week plan 6)
          (org-scribe-planner--get-day-of-week plan 0)))

(defun org-scribe-planner--get-day-of-week (plan day-number)
  "Get all dates in PLAN that fall on DAY-NUMBER (0=Sunday, 6=Saturday)."
  (unless (and (org-scribe-plan-start-date plan) (org-scribe-plan-days plan))
    (error "Start date and days must be set first"))

  (let ((current (org-scribe-planner--parse-date (org-scribe-plan-start-date plan)))
        (days-remaining (org-scribe-plan-days plan))
        (dates nil))

    (dotimes (_ days-remaining)
      (when (= (string-to-number (format-time-string "%w" current)) day-number)
        (push (org-scribe-planner--date-to-string current) dates))
      (setq current (time-add current (days-to-time 1))))

    (nreverse dates)))

;;; Org-mode Integration

(defun org-scribe-planner--save-plan (plan)
  "Save PLAN to an Org-mode file."
  (let* ((filename (concat (downcase (replace-regexp-in-string
                                     "[^[:alnum:]]" "-"
                                     (org-scribe-plan-title plan)))
                          ".org"))
         (filepath (expand-file-name filename org-scribe-planner-directory)))

    ;; Ensure directory exists
    (unless (file-exists-p org-scribe-planner-directory)
      (make-directory org-scribe-planner-directory t))

    ;; Create or update org file
    (with-current-buffer (find-file-noselect filepath)
      (erase-buffer)
      (org-mode)

      ;; Insert heading with properties
      (insert (format "* %s\n" (org-scribe-plan-title plan)))
      (org-set-property "TOTAL_WORDS" (number-to-string (org-scribe-plan-total-words plan)))
      (org-set-property "DAILY_WORDS" (number-to-string (org-scribe-plan-daily-words plan)))
      (org-set-property "DAYS" (number-to-string (org-scribe-plan-days plan)))
      (org-set-property "START_DATE" (org-scribe-plan-start-date plan))
      (org-set-property "END_DATE" (org-scribe-plan-end-date plan))
      (org-set-property "CURRENT_WORDS" (number-to-string (org-scribe-plan-current-words plan)))

      (when (org-scribe-plan-spare-days plan)
        (org-set-property "SPARE_DAYS" (mapconcat 'identity (org-scribe-plan-spare-days plan) ",")))

      (when (org-scribe-plan-daily-word-counts plan)
        (org-set-property "DAILY_WORD_COUNTS"
                         (mapconcat (lambda (pair)
                                     (format "%s:%d" (car pair) (cdr pair)))
                                   (org-scribe-plan-daily-word-counts plan)
                                   ",")))

      ;; Add schedule as content
      (goto-char (point-max))
      (insert "\n** Schedule\n\n")
      (let ((schedule (org-scribe-planner--generate-day-schedule plan))
            (daily-counts (org-scribe-plan-daily-word-counts plan)))
        (insert "| Date | Target | Cumulative | Actual | Progress % | Notes |\n")
        (insert "|------+--------+------------+--------+------------+-------|\n")
        (dolist (day schedule)
          (let* ((date (plist-get day :date))
                 (target (plist-get day :words))
                 (is-spare (plist-get day :is-spare-day))
                 (actual (cdr (assoc date daily-counts)))
                 (percentage (if (and actual (not is-spare) (> target 0))
                                (format "%.1f%%" (* 100.0 (/ (float actual) target)))
                              "")))
            (insert (format "| %s | %s | %d | %s | %s | %s |\n"
                           date
                           (if is-spare "REST" (number-to-string target))
                           (plist-get day :cumulative)
                           (if actual (number-to-string actual) "")
                           percentage
                           (if is-spare "Spare day" ""))))))

      (org-table-align)
      (save-buffer)
      (message "Plan saved to %s" filepath))))

(defun org-scribe-planner--load-plan (filepath)
  "Load a writing plan from FILEPATH."
  (with-current-buffer (find-file-noselect filepath)
    (goto-char (point-min))
    (let ((plan (make-org-scribe-plan)))
      (when (re-search-forward "^\\*" nil t)
        (setf (org-scribe-plan-title plan)
              (org-get-heading t t t t))
        (setf (org-scribe-plan-total-words plan)
              (string-to-number (or (org-entry-get nil "TOTAL_WORDS") "0")))
        (setf (org-scribe-plan-daily-words plan)
              (string-to-number (or (org-entry-get nil "DAILY_WORDS") "0")))
        (setf (org-scribe-plan-days plan)
              (string-to-number (or (org-entry-get nil "DAYS") "0")))
        (setf (org-scribe-plan-start-date plan)
              (org-entry-get nil "START_DATE"))
        (setf (org-scribe-plan-end-date plan)
              (org-entry-get nil "END_DATE"))
        (setf (org-scribe-plan-current-words plan)
              (string-to-number (or (org-entry-get nil "CURRENT_WORDS") "0")))

        (let ((spare-days-str (org-entry-get nil "SPARE_DAYS")))
          (when spare-days-str
            (setf (org-scribe-plan-spare-days plan)
                  (split-string spare-days-str "," t " "))))

        (let ((daily-counts-str (org-entry-get nil "DAILY_WORD_COUNTS")))
          (when daily-counts-str
            (setf (org-scribe-plan-daily-word-counts plan)
                  (mapcar (lambda (pair-str)
                           (let ((parts (split-string pair-str ":" t " ")))
                             (cons (car parts) (string-to-number (cadr parts)))))
                         (split-string daily-counts-str "," t " ")))))

        (setf (org-scribe-plan-org-heading-marker plan)
              (point-marker)))
      plan)))

;;;###autoload
(defun org-scribe-planner-load-plan ()
  "Load an existing writing plan."
  (interactive)
  (let* ((files (directory-files org-scribe-planner-directory t "\\.org$"))
         (file (completing-read "Select plan: " files nil t)))
    (when file
      (let ((plan (org-scribe-planner--load-plan file)))
        (org-scribe-planner-show-calendar plan)))))

;;; Calendar Visualization

(defun org-scribe-planner-show-calendar (plan)
  "Display a calendar visualization for PLAN."
  (let ((buffer (get-buffer-create org-scribe-planner-calendar-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-scribe-planner-calendar-mode)

        ;; Header
        (insert (propertize (format "Writing Plan: %s\n" (org-scribe-plan-title plan))
                           'face 'org-level-1))
        (insert (make-string 80 ?=) "\n\n")

        ;; Summary
        (insert (propertize "Summary:\n" 'face 'org-level-2))
        (insert (format "  Total Words:   %d\n" (org-scribe-plan-total-words plan)))
        (insert (format "  Daily Words:   %d\n" (org-scribe-plan-daily-words plan)))
        (insert (format "  Total Days:    %d\n" (org-scribe-plan-days plan)))
        (insert (format "  Working Days:  %d\n" (- (org-scribe-plan-days plan)
                                                   (length (org-scribe-plan-spare-days plan)))))
        (insert (format "  Start Date:    %s\n" (org-scribe-plan-start-date plan)))
        (insert (format "  End Date:      %s\n" (org-scribe-plan-end-date plan)))
        (insert (format "  Current Words: %d (%.1f%%)\n\n"
                       (org-scribe-plan-current-words plan)
                       (* 100 (/ (float (org-scribe-plan-current-words plan))
                                (org-scribe-plan-total-words plan)))))

        ;; Day-by-day schedule
        (insert (propertize "Daily Schedule:\n" 'face 'org-level-2))
        (insert (make-string 80 ?-) "\n")

        (let ((schedule (org-scribe-planner--generate-day-schedule plan))
              (daily-counts (org-scribe-plan-daily-word-counts plan))
              (week-num 1)
              (week-words 0)
              (prev-week-start nil)
              (cumulative-actual 0)
              (expected-total 0))

          (dolist (day schedule)
            (let* ((date (plist-get day :date))
                   (words (plist-get day :words))
                   (cumulative (plist-get day :cumulative))
                   (is-spare (plist-get day :is-spare-day))
                   (actual (cdr (assoc date daily-counts)))
                   ;; Parse date to get day of week (0=Sunday, 1=Monday, ..., 6=Saturday)
                   (date-parts (mapcar 'string-to-number (split-string date "-")))
                   (year (nth 0 date-parts))
                   (month (nth 1 date-parts))
                   (day-num (nth 2 date-parts))
                   (day-of-week (calendar-day-of-week (list month day-num year)))
                   (day-name (calendar-day-name (list month day-num year)))
                   (is-monday (= day-of-week 1))
                   (percentage (if (and actual (not is-spare) (> words 0))
                                  (format "[%.1f%%]" (* 100.0 (/ (float actual) words)))
                                ""))
                   (face (if is-spare 'org-agenda-dimmed-todo-face 'default)))

              ;; Update cumulative actual word count and expected total
              (if actual
                  ;; Has actual data - expected matches actual
                  (progn
                    (setq cumulative-actual (+ cumulative-actual actual))
                    (setq expected-total cumulative-actual))
                ;; No actual data - add daily target to expected (skip spare days)
                (unless is-spare
                  (setq expected-total (+ expected-total words))))

              ;; Week header (starts on Monday)
              (when (or is-monday (null prev-week-start))
                (insert (propertize (format "\nWeek %d:\n" week-num) 'face 'org-level-3))
                ;; Add column headers
                (insert (propertize
                        (format "  %-23s  %-12s     %-12s  |  %-13s  |  %s\n"
                               "Date (Day)"
                               "Daily Target"
                               "Expected Total"
                               "Actual Total"
                               "Daily Actual")
                        'face 'bold))
                (insert (propertize
                        (format "  %s  %s     %s  |  %s  |  %s\n"
                               (make-string 23 ?-)
                               (make-string 12 ?-)
                               (make-string 12 ?-)
                               (make-string 13 ?-)
                               (make-string 25 ?-))
                        'face 'org-level-4))
                (setq week-num (1+ week-num)
                      week-words 0
                      prev-week-start date))

              ;; Day entry with columnar formatting
              (let* ((date-col (format "%s (%-9s)" date day-name))
                     (target-col (if is-spare "REST" (format "%5d words" words)))
                     (expected-col (format "%6d total" expected-total))
                     (cumulative-actual-col (if (> cumulative-actual 0)
                                               (format "%6d actual" cumulative-actual)
                                             ""))
                     (daily-actual-col (if actual
                                          (format "%5d words %s" actual percentage)
                                        ""))
                     (note-col (if is-spare "(spare day)" "")))
                (insert (propertize
                        (format "  %-23s  %12s  →  %12s  |  %-13s  |  %s%s\n"
                               date-col
                               target-col
                               expected-col
                               cumulative-actual-col
                               (if actual "Daily: " "")
                               (if actual daily-actual-col note-col))
                        'face face)))

              (setq week-words (+ week-words words))

              ;; Week summary (show at end of week or end of schedule)
              (let ((next-day (cadr (member day schedule))))
                (when (or (null next-day)  ; last day
                         (let* ((next-date (plist-get next-day :date))
                                (next-parts (mapcar 'string-to-number (split-string next-date "-")))
                                (next-dow (calendar-day-of-week (list (nth 1 next-parts) (nth 2 next-parts) (nth 0 next-parts)))))
                           (= next-dow 1)))  ; next day is Monday
                  (insert (propertize (format "  Week total: %d words\n" week-words)
                                     'face 'org-level-4)))))))

        (insert "\n" (make-string 80 ?=) "\n")
        (insert "\nCommands: [q] quit  [r] recalculate  [u] update progress  [d] daily word count\n"))

      (goto-char (point-min))
      (display-buffer buffer))))

(define-derived-mode org-scribe-planner-calendar-mode special-mode "Writing-Plan"
  "Major mode for displaying writing plan calendars."
  (setq truncate-lines t))

(define-key org-scribe-planner-calendar-mode-map (kbd "q") #'quit-window)
(define-key org-scribe-planner-calendar-mode-map (kbd "r") #'org-scribe-planner-recalculate)
(define-key org-scribe-planner-calendar-mode-map (kbd "u") #'org-scribe-planner-update-progress)
(define-key org-scribe-planner-calendar-mode-map (kbd "d") #'org-scribe-planner-update-daily-word-count)

;;; Org-agenda Integration

(defcustom org-scribe-planner-sync-to-agenda t
  "Whether to automatically sync writing plans to org-agenda."
  :type 'boolean
  :group 'org-scribe-planner)

(defun org-scribe-planner--add-agenda-entries (plan filepath)
  "Add scheduled agenda entries for PLAN to FILEPATH."
  (when org-scribe-planner-sync-to-agenda
    (with-current-buffer (find-file-noselect filepath)
      (goto-char (point-max))

      ;; Add agenda entries heading
      (unless (re-search-backward "^\\*\\* Agenda Entries" nil t)
        (goto-char (point-max))
        (insert "\n** Agenda Entries\n")
        (insert ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n\n"))

      (goto-char (point-max))

      ;; Generate scheduled entries for each day
      (let ((schedule (org-scribe-planner--generate-day-schedule plan)))
        (dolist (day schedule)
          (unless (plist-get day :is-spare-day)
            (let ((date (plist-get day :date))
                  (words (plist-get day :words))
                  (cumulative (plist-get day :cumulative)))
              (insert (format "*** TODO Write %d words\n" words))
              (insert (format "SCHEDULED: <%s>\n" date))
              (insert (format ":PROPERTIES:\n"))
              (insert (format ":TARGET_WORDS: %d\n" words))
              (insert (format ":CUMULATIVE: %d\n" cumulative))
              (insert (format ":END:\n\n"))))))

      (save-buffer)
      (message "Agenda entries synced"))))

(defun org-scribe-planner--update-agenda-file-list (filepath)
  "Add FILEPATH to org-agenda-files if not already present."
  (require 'org-agenda)
  (unless (member filepath org-agenda-files)
    (customize-save-variable 'org-agenda-files
                            (cons filepath org-agenda-files))
    (message "Added %s to org-agenda-files" filepath)))

;;;###autoload
(defun org-scribe-planner-sync-agenda ()
  "Sync current writing plan to org-agenda."
  (interactive)
  (let* ((files (directory-files org-scribe-planner-directory t "\\.org$"))
         (file (completing-read "Select plan to sync: " files nil t)))
    (when file
      (let ((plan (org-scribe-planner--load-plan file)))
        (org-scribe-planner--add-agenda-entries plan file)
        (org-scribe-planner--update-agenda-file-list file)))))

;;; Plan Modification and Recalculation

;;;###autoload
(defun org-scribe-planner-update-progress ()
  "Update the current word count for a writing plan."
  (interactive)
  (let* ((files (directory-files org-scribe-planner-directory t "\\.org$"))
         (file (completing-read "Select plan: " files nil t)))
    (when file
      (let ((plan (org-scribe-planner--load-plan file))
            (new-count (read-number "Current word count: ")))

        (setf (org-scribe-plan-current-words plan) new-count)

        ;; Update the org file
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (when (re-search-forward "^\\*" nil t)
            (org-set-property "CURRENT_WORDS" (number-to-string new-count)))
          (save-buffer))

        ;; Show updated calendar
        (org-scribe-planner-show-calendar plan)
        (org-scribe-planner--show-progress-report plan)))))

;;;###autoload
(defun org-scribe-planner-update-daily-word-count ()
  "Update the actual word count for a specific day."
  (interactive)
  (let* ((files (directory-files org-scribe-planner-directory t "\\.org$"))
         (file (completing-read "Select plan: " files nil t)))
    (when file
      (let* ((plan (org-scribe-planner--load-plan file))
             (schedule (org-scribe-planner--generate-day-schedule plan))
             (dates (mapcar (lambda (day) (plist-get day :date)) schedule))
             (date (completing-read "Select date: " dates nil t))
             (word-count (read-number "Word count for this day: " 0)))

        ;; Update or add the daily word count
        (let ((existing (assoc date (org-scribe-plan-daily-word-counts plan))))
          (if existing
              (setcdr existing word-count)
            (push (cons date word-count) (org-scribe-plan-daily-word-counts plan))))

        ;; Save the updated plan
        (org-scribe-planner--save-plan plan)

        (message "Updated word count for %s to %d" date word-count)

        ;; Ask if user wants to recalculate future targets based on cumulative progress
        (when (y-or-n-p "Would you like to recalculate the plan based on your cumulative progress? ")
          (org-scribe-planner-recalculate-from-progress plan file))))))

(defun org-scribe-planner--show-progress-report (plan)
  "Display a progress report for PLAN."
  (let* ((total (org-scribe-plan-total-words plan))
         (current (org-scribe-plan-current-words plan))
         (remaining (- total current))
         (percent (* 100.0 (/ (float current) total)))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (today (format-time-string "%Y-%m-%d"))
         (days-elapsed 0)
         (expected-words 0))

    ;; Calculate expected words by today
    (dolist (day schedule)
      (when (string< (plist-get day :date) today)
        (setq days-elapsed (1+ days-elapsed))
        (unless (plist-get day :is-spare-day)
          (setq expected-words (+ expected-words (plist-get day :words))))))

    (let ((ahead-behind (- current expected-words)))
      (message
       "Progress: %d/%d words (%.1f%%) | %s: %s%d words"
       current total percent
       (if (>= ahead-behind 0) "Ahead" "Behind")
       (if (>= ahead-behind 0) "+" "")
       ahead-behind))))

(defun org-scribe-planner-recalculate-from-progress (plan _file)
  "Recalculate PLAN based on cumulative actual progress."
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         (cumulative-actual (apply '+ (mapcar 'cdr daily-counts)))
         (total-words (org-scribe-plan-total-words plan))
         (remaining-words (- total-words cumulative-actual))
         (today (format-time-string "%Y-%m-%d"))
         (end-date (org-scribe-plan-end-date plan))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (remaining-days 0))

    ;; Count remaining working days (from today until end, excluding spare days)
    (dolist (day schedule)
      (let ((date (plist-get day :date))
            (is-spare (plist-get day :is-spare-day)))
        (when (and (not (string< date today))
                  (not is-spare))
          (setq remaining-days (1+ remaining-days)))))

    (message "Current progress: %d words written. Remaining: %d words (%d working days left)"
             cumulative-actual remaining-words remaining-days)

    ;; Ask user what they want to adjust
    (let ((choice (completing-read
                   "How would you like to recalculate? "
                   '("Adjust end date (keep daily word count)"
                     "Adjust daily word count (keep end date)")
                   nil t)))

      (cond
       ((string-match "Adjust end date" choice)
        ;; Keep daily words the same, recalculate end date
        (let ((daily-words (org-scribe-plan-daily-words plan))
              (start-date (org-scribe-plan-start-date plan)))
          ;; Calculate new end date based on remaining words and current daily target
          (let* ((days-needed (ceiling (/ (float remaining-words) daily-words)))
                 (new-end-date (org-scribe-planner--add-days today days-needed)))
            ;; Keep total-words unchanged (original objective)
            ;; Keep daily-word-counts unchanged (user's history)
            ;; Update current-words to reflect actual progress
            (setf (org-scribe-plan-current-words plan) cumulative-actual)
            (setf (org-scribe-plan-end-date plan) new-end-date)
            ;; Recalculate days based on new end date
            (setf (org-scribe-plan-days plan)
                  (org-scribe-planner--days-between start-date new-end-date))
            (message "Recalculated: %d words remaining, new end date is %s (keeping %d words/day)"
                     remaining-words new-end-date daily-words))))

       ((string-match "Adjust daily word count" choice)
        ;; Keep end date, recalculate daily words to fit remaining work
        ;; Calculate new daily target based on remaining words and remaining days
        (let ((new-daily-words (if (> remaining-days 0)
                                  (ceiling (/ (float remaining-words) remaining-days))
                                0)))
          ;; Keep total-words unchanged (original objective)
          ;; Keep daily-word-counts unchanged (user's history)
          ;; Update current-words to reflect actual progress
          (setf (org-scribe-plan-current-words plan) cumulative-actual)
          (setf (org-scribe-plan-daily-words plan) new-daily-words)
          (message "Recalculated: %d words remaining, new daily target is %d words (keeping end date %s)"
                   remaining-words new-daily-words end-date)))))

    ;; Save and display
    (org-scribe-planner--save-plan plan)
    (org-scribe-planner-show-calendar plan)
    (message "Plan recalculated and saved")))

;;;###autoload
(defun org-scribe-planner-recalculate ()
  "Recalculate a writing plan with new parameters."
  (interactive)
  (let* ((files (directory-files org-scribe-planner-directory t "\\.org$"))
         (file (completing-read "Select plan to recalculate: " files nil t)))
    (when file
      (let ((plan (org-scribe-planner--load-plan file)))

        ;; Ask what to recalculate
        (let ((choice (completing-read
                      "What changed? "
                      '("Daily words (recalc days needed)"
                        "Days available (recalc daily words)"
                        "Total words (recalc daily words)"
                        "Add/modify spare days")
                      nil t)))

          (cond
           ((string-match "Daily words" choice)
            (let ((new-daily (read-number "New daily words: "
                                         (org-scribe-plan-daily-words plan))))
              (setf (org-scribe-plan-daily-words plan) new-daily)
              (setf (org-scribe-plan-days plan) nil)
              (org-scribe-planner--calculate-missing-variable plan)
              (org-scribe-planner--calculate-dates plan)))

           ((string-match "Days available" choice)
            (let ((new-days (read-number "New days available: "
                                        (org-scribe-plan-days plan))))
              (setf (org-scribe-plan-days plan) new-days)
              (setf (org-scribe-plan-daily-words plan) nil)
              (org-scribe-planner--calculate-missing-variable plan)
              (org-scribe-planner--calculate-dates plan)))

           ((string-match "Total words" choice)
            (let ((new-total (read-number "New total words: "
                                         (org-scribe-plan-total-words plan))))
              (setf (org-scribe-plan-total-words plan) new-total)
              (setf (org-scribe-plan-daily-words plan) nil)
              (org-scribe-planner--calculate-missing-variable plan)))

           ((string-match "spare days" choice)
            (org-scribe-planner--configure-spare-days plan)
            (setf (org-scribe-plan-daily-words plan) nil)
            (org-scribe-planner--calculate-missing-variable plan)
            (org-scribe-planner--calculate-dates plan))))

        ;; Save updated plan
        (org-scribe-planner--save-plan plan)

        ;; Show updated calendar
        (org-scribe-planner-show-calendar plan)
        (message "Plan recalculated and saved")))))

;;; Milestone Tracking

(defun org-scribe-planner--get-milestones (plan)
  "Calculate milestone dates for PLAN (25%, 50%, 75%, 100%)."
  (let* ((total (org-scribe-plan-total-words plan))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (milestones '((25 . nil) (50 . nil) (75 . nil) (100 . nil))))

    (dolist (day schedule)
      (let ((cumulative (plist-get day :cumulative))
            (percent (* 100.0 (/ (float cumulative) total))))

        (dolist (milestone milestones)
          (when (and (>= percent (car milestone))
                    (not (cdr milestone)))
            (setcdr milestone (plist-get day :date))))))

    milestones))

;;;###autoload
(defun org-scribe-planner-show-milestones ()
  "Show milestone dates for a writing plan."
  (interactive)
  (let* ((files (directory-files org-scribe-planner-directory t "\\.org$"))
         (file (completing-read "Select plan: " files nil t)))
    (when file
      (let* ((plan (org-scribe-planner--load-plan file))
             (milestones (org-scribe-planner--get-milestones plan))
             (buffer (get-buffer-create "*Writing Plan Milestones*")))

        (with-current-buffer buffer
          (erase-buffer)
          (insert (propertize (format "Milestones for: %s\n\n"
                                     (org-scribe-plan-title plan))
                             'face 'org-level-1))

          (dolist (milestone milestones)
            (insert (format "%3d%% - %s (%d words)\n"
                           (car milestone)
                           (or (cdr milestone) "Not reached")
                           (/ (* (car milestone) (org-scribe-plan-total-words plan)) 100)))))

        (display-buffer buffer)))))

;;; Provide

(provide 'org-scribe-planner)

;;; org-scribe-planner.el ends here
