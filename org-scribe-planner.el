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

(defcustom org-scribe-planner-directory (expand-file-name "writing-projects/" org-directory)
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
  (daily-word-counts nil :type list)) ; Alist of (date . plist) where plist has :words :note :target

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
                (error "Cannot calculate plan: not enough working days. Total days: %d, spare days: %d. Please reduce spare days or increase total days"
                       days num-spare-days)
              (setf (org-scribe-plan-daily-words plan)
                    (ceiling (/ (float total-words) working-days)))
              (setq missing-var 'daily-words))))

         ;; Calculate days
         ((not (numberp days))
          (if (<= daily-words 0)
              (error "Cannot calculate plan: daily words (%d) must be greater than 0. Please enter a positive number" daily-words)
            (setf (org-scribe-plan-days plan)
                  (+ (ceiling (/ (float total-words) daily-words))
                     num-spare-days))
            (setq missing-var 'days)))))

       ;; All 3 variables set - verify consistency and adjust if needed
       ((= non-nil-count 3)
        (let ((working-days (- days num-spare-days)))
          (when (<= working-days 0)
            (error "Cannot calculate plan: not enough working days. Total days: %d, spare days: %d, working days: %d. Please reduce spare days or increase total days"
                   days num-spare-days working-days))

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
      (error "Cannot calculate dates: days value is not set. Please ensure the plan has been properly calculated"))

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

(defun org-scribe-planner--validate-date-format (date-string)
  "Check if DATE-STRING matches YYYY-MM-DD format.
Returns t if valid format, nil otherwise."
  (and (stringp date-string)
       (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date-string)))

(defun org-scribe-planner--validate-date (date-string)
  "Validate DATE-STRING is in YYYY-MM-DD format and represents a valid date.
Returns t if valid, signals an error otherwise."
  ;; Check format first
  (unless (org-scribe-planner--validate-date-format date-string)
    (error "Invalid date format: '%s'. Expected YYYY-MM-DD (e.g., 2024-11-19)" date-string))

  ;; Parse date components
  (let* ((parts (mapcar 'string-to-number (split-string date-string "-")))
         (year (nth 0 parts))
         (month (nth 1 parts))
         (day (nth 2 parts)))

    ;; Validate month
    (unless (and (>= month 1) (<= month 12))
      (error "Invalid month in date '%s': month must be between 1 and 12" date-string))

    ;; Validate day using calendar functions
    ;; calendar-last-day-of-month expects (month year) format
    (let ((max-day (calendar-last-day-of-month month year)))
      (unless (and (>= day 1) (<= day max-day))
        (error "Invalid day in date '%s': day must be between 1 and %d for month %d"
               date-string max-day month)))

    ;; Additional validation: try to parse it
    (condition-case nil
        (progn
          (org-scribe-planner--parse-date date-string)
          t)
      (error
       (error "Invalid date: '%s'. Please check the date is correct" date-string)))))

(defun org-scribe-planner--read-date (prompt &optional default allow-empty)
  "Read a date from user with PROMPT, validating YYYY-MM-DD format.
If DEFAULT is provided, show it in prompt and use if user enters empty string.
If ALLOW-EMPTY is non-nil, empty input returns nil without error.
Returns validated date string or nil (if ALLOW-EMPTY and user entered nothing)."
  (let ((date-input nil)
        (valid nil))
    (while (not valid)
      (setq date-input
            (read-string
             (if default
                 (format "%s (default: %s): " prompt default)
               (format "%s: " prompt))))

      (cond
       ;; Empty input
       ((string-empty-p date-input)
        (cond
         ;; Use default if provided
         (default
          (setq date-input default)
          (setq valid t))
         ;; Allow empty if specified
         (allow-empty
          (setq date-input nil)
          (setq valid t))
         ;; Otherwise, require input
         (t
          (message "Date is required. Please enter a date in YYYY-MM-DD format")
          (sit-for 1))))

       ;; Validate the input
       (t
        (condition-case err
            (progn
              (org-scribe-planner--validate-date date-input)
              (setq valid t))
          (error
           (message "%s" (error-message-string err))
           (sit-for 1.5))))))

    date-input))

;;; File Selection Helper

(defun org-scribe-planner--select-plan-file (prompt)
  "Prompt user to select a plan file with PROMPT.
Returns the selected file path."
  (read-file-name prompt
                  org-scribe-planner-directory
                  nil
                  t
                  nil
		  ;; this allows now for path manipulation in the minibuffer
		  (lambda (name)
                    (or (file-directory-p name)
			(string-match-p "\\.org$" name)))
		  ))

;;; Input Validation Helpers

(defun org-scribe-planner--read-positive-number (prompt &optional default)
  "Read a positive number from the user with PROMPT.
Optional DEFAULT provides a default value.
Ensures the number is greater than zero."
  (let ((number nil)
        (valid nil))
    (while (not valid)
      (setq number (read-number prompt default))
      (if (<= number 0)
          (message "Please enter a positive number greater than zero. You entered: %d" number)
        (setq valid t)))
    number))

(defun org-scribe-planner--read-non-negative-number (prompt &optional default)
  "Read a non-negative number from the user with PROMPT.
Optional DEFAULT provides a default value.
Ensures the number is greater than or equal to zero."
  (let ((number nil)
        (valid nil))
    (while (not valid)
      (setq number (read-number prompt default))
      (if (< number 0)
          (message "Please enter a non-negative number (0 or greater). You entered: %d" number)
        (setq valid t)))
    number))

;;; Schedule Generation

(defun org-scribe-planner--generate-day-schedule (plan)
  "Generate a list of writing days with cumulative word counts for PLAN.
Returns a list of plists with :date, :words, :cumulative, :is-spare-day."
  (with-slots (start-date end-date daily-words spare-days) plan
    (unless (and start-date end-date)
      (error "Cannot generate schedule: start-date (%s) and end-date (%s) must be set. Please calculate dates first"
             start-date end-date))

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
              (org-scribe-planner--read-positive-number "Total words to write: "))
        (setf (org-scribe-plan-days plan)
              (org-scribe-planner--read-positive-number "Days available: ")))

       ;; Total words + Daily words → Calculate days needed
       ((string-match-p "Calculate days needed" var-choice)
        (setf (org-scribe-plan-total-words plan)
              (org-scribe-planner--read-positive-number "Total words to write: "))
        (setf (org-scribe-plan-daily-words plan)
              (org-scribe-planner--read-positive-number "Words per day you can write: ")))

       ;; Daily words + Days → Calculate total words
       ((string-match-p "Calculate total words" var-choice)
        (setf (org-scribe-plan-daily-words plan)
              (org-scribe-planner--read-positive-number "Words per day you can write: "))
        (setf (org-scribe-plan-days plan)
              (org-scribe-planner--read-positive-number "Days available: ")))

       ;; Fallback if nothing matched (shouldn't happen, but just in case)
       (t
        (error "Invalid choice: %s" var-choice))))

    ;; Ask about start date and set immediately (default to today)
    (setf (org-scribe-plan-start-date plan)
          (org-scribe-planner--read-date "Start date (YYYY-MM-DD)"
                                        (format-time-string "%Y-%m-%d")))

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

          ;; Ask user for save location
          (let* ((default-filename (concat (downcase (replace-regexp-in-string
                                                     "[^[:alnum:]]" "-"
                                                     (org-scribe-plan-title plan)))
                                          ".org"))
                 (default-filepath (expand-file-name default-filename org-scribe-planner-directory))
                 (save-location (read-file-name "Save plan to: "
                                              org-scribe-planner-directory
                                              default-filepath
                                              nil
                                              default-filename))
                 (save-dir (file-name-directory save-location)))

            ;; Ensure the directory exists
            (if (file-exists-p save-dir)
                ;; Directory exists, save the plan
                (org-scribe-planner--save-plan plan save-location)
              ;; Directory doesn't exist, ask to create it
              (if (y-or-n-p (format "Directory %s does not exist. Create it? " save-dir))
                  (progn
                    (make-directory save-dir t)
                    (org-scribe-planner--save-plan plan save-location))
                ;; User declined to create directory
                (error "Cannot save plan: directory does not exist")))

            ;; Display the plan
            (org-scribe-planner-show-calendar plan save-location)))
      (error
       (message "Error creating plan: %s" (error-message-string err))))))

(defun org-scribe-planner--configure-spare-days (plan)
  "Interactively configure spare days for PLAN."
  (let ((spare-days (or (org-scribe-plan-spare-days plan) nil))
        (continue t))

    (while continue
      (let ((method (completing-read
                    "Configure spare days: "
                    '("Add: Specific date"
                      "Add: Date range"
                      "Add: All weekends"
                      "Add: All Saturdays"
                      "Add: All Sundays"
                      "Remove: Specific date"
                      "Remove: All spare days"
                      "List current spare days"
                      "Done")
                    nil t)))
        (cond
         ((equal method "Add: Specific date")
          (let ((date (org-scribe-planner--read-date "Enter date (YYYY-MM-DD)" nil t)))
            (when date
              (push date spare-days)
              (message "Added %s as spare day" date))))

         ((equal method "Add: Date range")
          (let ((start (org-scribe-planner--read-date "Start date (YYYY-MM-DD)" nil t))
                (end (when start
                       (org-scribe-planner--read-date "End date (YYYY-MM-DD)" nil t))))
            (when (and start end)
              (setq spare-days (append spare-days
                                      (org-scribe-planner--generate-date-range start end)))
              (message "Added dates from %s to %s" start end))))

         ((equal method "Add: All weekends")
          (setq spare-days (append spare-days
                                  (org-scribe-planner--get-weekends plan)))
          (message "Added all weekends"))

         ((equal method "Add: All Saturdays")
          (setq spare-days (append spare-days
                                  (org-scribe-planner--get-day-of-week plan 6)))
          (message "Added all Saturdays"))

         ((equal method "Add: All Sundays")
          (setq spare-days (append spare-days
                                  (org-scribe-planner--get-day-of-week plan 0)))
          (message "Added all Sundays"))

         ((equal method "Remove: Specific date")
          (if (null spare-days)
              (message "No spare days to remove")
            (let ((date (completing-read "Select date to remove: "
                                        (sort (copy-sequence spare-days) 'string<)
                                        nil t)))
              (when date
                (setq spare-days (delete date spare-days))
                (message "Removed %s from spare days" date)))))

         ((equal method "Remove: All spare days")
          (if (null spare-days)
              (message "No spare days to remove")
            (when (y-or-n-p (format "Remove all %d spare days? " (length spare-days)))
              (setq spare-days nil)
              (message "Removed all spare days"))))

         ((equal method "List current spare days")
          (if (null spare-days)
              (message "No spare days configured")
            (let ((sorted-days (sort (copy-sequence spare-days) 'string<)))
              (message "Current spare days (%d): %s"
                      (length sorted-days)
                      (mapconcat 'identity sorted-days ", ")))))

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

(defun org-scribe-planner--buffer-safe-to-erase-p (buffer filepath)
  "Check if BUFFER at FILEPATH is safe to erase completely.
Returns t if safe to erase, nil if caution is needed.
Also checks for unsaved changes and prompts user if needed."
  (with-current-buffer buffer
    (let ((file-exists (file-exists-p filepath))
          (buffer-modified (buffer-modified-p))
          (has-content (> (buffer-size) 0)))

      (cond
       ;; Buffer has unsaved changes - warn user
       (buffer-modified
        (if (y-or-n-p (format "Buffer '%s' has unsaved changes. Overwrite anyway? "
                             (buffer-name)))
            t
          (error "Save cancelled to preserve unsaved changes")))

       ;; New file, no content - safe to write
       ((not has-content)
        t)

       ;; Existing file with content - check if it's our plan file
       (file-exists
        (org-scribe-planner--is-single-plan-file-p buffer))

       ;; Default to safe (new buffer, no file)
       (t t)))))

(defun org-scribe-planner--is-single-plan-file-p (buffer)
  "Check if BUFFER contains a single org-scribe-planner plan.
Returns t if the file appears to be a plan file we created (single heading
with our properties), nil otherwise."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((heading-count 0)
            (has-plan-properties nil))

        ;; Count top-level headings
        (while (re-search-forward "^\\* " nil t)
          (setq heading-count (1+ heading-count)))

        ;; Check for our properties at first heading
        (goto-char (point-min))
        (when (re-search-forward "^\\* " nil t)
          (setq has-plan-properties
                (and (org-entry-get nil "TOTAL_WORDS")
                     (org-entry-get nil "DAILY_WORDS")
                     (org-entry-get nil "DAYS"))))

        ;; Safe if exactly one heading with our properties
        (and (= heading-count 1) has-plan-properties)))))

(defun org-scribe-planner--save-plan (plan &optional filepath)
  "Save PLAN to an Org-mode file.
If FILEPATH is not provided, generate a default filename in org-scribe-planner-directory."
  (let* ((filename (concat (downcase (replace-regexp-in-string
                                     "[^[:alnum:]]" "-"
                                     (org-scribe-plan-title plan)))
                          ".org"))
         (filepath (or filepath
                      (expand-file-name filename org-scribe-planner-directory))))

    ;; Ensure directory exists and is writable
    (let ((save-dir (file-name-directory filepath)))
      (unless (file-exists-p save-dir)
        (make-directory save-dir t))
      (unless (file-writable-p save-dir)
        (error "Cannot save plan: directory '%s' is not writable. Check directory permissions" save-dir)))

    ;; Check if file exists and is writable (if it exists)
    (when (file-exists-p filepath)
      (unless (file-writable-p filepath)
        (error "Cannot save plan: file '%s' is not writable. Check file permissions" filepath)))

    ;; Create or update org file
    (let ((buf (find-file-noselect filepath)))
      (with-current-buffer buf
        ;; Safety check before erasing
        (unless (org-scribe-planner--buffer-safe-to-erase-p buf filepath)
          (error "Cannot save plan: file '%s' contains multiple headings or non-plan content. Please use a dedicated file for this plan" filepath))

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
                           (mapconcat (lambda (entry)
                                       (let* ((date (car entry))
                                              (data (cdr entry))
                                              ;; Handle both old format (word-count . note) and new format (plist)
                                              (word-count (if (listp data)
                                                            (or (plist-get data :words) (car data))
                                                            data))
                                              (note (if (listp data)
                                                       (or (plist-get data :note) (cdr data))
                                                     ""))
                                              (target (when (listp data) (plist-get data :target))))
                                         (cond
                                          ;; New format with target
                                          (target
                                           (if (and note (not (string-empty-p note)))
                                               (format "%s:%d:%s:%d" date word-count note target)
                                             (format "%s:%d::%d" date word-count target)))
                                          ;; Old format or no target
                                          ((and note (not (string-empty-p note)))
                                           (format "%s:%d:%s" date word-count note))
                                          (t
                                           (format "%s:%d" date word-count)))))
                                     (org-scribe-plan-daily-word-counts plan)
                                     ",")))

        ;; Add schedule as content
        (goto-char (point-max))
        (insert "\n** Schedule\n\n")
        (let ((schedule (org-scribe-planner--generate-day-schedule plan))
              (daily-counts (org-scribe-plan-daily-word-counts plan))
              (cumulative-actual 0)
              (expected-total 0))
          (insert "| Date | Target | Cumulative | Actual | Progress % | Notes |\n")
          (insert "|------+--------+------------+--------+------------+-------|\n")
          (dolist (day schedule)
            (let* ((date (plist-get day :date))
                   (daily-entry (assoc date daily-counts))
                   (daily-data (when daily-entry (cdr daily-entry)))
                   ;; Use stored target if available, otherwise use current plan's daily-words
                   (stored-target (when (and daily-data (listp daily-data))
                                   (plist-get daily-data :target)))
                   (target (or stored-target (plist-get day :words)))
                   (is-spare (plist-get day :is-spare-day))
                   (actual (when daily-data
                            (if (listp daily-data)
                                (plist-get daily-data :words)
                              (if (consp daily-data) (car daily-data) daily-data))))
                   (note (when daily-data
                          (if (listp daily-data)
                              (plist-get daily-data :note)
                            (if (consp daily-data) (cdr daily-data) ""))))
                   (percentage (if (and actual (not is-spare) (> target 0))
                                  (format "%.1f%%" (* 100.0 (/ (float actual) target)))
                                "")))

              ;; Calculate cumulative the same way as in the report
              (if actual
                  ;; Has actual data - expected matches actual
                  (progn
                    (setq cumulative-actual (+ cumulative-actual actual))
                    (setq expected-total cumulative-actual))
                ;; No actual data - add daily target to expected (skip spare days)
                (unless is-spare
                  (setq expected-total (+ expected-total target))))

              (insert (format "| %s | %s | %d | %s | %s | %s |\n"
                             date
                             (if is-spare "REST" (number-to-string target))
                             expected-total
                             (if actual (number-to-string actual) "")
                             percentage
                             (cond
                              (is-spare "Spare day")
                              ((and note (not (string-empty-p note))) note)
                              (t "")))))))

        (org-table-align)
        (save-buffer)
        (message "Plan saved to %s" filepath)))))

(defun org-scribe-planner--load-plan (filepath)
  "Load a writing plan from FILEPATH."
  ;; Check if file exists and is readable
  (unless (file-exists-p filepath)
    (error "Cannot load plan: file does not exist at '%s'" filepath))
  (unless (file-readable-p filepath)
    (error "Cannot load plan: file is not readable at '%s'. Check file permissions" filepath))

  (with-current-buffer (find-file-noselect filepath)
    (goto-char (point-min))
    (let ((plan (make-org-scribe-plan)))
      (unless (re-search-forward "^\\*" nil t)
        (error "Cannot load plan: no org heading found in '%s'. File may be corrupted or not a valid plan file" filepath))

      ;; Heading was found, extract plan data
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
                (mapcar (lambda (entry-str)
                         (let* ((parts (split-string entry-str ":" nil " "))  ; Don't skip empty parts for :: syntax
                                (date (nth 0 parts))
                                (word-count (string-to-number (nth 1 parts)))
                                (note-or-empty (nth 2 parts))
                                (target-str (nth 3 parts))
                                ;; If we have 4 parts, it's new format: date:words:note:target or date:words::target
                                ;; If we have 3 parts, it's old format: date:words:note
                                ;; If we have 2 parts, it's old format: date:words
                                (has-target (and target-str (not (string-empty-p target-str))))
                                (note (if (and note-or-empty (not (string-empty-p note-or-empty)))
                                         note-or-empty
                                       ""))
                                (target (when has-target (string-to-number target-str))))
                           (if has-target
                               ;; New format with target
                               (cons date (list :words word-count :note note :target target))
                             ;; Old format - convert to new format (no target stored)
                             (cons date (list :words word-count :note note)))))
                       (split-string daily-counts-str "," t " ")))))

      (setf (org-scribe-plan-org-heading-marker plan)
            (point-marker))
      plan)))

;;;###autoload
(defun org-scribe-planner-load-plan ()
  "Load an existing writing plan."
  (interactive)
  (let ((file (org-scribe-planner--select-plan-file "Select plan file: ")))
    (when file
      (let ((plan (org-scribe-planner--load-plan file)))
        (org-scribe-planner-show-calendar plan file)))))

;;; Calendar Visualization

(defun org-scribe-planner-show-calendar (plan &optional filepath)
  "Display a calendar visualization for PLAN.
Optional FILEPATH shows the location of the plan file."
  (let ((buffer (get-buffer-create org-scribe-planner-calendar-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-scribe-planner-calendar-mode)

        ;; Header
        (insert (propertize (format "Writing Plan: %s\n" (org-scribe-plan-title plan))
                           'face 'org-level-1))
        (when filepath
          (insert (propertize (format "Location: %s\n" filepath)
                             'face 'org-document-info)))
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
                   (current-daily-words (plist-get day :words))
                   (cumulative (plist-get day :cumulative))
                   (is-spare (plist-get day :is-spare-day))
                   (daily-entry (assoc date daily-counts))
                   (daily-data (when daily-entry (cdr daily-entry)))
                   ;; Use stored target for display if available, otherwise use current plan's daily-words
                   (stored-target (when (and daily-data (listp daily-data))
                                   (plist-get daily-data :target)))
                   (display-target (or stored-target current-daily-words))
                   (actual (when daily-data
                            (if (listp daily-data)
                                (plist-get daily-data :words)
                              (if (consp daily-data) (car daily-data) daily-data))))
                   (note (when daily-data
                          (if (listp daily-data)
                              (plist-get daily-data :note)
                            (if (consp daily-data) (cdr daily-data) ""))))
                   ;; Parse date to get day of week (0=Sunday, 1=Monday, ..., 6=Saturday)
                   (date-parts (mapcar 'string-to-number (split-string date "-")))
                   (year (nth 0 date-parts))
                   (month (nth 1 date-parts))
                   (day-num (nth 2 date-parts))
                   (day-of-week (calendar-day-of-week (list month day-num year)))
                   (day-name (calendar-day-name (list month day-num year)))
                   (is-monday (= day-of-week 1))
                   (percentage (if (and actual (not is-spare) (> display-target 0))
                                  (format "[%.1f%%]" (* 100.0 (/ (float actual) display-target)))
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
                  (setq expected-total (+ expected-total current-daily-words))))

              ;; Week header (starts on Monday)
              (when (or is-monday (null prev-week-start))
                (insert (propertize (format "\nWeek %d:\n" week-num) 'face 'org-level-3))
                ;; Add column headers
                (insert (propertize
                        (format "  %-25s  %-13s  %-15s  %-15s  %-23s  %s\n"
                               "Date (Day)"
                               "Daily Target"
                               "Expected Total"
                               "Actual Total"
                               "Daily Actual"
                               "Notes")
                        'face 'bold))
                (insert (propertize
                        (format "  %s  %s  %s  %s  %s  %s\n"
                               (make-string 25 ?-)
                               (make-string 13 ?-)
                               (make-string 15 ?-)
                               (make-string 15 ?-)
                               (make-string 23 ?-)
                               (make-string 30 ?-))
                        'face 'org-level-4))
                (setq week-num (1+ week-num)
                      week-words 0
                      prev-week-start date))

              ;; Day entry with columnar formatting
              (let* ((date-col (format "%s (%-9s)" date day-name))
                     (target-col (if is-spare "REST" (format "%d words" display-target)))
                     (expected-col (format "%d words" expected-total))
                     (cumulative-actual-col (if (> cumulative-actual 0)
                                               (format "%d words" cumulative-actual)
                                             ""))
                     (daily-actual-col (if actual
                                          (format "%d words %s" actual percentage)
                                        ""))
                     (note-col (cond
                                (is-spare "(spare day)")
                                ((and note (not (string-empty-p note))) note)
                                (t ""))))
                (insert (propertize
                        (format "  %-25s  %-13s  %-15s  %-15s  %-23s  %s\n"
                               date-col
                               target-col
                               expected-col
                               cumulative-actual-col
                               daily-actual-col
                               note-col)
                        'face face)))

              (setq week-words (+ week-words current-daily-words))

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
        (insert "\nCommands:\n")
        (insert "  [q] quit  [d] daily word count  [u] update progress\n")
        (insert "  [r] recalculate plan  [a] adjust remaining days (keep end date fixed)\n"))

      (goto-char (point-min))
      (display-buffer buffer))))

(define-derived-mode org-scribe-planner-calendar-mode special-mode "Writing-Plan"
  "Major mode for displaying writing plan calendars."
  (setq truncate-lines t))

(define-key org-scribe-planner-calendar-mode-map (kbd "q") #'quit-window)
(define-key org-scribe-planner-calendar-mode-map (kbd "r") #'org-scribe-planner-recalculate)
(define-key org-scribe-planner-calendar-mode-map (kbd "u") #'org-scribe-planner-update-progress)
(define-key org-scribe-planner-calendar-mode-map (kbd "d") #'org-scribe-planner-update-daily-word-count)
(define-key org-scribe-planner-calendar-mode-map (kbd "a") #'org-scribe-planner-adjust-remaining-plan)

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
  (let ((file (org-scribe-planner--select-plan-file "Select plan file to sync: ")))
    (when file
      (let ((plan (org-scribe-planner--load-plan file)))
        (org-scribe-planner--add-agenda-entries plan file)
        (org-scribe-planner--update-agenda-file-list file)))))

;;; Helper Functions for Daily Word Counts

(defun org-scribe-planner--get-entry-words (entry)
  "Extract word count from daily-word-count ENTRY.
Handles both old format (date . (word-count . note)) and new format (date . plist)."
  (let ((data (cdr entry)))
    (if (and (listp data) (plist-member data :words))
        (plist-get data :words)
      ;; Old format fallback
      (if (consp data)
          (car data)
        data))))

(defun org-scribe-planner--get-entry-note (entry)
  "Extract note from daily-word-count ENTRY.
Handles both old format (date . (word-count . note)) and new format (date . plist)."
  (let ((data (cdr entry)))
    (if (and (listp data) (plist-member data :note))
        (plist-get data :note)
      ;; Old format fallback
      (if (consp data)
          (or (cdr data) "")
        ""))))

(defun org-scribe-planner--get-entry-target (entry)
  "Extract target from daily-word-count ENTRY.
Returns nil if no target is stored (old format or not set)."
  (let ((data (cdr entry)))
    (when (and (listp data) (plist-member data :target))
      (plist-get data :target))))

;;; Plan Modification and Recalculation

;;;###autoload
(defun org-scribe-planner-update-progress ()
  "Update the current word count for a writing plan."
  (interactive)
  (let ((file (org-scribe-planner--select-plan-file "Select plan file: ")))
    (when file
      (let ((plan (org-scribe-planner--load-plan file))
            (new-count (org-scribe-planner--read-non-negative-number "Current word count: ")))

        (setf (org-scribe-plan-current-words plan) new-count)

        ;; Update the org file
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (when (re-search-forward "^\\*" nil t)
            (org-set-property "CURRENT_WORDS" (number-to-string new-count)))
          (save-buffer))

        ;; Show updated calendar
        (org-scribe-planner-show-calendar plan file)
        (org-scribe-planner--show-progress-report plan)))))

;;;###autoload
(defun org-scribe-planner-update-daily-word-count ()
  "Update the actual word count for a specific day."
  (interactive)
  (let ((file (org-scribe-planner--select-plan-file "Select plan file: ")))
    (when file
      (let* ((plan (org-scribe-planner--load-plan file))
             (schedule (org-scribe-planner--generate-day-schedule plan))
             (dates (mapcar (lambda (day) (plist-get day :date)) schedule))
             (date (completing-read "Select date: " dates nil t))
             (word-count (org-scribe-planner--read-non-negative-number "Word count for this day: " 0))
             (note (read-string "Notes (optional): " "")))

        ;; Update or add the daily word count, note, and target
        ;; Get the current daily target for this date from the schedule
        (let* ((day-info (cl-find date schedule :key (lambda (d) (plist-get d :date)) :test 'string=))
               (current-target (if day-info (plist-get day-info :words) (org-scribe-plan-daily-words plan)))
               (existing (assoc date (org-scribe-plan-daily-word-counts plan)))
               (entry-data (list :words word-count
                                :note note
                                :target current-target)))
          (if existing
              (setcdr existing entry-data)
            (push (cons date entry-data) (org-scribe-plan-daily-word-counts plan))))

        ;; Save the updated plan to the same file location
        (org-scribe-planner--save-plan plan file)

        (message "Updated word count for %s to %d%s" date word-count
                 (if (string-empty-p note) "" (format " (note: %s)" note)))

        ;; Ask if user wants to recalculate future targets based on cumulative progress
        (when (y-or-n-p "Would you like to recalculate the remaining plan based on your progress? ")
          (org-scribe-planner-recalculate-remaining-days plan file))))))

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

(defun org-scribe-planner-recalculate-remaining-days (plan file)
  "Recalculate daily target for remaining days based on cumulative progress.
Keeps end date and total words fixed. Adjusts only the daily target for
remaining working days.
PLAN is the writing plan to recalculate.
FILE is the path where the plan should be saved."
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         (cumulative-actual (if daily-counts
                               (apply '+ (mapcar #'org-scribe-planner--get-entry-words daily-counts))
                             0))
         (total-words (org-scribe-plan-total-words plan))
         (remaining-words (- total-words cumulative-actual))
         (today (format-time-string "%Y-%m-%d"))
         (end-date (org-scribe-plan-end-date plan))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (remaining-days 0))

    ;; Count remaining working days (days without actual word counts, excluding spare days)
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day))
             (has-actual (assoc date daily-counts)))
        (when (and (not has-actual)  ; Only count days without actual word counts
                  (not is-spare))     ; Don't count spare days
          (setq remaining-days (1+ remaining-days)))))

    (if (<= remaining-days 0)
        (message "No remaining days in the plan. Plan has ended or all remaining days are spare days.")
      (progn
        ;; Calculate new daily target for remaining days
        (let ((new-daily-words (ceiling (/ (float remaining-words) remaining-days))))

          (message "Recalculating plan: %d words completed, %d words remaining over %d working days"
                   cumulative-actual remaining-words remaining-days)

          ;; Update daily-words to the new target
          (setf (org-scribe-plan-daily-words plan) new-daily-words)

          ;; Keep total-words unchanged (original goal)
          ;; Keep end-date unchanged (fixed deadline)
          ;; Keep start-date unchanged (historical record)
          ;; Keep days unchanged (original plan span)
          ;; Keep spare-days unchanged (original configuration)
          ;; Keep daily-word-counts unchanged (actual history)
          ;; Update current-words to reflect cumulative actual progress
          (setf (org-scribe-plan-current-words plan) cumulative-actual)

          ;; Save and display
          (org-scribe-planner--save-plan plan file)
          (org-scribe-planner-show-calendar plan file)

          (message "Plan recalculated: New daily target is %d words/day (keeping %s end date, %d total words goal)"
                   new-daily-words end-date total-words))))))

(defun org-scribe-planner-recalculate-from-progress (plan file)
  "Recalculate PLAN based on cumulative actual progress.
FILE is the path where the plan should be saved."
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         (cumulative-actual (apply '+ (mapcar #'org-scribe-planner--get-entry-words daily-counts)))
         (total-words (org-scribe-plan-total-words plan))
         (remaining-words (- total-words cumulative-actual))
         (today (format-time-string "%Y-%m-%d"))
         (end-date (org-scribe-plan-end-date plan))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (remaining-days 0))

    ;; Count remaining working days (days without actual word counts, excluding spare days)
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day))
             (has-actual (assoc date daily-counts)))
        (when (and (not has-actual)  ; Only count days without actual word counts
                  (not is-spare))     ; Don't count spare days
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
        (let* ((new-daily-words (if (> remaining-days 0)
                                   (ceiling (/ (float remaining-words) remaining-days))
                                 0))
               ;; Calculate total working days in the plan (start to end, excluding spare days)
               (total-working-days (- (org-scribe-plan-days plan)
                                     (length (org-scribe-plan-spare-days plan))))
               ;; Calculate new total to keep math consistent:
               ;;   total = daily-words × total-working-days
               ;; This is necessary because the schedule generator applies daily-words to ALL days
               (new-total-words (* new-daily-words total-working-days)))
          ;; Keep start-date unchanged (historical record)
          ;; Keep end-date unchanged (user's choice)
          ;; Keep days unchanged (original plan span)
          ;; Keep spare-days unchanged (original configuration)
          ;; Keep daily-word-counts unchanged (actual history)
          ;; Update current-words to reflect cumulative actual progress
          (setf (org-scribe-plan-current-words plan) cumulative-actual)
          ;; Update daily-words to new target for remaining working days
          (setf (org-scribe-plan-daily-words plan) new-daily-words)
          ;; Update total-words to keep the math consistent in the schedule:
          ;; The schedule generator applies daily-words to ALL working days,
          ;; so total must equal daily-words × working-days for consistency
          (setf (org-scribe-plan-total-words plan) new-total-words)
          (message "Recalculated: %d words written, %d words remaining over %d working days. Adjusted goal to %d total words, new daily target: %d words (keeping end date %s)"
                   cumulative-actual remaining-words remaining-days new-total-words new-daily-words end-date)))))

    ;; Save and display (save to the file location that was passed in)
    (org-scribe-planner--save-plan plan file)
    (org-scribe-planner-show-calendar plan file)
    (message "Plan recalculated and saved")))

;;;###autoload
(defun org-scribe-planner-adjust-remaining-plan ()
  "Recalculate daily target for remaining days based on progress.
This command keeps your end date and total word goal fixed, and adjusts
only the daily target for remaining working days based on your actual
cumulative progress so far."
  (interactive)
  (let ((file (org-scribe-planner--select-plan-file "Select plan file to adjust: ")))
    (when file
      (let ((plan (org-scribe-planner--load-plan file)))
        (org-scribe-planner-recalculate-remaining-days plan file)))))

;;;###autoload
(defun org-scribe-planner-recalculate ()
  "Recalculate a writing plan with new parameters."
  (interactive)
  (let ((file (org-scribe-planner--select-plan-file "Select plan file to recalculate: ")))
    (when file
      (let ((plan (org-scribe-planner--load-plan file)))

        ;; Ask what to recalculate
        (let ((choice (completing-read
                      "What changed? "
                      '("Daily words (recalc days needed)"
                        "Days available (recalc daily words)"
                        "Total words (recalc daily words)"
                        "Add/modify spare days"
                        "Remove spare days"
                        "Clear all spare days")
                      nil t)))

          (cond
           ((string-match "recalc days" choice)
            (let ((new-daily (org-scribe-planner--read-positive-number "New daily words: "
                                         (org-scribe-plan-daily-words plan))))
              (setf (org-scribe-plan-daily-words plan) new-daily)
              (setf (org-scribe-plan-days plan) nil)
              (org-scribe-planner--calculate-missing-variable plan)
              (org-scribe-planner--calculate-dates plan)))

           ((string-match "Days available" choice)
            (let ((new-days (org-scribe-planner--read-positive-number "New days available: "
                                        (org-scribe-plan-days plan))))
              (setf (org-scribe-plan-days plan) new-days)
              (setf (org-scribe-plan-daily-words plan) nil)
              (org-scribe-planner--calculate-missing-variable plan)
              (org-scribe-planner--calculate-dates plan)))

           ((string-match "Total words" choice)
            (let ((new-total (org-scribe-planner--read-positive-number "New total words: "
                                         (org-scribe-plan-total-words plan))))
              (setf (org-scribe-plan-total-words plan) new-total)
              (setf (org-scribe-plan-daily-words plan) nil)
              (org-scribe-planner--calculate-missing-variable plan)))

           ((string-match "Add/modify spare days" choice)
            (org-scribe-planner--configure-spare-days plan)
            (setf (org-scribe-plan-daily-words plan) nil)
            (org-scribe-planner--calculate-missing-variable plan)
            (org-scribe-planner--calculate-dates plan))

           ((string-match "Remove spare days" choice)
            (let ((current-spare-days (org-scribe-plan-spare-days plan)))
              (if (null current-spare-days)
                  (message "No spare days to remove")
                (let ((date (completing-read "Select date to remove: "
                                            (sort (copy-sequence current-spare-days) 'string<)
                                            nil t)))
                  (when date
                    (setf (org-scribe-plan-spare-days plan)
                          (delete date current-spare-days))
                    (setf (org-scribe-plan-daily-words plan) nil)
                    (org-scribe-planner--calculate-missing-variable plan)
                    (org-scribe-planner--calculate-dates plan)
                    (message "Removed %s from spare days and recalculated plan" date))))))

           ((string-match "Clear all spare days" choice)
            (let ((current-spare-days (org-scribe-plan-spare-days plan)))
              (if (null current-spare-days)
                  (message "No spare days to clear")
                (when (y-or-n-p (format "Remove all %d spare days and recalculate? " (length current-spare-days)))
                  (setf (org-scribe-plan-spare-days plan) nil)
                  (setf (org-scribe-plan-daily-words plan) nil)
                  (org-scribe-planner--calculate-missing-variable plan)
                  (org-scribe-planner--calculate-dates plan)
                  (message "Cleared all spare days and recalculated plan")))))))

        ;; Save updated plan to the same file location
        (org-scribe-planner--save-plan plan file)

        ;; Show updated calendar
        (org-scribe-planner-show-calendar plan file)
        (message "Plan recalculated and saved")))))

;;; Milestone Tracking

(defun org-scribe-planner--get-milestones (plan)
  "Calculate milestone dates for PLAN (25%, 50%, 75%, 100%).
This is the old implementation, kept for backward compatibility.
Use `org-scribe-planner--get-enhanced-milestones' for better tracking."
  (let* ((total (org-scribe-plan-total-words plan))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (milestones '((25 . nil) (50 . nil) (75 . nil) (100 . nil))))

    (dolist (day schedule)
      (let* ((cumulative (plist-get day :cumulative))
             (percent (* 100.0 (/ (float cumulative) total))))

        (dolist (milestone milestones)
          (when (and (>= percent (car milestone))
                    (not (cdr milestone)))
            (setcdr milestone (plist-get day :date))))))

    milestones))

(defun org-scribe-planner--get-enhanced-milestones (plan)
  "Calculate enhanced milestone information for PLAN.
Returns a list of plists with :percent, :words, :reached, :date, and :expected.
Tracks actual progress and calculates expected dates for unreached milestones."
  (let* ((total-words (org-scribe-plan-total-words plan))
         (daily-counts (org-scribe-plan-daily-word-counts plan))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (milestone-percentages '(25 50 75 100))
         (cumulative-actual 0)
         (actual-by-date nil)
         (results nil))

    ;; Build cumulative actual progress by date
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (daily-entry (assoc date daily-counts)))
        (when daily-entry
          (let ((actual (org-scribe-planner--get-entry-words daily-entry)))
            (setq cumulative-actual (+ cumulative-actual actual))
            (push (cons date cumulative-actual) actual-by-date)))))

    ;; Reverse to get chronological order
    (setq actual-by-date (nreverse actual-by-date))

    ;; Process each milestone
    (dolist (percent milestone-percentages)
      (let* ((milestone-words (/ (* percent total-words) 100))
             (reached (>= cumulative-actual milestone-words))
             (actual-date nil)
             (expected-date nil))

        (if reached
            ;; Find date when milestone was reached
            (progn
              (dolist (entry actual-by-date)
                (when (and (not actual-date)
                          (>= (cdr entry) milestone-words))
                  (setq actual-date (car entry)))))

          ;; Calculate expected date for unreached milestone
          (let ((remaining-words (- milestone-words cumulative-actual))
                (projection-total cumulative-actual))

            (dolist (day schedule)
              (let* ((date (plist-get day :date))
                     (is-spare (plist-get day :is-spare-day))
                     (has-actual (assoc date daily-counts))
                     (current-daily (plist-get day :words)))

                ;; Project forward using days without actuals
                (when (and (not has-actual)
                          (not is-spare)
                          (not expected-date))
                  (setq projection-total (+ projection-total current-daily))

                  (when (>= projection-total milestone-words)
                    (setq expected-date date)))))))

        ;; Add milestone to results
        (push (list :percent percent
                   :words milestone-words
                   :reached reached
                   :date actual-date
                   :expected expected-date)
              results)))

    ;; Return in ascending order (25%, 50%, 75%, 100%)
    (nreverse results)))

;;;###autoload
(defun org-scribe-planner-show-milestones ()
  "Show milestone dates for a writing plan with actual and expected dates."
  (interactive)
  (let ((file (org-scribe-planner--select-plan-file "Select plan file: ")))
    (when file
      (let* ((plan (org-scribe-planner--load-plan file))
             (milestones (org-scribe-planner--get-enhanced-milestones plan))
             (buffer (get-buffer-create "*Writing Plan Milestones*")))

        (with-current-buffer buffer
          (erase-buffer)
          (insert (propertize (format "Milestones for: %s\n\n"
                                     (org-scribe-plan-title plan))
                             'face 'org-level-1))

          (dolist (milestone milestones)
            (let ((percent (plist-get milestone :percent))
                  (words (plist-get milestone :words))
                  (reached (plist-get milestone :reached))
                  (date (plist-get milestone :date))
                  (expected (plist-get milestone :expected)))

              (cond
               ;; Milestone reached - show actual date
               (reached
                (insert (propertize
                        (format "%3d%% - %s (%d words) - Reached\n"
                               percent
                               (or date "Unknown date")
                               words)
                        'face 'org-done)))

               ;; Milestone not reached with expected date
               (expected
                (insert (format "%3d%% - Not reached (%d words) - expected on %s\n"
                               percent
                               words
                               expected)))

               ;; Milestone not reached, no expected date (past end date or insufficient days)
               (t
                (insert (propertize
                        (format "%3d%% - Not reached (%d words) - cannot be reached by end date\n"
                               percent
                               words)
                        'face 'org-warning)))))))

        (display-buffer buffer)))))

;;; Provide

(provide 'org-scribe-planner)

;;; org-scribe-planner.el ends here
