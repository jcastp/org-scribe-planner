;;; org-scribe-planner-dashboards.el --- Dashboard visualizations for org-scribe-planner -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Javier Castilla
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org-scribe-planner "0.1.0"))
;; Keywords: org, writing, planning, visualization
;; URL: https://codeberg.org/jcastp/org-scribe-planner

;;; Commentary:

;; This package provides dashboard visualizations for org-scribe-planner,
;; including progress dashboards, burndown charts, velocity graphs, and
;; consistency heatmaps.
;;
;; Main commands:
;; - `org-scribe-planner-show-progress-dashboard' - Display progress overview
;; - `org-scribe-planner-dashboards-menu' - Show dashboard selection menu

;;; Code:

;; Declare functions from org-scribe-planner (avoid circular dependency)
(declare-function org-scribe-planner--get-current-plan "org-scribe-planner")
(declare-function org-scribe-planner--generate-day-schedule "org-scribe-planner")
(declare-function org-scribe-planner--get-entry-words "org-scribe-planner")
(declare-function org-scribe-planner--add-days "org-scribe-planner")
(declare-function org-scribe-planner--days-between "org-scribe-planner")
(declare-function org-scribe-planner-show-current-plan "org-scribe-planner")

;; Access to struct accessors
(declare-function org-scribe-plan-total-words "org-scribe-planner")
(declare-function org-scribe-plan-daily-word-counts "org-scribe-planner")
(declare-function org-scribe-plan-daily-words "org-scribe-planner")
(declare-function org-scribe-plan-end-date "org-scribe-planner")
(declare-function org-scribe-plan-title "org-scribe-planner")

;;; Helper Functions - Number Formatting

(defun org-scribe-planner--format-number (num)
  "Format NUM with thousands separators."
  (let ((str (number-to-string num))
        (result ""))
    (while (> (length str) 3)
      (setq result (concat "," (substring str -3) result))
      (setq str (substring str 0 -3)))
    (concat str result)))

;;; Helper Functions - ASCII Visualizations

(defun org-scribe-planner--ascii-progress-bar (percent width)
  "Create ASCII progress bar showing PERCENT completion with WIDTH characters."
  (let* ((filled (round (/ (* percent width) 100.0)))
         (empty (- width filled)))
    (format "[%s%s] %.1f%%"
            (propertize (make-string filled ?█) 'face 'org-done)
            (propertize (make-string empty ?░) 'face 'shadow)
            percent)))

;;; Helper Functions - Date Utilities

(defun org-scribe-planner--get-today-date ()
  "Get today's date in YYYY-MM-DD format."
  (format-time-string "%Y-%m-%d"))

(defun org-scribe-planner--get-today-target (plan)
  "Get the target word count for today from PLAN.
Returns nil if today is not in the plan or is a spare day."
  (let* ((today (org-scribe-planner--get-today-date))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (today-entry (cl-find today schedule
                              :key (lambda (day) (plist-get day :date))
                              :test 'string=)))
    (when today-entry
      (if (plist-get today-entry :is-spare-day)
          0
        (plist-get today-entry :words)))))

(defun org-scribe-planner--get-today-actual (plan)
  "Get the actual word count written today from PLAN.
Returns nil if no data has been logged for today."
  (let* ((today (org-scribe-planner--get-today-date))
         (daily-counts (org-scribe-plan-daily-word-counts plan))
         (today-entry (assoc today daily-counts)))
    (when today-entry
      (org-scribe-planner--get-entry-words today-entry))))

;;; Helper Functions - Streak Calculation

(defun org-scribe-planner--calculate-current-streak (plan)
  "Calculate current and longest writing streaks for PLAN.
Returns plist with :current :longest and :last-streak-date."
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (today (org-scribe-planner--get-today-date))
         (current-streak 0)
         (longest-streak 0)
         (temp-streak 0)
         (last-streak-date nil))

    ;; Sort daily counts by date
    (setq daily-counts (sort (copy-sequence daily-counts)
                            (lambda (a b) (string< (car a) (car b)))))

    ;; Calculate streaks
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day))
             (entry (assoc date daily-counts))
             (has-words (and entry
                           (numberp (org-scribe-planner--get-entry-words entry))
                           (> (org-scribe-planner--get-entry-words entry) 0))))

        ;; Only count working days (not spare days)
        (unless is-spare
          (if has-words
              (progn
                (setq temp-streak (1+ temp-streak))
                (setq last-streak-date date)
                (when (> temp-streak longest-streak)
                  (setq longest-streak temp-streak)))
            ;; Break in streak
            (setq temp-streak 0)))))

    ;; Current streak is the temp streak only if it extends to today or recent past
    (setq current-streak
          (if (and last-streak-date
                  (or (string= last-streak-date today)
                      (< (org-scribe-planner--days-between last-streak-date today) 3)))
              temp-streak
            0))

    (list :current current-streak
          :longest longest-streak
          :last-date last-streak-date)))

;;; Helper Functions - Velocity Calculation

(defun org-scribe-planner--calculate-velocity (plan)
  "Calculate velocity metrics for PLAN.
Returns plist with :average :recent :trend :projected-date."
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         (total-words (org-scribe-plan-total-words plan))
         (today (org-scribe-planner--get-today-date))
         ;; Filter out note-only entries
         (counts-with-words (cl-remove-if-not
                            (lambda (entry)
                              (numberp (org-scribe-planner--get-entry-words entry)))
                            daily-counts))
         (word-values (mapcar #'org-scribe-planner--get-entry-words
                             counts-with-words))
         (cumulative-actual (if word-values
                               (apply '+ word-values)
                             0))
         (days-logged (length counts-with-words))
         (average-velocity (if (> days-logged 0)
                             (/ (float cumulative-actual) days-logged)
                           0)))

    ;; Calculate recent velocity (last 7 days)
    (let* ((sorted-counts (sort (copy-sequence counts-with-words)
                               (lambda (a b) (string< (car a) (car b)))))
           (recent-counts (last sorted-counts (min 7 (length sorted-counts))))
           (recent-words (mapcar #'org-scribe-planner--get-entry-words recent-counts))
           (recent-velocity (if recent-words
                              (/ (float (apply '+ recent-words))
                                 (length recent-words))
                            0))
           (trend (cond
                  ((= average-velocity 0) "no data")
                  ((> recent-velocity (* 1.1 average-velocity)) "accelerating")
                  ((< recent-velocity (* 0.9 average-velocity)) "slowing")
                  (t "steady")))
           (remaining-words (- total-words cumulative-actual))
           (projected-date (when (and (> recent-velocity 0) (> remaining-words 0))
                           (let ((days-needed (ceiling (/ remaining-words recent-velocity))))
                             (org-scribe-planner--add-days today days-needed)))))

      (list :average average-velocity
            :recent recent-velocity
            :trend trend
            :projected-date projected-date
            :days-logged days-logged))))

(defun org-scribe-planner--format-trend (trend)
  "Format TREND string with appropriate face."
  (pcase trend
    ("accelerating" (propertize "↗ Accelerating" 'face 'org-done))
    ("slowing" (propertize "↘ Slowing" 'face 'org-warning))
    ("steady" (propertize "→ Steady" 'face 'org-scheduled))
    (_ (propertize "? No data" 'face 'shadow))))

;;; Helper Functions - Schedule Position

(defun org-scribe-planner--calculate-schedule-position (plan)
  "Calculate if PLAN is ahead or behind schedule.
Returns plist with :status :days-ahead :words-ahead."
  (let* ((schedule (org-scribe-planner--generate-day-schedule plan))
         (today (org-scribe-planner--get-today-date))
         (daily-counts (org-scribe-plan-daily-word-counts plan))
         (cumulative-actual 0)
         (expected-by-today 0)
         (days-completed 0)
         (expected-days 0))

    ;; Calculate expected vs actual up to today
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day))
             (entry (assoc date daily-counts))
             (actual-words (when entry
                            (org-scribe-planner--get-entry-words entry))))

        ;; Stop when we reach future dates
        ;; Use (not (string< today date)) instead of string<= for Emacs 27 compatibility
        (when (not (string< today date))
          ;; Add to expected cumulative (skip spare days)
          (unless is-spare
            (setq expected-by-today (+ expected-by-today (plist-get day :words)))
            (setq expected-days (1+ expected-days)))

          ;; Add to actual cumulative if we have data
          (when (numberp actual-words)
            (setq cumulative-actual (+ cumulative-actual actual-words))
            (unless is-spare
              (setq days-completed (1+ days-completed)))))))

    (let* ((words-ahead (- cumulative-actual expected-by-today))
           (daily-target (org-scribe-plan-daily-words plan))
           (days-ahead (if (> daily-target 0)
                         (/ (float words-ahead) daily-target)
                       0))
           (status (cond
                   ((>= words-ahead 0) 'ahead)
                   (t 'behind))))

      (list :status status
            :days-ahead days-ahead
            :words-ahead words-ahead
            :cumulative-actual cumulative-actual
            :expected-by-today expected-by-today))))

;;; Main Dashboard Function

;;;###autoload
(defun org-scribe-planner-show-progress-dashboard ()
  "Display a comprehensive progress dashboard for the active plan."
  (interactive)
  (let ((current (org-scribe-planner--get-current-plan t)))
    (when current
      (let* ((plan (car current))
             (total (org-scribe-plan-total-words plan))
             (daily-counts (org-scribe-plan-daily-word-counts plan))
             (counts-with-words (cl-remove-if-not
                                (lambda (entry)
                                  (numberp (org-scribe-planner--get-entry-words entry)))
                                daily-counts))
             (current-words (if counts-with-words
                               (apply '+ (mapcar #'org-scribe-planner--get-entry-words
                                                counts-with-words))
                             0))
             (percent (if (> total 0)
                        (/ (* 100.0 current-words) total)
                      0))
             (progress-bar (org-scribe-planner--ascii-progress-bar percent 40))
             (velocity (org-scribe-planner--calculate-velocity plan))
             (streak (org-scribe-planner--calculate-current-streak plan))
             (position (org-scribe-planner--calculate-schedule-position plan))
             (today-target (org-scribe-planner--get-today-target plan))
             (today-actual (org-scribe-planner--get-today-actual plan)))

        (with-current-buffer (get-buffer-create "*Writing Dashboard*")
          (let ((inhibit-read-only t))
            (erase-buffer)

            ;; Header
            (insert (propertize "WRITING PROGRESS DASHBOARD\n"
                              'face '(:weight bold :height 1.3)))
            (insert (propertize (format "%s\n" (org-scribe-plan-title plan))
                              'face 'org-level-1))
            (insert (make-string 70 ?═) "\n\n")

            ;; Overall Progress Section
            (insert (propertize "📊 Overall Progress\n" 'face 'org-level-2))
            (insert (format "  %s\n" progress-bar))
            (insert (format "  %s / %s words (%.1f%% complete)\n\n"
                          (propertize (org-scribe-planner--format-number current-words)
                                    'face 'org-done)
                          (org-scribe-planner--format-number total)
                          percent))

            ;; Today's Status Section
            (insert (propertize "📝 Today's Target\n" 'face 'org-level-2))
            (if today-target
                (progn
                  (insert (format "  Target: %s words\n"
                                (org-scribe-planner--format-number today-target)))
                  (if (numberp today-actual)
                      (let ((today-percent (if (> today-target 0)
                                             (/ (* 100.0 today-actual) today-target)
                                           0)))
                        (insert (format "  Actual: %s words (%s%.1f%%)\n"
                                      (propertize (org-scribe-planner--format-number today-actual)
                                                'face (if (>= today-percent 100)
                                                        'org-done
                                                      'org-warning))
                                      (if (>= today-percent 100) "✓ " "")
                                      today-percent)))
                    (insert (propertize "  Actual: Not logged yet\n" 'face 'shadow))))
              (insert (propertize "  No target for today (plan ended or spare day)\n"
                                'face 'shadow)))
            (insert "\n")

            ;; Schedule Position Section
            (insert (propertize "📈 Schedule Status\n" 'face 'org-level-2))
            (let* ((status (plist-get position :status))
                   (words-ahead (plist-get position :words-ahead))
                   (days-ahead (plist-get position :days-ahead))
                   (status-text (if (eq status 'ahead)
                                   (propertize "AHEAD OF SCHEDULE"
                                             'face 'org-done)
                                 (propertize "BEHIND SCHEDULE"
                                           'face 'org-warning)))
                   (sign (if (>= words-ahead 0) "+" "")))
              (insert (format "  Status: %s\n" status-text))
              (insert (format "  Progress: %s%s words (%s%.1f days)\n"
                            sign
                            (org-scribe-planner--format-number words-ahead)
                            sign
                            days-ahead))
              (insert (format "  Expected by today: %s words\n"
                            (org-scribe-planner--format-number
                             (plist-get position :expected-by-today))))
              (insert (format "  Actual by today: %s words\n\n"
                            (org-scribe-planner--format-number
                             (plist-get position :cumulative-actual)))))

            ;; Velocity & Momentum Section
            (insert (propertize "⚡ Velocity & Momentum\n" 'face 'org-level-2))
            (let ((avg-velocity (plist-get velocity :average))
                  (recent-velocity (plist-get velocity :recent))
                  (days-logged (plist-get velocity :days-logged)))
              (insert (format "  Current streak: %s\n"
                            (propertize (format "%d days" (plist-get streak :current))
                                      'face (if (> (plist-get streak :current) 0)
                                              'org-done
                                            'shadow))))
              (insert (format "  Longest streak: %d days\n"
                            (plist-get streak :longest)))
              (insert (format "  Average velocity: %s words/day (%d days logged)\n"
                            (propertize (format "%.0f" avg-velocity)
                                      'face 'org-scheduled)
                            days-logged))
              (insert (format "  Recent velocity: %.0f words/day (last 7 days)\n"
                            recent-velocity))
              (insert (format "  Trend: %s\n\n"
                            (org-scribe-planner--format-trend
                             (plist-get velocity :trend)))))

            ;; Forecast Section
            (insert (propertize "🔮 Forecast\n" 'face 'org-level-2))
            (insert (format "  Planned end date: %s\n"
                          (org-scribe-plan-end-date plan)))
            (let ((projected (plist-get velocity :projected-date)))
              (if projected
                  (let* ((planned-end (org-scribe-plan-end-date plan))
                         (ahead (string< projected planned-end))
                         (status-indicator (if ahead
                                             (propertize "✓ AHEAD" 'face 'org-done)
                                           (propertize "⚠ BEHIND" 'face 'org-warning))))
                    (insert (format "  Projected finish: %s [%s]\n"
                                  projected status-indicator)))
                (insert (propertize "  Projected finish: Insufficient data\n"
                                  'face 'shadow))))

            (insert "\n" (make-string 70 ?═) "\n")
            (insert (propertize "\nPress 'q' to close | 'r' to refresh | 'c' to view calendar\n"
                              'face 'shadow)))

          (goto-char (point-min))
          (org-scribe-planner-dashboard-mode)
          (display-buffer (current-buffer)))))))

;;; Dashboard Mode

(defvar org-scribe-planner-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "r") #'org-scribe-planner-show-progress-dashboard)
    (define-key map (kbd "c") #'org-scribe-planner-show-current-plan)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap for `org-scribe-planner-dashboard-mode'.")

(define-derived-mode org-scribe-planner-dashboard-mode special-mode "Writing-Dashboard"
  "Major mode for displaying writing progress dashboards.

\\{org-scribe-planner-dashboard-mode-map}"
  (setq truncate-lines t
        buffer-read-only t))

;;; Dashboard Menu

;;;###autoload
(defun org-scribe-planner-dashboards-menu ()
  "Show dashboard selection menu."
  (interactive)
  (let ((choice (completing-read
                 "Select dashboard: "
                 '("Progress Dashboard")
                 nil t)))
    (pcase choice
      ("Progress Dashboard" (org-scribe-planner-show-progress-dashboard)))))

;;; Provide

(provide 'org-scribe-planner-dashboards)

;;; org-scribe-planner-dashboards.el ends here
