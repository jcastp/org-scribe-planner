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

(require 'chart)  ; Built-in Emacs charting library

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
    (define-key map (kbd "r") #'org-scribe-planner-dashboard-refresh)
    (define-key map (kbd "c") #'org-scribe-planner-show-current-plan)
    (define-key map (kbd "D") #'org-scribe-planner-dashboards-menu)
    (define-key map (kbd "p") #'org-scribe-planner-show-progress-dashboard)
    (define-key map (kbd "b") #'org-scribe-planner-show-burndown)
    (define-key map (kbd "g") #'org-scribe-planner-show-cumulative-progress)
    (define-key map (kbd "v") #'org-scribe-planner-show-velocity)
    (define-key map (kbd "V") #'org-scribe-planner-show-velocity-chart)
    (define-key map (kbd "P") #'org-scribe-planner-show-performance-analytics)
    (define-key map (kbd "h") #'org-scribe-planner-show-heatmap)
    (define-key map (kbd "a") #'org-scribe-planner-show-all-dashboards)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap for `org-scribe-planner-dashboard-mode'.")

(defun org-scribe-planner-dashboard-refresh ()
  "Refresh the current dashboard based on buffer name."
  (interactive)
  (let ((buffer-name (buffer-name)))
    (cond
     ((string= buffer-name "*Writing Dashboard*")
      (org-scribe-planner-show-progress-dashboard))
     ((string= buffer-name "*Writing Dashboard (SVG)*")
      (org-scribe-planner-show-progress-dashboard-svg))
     ((string= buffer-name "*Burndown Chart*")
      (org-scribe-planner-show-burndown-ascii))
     ((string= buffer-name "*Burndown Chart (Gnuplot)*")
      (org-scribe-planner-show-burndown-gnuplot))
     ((string= buffer-name "*Cumulative Progress*")
      (org-scribe-planner-show-cumulative-progress))
     ((string= buffer-name "*Velocity Statistics*")
      (org-scribe-planner-show-velocity))
     ((string= buffer-name "*Velocity Chart*")
      (org-scribe-planner-show-velocity-chart))
     ((string= buffer-name "*Performance Analytics*")
      (org-scribe-planner-show-performance-analytics))
     ((string= buffer-name "*Writing Heatmap*")
      (org-scribe-planner-show-heatmap))
     (t
      (message "Unknown dashboard type, use specific refresh command")))))

(define-derived-mode org-scribe-planner-dashboard-mode special-mode "Writing-Dashboard"
  "Major mode for displaying writing progress dashboards.

\\{org-scribe-planner-dashboard-mode-map}"
  (setq truncate-lines t
        buffer-read-only t))

;;; Burndown Chart (ASCII)

(defun org-scribe-planner--scale-to-height (value max-value height)
  "Scale VALUE (0 to MAX-VALUE) to chart HEIGHT."
  (if (and (> max-value 0) (> height 0))
      (round (/ (* (- max-value value) height) (float max-value)))
    0))

(defun org-scribe-planner--replace-char-at (string pos char)
  "Replace character at POS in STRING with CHAR."
  (let ((chars (string-to-list string)))
    (when (< pos (length chars))
      (setf (nth pos chars) char))
    (concat chars)))

(defun org-scribe-planner--draw-ascii-burndown (data-points width height max-words)
  "Draw ASCII burndown chart from DATA-POINTS.
WIDTH and HEIGHT are chart dimensions, MAX-WORDS is the scale maximum."
  (let ((canvas (make-vector height nil))
        (points-count (length data-points)))

    ;; Initialize canvas rows
    (dotimes (i height)
      (aset canvas i (make-string width ?\s)))

    ;; Plot points
    (dotimes (i points-count)
      (when (< i width)
        (let* ((point (nth i data-points))
               (ideal (plist-get point :ideal))
               (actual (plist-get point :actual))
               (ideal-y (org-scribe-planner--scale-to-height ideal max-words height))
               (actual-y (when actual
                          (org-scribe-planner--scale-to-height actual max-words height))))

          ;; Plot ideal point
          (when (and (>= ideal-y 0) (< ideal-y height))
            (aset canvas ideal-y
                  (org-scribe-planner--replace-char-at
                   (aref canvas ideal-y) i ?-)))

          ;; Plot actual point (if exists)
          (when (and actual-y (>= actual-y 0) (< actual-y height))
            (aset canvas actual-y
                  (org-scribe-planner--replace-char-at
                   (aref canvas actual-y) i ?#))))))

    ;; Render canvas (top to bottom = high to low values)
    (dotimes (i height)
      (let ((row (aref canvas (- height i 1))))
        (insert "│" row "│\n")))

    ;; Draw X-axis
    (insert "└" (make-string width ?─) "┘\n")))

(defun org-scribe-planner-show-burndown-ascii-internal ()
  "Display ASCII burndown chart for the active plan (internal function)."
  (let ((current (org-scribe-planner--get-current-plan t)))
    (when current
      (let* ((plan (car current))
             (schedule (org-scribe-planner--generate-day-schedule plan))
             (total (org-scribe-plan-total-words plan))
             (daily-counts (org-scribe-plan-daily-word-counts plan))
             (chart-width 70)
             (chart-height 20)
             (data-points nil))

        ;; Build data points: (date ideal-remaining actual-remaining)
        (let ((cumulative-actual 0))
          (dolist (day schedule)
            (let* ((date (plist-get day :date))
                   (cumulative-planned (plist-get day :cumulative))
                   (ideal-remaining (- total cumulative-planned))
                   (entry (assoc date daily-counts))
                   (actual-words (when entry
                                  (org-scribe-planner--get-entry-words entry))))

              (when (numberp actual-words)
                (setq cumulative-actual (+ cumulative-actual actual-words)))

              (push (list :date date
                         :ideal ideal-remaining
                         :actual (- total cumulative-actual))
                    data-points))))

        (setq data-points (nreverse data-points))

        ;; Render chart
        (with-current-buffer (get-buffer-create "*Burndown Chart*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "BURNDOWN CHART\n"
                              'face '(:weight bold :height 1.2)))
            (insert (propertize (format "%s\n" (org-scribe-plan-title plan))
                              'face 'org-level-1))
            (insert (make-string 75 ?═) "\n\n")

            ;; Y-axis label
            (insert (format "%s words remaining\n"
                          (org-scribe-planner--format-number total)))
            (insert "↑\n")

            ;; Draw chart
            (org-scribe-planner--draw-ascii-burndown
             data-points chart-width chart-height total)

            (insert "  ")
            (insert (format "Start: %s" (org-scribe-plan-start-date plan)))
            (insert (make-string (- chart-width 35) ?\s))
            (insert (format "End: %s\n\n" (org-scribe-plan-end-date plan)))

            (insert "Legend:\n")
            (insert "  " (propertize "---" 'face 'org-done) " Ideal burndown\n")
            (insert "  " (propertize "###" 'face 'org-warning) " Actual burndown\n\n")

            (insert (propertize "Interpretation:\n" 'face 'org-level-2))
            (insert "  • Actual below ideal = Ahead of schedule\n")
            (insert "  • Actual above ideal = Behind schedule\n")
            (insert "  • Lines converging = Catching up\n")
            (insert "  • Lines diverging = Falling further behind\n\n")

            (insert (propertize "Press 'q' to close | 'r' to refresh | 'c' to view calendar\n"
                              'face 'shadow)))

          (goto-char (point-min))
          (org-scribe-planner-dashboard-mode)
          (display-buffer (current-buffer)))))))

;;;###autoload
(defun org-scribe-planner-show-burndown (&optional force-ascii)
  "Display burndown chart for the active plan.
Prefers gnuplot version if available, falls back to ASCII.
With prefix argument FORCE-ASCII, always use ASCII version."
  (interactive "P")
  (if (and (not force-ascii)
           (executable-find "gnuplot")
           (display-graphic-p))
      (org-scribe-planner-show-burndown-gnuplot)
    (org-scribe-planner-show-burndown-ascii-internal)))

;;;###autoload
(defun org-scribe-planner-show-burndown-ascii ()
  "Display ASCII burndown chart (text-based)."
  (interactive)
  (org-scribe-planner-show-burndown-ascii-internal))

;;; Gnuplot Burndown Chart

(defun org-scribe-planner--gnuplot-available-p ()
  "Check if gnuplot is available on the system."
  (executable-find "gnuplot"))

(defun org-scribe-planner--generate-gnuplot-burndown (plan filepath)
  "Generate gnuplot burndown chart for PLAN, save to FILEPATH.
Returns t if successful, nil otherwise."
  (let* ((schedule (org-scribe-planner--generate-day-schedule plan))
         (total (org-scribe-plan-total-words plan))
         (daily-counts (org-scribe-plan-daily-word-counts plan))
         (data-file (make-temp-file "org-scribe-burndown" nil ".dat"))
         (script-file (make-temp-file "org-scribe-burndown" nil ".gp")))

    (condition-case err
        (progn
          ;; Write data file for gnuplot
          (with-temp-file data-file
            (insert "# Date Ideal-Remaining Actual-Remaining\n")
            (let ((cumulative-actual 0))
              (dolist (day schedule)
                (let* ((date (plist-get day :date))
                       (cumulative-planned (plist-get day :cumulative))
                       (ideal-remaining (- total cumulative-planned))
                       (entry (assoc date daily-counts))
                       (actual-words (when entry
                                      (org-scribe-planner--get-entry-words entry))))

                  (when (numberp actual-words)
                    (setq cumulative-actual (+ cumulative-actual actual-words)))

                  (insert (format "%s %d %d\n"
                                date
                                ideal-remaining
                                (- total cumulative-actual)))))))

          ;; Write gnuplot script
          (with-temp-file script-file
            (insert (format "
set terminal png size 1000,600 enhanced font 'Arial,12'
set output '%s'
set title 'Burndown Chart: %s' font 'Arial,16'
set xlabel 'Date' font 'Arial,12'
set ylabel 'Words Remaining' font 'Arial,12'
set xdata time
set timefmt '%%Y-%%m-%%d'
set format x '%%m/%%d'
set grid ytics xtics
set key left top box
set style line 1 lc rgb '#2E7D32' lt 1 lw 2
set style line 2 lc rgb '#1976D2' lt 1 lw 3
set style line 3 lc rgb '#E0E0E0' lt 2 lw 1
set border lw 1.5

plot '%s' using 1:2 with lines ls 1 title 'Ideal Burndown', \\
     '' using 1:3 with lines ls 2 title 'Actual Progress'
"
                           filepath
                           (org-scribe-plan-title plan)
                           data-file)))

          ;; Execute gnuplot
          (let ((result (call-process "gnuplot" nil nil nil script-file)))
            (when (= result 0)
              ;; Clean up temp files
              (delete-file data-file)
              (delete-file script-file)
              (file-exists-p filepath))))

      (error
       (message "Error generating gnuplot chart: %s" (error-message-string err))
       nil))))

;;;###autoload
(defun org-scribe-planner-show-burndown-gnuplot ()
  "Display burndown chart using gnuplot (high quality)."
  (interactive)
  (let ((current (org-scribe-planner--get-current-plan t)))
    (when current
      (let* ((plan (car current))
             (output-file (make-temp-file "org-scribe-burndown" nil ".png")))

        (if (org-scribe-planner--gnuplot-available-p)
            (if (org-scribe-planner--generate-gnuplot-burndown plan output-file)
                (progn
                  ;; Display image in buffer
                  (with-current-buffer (get-buffer-create "*Burndown Chart (Gnuplot)*")
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert (propertize "BURNDOWN CHART (GNUPLOT)\n"
                                        'face '(:weight bold :height 1.2)))
                      (insert (propertize (format "%s\n" (org-scribe-plan-title plan))
                                        'face 'org-level-1))
                      (insert (make-string 75 ?═) "\n\n")

                      (when (display-graphic-p)
                        (insert-image (create-image output-file)))

                      (insert "\n\n")
                      (insert (propertize "Chart saved to: " 'face 'org-level-2))
                      (insert (format "%s\n\n" output-file))

                      (insert (propertize "Interpretation:\n" 'face 'org-level-2))
                      (insert "  • Green line = Ideal burndown (linear decline)\n")
                      (insert "  • Blue line = Actual progress\n")
                      (insert "  • Actual below ideal = Ahead of schedule ✓\n")
                      (insert "  • Actual above ideal = Behind schedule ⚠\n")
                      (insert "  • Lines converging = Catching up\n")
                      (insert "  • Lines diverging = Falling further behind\n\n")

                      (insert (make-string 75 ?═) "\n")
                      (insert (propertize "Press 'q' to close | 'r' to refresh | 's' to save\n"
                                        'face 'shadow)))

                    (goto-char (point-min))
                    (org-scribe-planner-dashboard-mode)
                    (display-buffer (current-buffer))))
              (message "Failed to generate gnuplot chart, falling back to ASCII")
              (org-scribe-planner-show-burndown-ascii))
          (message "Gnuplot not found, falling back to ASCII version")
          (org-scribe-planner-show-burndown-ascii))))))

;;; Gnuplot Cumulative Progress Graph

(defun org-scribe-planner--generate-gnuplot-cumulative (plan filepath)
  "Generate gnuplot cumulative progress chart for PLAN, save to FILEPATH.
Returns t if successful, nil otherwise."
  (let* ((schedule (org-scribe-planner--generate-day-schedule plan))
         (daily-counts (org-scribe-plan-daily-word-counts plan))
         (data-file (make-temp-file "org-scribe-cumulative" nil ".dat"))
         (script-file (make-temp-file "org-scribe-cumulative" nil ".gp"))
         (today (org-scribe-planner--get-today-date)))

    (condition-case err
        (progn
          ;; Write data file for gnuplot
          (with-temp-file data-file
            (insert "# Date Planned-Cumulative Actual-Cumulative\n")
            (let ((cumulative-actual 0))
              (dolist (day schedule)
                (let* ((date (plist-get day :date))
                       (cumulative-planned (plist-get day :cumulative))
                       (entry (assoc date daily-counts))
                       (actual-words (when entry
                                      (org-scribe-planner--get-entry-words entry))))

                  (when (numberp actual-words)
                    (setq cumulative-actual (+ cumulative-actual actual-words)))

                  ;; Write data point (use NA for actual if no data yet)
                  (insert (format "%s %d %s\n"
                                date
                                cumulative-planned
                                (if (> cumulative-actual 0)
                                    (number-to-string cumulative-actual)
                                  "?")))))))

          ;; Write gnuplot script
          (with-temp-file script-file
            (insert (format "
set terminal png size 1200,700 enhanced font 'Arial,12'
set output '%s'
set title 'Cumulative Progress: %s' font 'Arial,16'
set xlabel 'Date' font 'Arial,12'
set ylabel 'Total Words Written' font 'Arial,12'
set xdata time
set timefmt '%%Y-%%m-%%d'
set format x '%%m/%%d'
set grid ytics xtics
set key left top box
set style line 1 lc rgb '#2E7D32' lt 1 lw 2 dt 2
set style line 2 lc rgb '#1976D2' lt 1 lw 3
set style line 3 lc rgb '#D32F2F' lt 2 lw 1
set border lw 1.5

# Add a vertical line for today
set arrow from '%s',graph 0 to '%s',graph 1 nohead lc rgb '#FF9800' lw 2 dt 3

plot '%s' using 1:2 with lines ls 1 title 'Planned Progress', \\
     '' using 1:3 with lines ls 2 title 'Actual Progress'
"
                           filepath
                           (org-scribe-plan-title plan)
                           today
                           today
                           data-file)))

          ;; Execute gnuplot
          (let ((result (call-process "gnuplot" nil nil nil script-file)))
            (when (= result 0)
              ;; Clean up temp files
              (delete-file data-file)
              (delete-file script-file)
              (file-exists-p filepath))))

      (error
       (message "Error generating cumulative progress chart: %s" (error-message-string err))
       nil))))

;;;###autoload
(defun org-scribe-planner-show-cumulative-progress ()
  "Display cumulative progress chart using gnuplot."
  (interactive)
  (let ((current (org-scribe-planner--get-current-plan t)))
    (when current
      (let* ((plan (car current))
             (output-file (make-temp-file "org-scribe-cumulative" nil ".png")))

        (if (org-scribe-planner--gnuplot-available-p)
            (if (org-scribe-planner--generate-gnuplot-cumulative plan output-file)
                (progn
                  ;; Calculate current stats
                  (let* ((position (org-scribe-planner--calculate-schedule-position plan))
                         (cumulative-actual (plist-get position :cumulative-actual))
                         (expected-by-today (plist-get position :expected-by-today))
                         (words-ahead (plist-get position :words-ahead))
                         (status (plist-get position :status)))

                    ;; Display image in buffer
                    (with-current-buffer (get-buffer-create "*Cumulative Progress*")
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert (propertize "CUMULATIVE PROGRESS CHART\n"
                                          'face '(:weight bold :height 1.2)))
                        (insert (propertize (format "%s\n" (org-scribe-plan-title plan))
                                          'face 'org-level-1))
                        (insert (make-string 75 ?═) "\n\n")

                        (when (display-graphic-p)
                          (insert-image (create-image output-file)))

                        (insert "\n\n")
                        (insert (propertize "Current Status\n" 'face 'org-level-2))
                        (insert (format "  Actual words written:   %s\n"
                                      (org-scribe-planner--format-number cumulative-actual)))
                        (insert (format "  Expected by today:      %s\n"
                                      (org-scribe-planner--format-number expected-by-today)))
                        (let ((status-text (if (eq status 'ahead)
                                              (propertize "AHEAD OF SCHEDULE ✓"
                                                        'face 'org-done)
                                            (propertize "BEHIND SCHEDULE ⚠"
                                                      'face 'org-warning)))
                              (sign (if (>= words-ahead 0) "+" "")))
                          (insert (format "  Status:                 %s (%s%s words)\n\n"
                                        status-text
                                        sign
                                        (org-scribe-planner--format-number words-ahead))))

                        (insert (propertize "Chart saved to: " 'face 'org-level-2))
                        (insert (format "%s\n\n" output-file))

                        (insert (propertize "Interpretation:\n" 'face 'org-level-2))
                        (insert "  • Green dashed line = Planned cumulative progress\n")
                        (insert "  • Blue solid line = Your actual cumulative progress\n")
                        (insert "  • Orange vertical line = Today\n")
                        (insert "  • Blue above green = Ahead of schedule ✓\n")
                        (insert "  • Blue below green = Behind schedule ⚠\n")
                        (insert "  • Gap widening = Building momentum\n")
                        (insert "  • Gap narrowing = Losing momentum or catching up\n\n")

                        (insert (make-string 75 ?═) "\n")
                        (insert (propertize "Press 'q' to close | 'r' to refresh | 'c' to calendar\n"
                                          'face 'shadow)))

                      (goto-char (point-min))
                      (org-scribe-planner-dashboard-mode)
                      (display-buffer (current-buffer)))))
              (message "Failed to generate cumulative progress chart"))
          (message "Gnuplot not available. Install gnuplot to use this feature."))))))

;;; SVG Progress Indicators

(defun org-scribe-planner--svg-available-p ()
  "Check if SVG rendering is available."
  (and (fboundp 'svg-create)
       (display-graphic-p)))

(defun org-scribe-planner--create-svg-progress-bar (percent width height)
  "Create an SVG progress bar showing PERCENT completion.
WIDTH and HEIGHT are dimensions in pixels."
  (require 'svg)
  (let* ((svg (svg-create width height))
         (filled-width (* width (/ percent 100.0)))
         (border-radius 4)
         ;; Color based on completion
         (fill-color (cond
                     ((>= percent 100) "#4CAF50")  ; Green
                     ((>= percent 75) "#2196F3")   ; Blue
                     ((>= percent 50) "#FF9800")   ; Orange
                     (t "#F44336"))))              ; Red

    ;; Background (empty part)
    (svg-rectangle svg 0 0 width height
                   :fill "#E0E0E0"
                   :rx border-radius
                   :ry border-radius)

    ;; Filled part (progress)
    (when (> filled-width 0)
      (svg-rectangle svg 0 0 filled-width height
                     :fill fill-color
                     :rx border-radius
                     :ry border-radius))

    ;; Border
    (svg-rectangle svg 0 0 width height
                   :fill "none"
                   :stroke "#BDBDBD"
                   :stroke-width 1
                   :rx border-radius
                   :ry border-radius)

    ;; Text label (centered)
    (svg-text svg (format "%.1f%%" percent)
              :x (/ width 2)
              :y (/ height 1.5)
              :font-size 12
              :font-family "Arial, sans-serif"
              :font-weight "bold"
              :fill (if (< percent 50) "#424242" "#FFFFFF")
              :text-anchor "middle")

    svg))

(defun org-scribe-planner--create-svg-sparkline (values width height)
  "Create an SVG sparkline from VALUES list.
WIDTH and HEIGHT are dimensions in pixels."
  (require 'svg)
  (when (and values (> (length values) 1))
    (let* ((svg (svg-create width height))
           (min-val (apply 'min values))
           (max-val (apply 'max values))
           (range (- max-val min-val))
           (step (/ (float width) (1- (length values))))
           (points nil))

      ;; Calculate points
      (dotimes (i (length values))
        (let* ((val (nth i values))
               (x (* i step))
               (y (if (= range 0)
                     (/ height 2)
                   (- height (* (/ (- val min-val) (float range)) height)))))
          (push (cons x y) points)))

      (setq points (nreverse points))

      ;; Draw area fill
      (let ((path-data (format "M 0,%d " height)))
        (dolist (point points)
          (setq path-data (concat path-data (format "L %.1f,%.1f " (car point) (cdr point)))))
        (setq path-data (concat path-data (format "L %d,%d Z" width height)))
        (svg-node svg 'path
                  :d path-data
                  :fill "#E3F2FD"
                  :opacity 0.5))

      ;; Draw line
      (let ((path-data ""))
        (dolist (point points)
          (if (string-empty-p path-data)
              (setq path-data (format "M %.1f,%.1f" (car point) (cdr point)))
            (setq path-data (concat path-data (format " L %.1f,%.1f" (car point) (cdr point))))))
        (svg-node svg 'path
                  :d path-data
                  :fill "none"
                  :stroke "#2196F3"
                  :stroke-width 2))

      ;; Draw points
      (dolist (point points)
        (svg-circle svg (car point) (cdr point) 3
                    :fill "#1976D2"
                    :stroke "#FFFFFF"
                    :stroke-width 1))

      svg)))

(defun org-scribe-planner--create-svg-milestone-badge (percent label width height)
  "Create an SVG milestone badge showing PERCENT with LABEL.
WIDTH and HEIGHT are dimensions in pixels."
  (require 'svg)
  (let* ((svg (svg-create width height))
         (achieved (>= percent 100))
         (badge-color (if achieved "#4CAF50" "#9E9E9E"))
         (text-color (if achieved "#FFFFFF" "#757575")))

    ;; Circle background
    (svg-circle svg (/ width 2) (/ height 2) (/ (min width height) 2.2)
                :fill badge-color
                :stroke (if achieved "#388E3C" "#616161")
                :stroke-width 2)

    ;; Checkmark or percent
    (if achieved
        ;; Draw checkmark
        (let ((cx (/ width 2))
              (cy (/ height 2)))
          (svg-node svg 'path
                    :d (format "M %d,%d L %d,%d L %d,%d"
                             (- cx 8) cy
                             (- cx 2) (+ cy 8)
                             (+ cx 10) (- cy 8))
                    :fill "none"
                    :stroke "#FFFFFF"
                    :stroke-width 3
                    :stroke-linecap "round"
                    :stroke-linejoin "round"))
      ;; Draw percent
      (svg-text svg (format "%.0f%%" percent)
                :x (/ width 2)
                :y (+ (/ height 2) 5)
                :font-size 16
                :font-family "Arial, sans-serif"
                :font-weight "bold"
                :fill text-color
                :text-anchor "middle"))

    ;; Label below
    (when label
      (svg-text svg label
                :x (/ width 2)
                :y (- height 5)
                :font-size 10
                :font-family "Arial, sans-serif"
                :fill "#424242"
                :text-anchor "middle"))

    svg))

(defun org-scribe-planner--insert-svg-image (svg)
  "Insert SVG as an image at point."
  (when (org-scribe-planner--svg-available-p)
    (insert-image (svg-image svg))))

;;; Enhanced Progress Dashboard with SVG

;;;###autoload
(defun org-scribe-planner-show-progress-dashboard-svg ()
  "Display enhanced progress dashboard with SVG indicators."
  (interactive)
  (if (not (org-scribe-planner--svg-available-p))
      (progn
        (message "SVG not available, showing standard dashboard")
        (org-scribe-planner-show-progress-dashboard))

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
               (velocity (org-scribe-planner--calculate-velocity plan))
               (streak (org-scribe-planner--calculate-current-streak plan))
               (position (org-scribe-planner--calculate-schedule-position plan))
               (today-target (org-scribe-planner--get-today-target plan))
               (today-actual (org-scribe-planner--get-today-actual plan)))

          (with-current-buffer (get-buffer-create "*Writing Dashboard (SVG)*")
            (let ((inhibit-read-only t))
              (erase-buffer)

              ;; Header
              (insert (propertize "WRITING PROGRESS DASHBOARD\n"
                                'face '(:weight bold :height 1.3)))
              (insert (propertize (format "%s\n" (org-scribe-plan-title plan))
                                'face 'org-level-1))
              (insert (make-string 70 ?═) "\n\n")

              ;; Overall Progress with SVG Bar
              (insert (propertize "📊 Overall Progress\n" 'face 'org-level-2))
              (insert "  ")
              (org-scribe-planner--insert-svg-image
               (org-scribe-planner--create-svg-progress-bar percent 400 30))
              (insert (format "\n  %s / %s words\n\n"
                            (propertize (org-scribe-planner--format-number current-words)
                                      'face 'org-done)
                            (org-scribe-planner--format-number total)))

              ;; Milestones with SVG Badges
              (insert (propertize "🎯 Milestones\n" 'face 'org-level-2))
              (insert "  ")
              (dolist (milestone '(25 50 75 100))
                (org-scribe-planner--insert-svg-image
                 (org-scribe-planner--create-svg-milestone-badge
                  (/ (* 100.0 percent) milestone)
                  (format "%d%%" milestone)
                  60 80))
                (insert " "))
              (insert "\n\n")

              ;; Recent Velocity Sparkline
              (when (> (length counts-with-words) 2)
                (let* ((sorted-entries (sort (copy-sequence counts-with-words)
                                            (lambda (a b) (string< (car a) (car b)))))
                       (recent-entries (last sorted-entries (min 14 (length sorted-entries))))
                       (word-counts (mapcar #'org-scribe-planner--get-entry-words recent-entries)))
                  (insert (propertize "📈 Recent Velocity (Last 14 Days)\n" 'face 'org-level-2))
                  (insert "  ")
                  (org-scribe-planner--insert-svg-image
                   (org-scribe-planner--create-svg-sparkline word-counts 400 60))
                  (insert (format "\n  Average: %.0f words/day\n\n"
                                (plist-get velocity :average)))))

              ;; Today's Progress
              (insert (propertize "📝 Today's Target\n" 'face 'org-level-2))
              (if today-target
                  (let ((today-percent (if (and (numberp today-actual) (> today-target 0))
                                          (/ (* 100.0 today-actual) today-target)
                                        0)))
                    (insert "  ")
                    (org-scribe-planner--insert-svg-image
                     (org-scribe-planner--create-svg-progress-bar today-percent 400 25))
                    (insert (format "\n  Target: %s | Actual: %s\n\n"
                                  (org-scribe-planner--format-number today-target)
                                  (if (numberp today-actual)
                                      (org-scribe-planner--format-number today-actual)
                                    "Not logged"))))
                (insert (propertize "  No target for today\n\n" 'face 'shadow)))

              ;; Status Summary (text)
              (insert (propertize "📈 Schedule Status\n" 'face 'org-level-2))
              (let* ((status (plist-get position :status))
                     (words-ahead (plist-get position :words-ahead))
                     (status-text (if (eq status 'ahead)
                                     (propertize "AHEAD OF SCHEDULE"
                                               'face 'org-done)
                                   (propertize "BEHIND SCHEDULE"
                                             'face 'org-warning)))
                     (sign (if (>= words-ahead 0) "+" "")))
                (insert (format "  Status: %s\n" status-text))
                (insert (format "  Progress: %s%s words\n\n"
                              sign
                              (org-scribe-planner--format-number words-ahead))))

              ;; Momentum
              (insert (propertize "⚡ Momentum\n" 'face 'org-level-2))
              (insert (format "  Current streak: %d days\n"
                            (plist-get streak :current)))
              (insert (format "  Trend: %s\n\n"
                            (org-scribe-planner--format-trend
                             (plist-get velocity :trend))))

              (insert (make-string 70 ?═) "\n")
              (insert (propertize "Press 'q' to close | 'r' to refresh | 'c' to calendar\n"
                                'face 'shadow)))

            (goto-char (point-min))
            (org-scribe-planner-dashboard-mode)
            (display-buffer (current-buffer))))))))

;;; Helper Functions - Moving Average and Date Formatting

(defun org-scribe-planner--moving-average (values window)
  "Calculate WINDOW-day moving average of VALUES.
VALUES is a list of numbers, WINDOW is the number of days to average over."
  (let ((result nil))
    (dotimes (i (length values))
      (let* ((start (max 0 (- i (1- window))))
             (window-values (cl-subseq values start (1+ i)))
             (avg (/ (float (apply '+ window-values)) (length window-values))))
        (push avg result)))
    (nreverse result)))

(defun org-scribe-planner--format-date-labels (dates &optional max-labels)
  "Format DATES list for chart labels.
Shows every Nth date to avoid overcrowding. MAX-LABELS defaults to 10."
  (unless max-labels (setq max-labels 10))
  (let ((step (max 1 (/ (length dates) max-labels))))
    (cl-loop for date in dates
             for i from 0
             collect (if (= 0 (mod i step))
                        (substring date 5)  ; "MM-DD" format
                      ""))))

;;; Velocity Statistics Display

;;;###autoload
(defun org-scribe-planner-show-velocity ()
  "Display velocity statistics and trends for the active plan."
  (interactive)
  (let ((current (org-scribe-planner--get-current-plan t)))
    (when current
      (let* ((plan (car current))
             (daily-counts (org-scribe-plan-daily-word-counts plan))
             (counts-with-words (cl-remove-if-not
                                (lambda (entry)
                                  (numberp (org-scribe-planner--get-entry-words entry)))
                                daily-counts))
             (sorted-entries (sort (copy-sequence counts-with-words)
                                  (lambda (a b) (string< (car a) (car b)))))
             (word-counts (mapcar #'org-scribe-planner--get-entry-words sorted-entries))
             (target (org-scribe-plan-daily-words plan))
             (velocity (org-scribe-planner--calculate-velocity plan)))

        (with-current-buffer (get-buffer-create "*Velocity Statistics*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "VELOCITY STATISTICS\n"
                              'face '(:weight bold :height 1.2)))
            (insert (propertize (format "%s\n" (org-scribe-plan-title plan))
                              'face 'org-level-1))
            (insert (make-string 70 ?═) "\n\n")

            ;; Summary statistics
            (insert (propertize "📊 Summary Statistics\n" 'face 'org-level-2))
            (when (> (length word-counts) 0)
              (let ((total (apply '+ word-counts))
                    (avg (plist-get velocity :average))
                    (recent (plist-get velocity :recent))
                    (max-words (apply 'max word-counts))
                    (min-words (apply 'min word-counts))
                    (days-logged (plist-get velocity :days-logged)))

                (insert (format "  Days logged:      %d\n" days-logged))
                (insert (format "  Total words:      %s\n"
                              (org-scribe-planner--format-number total)))
                (insert (format "  Average:          %.0f words/day\n" avg))
                (insert (format "  Recent (7 days):  %.0f words/day\n" recent))
                (insert (format "  Target:           %s words/day\n"
                              (org-scribe-planner--format-number target)))
                (insert (format "  Best day:         %s words\n"
                              (org-scribe-planner--format-number max-words)))
                (insert (format "  Worst day:        %s words\n"
                              (org-scribe-planner--format-number min-words)))
                (insert (format "  Trend:            %s\n\n"
                              (org-scribe-planner--format-trend
                               (plist-get velocity :trend)))))

              ;; Chart.el visualization
              (when (> (length word-counts) 0)
                (insert (propertize "📊 Daily Word Counts Chart\n" 'face 'org-level-2))
                (insert "\n")

                ;; Use chart.el to create vertical bar chart
                (let* ((chart-entries (last sorted-entries (min 20 (length sorted-entries))))
                       (chart-dates (mapcar #'car chart-entries))
                       (chart-words (mapcar #'org-scribe-planner--get-entry-words chart-entries))
                       (chart-labels (org-scribe-planner--format-date-labels chart-dates 10)))

                  (chart-bar-quickie 'vertical
                                    (format "Daily Word Counts - %s"
                                            (org-scribe-plan-title plan))
                                    chart-labels
                                    "Days"
                                    chart-words
                                    "Words")
                  (insert "\n"))

                (insert (make-string 70 ?─) "\n\n"))

              ;; Performance bar chart (detailed ASCII)
              (insert (propertize "📈 Performance Overview (Last 14 Days)\n" 'face 'org-level-2))
              (insert (format "  Target: %s words/day\n\n"
                            (org-scribe-planner--format-number target)))

              ;; Show last 14 days as bars
              (let ((recent-entries (last sorted-entries (min 14 (length sorted-entries)))))
                (dolist (entry recent-entries)
                  (let* ((date (car entry))
                         (words (org-scribe-planner--get-entry-words entry))
                         (percent (if (> target 0)
                                    (/ (* 100.0 words) target)
                                  100))
                         (bar-length (min 40 (round (/ (* percent 40) 100.0))))
                         (bar (make-string bar-length ?█))
                         (face (cond
                               ((>= percent 100) 'org-done)
                               ((>= percent 75) 'org-scheduled)
                               (t 'org-warning))))

                    (insert (format "  %s  " date))
                    (insert (propertize bar 'face face))
                    (insert (format " %s (%.0f%%)\n"
                                  (org-scribe-planner--format-number words)
                                  percent)))))

              (insert "\n")
              (insert (propertize "Color coding:\n" 'face 'org-level-3))
              (insert "  " (propertize "█" 'face 'org-done)
                     " ≥100% of target\n")
              (insert "  " (propertize "█" 'face 'org-scheduled)
                     " 75-99% of target\n")
              (insert "  " (propertize "█" 'face 'org-warning)
                     " <75% of target\n"))

            (insert "\n" (make-string 70 ?═) "\n")
            (insert (propertize "Press 'q' to close | 'r' to refresh | 'c' to view calendar\n"
                              'face 'shadow)))

          (goto-char (point-min))
          (org-scribe-planner-dashboard-mode)
          (display-buffer (current-buffer)))))))

;;; Performance Analytics Helper Functions

(defun org-scribe-planner--calculate-day-of-week-stats (plan)
  "Calculate day-of-week performance statistics for PLAN.
Returns an alist with keys 0-6 (Sunday-Saturday) and values as plists
containing :day-name :total-words :count :average."
  (let ((schedule (org-scribe-planner--generate-day-schedule plan))
        (daily-counts (org-scribe-plan-daily-word-counts plan))
        (dow-data (make-vector 7 nil)))

    ;; Initialize day-of-week data structure
    (dotimes (i 7)
      (aset dow-data i (list :day-name (calendar-day-name i nil t)
                             :total-words 0
                             :count 0
                             :average 0.0)))

    ;; Accumulate data by day of week
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day))
             (entry (assoc date daily-counts))
             (actual-words (when entry
                            (org-scribe-planner--get-entry-words entry))))

        ;; Only count non-spare days with data
        (when (and (not is-spare) (numberp actual-words))
          (let* ((date-parts (mapcar 'string-to-number (split-string date "-")))
                 (year (nth 0 date-parts))
                 (month (nth 1 date-parts))
                 (day-num (nth 2 date-parts))
                 (dow (calendar-day-of-week (list month day-num year)))
                 (dow-entry (aref dow-data dow)))

            (plist-put dow-entry :total-words
                      (+ (plist-get dow-entry :total-words) actual-words))
            (plist-put dow-entry :count
                      (1+ (plist-get dow-entry :count)))))))

    ;; Calculate averages
    (dotimes (i 7)
      (let ((dow-entry (aref dow-data i)))
        (when (> (plist-get dow-entry :count) 0)
          (plist-put dow-entry :average
                    (/ (float (plist-get dow-entry :total-words))
                       (plist-get dow-entry :count))))))

    ;; Convert vector to alist
    (let ((result nil))
      (dotimes (i 7)
        (push (cons i (aref dow-data i)) result))
      (nreverse result))))

(defun org-scribe-planner--calculate-consistency-score (plan)
  "Calculate consistency score for PLAN.
Returns a plist with :score (0-100), :days-logged, :working-days, :rate."
  (let* ((schedule (org-scribe-planner--generate-day-schedule plan))
         (daily-counts (org-scribe-plan-daily-word-counts plan))
         (working-days 0)
         (days-logged 0))

    ;; Count working days and days with logged data
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day))
             (entry (assoc date daily-counts))
             (has-data (and entry
                           (numberp (org-scribe-planner--get-entry-words entry)))))

        (unless is-spare
          (setq working-days (1+ working-days))
          (when has-data
            (setq days-logged (1+ days-logged))))))

    (let ((rate (if (> working-days 0)
                   (/ (* 100.0 days-logged) working-days)
                 0)))
      (list :score (round rate)
            :days-logged days-logged
            :working-days working-days
            :rate rate))))

(defun org-scribe-planner--calculate-target-achievement-rate (plan)
  "Calculate target achievement rate for PLAN.
Returns a plist with :rate, :days-met, :days-partial, :days-missed, :total-days."
  (let* ((schedule (org-scribe-planner--generate-day-schedule plan))
         (daily-counts (org-scribe-plan-daily-word-counts plan))
         (days-met 0)
         (days-partial 0)
         (days-missed 0)
         (total-days 0))

    ;; Categorize each working day
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day))
             (target (plist-get day :words))
             (entry (assoc date daily-counts))
             (actual (when entry
                      (org-scribe-planner--get-entry-words entry))))

        (when (and (not is-spare) (numberp actual))
          (setq total-days (1+ total-days))
          (cond
           ((>= actual target) (setq days-met (1+ days-met)))
           ((>= actual (* 0.75 target)) (setq days-partial (1+ days-partial)))
           (t (setq days-missed (1+ days-missed)))))))

    (let ((rate (if (> total-days 0)
                   (/ (* 100.0 days-met) total-days)
                 0)))
      (list :rate rate
            :days-met days-met
            :days-partial days-partial
            :days-missed days-missed
            :total-days total-days))))

(defun org-scribe-planner--calculate-efficiency-ratio (plan)
  "Calculate efficiency ratio for PLAN.
Returns actual velocity / planned velocity as a percentage."
  (let* ((velocity (org-scribe-planner--calculate-velocity plan))
         (avg-velocity (plist-get velocity :average))
         (planned-velocity (org-scribe-plan-daily-words plan)))

    (if (> planned-velocity 0)
        (* 100.0 (/ avg-velocity planned-velocity))
      0)))

;;; Performance Analytics Dashboard

;;;###autoload
(defun org-scribe-planner-show-performance-analytics ()
  "Display comprehensive performance analytics for the active plan.
Shows day-of-week patterns, consistency scores, and target achievement rates."
  (interactive)
  (let ((current (org-scribe-planner--get-current-plan t)))
    (when current
      (let* ((plan (car current))
             (dow-stats (org-scribe-planner--calculate-day-of-week-stats plan))
             (consistency (org-scribe-planner--calculate-consistency-score plan))
             (achievement (org-scribe-planner--calculate-target-achievement-rate plan))
             (efficiency (org-scribe-planner--calculate-efficiency-ratio plan))
             (velocity (org-scribe-planner--calculate-velocity plan))
             (streak (org-scribe-planner--calculate-current-streak plan)))

        (with-current-buffer (get-buffer-create "*Performance Analytics*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "PERFORMANCE ANALYTICS\n"
                              'face '(:weight bold :height 1.2)))
            (insert (propertize (format "%s\n" (org-scribe-plan-title plan))
                              'face 'org-level-1))
            (insert (make-string 80 ?═) "\n\n")

            ;; Consistency Score Section
            (insert (propertize "📅 Consistency Score\n" 'face 'org-level-2))
            (let* ((score (plist-get consistency :score))
                   (score-face (cond
                               ((>= score 90) 'org-done)
                               ((>= score 70) 'org-scheduled)
                               (t 'org-warning)))
                   (grade (cond
                          ((>= score 95) "A+")
                          ((>= score 90) "A")
                          ((>= score 85) "B+")
                          ((>= score 80) "B")
                          ((>= score 75) "C+")
                          ((>= score 70) "C")
                          ((>= score 60) "D")
                          (t "F"))))

              (insert (format "  Score: %s (%s)\n"
                            (propertize (format "%d%%" score) 'face score-face)
                            (propertize grade 'face score-face)))
              (insert (format "  Days logged: %d / %d working days\n"
                            (plist-get consistency :days-logged)
                            (plist-get consistency :working-days)))
              (insert "  ")
              (insert (org-scribe-planner--ascii-progress-bar
                      (plist-get consistency :rate) 50))
              (insert "\n\n"))

            ;; Target Achievement Rate Section
            (insert (propertize "🎯 Target Achievement Rate\n" 'face 'org-level-2))
            (let* ((rate (plist-get achievement :rate))
                   (rate-face (cond
                              ((>= rate 80) 'org-done)
                              ((>= rate 60) 'org-scheduled)
                              (t 'org-warning))))

              (insert (format "  Achievement rate: %s\n"
                            (propertize (format "%.1f%%" rate) 'face rate-face)))
              (insert (format "  Days met target (100%%+):  %s\n"
                            (propertize (format "%d" (plist-get achievement :days-met))
                                      'face 'org-done)))
              (insert (format "  Days partial (75-99%%):     %d\n"
                            (plist-get achievement :days-partial)))
              (insert (format "  Days missed (<75%%):        %s\n"
                            (propertize (format "%d" (plist-get achievement :days-missed))
                                      'face 'org-warning)))
              (insert (format "  Total days evaluated:       %d\n\n"
                            (plist-get achievement :total-days))))

            ;; Efficiency Ratio Section
            (insert (propertize "⚡ Efficiency Ratio\n" 'face 'org-level-2))
            (let ((eff-face (cond
                            ((>= efficiency 100) 'org-done)
                            ((>= efficiency 80) 'org-scheduled)
                            (t 'org-warning))))
              (insert (format "  Efficiency: %s\n"
                            (propertize (format "%.1f%%" efficiency) 'face eff-face)))
              (insert (format "  Actual velocity: %.0f words/day\n"
                            (plist-get velocity :average)))
              (insert (format "  Planned velocity: %d words/day\n"
                            (org-scribe-plan-daily-words plan)))
              (insert (format "  Interpretation: %s\n\n"
                            (cond
                             ((>= efficiency 120) "Exceeding expectations!")
                             ((>= efficiency 100) "On track")
                             ((>= efficiency 80) "Slightly below target")
                             (t "Needs improvement")))))

            ;; Streaks Section
            (insert (propertize "🔥 Writing Streaks\n" 'face 'org-level-2))
            (insert (format "  Current streak: %s\n"
                          (propertize (format "%d days" (plist-get streak :current))
                                    'face (if (> (plist-get streak :current) 0)
                                            'org-done
                                          'shadow))))
            (insert (format "  Longest streak: %d days\n"
                          (plist-get streak :longest)))
            (insert (format "  Trend: %s\n\n"
                          (org-scribe-planner--format-trend
                           (plist-get velocity :trend))))

            (insert (make-string 80 ?─) "\n\n")

            ;; Day of Week Performance Section
            (insert (propertize "📊 Performance by Day of Week\n" 'face 'org-level-2))
            (insert "\n")

            ;; Find max for scaling bars
            (let* ((max-avg (apply 'max
                                  (mapcar (lambda (entry)
                                           (plist-get (cdr entry) :average))
                                         dow-stats)))
                   (sorted-dow (sort (copy-sequence dow-stats)
                                    (lambda (a b) (< (car a) (car b))))))

              ;; Display bars for each day
              (dolist (entry sorted-dow)
                (let* ((data (cdr entry))
                       (day-name (plist-get data :day-name))
                       (average (plist-get data :average))
                       (count (plist-get data :count))
                       (bar-length (if (> max-avg 0)
                                     (round (* 40 (/ average max-avg)))
                                   0))
                       (bar (if (> bar-length 0)
                               (make-string bar-length ?█)
                             "")))

                  (when (> count 0)
                    (insert (format "  %-10s %s %s (%d days)\n"
                                  (concat day-name ":")
                                  (propertize bar 'face 'org-scheduled)
                                  (propertize (format "%.0f words" average)
                                            'face 'org-done)
                                  count)))))

              ;; Find best and worst days
              (let* ((days-with-data (cl-remove-if
                                     (lambda (entry)
                                       (= 0 (plist-get (cdr entry) :count)))
                                     dow-stats))
                     (best-entry (when days-with-data
                                  (car (sort (copy-sequence days-with-data)
                                            (lambda (a b)
                                              (> (plist-get (cdr a) :average)
                                                 (plist-get (cdr b) :average)))))))
                     (worst-entry (when days-with-data
                                   (car (sort (copy-sequence days-with-data)
                                             (lambda (a b)
                                               (< (plist-get (cdr a) :average)
                                                  (plist-get (cdr b) :average))))))))

                (when best-entry
                  (insert "\n")
                  (insert (format "  Best day:  %s (%.0f words average)\n"
                                (propertize (plist-get (cdr best-entry) :day-name)
                                          'face 'org-done)
                                (plist-get (cdr best-entry) :average))))

                (when worst-entry
                  (insert (format "  Worst day: %s (%.0f words average)\n"
                                (propertize (plist-get (cdr worst-entry) :day-name)
                                          'face 'org-warning)
                                (plist-get (cdr worst-entry) :average))))))

            (insert "\n" (make-string 80 ?═) "\n")
            (insert (propertize "Press 'q' to close | 'r' to refresh | 'c' to view calendar\n"
                              'face 'shadow)))

          (goto-char (point-min))
          (org-scribe-planner-dashboard-mode)
          (display-buffer (current-buffer)))))))

;;; Velocity Chart (Chart.el Bar Chart)

;;;###autoload
(defun org-scribe-planner-show-velocity-chart ()
  "Display velocity bar chart using chart.el for the active plan.
Shows daily word counts over time with a 7-day moving average."
  (interactive)
  (let ((current (org-scribe-planner--get-current-plan t)))
    (when current
      (let* ((plan (car current))
             (daily-counts (org-scribe-plan-daily-word-counts plan))
             (counts-with-words (cl-remove-if-not
                                (lambda (entry)
                                  (numberp (org-scribe-planner--get-entry-words entry)))
                                daily-counts))
             (sorted-entries (sort (copy-sequence counts-with-words)
                                  (lambda (a b) (string< (car a) (car b)))))
             (word-counts (mapcar #'org-scribe-planner--get-entry-words sorted-entries))
             (target (org-scribe-plan-daily-words plan))
             (moving-avg (org-scribe-planner--moving-average word-counts 7)))

        (if (< (length word-counts) 1)
            (message "No word count data available to chart")

          (with-current-buffer (get-buffer-create "*Velocity Chart*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize "VELOCITY BAR CHART\n"
                                'face '(:weight bold :height 1.2)))
              (insert (propertize (format "%s\n" (org-scribe-plan-title plan))
                                'face 'org-level-1))
              (insert (make-string 80 ?═) "\n\n")

              ;; Chart.el vertical bar chart
              (let* ((chart-entries (if (> (length sorted-entries) 30)
                                       (last sorted-entries 30)
                                     sorted-entries))
                     (chart-dates (mapcar #'car chart-entries))
                     (chart-words (mapcar #'org-scribe-planner--get-entry-words chart-entries))
                     (chart-labels (org-scribe-planner--format-date-labels chart-dates 12)))

                (chart-bar-quickie 'vertical
                                  (format "Daily Word Counts - %s"
                                          (org-scribe-plan-title plan))
                                  chart-labels
                                  "Days"
                                  chart-words
                                  "Words")
                (insert "\n"))

              (insert (make-string 80 ?─) "\n\n")

              ;; Statistics below chart
              (insert (propertize "Statistics\n" 'face 'org-level-2))
              (let* ((total (apply '+ word-counts))
                     (avg (/ (float (apply '+ word-counts)) (length word-counts)))
                     (max-words (apply 'max word-counts))
                     (min-words (apply 'min word-counts))
                     (recent-avg (if (>= (length moving-avg) 1)
                                    (car (last moving-avg))
                                  avg)))

                (insert (format "  Target:           %s words/day\n"
                              (org-scribe-planner--format-number target)))
                (insert (format "  Average:          %.0f words/day (%d days)\n"
                              avg (length word-counts)))
                (insert (format "  Recent (7-day MA): %.0f words/day\n" recent-avg))
                (insert (format "  Best day:         %s words\n"
                              (org-scribe-planner--format-number max-words)))
                (insert (format "  Worst day:        %s words\n"
                              (org-scribe-planner--format-number min-words)))
                (insert (format "  Total written:    %s words\n\n"
                              (org-scribe-planner--format-number total))))

              (insert (propertize "Interpretation:\n" 'face 'org-level-2))
              (insert "  • Bars show daily word count output\n")
              (insert "  • Compare heights to target to assess performance\n")
              (insert "  • Look for patterns: consistent bars = steady pace\n")
              (insert "  • Increasing heights = building momentum\n")
              (insert "  • Gaps = days without logged data\n\n")

              (insert (make-string 80 ?═) "\n")
              (insert (propertize "Press 'q' to close | 'r' to refresh | 'c' to view calendar\n"
                                'face 'shadow)))

            (goto-char (point-min))
            (org-scribe-planner-dashboard-mode)
            (display-buffer (current-buffer))))))))

;;; Consistency Heatmap

;;;###autoload
(defun org-scribe-planner-show-heatmap ()
  "Display calendar heatmap showing writing consistency."
  (interactive)
  (let ((current (org-scribe-planner--get-current-plan t)))
    (when current
      (let* ((plan (car current))
             (schedule (org-scribe-planner--generate-day-schedule plan))
             (daily-counts (org-scribe-plan-daily-word-counts plan)))

        (with-current-buffer (get-buffer-create "*Writing Heatmap*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "WRITING CONSISTENCY HEATMAP\n"
                              'face '(:weight bold :height 1.2)))
            (insert (propertize (format "%s\n" (org-scribe-plan-title plan))
                              'face 'org-level-1))
            (insert (make-string 70 ?═) "\n\n")

            ;; Month headers and calendar grid
            (let ((current-month nil)
                  (current-year nil))
              (dolist (day schedule)
                (let* ((date (plist-get day :date))
                       (date-parts (mapcar 'string-to-number (split-string date "-")))
                       (year (nth 0 date-parts))
                       (month (nth 1 date-parts))
                       (day-num (nth 2 date-parts))
                       (dow (calendar-day-of-week (list month day-num year)))
                       (entry (assoc date daily-counts))
                       (target (plist-get day :words))
                       (actual (when entry (org-scribe-planner--get-entry-words entry)))
                       (is-spare (plist-get day :is-spare-day))
                       (performance (cond
                                    (is-spare 'spare)
                                    ((not (numberp actual)) 'no-data)
                                    ((>= actual target) 'met)
                                    ((>= actual (* 0.75 target)) 'partial)
                                    (t 'missed)))
                       (char (pcase performance
                              ('spare ?·)
                              ('no-data ?□)
                              ('met ?█)
                              ('partial ?▓)
                              ('missed ?░)))
                       (face (pcase performance
                              ('spare 'org-agenda-dimmed-todo-face)
                              ('no-data 'shadow)
                              ('met 'org-done)
                              ('partial 'org-scheduled)
                              ('missed 'org-warning))))

                  ;; New month header
                  (when (not (and (eq current-month month) (eq current-year year)))
                    (when current-month (insert "\n\n"))
                    (insert (propertize
                            (format "%s %d\n"
                                   (calendar-month-name month)
                                   year)
                            'face 'org-level-2))
                    (insert "  Mo Tu We Th Fr Sa Su\n")
                    (setq current-month month
                          current-year year)

                    ;; Padding for first week (1=Monday, 0=Sunday)
                    (when (> dow 0)
                      (insert (make-string (* 3 dow) ?\s))))

                  ;; Day cell
                  (insert (propertize (format " %c " char) 'face face))

                  ;; New line on Sunday
                  (when (= dow 0)
                    (insert "\n")))))

            (insert "\n\n")
            (insert (propertize "Legend:\n" 'face 'org-level-2))
            (insert "  " (propertize "█ " 'face 'org-done) "Met target (100%+)\n")
            (insert "  " (propertize "▓ " 'face 'org-scheduled) "Partial (75-99%)\n")
            (insert "  " (propertize "░ " 'face 'org-warning) "Missed (<75%)\n")
            (insert "  " (propertize "□ " 'face 'shadow) "No data\n")
            (insert "  " (propertize "· " 'face 'org-agenda-dimmed-todo-face) "Spare day\n\n")

            (insert (propertize "Patterns to look for:\n" 'face 'org-level-2))
            (insert "  • Dense green clusters = Productive periods\n")
            (insert "  • Red patches = Struggle periods\n")
            (insert "  • Empty squares = Missing data (need to log)\n")
            (insert "  • Vertical patterns = Identify best/worst days of week\n\n")

            (insert (make-string 70 ?═) "\n")
            (insert (propertize "Press 'q' to close | 'r' to refresh | 'c' to view calendar\n"
                              'face 'shadow)))

          (goto-char (point-min))
          (org-scribe-planner-dashboard-mode)
          (display-buffer (current-buffer)))))))

;;; Dashboard Menu

;;;###autoload
(defun org-scribe-planner-dashboards-menu ()
  "Show dashboard selection menu."
  (interactive)
  (let ((choice (completing-read
                 "Select dashboard: "
                 '("Progress Dashboard"
                   "Progress Dashboard (SVG)"
                   "Burndown Chart"
                   "Cumulative Progress"
                   "Velocity Statistics"
                   "Velocity Chart (Chart.el)"
                   "Performance Analytics"
                   "Consistency Heatmap"
                   "Show All Dashboards")
                 nil t)))
    (pcase choice
      ("Progress Dashboard" (org-scribe-planner-show-progress-dashboard))
      ("Progress Dashboard (SVG)" (org-scribe-planner-show-progress-dashboard-svg))
      ("Burndown Chart" (org-scribe-planner-show-burndown))
      ("Cumulative Progress" (org-scribe-planner-show-cumulative-progress))
      ("Velocity Statistics" (org-scribe-planner-show-velocity))
      ("Velocity Chart (Chart.el)" (org-scribe-planner-show-velocity-chart))
      ("Performance Analytics" (org-scribe-planner-show-performance-analytics))
      ("Consistency Heatmap" (org-scribe-planner-show-heatmap))
      ("Show All Dashboards" (org-scribe-planner-show-all-dashboards)))))

;;;###autoload
(defun org-scribe-planner-show-all-dashboards ()
  "Display all dashboards in split windows."
  (interactive)
  (delete-other-windows)
  (org-scribe-planner-show-progress-dashboard)
  (split-window-below)
  (other-window 1)
  (org-scribe-planner-show-burndown)
  (split-window-right)
  (other-window 1)
  (org-scribe-planner-show-velocity)
  (other-window 1)
  (split-window-right)
  (other-window 1)
  (org-scribe-planner-show-heatmap)
  (balance-windows))

;;; Provide

(provide 'org-scribe-planner-dashboards)

;;; org-scribe-planner-dashboards.el ends here
