;;; test-schedule.el --- Tests for schedule generation and date-range helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for org-scribe-planner--generate-day-schedule,
;; org-scribe-planner--generate-date-range, and day-of-week helpers.
;;
;; Reference calendar used in day-of-week tests:
;;   2024-11-01 = Friday  (dow 5)
;;   2024-11-02 = Saturday (dow 6)
;;   2024-11-03 = Sunday  (dow 0)
;;   2024-11-04 = Monday  (dow 1)

;;; Code:

(require 'ert)
(let ((dir (file-name-directory (or (and load-file-name (expand-file-name load-file-name))
                                    buffer-file-name ""))))
  (load-file (expand-file-name "../org-scribe-planner.el" dir)))

;;; --generate-date-range

(ert-deftest test-generate-date-range-inclusive ()
  "Range includes both start and end date."
  (should (equal (org-scribe-planner--generate-date-range "2024-11-01" "2024-11-03")
                 '("2024-11-01" "2024-11-02" "2024-11-03"))))

(ert-deftest test-generate-date-range-single-day ()
  "Same start and end date returns a one-element list."
  (should (equal (org-scribe-planner--generate-date-range "2024-11-01" "2024-11-01")
                 '("2024-11-01"))))

(ert-deftest test-generate-date-range-across-month-boundary ()
  "Range spanning month end and month start is generated correctly."
  (should (equal (org-scribe-planner--generate-date-range "2024-01-30" "2024-02-01")
                 '("2024-01-30" "2024-01-31" "2024-02-01"))))

;;; --get-day-of-week / --get-weekends

(ert-deftest test-get-day-of-week-saturdays ()
  "Returns only the Saturdays within the plan's date range."
  ;; 2024-11-01 (Fri) to 2024-11-07 (Thu): only Saturday is 2024-11-02
  (let ((plan (make-org-scribe-plan :start-date "2024-11-01" :days 7)))
    (should (equal (org-scribe-planner--get-day-of-week plan 6)
                   '("2024-11-02")))))

(ert-deftest test-get-day-of-week-sundays ()
  "Returns only the Sundays within the plan's date range."
  ;; 2024-11-01 (Fri) to 2024-11-07 (Thu): only Sunday is 2024-11-03
  (let ((plan (make-org-scribe-plan :start-date "2024-11-01" :days 7)))
    (should (equal (org-scribe-planner--get-day-of-week plan 0)
                   '("2024-11-03")))))

(ert-deftest test-get-weekends-sorted-chronologically ()
  "Returns all Saturdays and Sundays sorted by date."
  ;; 2024-11-01 (Fri) to 2024-11-07 (Thu): Sat=2024-11-02, Sun=2024-11-03
  (let ((plan (make-org-scribe-plan :start-date "2024-11-01" :days 7)))
    (should (equal (org-scribe-planner--get-weekends plan)
                   '("2024-11-02" "2024-11-03")))))

(ert-deftest test-get-weekends-two-full-weeks ()
  "Returns all four weekend days in a two-week span."
  ;; 2024-11-04 (Mon) to 2024-11-17 (Sun): weekends are 9,10,16,17
  (let ((plan (make-org-scribe-plan :start-date "2024-11-04" :days 14)))
    (should (equal (org-scribe-planner--get-weekends plan)
                   '("2024-11-09" "2024-11-10" "2024-11-16" "2024-11-17")))))

;;; --generate-day-schedule

(ert-deftest test-schedule-length-matches-days ()
  "Schedule has exactly one entry per calendar day."
  (let ((plan (make-org-scribe-plan :total-words 3000 :daily-words 1000
                                    :days 3 :start-date "2024-11-01"
                                    :end-date "2024-11-03" :spare-days nil)))
    (should (= (length (org-scribe-planner--generate-day-schedule plan)) 3))))

(ert-deftest test-schedule-dates-are-sequential ()
  "Schedule entries are in chronological order."
  (let* ((plan (make-org-scribe-plan :total-words 3000 :daily-words 1000
                                     :days 3 :start-date "2024-11-01"
                                     :end-date "2024-11-03" :spare-days nil))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (dates (mapcar (lambda (d) (plist-get d :date)) schedule)))
    (should (equal dates '("2024-11-01" "2024-11-02" "2024-11-03")))))

(ert-deftest test-schedule-spare-days-have-zero-words ()
  "Spare day entries carry zero words and set :is-spare-day."
  (let* ((plan (make-org-scribe-plan :total-words 2000 :daily-words 1000
                                     :days 3 :start-date "2024-11-01"
                                     :end-date "2024-11-03"
                                     :spare-days '("2024-11-02")))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (spare-entry (cl-find "2024-11-02" schedule
                               :key (lambda (d) (plist-get d :date))
                               :test #'string=)))
    (should spare-entry)
    (should (= (plist-get spare-entry :words) 0))
    (should (plist-get spare-entry :is-spare-day))))

(ert-deftest test-schedule-non-spare-days-have-target-words ()
  "Non-spare day entries carry the plan's daily-words target."
  (let* ((plan (make-org-scribe-plan :total-words 2000 :daily-words 1000
                                     :days 2 :start-date "2024-11-01"
                                     :end-date "2024-11-02" :spare-days nil))
         (schedule (org-scribe-planner--generate-day-schedule plan)))
    (should (= (plist-get (nth 0 schedule) :words) 1000))
    (should (= (plist-get (nth 1 schedule) :words) 1000))))

(ert-deftest test-schedule-cumulative-excludes-spare-days ()
  "Cumulative count grows on working days and holds steady on spare days."
  ;; Plan: 3 days, 1000 words/day, middle day is spare
  ;; Expected cumulative: day1=1000, day2(spare)=1000, day3=2000
  (let* ((plan (make-org-scribe-plan :total-words 2000 :daily-words 1000
                                     :days 3 :start-date "2024-11-01"
                                     :end-date "2024-11-03"
                                     :spare-days '("2024-11-02")))
         (schedule (org-scribe-planner--generate-day-schedule plan)))
    (should (= (plist-get (nth 0 schedule) :cumulative) 1000))
    (should (= (plist-get (nth 1 schedule) :cumulative) 1000))  ; spare, no change
    (should (= (plist-get (nth 2 schedule) :cumulative) 2000))))

(ert-deftest test-schedule-memoization ()
  "Two calls with identical plan parameters return the same list object."
  (let ((org-scribe-planner--schedule-cache nil))  ; isolate from other tests
    (let ((plan (make-org-scribe-plan :total-words 2000 :daily-words 1000
                                      :days 2 :start-date "2024-11-01"
                                      :end-date "2024-11-02" :spare-days nil)))
      (let ((r1 (org-scribe-planner--generate-day-schedule plan))
            (r2 (org-scribe-planner--generate-day-schedule plan)))
        (should (eq r1 r2))))))

(ert-deftest test-schedule-cache-miss-on-different-plan ()
  "Changing daily-words invalidates the cache and produces a new result."
  (let ((org-scribe-planner--schedule-cache nil))
    (let* ((plan1 (make-org-scribe-plan :total-words 2000 :daily-words 1000
                                        :days 2 :start-date "2024-11-01"
                                        :end-date "2024-11-02" :spare-days nil))
           (plan2 (make-org-scribe-plan :total-words 4000 :daily-words 2000
                                        :days 2 :start-date "2024-11-01"
                                        :end-date "2024-11-02" :spare-days nil))
           (r1 (org-scribe-planner--generate-day-schedule plan1))
           (r2 (org-scribe-planner--generate-day-schedule plan2)))
      ;; Different plans should produce different results
      (should (not (eq r1 r2)))
      (should (= (plist-get (car r2) :words) 2000)))))

;;; test-schedule.el ends here
