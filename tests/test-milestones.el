;;; test-milestones.el --- Tests for milestone tracking and recalculation data -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for org-scribe-planner--get-enhanced-milestones and
;; org-scribe-planner--compute-recalculation-data.

;;; Code:

(require 'ert)
(let ((dir (file-name-directory (or (and load-file-name (expand-file-name load-file-name))
                                    buffer-file-name ""))))
  (load-file (expand-file-name "../org-scribe-planner.el" dir)))

;;; --compute-recalculation-data

(ert-deftest test-recalc-data-no-entries ()
  "With no daily entries, cumulative-actual is 0 and all days are remaining."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2024-11-01" :end-date "2024-11-04"
                :spare-days nil :daily-word-counts nil))
         (data (org-scribe-planner--compute-recalculation-data plan)))
    (should (= (plist-get data :cumulative-actual) 0))
    (should (= (plist-get data :remaining-words) 4000))
    (should (= (plist-get data :remaining-days) 4))))

(ert-deftest test-recalc-data-with-entries ()
  "Entries for 2 of 4 days yield correct cumulative, remaining, and remaining-days."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2024-11-01" :end-date "2024-11-04"
                :spare-days nil
                :daily-word-counts
                '(("2024-11-01" . (:words 800 :note "" :target 1000))
                  ("2024-11-02" . (:words 1200 :note "" :target 1000)))))
         (data (org-scribe-planner--compute-recalculation-data plan)))
    (should (= (plist-get data :cumulative-actual) 2000))
    (should (= (plist-get data :remaining-words) 2000))
    ;; Nov 3 and Nov 4 have no entries and are not spare days
    (should (= (plist-get data :remaining-days) 2))))

(ert-deftest test-recalc-data-spare-days-not-counted-as-remaining ()
  "Spare days without entries are excluded from remaining-days."
  (let* ((plan (make-org-scribe-plan
                :total-words 3000 :daily-words 1000 :days 4
                :start-date "2024-11-01" :end-date "2024-11-04"
                :spare-days '("2024-11-02")  ; Nov 2 is spare
                :daily-word-counts nil))
         (data (org-scribe-planner--compute-recalculation-data plan)))
    ;; Nov 1, 3, 4 are working days with no entries → 3 remaining
    (should (= (plist-get data :remaining-days) 3))))

(ert-deftest test-recalc-data-note-only-entries-not-counted ()
  "Note-only spare day entries (no :words) do not contribute to cumulative-actual."
  (let* ((plan (make-org-scribe-plan
                :total-words 2000 :daily-words 1000 :days 2
                :start-date "2024-11-01" :end-date "2024-11-02"
                :spare-days '("2024-11-02")
                :daily-word-counts
                ;; Note-only entry for spare day
                '(("2024-11-02" . (:note "Rest day")))))
         (data (org-scribe-planner--compute-recalculation-data plan)))
    (should (= (plist-get data :cumulative-actual) 0))))

;;; --get-enhanced-milestones

(ert-deftest test-milestones-returns-four-entries ()
  "Milestones list always contains exactly 4 entries (25, 50, 75, 100 %)."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2030-06-01" :end-date "2030-06-04"
                :daily-word-counts nil))
         (milestones (org-scribe-planner--get-enhanced-milestones plan)))
    (should (= (length milestones) 4))
    (should (= (plist-get (nth 0 milestones) :percent) 25))
    (should (= (plist-get (nth 1 milestones) :percent) 50))
    (should (= (plist-get (nth 2 milestones) :percent) 75))
    (should (= (plist-get (nth 3 milestones) :percent) 100))))

(ert-deftest test-milestones-all-unreached-for-fresh-plan ()
  "A plan with no daily entries has all milestones unreached."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2030-06-01" :end-date "2030-06-04"
                :daily-word-counts nil))
         (milestones (org-scribe-planner--get-enhanced-milestones plan)))
    (dolist (m milestones)
      (should-not (plist-get m :reached)))))

(ert-deftest test-milestones-unreached-have-expected-dates ()
  "Unreached milestones on a future plan should have projected expected dates."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2030-06-01" :end-date "2030-06-04"
                :daily-word-counts nil))
         (milestones (org-scribe-planner--get-enhanced-milestones plan)))
    ;; Every milestone should have an expected date since there are enough days
    (dolist (m milestones)
      (should (plist-get m :expected)))))

(ert-deftest test-milestones-25-percent-reached-correctly ()
  "25% milestone is marked reached with correct date when actual progress crosses it."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2024-11-01" :end-date "2024-11-04"
                :daily-word-counts
                '(("2024-11-01" . (:words 1000 :note "" :target 1000)))))
         (milestones (org-scribe-planner--get-enhanced-milestones plan))
         (m25 (cl-find 25 milestones :key (lambda (m) (plist-get m :percent)))))
    (should (plist-get m25 :reached))
    (should (string= (plist-get m25 :date) "2024-11-01"))))

(ert-deftest test-milestones-50-percent-not-reached-at-25 ()
  "50% milestone is not reached when only 25% of words have been written."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2024-11-01" :end-date "2024-11-04"
                :daily-word-counts
                '(("2024-11-01" . (:words 1000 :note "" :target 1000)))))
         (milestones (org-scribe-planner--get-enhanced-milestones plan))
         (m50 (cl-find 50 milestones :key (lambda (m) (plist-get m :percent)))))
    (should-not (plist-get m50 :reached))))

(ert-deftest test-milestones-100-percent-reached ()
  "100% milestone is reached when all words are written."
  (let* ((plan (make-org-scribe-plan
                :total-words 2000 :daily-words 1000 :days 2
                :start-date "2024-11-01" :end-date "2024-11-02"
                :daily-word-counts
                '(("2024-11-01" . (:words 1000 :note "" :target 1000))
                  ("2024-11-02" . (:words 1000 :note "" :target 1000)))))
         (milestones (org-scribe-planner--get-enhanced-milestones plan))
         (m100 (cl-find 100 milestones :key (lambda (m) (plist-get m :percent)))))
    (should (plist-get m100 :reached))
    (should (plist-get m100 :date))))

;;; test-milestones.el ends here
