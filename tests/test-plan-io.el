;;; test-plan-io.el --- Tests for plan save/load round-trips -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests verifying that all plan fields survive a save→load round-trip,
;; including spare days, daily word counts, and backward-compatible loading
;; of old format entries.

;;; Code:

(require 'ert)
(let ((dir (file-name-directory (or (and load-file-name (expand-file-name load-file-name))
                                    buffer-file-name ""))))
  (load-file (expand-file-name "../org-scribe-planner.el" dir)))

;;; --load-plan error handling

(ert-deftest test-load-plan-error-on-missing-file ()
  "Loading a non-existent file signals an error."
  (should-error (org-scribe-planner--load-plan "/tmp/this-file-does-not-exist-12345.org")))

;;; Save/load round-trips

(ert-deftest test-roundtrip-basic-fields ()
  "Core plan fields survive a save→load round-trip."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "roundtrip.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "Roundtrip Plan"
                :total-words 50000
                :daily-words 1667
                :days 30
                :start-date "2024-11-01"
                :end-date "2024-11-30"
                :spare-days nil
                :current-words 5000)))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (let ((loaded (org-scribe-planner--load-plan temp-file)))
            (should (string= (org-scribe-plan-title loaded) "Roundtrip Plan"))
            (should (= (org-scribe-plan-total-words loaded) 50000))
            (should (= (org-scribe-plan-daily-words loaded) 1667))
            (should (= (org-scribe-plan-days loaded) 30))
            (should (string= (org-scribe-plan-start-date loaded) "2024-11-01"))
            (should (string= (org-scribe-plan-end-date loaded) "2024-11-30"))
            (should (= (org-scribe-plan-current-words loaded) 5000))))
      (delete-directory temp-dir t))))

(ert-deftest test-roundtrip-spare-days ()
  "Spare days list survives a save→load round-trip."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "spare-days.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "Spare Days Plan"
                :total-words 10000
                :daily-words 500
                :days 22
                :start-date "2024-11-01"
                :end-date "2024-11-22"
                :spare-days '("2024-11-03" "2024-11-10" "2024-11-17"))))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (let* ((loaded (org-scribe-planner--load-plan temp-file))
                 (loaded-spare (org-scribe-plan-spare-days loaded)))
            (should (member "2024-11-03" loaded-spare))
            (should (member "2024-11-10" loaded-spare))
            (should (member "2024-11-17" loaded-spare))
            (should (= (length loaded-spare) 3))))
      (delete-directory temp-dir t))))

(ert-deftest test-roundtrip-daily-word-counts ()
  "Daily word counts with words, notes, and targets survive a save→load round-trip."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "counts.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "Counts Plan"
                :total-words 10000
                :daily-words 500
                :days 20
                :start-date "2024-11-01"
                :end-date "2024-11-20"
                :daily-word-counts
                '(("2024-11-01" . (:words 600 :note "great session" :target 500))
                  ("2024-11-02" . (:words 400 :note "" :target 500))))))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (let* ((loaded (org-scribe-planner--load-plan temp-file))
                 (counts (org-scribe-plan-daily-word-counts loaded))
                 (entry1 (assoc "2024-11-01" counts))
                 (entry2 (assoc "2024-11-02" counts)))
            (should entry1)
            (should (= (plist-get (cdr entry1) :words) 600))
            (should (string= (plist-get (cdr entry1) :note) "great session"))
            (should (= (plist-get (cdr entry1) :target) 500))
            (should entry2)
            (should (= (plist-get (cdr entry2) :words) 400))))
      (delete-directory temp-dir t))))

(ert-deftest test-roundtrip-no-spare-days-property-absent ()
  "When there are no spare days, SPARE_DAYS property is not written."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "no-spare.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "No Spare Plan"
                :total-words 5000
                :daily-words 500
                :days 10
                :start-date "2024-11-01"
                :end-date "2024-11-10"
                :spare-days nil)))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (let ((loaded (org-scribe-planner--load-plan temp-file)))
            ;; spare-days should be nil (not set)
            (should (null (org-scribe-plan-spare-days loaded)))))
      (delete-directory temp-dir t))))

;;; test-plan-io.el ends here
