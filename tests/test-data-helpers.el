;;; test-data-helpers.el --- Tests for daily-count data helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the daily word count serialization, migration, accessors,
;; and spare day note management functions.

;;; Code:

(require 'ert)
(let ((dir (file-name-directory (or (and load-file-name (expand-file-name load-file-name))
                                    buffer-file-name ""))))
  (load-file (expand-file-name "../org-scribe-planner.el" dir)))

;;; --format-daily-count-entry

(ert-deftest test-format-entry-words-only ()
  "Entry with only words serializes as DATE:WORDS."
  (should (string= (org-scribe-planner--format-daily-count-entry
                    '("2024-11-01" . (:words 1500 :note "" :target nil)))
                   "2024-11-01:1500")))

(ert-deftest test-format-entry-words-and-note ()
  "Entry with words and a note serializes as DATE:WORDS:NOTE."
  (should (string= (org-scribe-planner--format-daily-count-entry
                    '("2024-11-01" . (:words 1500 :note "great session" :target nil)))
                   "2024-11-01:1500:great session")))

(ert-deftest test-format-entry-words-and-target ()
  "Entry with words and a target serializes as DATE:WORDS::TARGET."
  (should (string= (org-scribe-planner--format-daily-count-entry
                    '("2024-11-01" . (:words 1500 :note "" :target 2000)))
                   "2024-11-01:1500::2000")))

(ert-deftest test-format-entry-all-fields ()
  "Entry with words, note, and target serializes as DATE:WORDS:NOTE:TARGET."
  (should (string= (org-scribe-planner--format-daily-count-entry
                    '("2024-11-01" . (:words 1500 :note "good day" :target 2000)))
                   "2024-11-01:1500:good day:2000")))

;;; --migrate-daily-counts

(ert-deftest test-migrate-already-new-format-unchanged ()
  "Entries already in new plist format are returned as-is."
  (let ((counts '(("2024-11-01" . (:words 1000 :note "" :target 1000)))))
    (should (equal (org-scribe-planner--migrate-daily-counts counts) counts))))

(ert-deftest test-migrate-old-plain-number-format ()
  "Old format (date . N) is promoted to plist with :words key."
  (let* ((old '(("2024-11-01" . 1000)))
         (result (org-scribe-planner--migrate-daily-counts old)))
    (should (= (length result) 1))
    (should (= (plist-get (cdar result) :words) 1000))
    (should (keywordp (car (cdar result))))))

(ert-deftest test-migrate-old-cons-cell-format ()
  "Old format (date . (words . note)) is promoted to plist."
  (let* ((old '(("2024-11-01" . (800 . "slow start"))))
         (result (org-scribe-planner--migrate-daily-counts old)))
    (should (= (plist-get (cdar result) :words) 800))
    (should (string= (plist-get (cdar result) :note) "slow start"))))

;;; --counts-with-words

(ert-deftest test-counts-with-words-keeps-word-entries ()
  "Entries with a numeric :words field are kept."
  (let ((counts '(("2024-11-01" . (:words 1000 :note "" :target nil)))))
    (should (= (length (org-scribe-planner--counts-with-words counts)) 1))))

(ert-deftest test-counts-with-words-filters-note-only-entries ()
  "Note-only entries (no numeric :words) are excluded."
  (let ((counts '(("2024-11-01" . (:words 1000 :note ""))
                  ("2024-11-02" . (:note "rest day")))))  ; note-only, no :words
    (should (= (length (org-scribe-planner--counts-with-words counts)) 1))
    (should (string= (caar (org-scribe-planner--counts-with-words counts))
                     "2024-11-01"))))

;;; Entry accessors

(ert-deftest test-get-entry-words ()
  "Extracts the :words value from an entry."
  (let ((entry '("2024-11-01" . (:words 750 :note "good" :target 1000))))
    (should (= (org-scribe-planner--get-entry-words entry) 750))))

(ert-deftest test-get-entry-note-returns-value ()
  "Extracts the :note value from an entry."
  (let ((entry '("2024-11-01" . (:words 750 :note "good session" :target 1000))))
    (should (string= (org-scribe-planner--get-entry-note entry) "good session"))))

(ert-deftest test-get-entry-note-defaults-to-empty-string ()
  "Returns empty string when :note is absent."
  (let ((entry '("2024-11-01" . (:words 750 :target 1000))))
    (should (string= (org-scribe-planner--get-entry-note entry) ""))))

(ert-deftest test-get-entry-target-returns-value ()
  "Extracts the :target value from an entry."
  (let ((entry '("2024-11-01" . (:words 750 :note "" :target 1000))))
    (should (= (org-scribe-planner--get-entry-target entry) 1000))))

(ert-deftest test-get-entry-target-returns-nil-when-absent ()
  "Returns nil when :target is not stored."
  (let ((entry '("2024-11-01" . (:words 750 :note ""))))
    (should (null (org-scribe-planner--get-entry-target entry)))))

;;; --add-spare-day-note / --remove-spare-day-note

(ert-deftest test-add-spare-day-note-creates-new-entry ()
  "Adding a note to a date with no existing entry creates a note-only entry."
  (let ((plan (make-org-scribe-plan)))
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "Holiday")
    (let ((entry (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan))))
      (should entry)
      (should (string= (plist-get (cdr entry) :note) "Holiday"))
      ;; Note-only: no :words field (or nil)
      (should (not (numberp (plist-get (cdr entry) :words)))))))

(ert-deftest test-add-spare-day-note-updates-existing-note-only-entry ()
  "Adding a note over an existing note-only entry replaces the note."
  (let ((plan (make-org-scribe-plan)))
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "First note")
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "Updated note")
    (let ((entry (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan))))
      (should (string= (plist-get (cdr entry) :note) "Updated note")))))

(ert-deftest test-add-spare-day-note-preserves-words-in-existing-entry ()
  "Adding a note to an entry that already has words preserves the word count."
  (let ((plan (make-org-scribe-plan
               :daily-word-counts
               '(("2024-11-02" . (:words 500 :note "" :target 1000))))))
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "Wrote on rest day")
    (let ((entry (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan))))
      (should (= (plist-get (cdr entry) :words) 500))
      (should (string= (plist-get (cdr entry) :note) "Wrote on rest day")))))

(ert-deftest test-remove-spare-day-note-removes-note-only-entry ()
  "Removing a note from a note-only entry deletes the entry entirely."
  (let ((plan (make-org-scribe-plan)))
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "Holiday")
    (org-scribe-planner--remove-spare-day-note plan "2024-11-02")
    (should (null (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan))))))

(ert-deftest test-remove-spare-day-note-preserves-entry-with-words ()
  "Removing a note does not delete an entry that carries real word counts."
  (let ((plan (make-org-scribe-plan
               :daily-word-counts
               '(("2024-11-02" . (:words 500 :note "Holiday" :target 1000))))))
    (org-scribe-planner--remove-spare-day-note plan "2024-11-02")
    ;; Entry should still exist because it has words
    (should (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan)))))

;;; test-data-helpers.el ends here
