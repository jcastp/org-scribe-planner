;;; test-buffer-safety.el --- Tests for buffer erase safety -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests to verify buffer safety checks prevent data loss

;;; Code:

(require 'ert)
(load-file "org-scribe-planner.el")

;; Test helpers
(defun test-create-temp-file (content)
  "Create a temporary file with CONTENT and return its path."
  (let ((temp-file (make-temp-file "org-scribe-test-" nil ".org")))
    (with-temp-file temp-file
      (insert content))
    temp-file))

(defun test-cleanup-file (filepath)
  "Delete FILEPATH if it exists."
  (when (and filepath (file-exists-p filepath))
    (delete-file filepath)))

;; Test is-single-plan-file-p
(ert-deftest test-single-plan-file-recognized ()
  "Test that a single plan file is recognized."
  (let ((temp-file (test-create-temp-file
                    "* My Writing Plan
:PROPERTIES:
:TOTAL_WORDS: 10000
:DAILY_WORDS: 500
:DAYS: 20
:START_DATE: 2024-11-01
:END:

** Schedule
| Date | Words |
")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should (org-scribe-planner--is-single-plan-file-p (current-buffer))))
      (test-cleanup-file temp-file))))

(ert-deftest test-multiple-heading-file-not-plan ()
  "Test that files with multiple headings are not considered single plan files."
  (let ((temp-file (test-create-temp-file
                    "* My Writing Plan
:PROPERTIES:
:TOTAL_WORDS: 10000
:DAILY_WORDS: 500
:DAYS: 20
:END:

* Another Heading

Some content here.
")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should-not (org-scribe-planner--is-single-plan-file-p (current-buffer))))
      (test-cleanup-file temp-file))))

(ert-deftest test-file-without-plan-properties-not-plan ()
  "Test that files without plan properties are not considered plan files."
  (let ((temp-file (test-create-temp-file
                    "* Some Random Heading

This is just a regular org file.
")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (should-not (org-scribe-planner--is-single-plan-file-p (current-buffer))))
      (test-cleanup-file temp-file))))

(ert-deftest test-empty-buffer-is-safe ()
  "Test that empty buffers are considered safe to erase."
  (let ((temp-file (make-temp-file "org-scribe-test-" nil ".org")))
    (unwind-protect
        (let ((buf (find-file-noselect temp-file)))
          (with-current-buffer buf
            (should (= (buffer-size) 0))
            ;; Empty buffer should be safe
            (should (org-scribe-planner--buffer-safe-to-erase-p buf temp-file))))
      (test-cleanup-file temp-file))))

(ert-deftest test-single-plan-file-is-safe ()
  "Test that single plan files are considered safe to erase."
  (let ((temp-file (test-create-temp-file
                    "* My Writing Plan
:PROPERTIES:
:TOTAL_WORDS: 10000
:DAILY_WORDS: 500
:DAYS: 20
:END:
")))
    (unwind-protect
        (let ((buf (find-file-noselect temp-file)))
          (with-current-buffer buf
            (should (org-scribe-planner--buffer-safe-to-erase-p buf temp-file))))
      (test-cleanup-file temp-file))))

(ert-deftest test-multiple-heading-file-not-safe ()
  "Test that files with multiple headings are not safe to erase without confirmation."
  (let ((temp-file (test-create-temp-file
                    "* Heading One

Content.

* Heading Two

More content.
")))
    (unwind-protect
        (let ((buf (find-file-noselect temp-file)))
          (with-current-buffer buf
            ;; Should return nil (not safe) for multiple headings
            (should-not (org-scribe-planner--buffer-safe-to-erase-p buf temp-file))))
      (test-cleanup-file temp-file))))

;; Test save-plan with safety checks
(ert-deftest test-save-plan-to-new-file ()
  "Test saving a plan to a new file works."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "new-plan.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "Test Plan"
                :total-words 10000
                :daily-words 500
                :days 20
                :start-date "2024-11-01"
                :end-date "2024-11-20"
                :spare-days nil
                :current-words 0)))
    (unwind-protect
        (progn
          ;; Should save without error
          (org-scribe-planner--save-plan plan temp-file)
          (should (file-exists-p temp-file))

          ;; Verify content
          (with-current-buffer (find-file-noselect temp-file)
            (goto-char (point-min))
            (should (search-forward "* Test Plan" nil t))
            (should (org-entry-get nil "TOTAL_WORDS"))
            (should (string= (org-entry-get nil "TOTAL_WORDS") "10000"))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-save-plan-updates-existing-plan-file ()
  "Test that saving to an existing plan file works (update)."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "existing-plan.org" temp-dir))
         (plan1 (make-org-scribe-plan
                 :title "Original Plan"
                 :total-words 10000
                 :daily-words 500
                 :days 20
                 :start-date "2024-11-01"
                 :end-date "2024-11-20"
                 :spare-days nil
                 :current-words 0))
         (plan2 (make-org-scribe-plan
                 :title "Updated Plan"
                 :total-words 15000
                 :daily-words 750
                 :days 20
                 :start-date "2024-11-01"
                 :end-date "2024-11-20"
                 :spare-days nil
                 :current-words 5000)))
    (unwind-protect
        (progn
          ;; Save first plan
          (org-scribe-planner--save-plan plan1 temp-file)
          (should (file-exists-p temp-file))

          ;; Update with second plan
          (org-scribe-planner--save-plan plan2 temp-file)

          ;; Verify updated content
          (with-current-buffer (find-file-noselect temp-file)
            (revert-buffer t t t)  ; Reload from disk
            (goto-char (point-min))
            (should (search-forward "* Updated Plan" nil t))
            (should (string= (org-entry-get nil "TOTAL_WORDS") "15000"))
            (should (string= (org-entry-get nil "CURRENT_WORDS") "5000"))))
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-save-plan-refuses-multiple-heading-file ()
  "Test that saving refuses to overwrite files with multiple headings."
  (let ((temp-file (test-create-temp-file
                    "* Heading One

Important content here.

* Heading Two

More important content.
"))
        (plan (make-org-scribe-plan
               :title "Test Plan"
               :total-words 10000
               :daily-words 500
               :days 20
               :start-date "2024-11-01"
               :end-date "2024-11-20")))
    (unwind-protect
        (progn
          ;; Should error when trying to save to multi-heading file
          (should-error (org-scribe-planner--save-plan plan temp-file))

          ;; Verify original content preserved
          (with-current-buffer (find-file-noselect temp-file)
            (revert-buffer t t t)
            (goto-char (point-min))
            (should (search-forward "Heading One" nil t))
            (should (search-forward "Heading Two" nil t))))
      (test-cleanup-file temp-file))))

;;; test-buffer-safety.el ends here
