;;; test-hooks.el --- Tests for integration hooks and pluggable fn-vars -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests verifying that integration hooks and pluggable function variables
;; work correctly in isolation, without requiring org-scribe to be present.

;;; Code:

(require 'ert)
(let ((dir (file-name-directory (or (and load-file-name (expand-file-name load-file-name))
                                    buffer-file-name ""))))
  (load-file (expand-file-name "../org-scribe-planner.el" dir)))

;;; Helpers

(defmacro test-hooks--with-plan-file (plan-var file-var &rest body)
  "Bind PLAN-VAR and FILE-VAR to a temporary plan and file, then run BODY.
Cleans up the temp file unconditionally."
  (declare (indent 2))
  `(let* ((,file-var (make-temp-file "test-hooks-plan-" nil ".org"))
          (,plan-var (make-org-scribe-plan
                      :title "Hook Test Plan"
                      :total-words 10000
                      :daily-words 500
                      :days 20
                      :start-date "2026-01-01"
                      :end-date "2026-01-20"
                      :current-words 0)))
     (unwind-protect
         (progn
           (org-scribe-planner--save-plan ,plan-var ,file-var)
           ,@body)
       (ignore-errors (delete-file ,file-var)))))

;;; Tests for after-plan-load-hook

(ert-deftest test-hooks-after-plan-load-hook-fires-on-load-plan ()
  "after-plan-load-hook is called with (plan file) when load-plan succeeds."
  (test-hooks--with-plan-file plan file
    (let* ((hook-calls nil)
           (org-scribe-planner-after-plan-load-hook
            (list (lambda (p f) (push (list p f) hook-calls))))
           (org-scribe-planner--current-plan nil)
           (org-scribe-planner--current-plan-file nil)
           (org-scribe-planner-project-root-function nil))
      ;; Simulate load-plan by calling it non-interactively after stubbing
      ;; read-file-name so it returns our temp file without prompting.
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _) file))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-load-plan))
      (should (= (length hook-calls) 1))
      (let ((args (car hook-calls)))
        (should (org-scribe-plan-p (nth 0 args)))
        (should (string= (nth 1 args) file))))))

(ert-deftest test-hooks-after-plan-load-hook-fires-on-new-plan ()
  "after-plan-load-hook is called when new-plan activates a plan."
  (let* ((hook-calls nil)
         (org-scribe-planner-after-plan-load-hook
          (list (lambda (p f) (push (list p f) hook-calls))))
         (org-scribe-planner--current-plan nil)
         (org-scribe-planner--current-plan-file nil)
         (temp-dir (make-temp-file "test-hooks-newplan-" t))
         (temp-file (expand-file-name "hook-new-plan.org" temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (prompt &rest _)
                     (if (string-match-p "[Tt]itle" prompt) "Hook Plan" "")))
                  ((symbol-function 'completing-read)
                   (lambda (_ choices &rest _)
                     ;; Choose "Total words + Days → Calculate daily words"
                     (car choices)))
                  ((symbol-function 'org-scribe-planner--read-positive-number)
                   (lambda (&rest _) 10000))
                  ((symbol-function 'org-scribe-planner--read-days)
                   (lambda (&rest _) 20))
                  ((symbol-function 'org-scribe-planner--read-date)
                   (lambda (&rest _) "2026-01-01"))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest _) temp-file))
                  ((symbol-function 'org-scribe-planner-show-calendar)
                   (lambda (&rest _) nil)))
          (org-scribe-planner-new-plan))
      (delete-directory temp-dir t))
    (should (>= (length hook-calls) 1))
    (let ((args (car hook-calls)))
      (should (org-scribe-plan-p (nth 0 args))))))

;;; Tests for after-progress-update-hook

(ert-deftest test-hooks-after-progress-update-hook-fires-cumulative ()
  "after-progress-update-hook fires with (plan count nil) on update-progress."
  (test-hooks--with-plan-file plan file
    (let* ((hook-calls nil)
           (org-scribe-planner-after-progress-update-hook
            (list (lambda (p c d) (push (list p c d) hook-calls))))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-wordcount-function nil))
      (cl-letf (((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 5000))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-scribe-planner--show-progress-report)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-update-progress))
      (should (= (length hook-calls) 1))
      (let ((args (car hook-calls)))
        (should (org-scribe-plan-p (nth 0 args)))
        (should (= (nth 1 args) 5000))
        (should (null (nth 2 args)))))))  ; date is nil for cumulative update

(ert-deftest test-hooks-after-progress-update-hook-fires-daily ()
  "after-progress-update-hook fires with (plan count date) on update-daily."
  (test-hooks--with-plan-file plan file
    (let* ((hook-calls nil)
           (org-scribe-planner-after-progress-update-hook
            (list (lambda (p c d) (push (list p c d) hook-calls))))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner--schedule-cache nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "2026-01-01"))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 750))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "good session"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (org-scribe-planner-update-daily-word-count))
      (should (= (length hook-calls) 1))
      (let ((args (car hook-calls)))
        (should (org-scribe-plan-p (nth 0 args)))
        (should (= (nth 1 args) 750))
        (should (string= (nth 2 args) "2026-01-01"))))))

;;; Tests for wordcount-function

(ert-deftest test-hooks-wordcount-function-used-when-set ()
  "update-progress uses wordcount-function return value without prompting."
  (test-hooks--with-plan-file plan file
    (let* ((prompt-called nil)
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-after-progress-update-hook nil)
           (org-scribe-planner-wordcount-function (lambda () 12345)))
      (cl-letf (((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) (setq prompt-called t) 0))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-scribe-planner--show-progress-report)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-update-progress))
      ;; Prompt must NOT have been called
      (should-not prompt-called)
      ;; Plan must reflect the value from wordcount-function
      (should (= (org-scribe-plan-current-words org-scribe-planner--current-plan)
                 12345)))))

(ert-deftest test-hooks-wordcount-function-nil-falls-back-to-prompt ()
  "update-progress falls back to read-number when wordcount-function returns nil."
  (test-hooks--with-plan-file plan file
    (let* ((prompt-called nil)
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-after-progress-update-hook nil)
           ;; Function that returns nil → should trigger the prompt
           (org-scribe-planner-wordcount-function (lambda () nil)))
      (cl-letf (((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) (setq prompt-called t) 9999))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-scribe-planner--show-progress-report)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-update-progress))
      ;; Prompt MUST have been called since function returned nil
      (should prompt-called)
      (should (= (org-scribe-plan-current-words org-scribe-planner--current-plan)
                 9999)))))

;;; Tests for project-root-function

(ert-deftest test-hooks-project-root-function-used-by-load-plan ()
  "load-plan passes project-root-function result to the file picker."
  (test-hooks--with-plan-file plan file
    (let* ((picker-dir nil)
           (org-scribe-planner--current-plan nil)
           (org-scribe-planner--current-plan-file nil)
           (org-scribe-planner-after-plan-load-hook nil)
           (org-scribe-planner-project-root-function
            (lambda () "/tmp/my-writing-project")))
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (_ dir &rest _)
                   (setq picker-dir dir)
                   file))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-load-plan))
      (should (string= picker-dir "/tmp/my-writing-project")))))

;;; test-hooks.el ends here
