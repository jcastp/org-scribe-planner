;;; example-config.el --- Example configuration for org-scribe-planner

;;; Commentary:
;;
;; Copy relevant sections to your Emacs init file (~/.emacs or ~/.emacs.d/init.el)

;;; Code:

;; ========================================
;; Basic Configuration
;; ========================================

;; Add to load-path and require
(add-to-list 'load-path "/path/to/org-scribe-planner")
(require 'org-scribe-planner)

;; Set custom directory for writing projects
(setq org-scribe-planner-directory (expand-file-name "~/Documents/writing-projects"))

;; Enable automatic org-agenda sync
(setq org-scribe-planner-sync-to-agenda t)

;; ========================================
;; Key Bindings
;; ========================================

;; Option 1: Simple global key bindings
(global-set-key (kbd "C-c w n") 'org-scribe-planner-new-plan)
(global-set-key (kbd "C-c w l") 'org-scribe-planner-load-plan)
(global-set-key (kbd "C-c w u") 'org-scribe-planner-update-progress)
(global-set-key (kbd "C-c w r") 'org-scribe-planner-recalculate)
(global-set-key (kbd "C-c w m") 'org-scribe-planner-show-milestones)
(global-set-key (kbd "C-c w s") 'org-scribe-planner-sync-agenda)

;; Option 2: Using use-package (recommended)
(use-package org-scribe-planner
  :load-path "/path/to/org-scribe-planner"
  :demand t
  :custom
  (org-scribe-planner-directory "~/Documents/writing-projects")
  (org-scribe-planner-sync-to-agenda t)
  :bind
  (("C-c w n" . org-scribe-planner-new-plan)
   ("C-c w l" . org-scribe-planner-load-plan)
   ("C-c w u" . org-scribe-planner-update-progress)
   ("C-c w r" . org-scribe-planner-recalculate)
   ("C-c w m" . org-scribe-planner-show-milestones)
   ("C-c w s" . org-scribe-planner-sync-agenda)))

;; ========================================
;; Advanced Configuration
;; ========================================

;; Custom calendar buffer name
(setq org-scribe-planner-calendar-buffer "*My Writing Schedule*")

;; Set default spare days
(setq org-scribe-planner-default-spare-days 2)

;; ========================================
;; Workflow Integration Examples
;; ========================================

;; Example 1: Automatically update word count from current org buffer
(defun my/update-writing-plan-from-buffer ()
  "Count words in current buffer and update writing plan."
  (interactive)
  (let ((word-count (count-words (point-min) (point-max))))
    (message "Current buffer has %d words" word-count)
    (when (y-or-n-p (format "Update plan with %d words? " word-count))
      (org-scribe-planner-update-progress))))

;; Example 2: Quick access via hydra (requires hydra package)
(when (require 'hydra nil 'noerror)
  (defhydra hydra-writing (:color blue :hint nil)
    "
Writing Plan Commands
────────────────────────────────────
_n_: New plan       _u_: Update progress
_l_: Load plan      _r_: Recalculate
_m_: Milestones     _s_: Sync agenda
_q_: Quit
"
    ("n" org-scribe-planner-new-plan)
    ("l" org-scribe-planner-load-plan)
    ("u" org-scribe-planner-update-progress)
    ("r" org-scribe-planner-recalculate)
    ("m" org-scribe-planner-show-milestones)
    ("s" org-scribe-planner-sync-agenda)
    ("q" nil))

  (global-set-key (kbd "C-c w") 'hydra-writing/body))

;; Example 3: Integration with org-pomodoro for writing sessions
(when (require 'org-pomodoro nil 'noerror)
  (defun my/start-writing-pomodoro ()
    "Start a pomodoro timer for a writing session."
    (interactive)
    (org-pomodoro)
    (message "Starting writing pomodoro! Focus time."))

  (define-key org-scribe-planner-calendar-mode-map (kbd "p")
    'my/start-writing-pomodoro))

;; Example 4: Hook to auto-sync when creating/updating plans
(defun my/auto-sync-to-agenda ()
  "Automatically sync writing plan to org-agenda after save."
  (when (and (buffer-file-name)
             (string-match-p (regexp-quote org-scribe-planner-directory)
                           (buffer-file-name)))
    (org-scribe-planner-sync-agenda)))

;; Uncomment to enable auto-sync
;; (add-hook 'after-save-hook 'my/auto-sync-to-agenda)

;; ========================================
;; Theme/Appearance Customization
;; ========================================

;; Customize faces for calendar display
(custom-set-faces
 '(org-level-1 ((t (:foreground "#87ceeb" :weight bold :height 1.3))))
 '(org-level-2 ((t (:foreground "#98fb98" :weight bold :height 1.2))))
 '(org-level-3 ((t (:foreground "#ffd700" :weight bold :height 1.1))))
 '(org-level-4 ((t (:foreground "#ff7f50" :weight normal))))
 '(org-agenda-dimmed-todo-face ((t (:foreground "#808080" :slant italic)))))

;; ========================================
;; Useful Functions
;; ========================================

;; Quick function to open writing projects directory
(defun my/open-writing-projects ()
  "Open the writing projects directory in dired."
  (interactive)
  (dired org-scribe-planner-directory))

(global-set-key (kbd "C-c w d") 'my/open-writing-projects)

;;; example-config.el ends here
