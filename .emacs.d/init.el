(setq inhibit-startup-mesage t) ; Disable startup messages
(setq inhibit-splash-screen t) ; Disable splash screen
(setq initial-scratch-message nil) ; Disable scratch message

(scroll-bar-mode -1) ; Disable visible scroll bar
(tool-bar-mode -1) ; Disable tool bar
(tooltip-mode -1) ; Disable tooltips
(menu-bar-mode -1) ; Disable menu bar

(set-fringe-mode 10) ; Add margins

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Enable escape for quit

(setq auto-save-default nil) ; Disable auto-save

(set-face-attribute 'default nil :font "Iosevka" :height 150) ; Set font

(load-theme 'grayscale t) ; Set theme

(setq make-backup-files nil) ; Disable backup files

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package vertico
  :bind (:map vertico-map
	 ("C-j" . vertico-next)
	 ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :init
  (marginalia-mode))
(setq marginalia-annotator-registry
  (assq-delete-all 'file marginalia-annotator-registry))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package ox-hugo
  :after ox)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general)

(setq evil-want-keybinding nil)

(use-package evil
  :init
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package org-roam
  :config
  (setq org-roam-directory "~/org/roam"))

; Line numbers
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)
(global-display-line-numbers-mode)
(column-number-mode)

(dolist (mode '(eshell-mode-hook
		term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-roam compat bind-key transient tomelr s dash f shrink-path org org-edna org-agenda-property org-gtd nerd-icons goto-chg annalist evil-collection evil general amx which-key vertico use-package ox-hugo marginalia doom-modeline all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; GTD Setup
(setq org-capture-templates '(("t" "Inbox" entry (file+headline "~/org/todo/inbox.org" "Inbox") "* TODO %i%?")))
(setq org-agenda-files '("~/org/todo/next.org" "~/org/todo/projects.org" "~/org/todo/calendar.org" "~/org/todo/reminders.org"))

(defvar org-refile-targets-original (if (boundp 'org-refile-targets)
					org-refile-targets
				      nil))

(defun set-refile-target-todo ()
  (if (and buffer-file-name
	     (string-prefix-p (expand-file-name "~/org/todo/") buffer-file-name))
    (setq org-refile-targets
	  '(("~/org/todo/next.org" :level . 1)
	    ("~/org/todo/projects.org" :maxlevel . 3)
            ("~/org/todo/delegated.org" :level . 1)
            ("~/org/todo/reminders.org" :level . 1)
            ("~/org/todo/calendar.org" :level . 1)
            ("~/org/todo/potential.org" :level . 1)))
    (setq org-refile-targets org-refile-targets-original)))
  

(add-hook 'post-command-hook 'set-refile-target-todo)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(defun find-todo ()
  (interactive)
  (let ((default-directory "~/org/todo/"))
    (call-interactively 'find-file)))
(global-set-key (kbd "C-c t") 'find-todo)

;; Evil keybindings
(defun evil-keyboard-quit ()
  (interactive)
  (and evil-mode (evil-force-normal-state))
  (keyboard-quit))

(define-key evil-normal-state-map (kbd "C-g") 'evil-keyboard-quit)
(define-key evil-motion-state-map (kbd "C-g") 'evil-keyboard-quit)
(define-key evil-insert-state-map (kbd "C-g") 'evil-keyboard-quit)
(define-key evil-window-map (kbd "C-g") 'evil-keyboard-quit)
(define-key evil-operator-state-map (kbd "C-g") 'evil-keyboard-quit)

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line))

; Roam keybindings
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)

; Notes
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("notes"
		 "\\documentclass{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(defun org-latex-ref-to-cref (text backend info)
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\\\\ref{" "\\\\Cref{" text)))

(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-final-output-functions
	       'org-latex-ref-to-cref))

(with-eval-after-load 'org-roam
  (add-to-list 'org-roam-capture-templates
	       '("n" "notes" plain (file "~/org/templates/notes.org")
		:target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
				   "#+title: ${title}\n")
		:unnarrowed t)))

(defun get-org-buffer-title ()
  (cadar (org-collect-keywords '("TITLE"))))

(defun process-title (title)
  (if title
      (concat
       (replace-regexp-in-string " " "-" (downcase title))
       ".org")
    ""))

(defun process-post (src dst)
  (with-temp-buffer
    (insert-file-contents src)
    (goto-char (point-min))
    (insert-file-contents "~/org/templates/post.org")
    (write-file dst))
  (shell-command (concat "sed -i '' -E '/^:/ d' " dst)))

(defun publish-note ()
  (interactive)
  (if (and buffer-file-name
	     (string-prefix-p (expand-file-name org-roam-directory) buffer-file-name))
      (let* ((file-name (process-title (get-org-buffer-title)))
	    (post-name (read-string
			"Enter the post file name: "
			file-name nil file-name nil))
	    (publish-dir "~/Projects/krishxmatta.dev")
	    (content-org-file (concat publish-dir (concat "/content-org/posts/" post-name))))
	(process-post buffer-file-name content-org-file)
	(find-file content-org-file))
    (message "Not in an org-roam file.")))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c p") 'publish-note))
