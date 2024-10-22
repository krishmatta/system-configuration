;;; Basic UI
(setq inhibit-startup-mesage t)         ; Disable startup messages
(setq inhibit-splash-screen t)          ; Disable splash screen
(setq initial-scratch-message nil)      ; Disable scratch message

(scroll-bar-mode -1)                    ; Disable visible scroll bar
(tool-bar-mode -1)                      ; Disable tool bar
(tooltip-mode -1)                       ; Disable tooltips
(menu-bar-mode -1)                      ; Disable menu bar

(set-fringe-mode 10)                    ; Add margins

;; Set font
(set-face-attribute 'default nil :font "Iosevka" :height 150)

;;; Remove Unnecessary Files
(setq auto-save-default nil)            ; Disable auto-save
(setq make-backup-files nil)            ; Disable backup files

;;; Package Management
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

;;; Completion
(use-package vertico
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts, so Vertico can sort by history position
(use-package savehist
  :init
  (savehist-mode))

;; Use orderless completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Minibuffer annotations
(use-package marginalia
  :init
  (marginalia-mode))

;; Disable file annotations
(setq marginalia-annotator-registry
  (assq-delete-all 'file marginalia-annotator-registry))

;;; UI
(use-package ewal
  :init (setq ewal-use-built-in-always-p nil))

(use-package ewal-doom-themes
  :config (progn
            (load-theme 'ewal-doom-one t)
            (enable-theme 'ewal-doom-one)))

(use-package ewal-evil-cursors
  :after (ewal-doom-themes)
  :config (ewal-evil-cursors-get-colors
	   :apply t :spaceline t))

(use-package nerd-icons)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;; Line numbers
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)
(global-display-line-numbers-mode)
(column-number-mode)

(dolist (mode '(pdf-view-mode-hook eshell-mode-hook term-mode-hook)) (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Keybinds
;; Enable escape for quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Shows available keybinds
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

;; Org keybindings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(defun krishxmatta/find-todo ()
  (interactive)
  (let ((default-directory "~/org/todo/"))
    (call-interactively 'find-file)))
(global-set-key (kbd "C-c t") 'krishxmatta/find-todo)

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

;; Roam keybindings
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)

;;; PDF Viewing
(use-package pdf-tools
  :config
  (setq pdf-view-midnight-colors (cons (face-foreground 'default) (face-background 'default)))
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
  (add-hook 'pdf-view-mode-hook (lambda () (setq-local evil-normal-state-cursor (list nil)))))

(pdf-tools-install)

(use-package pdf-view-restore
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

;;; Org Mode
(use-package ox-hugo
  :after ox
  :config
  (setq org-hugo-front-matter-format "yaml"))

(use-package org-roam
  :config
  (setq org-roam-directory "~/org/roam")
  (org-roam-db-autosync-mode))

(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

;; Easy image manipulation
(use-package org-download
  :after org
  :config
  (org-download-enable))

;;;; Task Management Setup
(setq org-capture-templates '(("t" "Inbox" entry (file+headline "~/org/todo/inbox.org" "Inbox") "* TODO %i%?")))
(setq org-agenda-files '("~/org/todo"))

(defvar krishxmatta/org-refile-targets-original
  (if (boundp 'org-refile-targets)
    org-refile-targets
    nil))

(defun krishxmatta/set-refile-target-todo ()
  (if (and buffer-file-name
    (string-prefix-p (expand-file-name "~/org/todo/") buffer-file-name))
    (setq org-refile-targets
	'(("~/org/todo/inbox.org" :maxlevel . 9)
	  ("~/org/todo/next.org" :maxlevel . 9)
	  ("~/org/todo/potential.org" :maxlevel . 9)
	  ("~/org/todo/calendar.org" :maxlevel . 9)
	  ("~/org/todo/reminders.org" :maxlevel . 9)))
    (setq org-refile-targets krishxmatta/org-refile-targets-original)))

(add-hook 'post-command-hook 'krishxmatta/set-refile-target-todo)

;;;; Note-taking Setup
(setq org-preview-latex-default-process 'dvisvgm)

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

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :target (file+head "main/${slug}.org"
                            "#+title: ${title}\n#+date: %t\n#+hugo_section: main\n#+hugo_lastmod: %t\n#+hugo_tags: noexport\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain
         (file "~/org/templates/notes.org")
         :target (file+head "reference/${slug}.org"
                            "#+title: ${title}\n#+date: %t\n#+hugo_section: reference\n#+hugo_lastmod: %t\n#+hugo_tags: noexport\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain
         "%?"
         :target (file+head "articles/${slug}.org"
                            "#+title: ${title}\n#+date: %t\n#+hugo_section: articles\n#+hugo_lastmod: %t\n#+hugo_tags: noexport\n")
         :immediate-finish t
         :unnarrowed t)))

(setq krishxmatta/org-roam-draft-dirs '("/Users/krishxmatta/org/roam/reference/" "/Users/krishxmatta/org/roam/articles/"))

(defun krishxmatta/org-roam-tag-new-node-as-draft ()
  (if (member (file-name-directory buffer-file-name) krishxmatta/org-roam-draft-dirs)
      (org-roam-tag-add '("draft"))))

(add-hook 'org-roam-capture-new-node-hook #'krishxmatta/org-roam-tag-new-node-as-draft)

;; Automatically update last modified date in ox-hugo export (TODO: could be moved elsewhere)
(setq time-stamp-active t
	time-stamp-start "#\\+hugo_lastmod:[ \t]*"
	time-stamp-end "$"
	time-stamp-format "\[%Y-%m-%d\]")
(add-hook 'before-save-hook 'time-stamp)

;; Enable search by tag
(setq org-roam-node-display-template
      (concat "${title} " (propertize "${tags}" 'face 'org-tag)))

;;;; Reading list setup
(setq org-capture-templates (append org-capture-templates
				    '(("r" "Reading List" entry (file "~/org/reading-list.org")
				       "* TOREAD %?\n:PROPERTIES:\n:AUTHOR: \n:END:"))))

;;; Feed reader
(use-package elfeed
  :bind (("C-x w" . elfeed)))

(setq elfeed-feeds
      '("https://www.youtube.com/feeds/videos.xml?channel_id=UCP40_9XpPtmDPvGTlEEoGcw"
	"https://www.to-rss.xyz/wikipedia/current_events/"))

;;; Customize interface
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(quelpa pdf-continuous-scroll-mode pdf-view-restore org-roam-ui websocket elfeed ewal-evil-cursors ewal-spacemacs-themes ewal orderless which-key vertico use-package pdf-tools ox-hugo org-roam org-gtd org-download marginalia general evil-collection doom-modeline amx all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
