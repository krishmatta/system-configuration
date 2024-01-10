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

(load-theme 'grayscale t)               ; Set theme

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
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;; Line numbers
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)
(global-display-line-numbers-mode)
(column-number-mode)

(dolist (mode '(eshell-mode-hook term-mode-hook)) (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;;; Org Mode
(use-package ox-hugo
  :after ox)

(use-package org-roam
  :config
  (setq org-roam-directory "~/org/roam")
  (org-roam-db-autosync-mode))

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
      '(("~/org/todo/next.org" :level . 1)
        ("~/org/todo/projects.org" :level . 1)
        ("~/org/todo/delegated.org" :level . 1)
        ("~/org/todo/reminders.org" :level . 1)
        ("~/org/todo/calendar.org" :level . 1)
	("~/org/todo/potential.org" :level . 1)))
    (setq org-refile-targets krishxmatta/org-refile-targets-original)))

(add-hook 'post-command-hook 'krishxmatta/set-refile-target-todo)

;;;; Note-taking Setup
(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :target (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain
         "%?"
         :target (file+head "reference/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain
         (file "~/org/templates/notes.org")
         :target (file+head "articles/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)))

(defun krishxmatta/org-roam-tag-new-node-as-draft ()
  (org-roam-tag-add '("draft")))

(add-hook 'org-roam-capture-new-node-hook #'krishxmatta/org-roam-tag-new-node-as-draft)

;; Enable search by tag
(setq org-roam-node-display-template
      (concat "${title} " (propertize "${tags}" 'face 'org-tag)))

;;; Customize interface
(custom-set-variables
 '(package-selected-packages
   '(orderless which-key vertico use-package pdf-tools ox-hugo org-roam org-gtd org-download marginalia general evil-collection doom-modeline amx all-the-icons)))
(custom-set-faces)
