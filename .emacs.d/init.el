(setq inhibit-startup-mesage t) ; Disable startup messages
(setq inhibit-splash-screen t) ; Disable splash screen
(setq initial-scratch-message nil) ; Disable scratch message

(scroll-bar-mode -1) ; Disable visible scroll bar
(tool-bar-mode -1) ; Disable tool bar
(tooltip-mode -1) ; Disable tooltips
(menu-bar-mode -1) ; Disable menu bar

(set-fringe-mode 10) ; Add margins

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Enable escape for quit

(set-face-attribute 'default nil :font "Iosevka" :height 150) ; Set font

(load-theme 'grayscale t) ; Set theme

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
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
