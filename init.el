;;; package --- Summary

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;; PACKAGE CONFIGURATION

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("gnu" . "https://elpa.gnu.org/packages/"))

;; Activate packages
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Avoid writing package-selected-packages on init.el
(defun package--save-selected-packages (&rest opt) nil)

;;;;;;;;;;;;;;;;;;;;; GENERAL

;; Auto complete
(use-package company
  :bind (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :init
  (global-company-mode t)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-arount t))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; Select regions by expanding chunks of text
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; Syntax checker
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Project manager
(use-package projectile
  :init
  (projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;; Show each delimiter (parenthesis, brackets, etc) with different colors
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Minibuffer completion (alternative to ido, ivy and helm)
(use-package selectrum
  :init
  (selectrum-mode +1))

(use-package hotfuzz
  :init
  (hotfuzz-selectrum-mode +1))

;; Show command suggestions
(use-package which-key
  :init
  (which-key-mode +1))

;; Insert matching delimiters (parenthesis, brackets, etc)
(electric-pair-mode 1)

;; Highlight parens
(show-paren-mode t)

;; Enable moving to buffers using arrow keys
(windmove-default-keybindings)

;; Reload buffers on disk change
(global-auto-revert-mode t)

;; Just type y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Avoid generating auto save files
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;; UI

;; Show icons
;; Run M-x all-the-icons-install-fonts in the first time
(use-package all-the-icons
  :if (display-graphic-p))

;; Theme
(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

;; (use-package modus-themes
;;   :init
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-vivendi)
;;   :bind ("<f5>" . modus-themes-toggle))

;; Font
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono")
  (set-face-attribute 'default nil :height 130))

;; Set font encoding to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; Avoid slowness with some fonts
(setq inhibit-compacting-font-caches t)

;; Remove scroll bar
(scroll-bar-mode -1)

;; Remove top bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Highlight current line
(global-hl-line-mode t)

;; Show line numbers
(global-display-line-numbers-mode t)

;; Smooth scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil)

;; Disable startup screen
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;; LSP

(use-package lsp-mode
  :hook (((go-mode) . lsp-deferred)
	 (before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports))
  :config
  (lsp-enable-which-key-integration t)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signuature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

;;;;;;;;;;;;;;;;;;;;; GOLANG

(use-package go-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here

