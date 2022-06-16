;;; package --- Summary

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;; NATIVE COMP

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

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
(defun package--save-selected-packages (&rest _opt)
  "Avoid writing package-selected-packages on init.el."
  nil)

;; Shorten minor modes (to be used with use-package)
(use-package diminish
  :init
  (diminish 'eldoc-mode))

;;;;;;;;;;;;;;;;;;;;; GENERAL

;; Use ag in projectile search
(use-package ag)

;; Auto complete
(use-package company
  :diminish
  :bind (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :init
  (global-company-mode t)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t))

;; Load env variables from PATH inside Emacs
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; Select regions by expanding chunks of text
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; Syntax checker
(use-package flycheck
  :diminish
  :init
  (global-flycheck-mode))

;; Minibuffer completion
(use-package helm
  :diminish
  :init
  (helm-mode t)
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  ;; Set TAB key to use helm TAB completion
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action))

;; Use ag (the silver searcher) to search using helm
(use-package helm-ag)

;; Keep parenthesis balanced
(use-package paredit
  :diminish)

;; Project manager
(use-package projectile
  :init
  (projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;; Helm + Projectile integration <3
(use-package helm-projectile
  :init
  (helm-projectile-on))

;; Show each delimiter (parenthesis, brackets, etc) with different colors
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Make HTTP requests inside Emacs
(use-package restclient)

;; Dealing with pairs (parenthesis, brackets, etc)
(use-package smartparens
  :diminish
  :init
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  (electric-pair-mode +1))

;; File tree sidebar
(use-package treemacs
  :bind ("<f8>" . treemacs))

(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-load-all-the-icons-with-workaround-font t))

;; Terminal inside emacs
(use-package vterm)

;; Show command suggestions
(use-package which-key
  :diminish
  :config
  (which-key-mode +1))

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
(setq make-backup-files nil)

;; Set command as meta key in mac
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Ask before exit
(setq confirm-kill-emacs 'y-or-n-p)

;; At last some piece and quiet
(setq ring-bell-function 'ignore)

;; Remove blinking cursor
(blink-cursor-mode -1)

;; Performance tunning
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;;;;;;;;;;;;;;;;;;;; UI

;; Better icons
;; Run M-x all-the-icons-install-fonts in the first time
(use-package all-the-icons
  :if (display-graphic-p))

;; Font
(set-face-attribute 'default nil
		    :font "MonoLisa"
		    :weight 'regular
		    :height 170)

;; Current theme
(use-package modus-themes
  :config
  (modus-themes-load-themes)
  (load-theme 'modus-vivendi t))

;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-one t)

;;   ;; Enable doom treemacs theme
;;   (setq doom-themes-treemacs-theme "doom-colors")
;;   (doom-themes-treemacs-config)

;;   ;; Improve org-mode
;;   (doom-themes-org-config))

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
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; Show line numbers
(global-display-line-numbers-mode t)

;; Show cursor position
(line-number-mode t)
(column-number-mode t)

;; Smooth scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil)

;; Disable startup screen
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;; LSP

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind
  ("M-." . xref-find-definitions)
  :hook
  (((go-mode
     clojure-mode
     clojurec-mode
     clojurescript-mode
     elixir-mode
     java-mode
     rust-mode
     rustic-mode) . lsp-deferred))
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-lens-enable nil)
  (setq lsp-use-plists t)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-header nil)
  (setq lsp-ui-doc-include-signuature nil))

;;;;;;;;;;;;;;;;;;;;; CLOJURE

(use-package clojure-mode
  :hook
  (clojure-mode . paredit-mode)
  :config
  (setq clojure-align-forms-automatically t))

;; Better visualization of test results
(defun custom--cider-ansi-color-string-p (value)
  "Check for extra ANSI chars on VALUE."
  (or (string-match "^\\[" value)
      (string-match "\u001B\\[" value)))

;; Improve matcher-combinators assertion results
(defun custom--cider-font-lock-as (mode string)
  "Use MODE to font-lock the STRING.
Copied from cider-util.el, it does the same but doesn't remove
string properties and doesn't check for valid clojure-code, fixing
matcher-combinators assertions."
  (let ((string (if (cider-ansi-color-string-p string)
                    (ansi-color-apply string)
                  string)))
    (if (or (null cider-font-lock-max-length)
            (< (length string) cider-font-lock-max-length))
        (with-current-buffer (cider--make-buffer-for-mode mode)
          (erase-buffer)
          (insert string)
          (font-lock-fontify-region (point-min) (point-max))
          (buffer-string))
      string)))

(use-package cider
  :bind
  ("C-c M-b" . cider-repl-clear-buffer)
  :config
  (setq cider-prompt-for-symbol nil)
  (unbind-key "M-." cider-mode-map)
  (unbind-key "M-," cider-mode-map)
  (setq cider-test-defining-forms
        (append cider-test-defining-forms '("defflow"
                                            "defflow-i18n"
                                            "defflow-loopback-false"
                                            "defflow-new-system!")))
  (advice-add 'cider-ansi-color-string-p :override #'custom--cider-ansi-color-string-p)
  (advice-add 'cider-font-lock-as :override #'custom--cider-font-lock-as))

(use-package helm-cider
  :hook
  (cider-mode . helm-cider-mode))

;;;;;;;;;;;;;;;;;;;;; GO

(use-package go-mode)

;;;;;;;;;;;;;;;;;;;;; JAVA

(use-package lsp-java
  :after lsp)

;;;;;;;;;;;;;;;;;;;;; RUST

(use-package rustic)

;;;;;;;;;;;;;;;;;;;;; TYPESCRIPT

(use-package typescript-mode)

;;;;;;;;;;;;;;;;;;;;; ELIXIR

(use-package elixir-mode)

;;;;;;;;;;;;;;;;;;;;; GRAPHQL

(use-package graphql-mode)

;;;;;;;;;;;;;;;;;;;;; YAML

(use-package yaml-mode)

(provide 'init)

;;; init.el ends here
