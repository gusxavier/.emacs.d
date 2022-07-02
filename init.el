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

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))

;; Activate packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
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

;;;;;;;;;;;;;;;;;;;;; UI

;; Better icons
;; Run M-x all-the-icons-install-fonts in the first time
(use-package all-the-icons
  :if (display-graphic-p))

;; Font
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil
			:font "MonoLisa Light"
			:height 160)
  (set-face-attribute 'default nil
		      :font "MonoLisa Custom Light"
		      :height 140))


;; Current theme

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Other themes that I use frequently

;; (use-package modus-themes
;;   :bind ("<f5>" . modus-themes-toggle)
;;   :init
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-vivendi))

;; Modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

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

;; Remove tooltips
(tooltip-mode -1)

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

;; Set window as maximized
(toggle-frame-maximized)

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

;; Completion framework
;; (use-package selectrum
;;   :init
;;   (selectrum-mode +1))

;; (use-package orderless
;;   :init
;;   (setq completion-styles '(orderless basic)
;; 	completion-category-defaults nil
;; 	completion-category-overrides '((file (styles basic partial-completion)))
;; 	orderless-component-separator "[ &]")

;;   ;; Highlight text matches on company
;;   (defun just-one-face (fn &rest args)
;;     (let ((orderless-match-faces [completions-common-part]))
;;       (apply fn args)))
;;   (advice-add 'company-capf--candidates :around #'just-one-face))

;; (use-package savehist
;;   :init
;;   (savehist-mode))

;; (use-package marginalia
;;   :after selectrum
;;   :custom
;;   (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
;;   :init
;;   (marginalia-mode))

;; (use-package consult
;;   :after selectrum)

;; Git + Emacs = <3
(use-package magit)

;; Keep parenthesis balanced
(use-package paredit
  :diminish
  :hook
  (prog-mode . paredit-mode))

;; Project manager
(use-package projectile
  :init
  (setq projectile-completion-system 'default)
  (projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;; Helm + Projectile integration <3w
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
;; (use-package smartparens
;;   :diminish
;;   :init
;;   (require 'smartparens-config)
;;   (smartparens-global-mode +1)
;;   (electric-pair-mode +1))

;; file tree sidebar
(use-package treemacs
  :bind ("<f8>" . treemacs))

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
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none))

;; Ask before exit
(setq confirm-kill-emacs 'y-or-n-p)

;; At last some piece and quiet
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Remove blinking cursor
(blink-cursor-mode -1)

;; Performance tunning
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;;;;;;;;;;;;;;;;;;;; LSP

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind
  ("M-." . #'xref-find-definitions)
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
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  :commands lsp)

;;;;;;;;;;;;;;;;;;;;; CLOJURE

(use-package clojure-mode
  :hook
  (clojure-mode . paredit-mode)
  :config
  (setq clojure-align-forms-automatically t))

(use-package cider
  :hook
  (cider-repl-mode . paredit-mode)
  :bind
  ("C-c M-b" . cider-repl-clear-buffer)
  :config
  (unbind-key "M-." cider-mode-map)
  (unbind-key "M-," cider-mode-map)

  (setq cider-prompt-for-symbol nil)
  
  (setq cider-test-defining-forms
	(delete-dups (append cider-test-defining-forms '("defflow"
							 "defflow-i18n"
							 "defflow-loopback-false"
							 "defflow-new-system!"))))

  (setq cider-test-show-report-on-success t)

  ;; Better visualization of test results
  (defun custom--cider-ansi-color-string-p (value)
    "Check for extra ANSI chars on VALUE."
    (or (string-match "^\\[" value)
	(string-match "\u001B\\[" value)))
  (advice-add 'cider-ansi-color-string-p :override #'custom--cider-ansi-color-string-p)

  ;; Improve matcher-combinators assertion results
  (defun custom--cider-font-lock-as (mode string)
    "Use MODE to font-lock the STRING (fixing matcher-combinators assertions.)."
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

;; Start emacs server to enable emacsclient
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
   (server-start))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((cider-clojure-cli-aliases . ":dev:test:nrepl"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
