;;; package --- Summary

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;; NATIVE COMP

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
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
  "Avoid writing 'package-selected-packages' on init.el."
  nil)

;; Shorten minor modes (to be used with use-package)
(use-package diminish
  :init
  (diminish 'eldoc-mode))

;;;;;;;;;;;;;;;;;;;;; CORE

;; Show flymake errors first in eldoc
(defun my/improve-eldoc-flymake ()
  "Improve eldoc and flymake integration."
  (setq eldoc-documentation-functions
        (cons #'flymake-eldoc-function
              (remove #'flymake-eldoc-function eldoc-documentation-functions)))
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package emacs
  :bind
  (;; Flymake
   ("M-n" . #'flymake-goto-next-error)
   ("M-p" . #'flymake-goto-prev-error)

   ;; Modus themes
   ("<f5>" . #'modus-themes-toggle))

  :hook
  (;; Enable flymake by default
   (prog-mode . flymake-mode)
   (text-mode . flymake-mode)

   ;; Enable smartparens when editting elisp
   (elisp-mode . smartparens-strict-mode))

  :init
  ;; Modus themes improvements
  (setq modus-themes-mode-line '(accented borderless)
	modus-themes-region '(bg-only)
	modus-themes-bold-constructs t
	modus-themes-italic-constructs t
	modus-themes-paren-match '(bold intense)
	modus-themes-prompts '(intense)
	modus-themes-tabs-accented t
	modus-themes-subtle-line-numbers t
	modus-themes-lang-checkers '(background faint))

  :config
  ;; Modus themes for the win!
  (load-theme 'modus-vivendi t)

  ;; Font
  (if (eq system-type 'darwin)
      (set-face-attribute 'default nil
			  :font "MonoLisa Custom Light"
			  :height 160)
    (set-face-attribute 'default nil
			:font "MonoLisa Light"
			:height 140))

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

  ;; Fix eldoc overriding flymake errors
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-hook 'prog-mode-hook 'my/improve-eldoc-flymake)

  ;; Use tab to open autocomplete
  (setq tab-always-indent 'complete)

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
  (setq read-process-output-max (* 1024 1024)))

;;;;;;;;;;;;;;;;;;;;; UI

;; Better icons
;; Run M-x all-the-icons-install-fonts in the first time
(use-package all-the-icons
  :if (display-graphic-p))

;; Theme
(use-package doom-themes
  :config
  ;; (load-theme 'doom-dracula t)
  (setq doom-themes-treemacs-theme "doom-atom"
	doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Improved modeline
(use-package mood-line
  :init
  (mood-line-mode 1))

;;;;;;;;;;;;;;;;;;;;; GENERAL

;; Autocomplete
(use-package corfu
  :after orderless
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-cycle t))

;; Improve completing-read
(use-package consult
  :after vertico
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-c C-j" . consult-imenu)
   ("C-c p s s" . consult-ripgrep))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; LSP client
(use-package eglot
  :hook
  (((clojure-mode
     clojurec-mode
     clojurescript-mode
     elixir-mode
     go-mode
     java-mode
     python-mode
     rust-mode
     rustic-mode) . eglot-ensure))
  :config
  (setq eglot-extend-to-xref t
	eglot-ignored-server-capabilites '(:documentHighlightProvider)
	eglot-connect-timeout 120)
  (add-hook 'eglot-managed-mode-hook 'my/improve-eldoc-flymake))

;; Basically a right click but with buffers
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark + Consult = <3
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Load env variables from PATH inside Emacs
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; Select regions by expanding chunks of text
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; Git + Emacs = <3
(use-package magit)

;; Annotations in the completion framework
(use-package marginalia
  :after vertico
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Better completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Show each delimiter (parenthesis, brackets, etc) with different colors
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Make HTTP requests inside Emacs
(use-package restclient)

;; Persist history over Emacs restarts
(use-package savehist
  :init
  (savehist-mode))

;; Dealing with pairs (parenthesis, brackets, etc)
(use-package smartparens
  :diminish
  :init
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  (sp-use-smartparens-bindings))

;; File tree sidebar
(use-package treemacs
  :bind
  ("<f8>" . treemacs)
  :config)

;; Completion framework
(use-package vertico
  :init
  (vertico-mode))

;; Terminal inside emacs
(use-package vterm)

;; Show command suggestions
(use-package which-key
  :diminish
  :config
  (which-key-mode +1))

;;;;;;;;;;;;;;;;;;;;; CLOJURE

(use-package clojure-mode
  :hook
  (clojure-mode . smartparens-strict-mode)
  :config
  (setq clojure-align-forms-automatically t))

(use-package cider
  :hook
  (cider-repl-mode . smartparens-strict-mode)
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

;;;;;;;;;;;;;;;;;;;;; GO

(use-package go-mode)

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
 '(safe-local-variable-values
   '((cider-clojure-cli-aliases . ":dev:test")
     (cider-clojure-cli-aliases . ":dev:test:nrepl"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
