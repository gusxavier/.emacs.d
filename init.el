(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections"
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)

;; Make GC threshold high to startup faster
(setq gc-cons-threshold (* 50 1000 1000))

;; Increase the number of bytes read from subprocesses
(setq read-process-output-max (* 1024 1024))

;;; package --- Summary

;;; Commentary:

;;; Code:

;; NOTE: This file was generated from Emacs.org. Do not edit it by hand
;; and update Emacs.org instead.
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Activate packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Always install missing packages
(setq use-package-always-ensure t)

;; Avoid writing package-selected-packages on init.el
(defun package--save-selected-packages (&rest _opt)
  "Avoid writing `package-selected-packages' on init.el."
  nil)

;; Load custom or local code packages
(add-to-list 'load-path (expand-file-name "custom/" user-emacs-directory))

(use-package no-littering)

;; Store auto save files in the no-littering specific dir
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Reload buffers on disk change
(global-auto-revert-mode t)

;; Just type y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Avoid generating auto save files
;; [TODO]: use no-littering package
;; (setq auto-save-default nil)
;; (setq create-lockfiles nil)
;; (setq make-backup-files nil)

;; Set command as meta key in mac
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none))

;; Ask before exit
(setq confirm-kill-emacs 'y-or-n-p)

;; Better comments
(use-package evil-nerd-commenter
  :defer t
  :bind
  (("M-;" . 'evilnc-comment-or-uncomment-lines)))

;; Load env variables from PATH inside Emacs
;; P.S: only needed for Mac OS
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

;; Select regions by expanding chunks of text
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; Persist history over Emacs restarts
(use-package savehist
  :init
  (savehist-mode))

;; Show keybindings suggestions
(use-package which-key
  :defer 0
  :config
  (which-key-mode +1))

(use-package corfu
  :after orderless
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-cycle t))

;; Better completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

;; Completion framework
(use-package vertico
  :init
  (vertico-mode))

;; Improve completing-read
(use-package consult
  :after vertico
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-c C-j" . consult-imenu)
   ("C-c p s s" . consult-ripgrep)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   :map minibuffer-local-map
   ("C-h" . consult-history))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Basically a right click but with buffers
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark + Consult = <3
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Annotations in the completion framework
(use-package marginalia
  :after vertico
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Use tab to open autocomplete
(setq tab-always-indent 'complete)

;; Highlight errors on buffer
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Terminal inside emacs
(use-package vterm
  :commands vterm)

;; Use same keybindings as projectile
(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p f") 'project-find-file)

(defun my/org-mode-setup ()
  "Custom 'org-mode' setup."
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :pin org

  :commands (org-capute org-agenda)

  :hook
  (org-mode . my/org-mode-setup)

  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "REVIEW(r)" "|" "DONE(d!)")))

  (setq org-agenda-start-with-log-mode t)

  (setq org-log-done 'time)

  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)))

  (setq org-confirm-babel-evaluate nil)

  (defun my/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "Emacs.org" user-emacs-directory))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  ;; Automatically tangle our Emacs.org config file when we save it
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'my/org-babel-tangle-config))))

(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defvar my/default-font "PragmataPro Liga")

;; Set a different font size between MacOS and Linux
(defvar my/default-font-height (if (eq system-type 'darwin) 220 190))

(set-face-attribute 'default nil
                    :family my/default-font
                    :height my/default-font-height
                    :weight 'regular)

;; Function to set the mode-line font a little smaller
;; Will be used in the configuration below
(defun my/set-modeline-font-size ()
  (set-face-attribute 'mode-line nil
                      :height (- my/default-font-height 30))
  (set-face-attribute 'mode-line-inactive nil
                      :height (- my/default-font-height 30)))

;; Set encoding to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; Avoid slowness with some fonts
(setq inhibit-compacting-font-caches t)

;; Enable PragmataPro font ligatures
(require 'pragmatapro-lig)
(pragmatapro-lig-global-mode)

;; Run M-x all-the-icons-install-fonts in the first time
(use-package all-the-icons
  :if (display-graphic-p))

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

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                cider-repl-mode-hook
                cider-stacktrace-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; Highlight parens
(show-paren-mode t)

;; At last some piece and quiet
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Remove blinking cursor
(blink-cursor-mode -1)

;; Enable moving to buffers using arrow keys
;; [TODO]: Find a set of keybindings that do not
;; conflict with org-mode
;; (windmove-default-keybindings)

;; File tree sidebar
(use-package treemacs
  :bind
  ("<f8>" . treemacs))

;; Show each delimiter (parenthesis, brackets, etc) with different colors
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Doom-themes for the win!
;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-one t)
;;   Add padding to the modeline
;;   (setq doom-themes-padded-modeline t))

;; Uncomment to enable modus-themes
(setq modus-themes-mode-line '(accented borderless)
      modus-themes-region '(bg-only accented)
      modus-themes-italic-constructs t
      modus-themes-paren-match '(intense)
      modus-themes-subtle-line-numbers t
      modus-themes-org-blocks 'gray-background)

(defun my/modus-themes-toggle ()
  (interactive)
  (modus-themes-toggle)
  (my/set-modeline-font-size))

;; Switch between dark and light themes
(global-set-key (kbd "<f5>") 'my/modus-themes-toggle)

(load-theme 'modus-operandi t)

;; Better modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (my/set-modeline-font-size))

;; Dealing with pairs (parenthesis, brackets, etc)
(use-package smartparens
  :hook
  ((prog-mode . smartparens-strict-mode)
   (org-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))

;; Make HTTP requests inside Emacs
(use-package restclient
  :commands restclient-mode)

;; Git + Emacs = <3
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; LSP client
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-use-plists t)
  :config
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-lens-enable nil)
  (setq lsp-enable-indentation nil)
  ;; Use corfu as completion
  (setq lsp-completion-provider :none)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"))

;; LSP + Treemacs integration
(use-package lsp-treemacs
  :after lsp)

(add-hook #'emacs-lisp-hook 'smartparens-strict-mode)

(use-package clojure-mode
  :hook
  ((clojure-mode . smartparens-strict-mode)
   (clojure-mode . lsp-deferred)))

(use-package cider
  :commands cider-jack-in
  :bind
  ("C-c M-b" . cider-repl-clear-buffer)
  :config
  (unbind-key "M-." cider-mode-map)
  (unbind-key "M-," cider-mode-map)
  (setq cider-prompt-for-symbol nil
        cider-test-defining-forms '("deftest" "defspec" "defflow" "init-flow")))

;; Better visualization of test results
(defun my/cider-ansi-color-string-p (value)
  "Check for extra ANSI chars on VALUE."
  (or (string-match "^\\[" value)
      (string-match "\u001B\\[" value)))
(advice-add 'cider-ansi-color-string-p :override #'my/cider-ansi-color-string-p)

;; Improve matcher-combinators assertion results
(defun my/cider-font-lock-as (mode string)
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
(advice-add 'cider-font-lock-as :override #'my/cider-font-lock-as)

(use-package go-mode
  :hook
  (go-mode . lsp-deferred))

(use-package rustic
  :hook
  (rustic-mode . lsp-deferred))

(use-package typescript-mode
  :hook
  (typescript-mode . lsp-deferred))

(use-package elixir-mode
  :hook
  (elixir-mode . lsp-deferred))

(use-package graphql-mode
  :defer t
  :commands graphql-mode)

(use-package yaml-mode
  :defer t
  :commands yaml-mode)

;; Start emacs server to enable emacsclient
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; Bring GC threshold back to a more reasonable amount
(setq gc-cons-threshold (* 10 1000 1000))

(provide 'init)

;;; init.el ends here.
