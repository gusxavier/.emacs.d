;; -*- lexical-binding: t; -*-

;;; package --- Summary

;;; Commentary:

;;; Code:

;; NOTE: This file was generated from Emacs.org. Do not edit it by hand
;; and update Emacs.org instead.

;; Display the time it took to make the editor usable
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

;; Native comp config
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

;; Keep folders clean
(use-package no-littering
  :custom
  ;; Store auto save files in the no-littering specific dir
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Diminish modes on modeline
(use-package diminish
  :demand t
  :init
  (diminish 'visual-line-mode)
  (diminish 'eldoc-mode))

;; Better comments
(use-package evil-nerd-commenter
  :defer t
  :bind
  (("M-;" . 'evilnc-comment-or-uncomment-lines)))

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
  :diminish
  :defer 0
  :config
  (which-key-mode +1))

(use-package helpful
  :commands
  (helpful-callable
   helpful-variable
   helpful-command
   helpful-key helpful-at-point)
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   ([remap describe-command] . helpful-command)
   ("C-h p" . #'helpful-at-point)
   ("C-h F" . #'helpful-function)))

;; Reload buffers on disk change
(global-auto-revert-mode t)

;; Just type y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Set command as meta key in mac
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none))

;; Ask before exit
(setq confirm-kill-emacs 'y-or-n-p)

(defvar my/config-file
  (expand-file-name "Emacs.org" user-emacs-directory))

(defun my/load-config-file ()
  (interactive)
  (switch-to-buffer (find-file-noselect my/config-file)))

;; Keybinding to open config file
(global-set-key (kbd "<f9>") 'my/load-config-file)

(use-package corfu
  :after orderless
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 1))

;; Better completion style
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-prefixes orderless-initialism orderless-regexp)))

;; Completion framework
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

;; Improve completing-read
(use-package consult
  :demand t
  :after vertico
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-c C-j" . consult-imenu)
   ("C-c p s s" . consult-ripgrep)
   ("C-x p g" . consult-ripgrep)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("C-x p b" . consult-project-buffer)
   ("M-y" . consult-yank-pop)
   ("M-g f". consult-flycheck)
   ("<help> a" . consult-apropos)
   ("M-g o" . consult-org-heading)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Basically a right click but with buffers
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
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
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; Use tab to open autocomplete
(setq tab-always-indent 'complete)

;; Highlight errors on buffer
(use-package flycheck
  :diminish
  :hook (prog-mode . flycheck-mode))

(use-package consult-flycheck
  :after (consult flycheck))

;; Terminal inside emacs
(use-package vterm
  :commands vterm)

(defvar my/default-font "PragmataPro Mono Liga")

;; Set a different font size between MacOS and Linux
(defvar my/default-font-height (if (eq system-type 'darwin) 210 160))

(set-face-attribute 'default nil
                    :family my/default-font
                    :height my/default-font-height
                    :weight 'regular)

;; Fixed pitch font is used in places like org mode src blocks
(set-face-attribute 'fixed-pitch nil
                    :family my/default-font
                    :height my/default-font-height
                    :weight 'regular)

;; Set encoding to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; Avoid slowness with some fonts
(setq inhibit-compacting-font-caches t)

;; Enable PragmataPro font ligatures
;; (require 'pragmatapro-lig)
;; (pragmatapro-lig-global-mode)
;; (diminish 'pragmatapro-lig-mode)

;; Enable emoji support
(use-package emojify
  ;; :hook
  ;; (after-init . global-emojify-mode)
  )

;; Run M-x all-the-icons-install-fonts in the first time
(use-package all-the-icons
  :if (display-graphic-p))

;; Show icons in completion
;; (use-package all-the-icons-completion
;;   :after
;;   (marginalia all-the-icons)
;;   :hook
;;   (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))

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
                vterm-mode-hook
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

;; Set blinking cursor
(blink-cursor-mode +1)

;; Set the cursor type as a thin vertical bar
;; (setq-default cursor-type 'bar)

;; File tree sidebar
(use-package treemacs
  :commands treemacs
  :bind
  ("<f8>" . treemacs)
  :custom
  (treemacs-text-scale -0.5))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-all-the-icons-with-workaround-font my/default-font))

;; Show each delimiter (parenthesis, brackets, etc) with different colors
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-org-config))

;; (use-package modus-themes
;;   :bind
;;   ("<f5>" . modus-themes-toggle)
;;   :init
;;   (setq modus-themes-mode-line '(accented borderless)
;;         modus-themes-region '(bg-only accented)
;;         modus-themes-italic-constructs t
;;         modus-themes-paren-match '(intense)
;;         modus-themes-subtle-line-numbers t
;;         modus-themes-org-blocks 'gray-background)
;;   (load-theme 'modus-vivendi t))

;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))

;; (use-package mindre-theme
;;   :custom
;;   (mindre-use-more-bold nil)
;;   (mindre-use-faded-lisp-parens nil)
;;   :custom-face
;;   (mindre-faded ((t (:foreground "#585c60"))))
;;   :config
;;   (load-theme 'mindre t)
;;   (set-face-attribute 'mode-line nil
;;                       :background "#5c3e99" :foreground "#eceff1"
;;                       :height (- my/default-font-height 20)
;;                       :box '(:line-width 6 :color "#5c3e99"))
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :background "#a991cf" :foreground "#eceff1"
;;                       :height (- my/default-font-height 20)
;;                       :box '(:line-width 6 :color "#a991cf")))

;; Better modeline
;; (use-package doom-modeline
;;   :init
;;   (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-height 1)
;;   :config
;;   (custom-set-faces
;;    '(mode-line ((t (:height 0.9))))
;;    '(mode-line-active ((t (:height 0.9))))
;;    '(mode-line-inactive ((t (:height 0.9))))))

;; (use-package spaceline
;;   :init
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme))

;; (defun my/config-default-modeline ()
;;   (let ((bg-color (face-attribute 'mode-line :background))
;;         (inactive-bg-color (face-attribute 'mode-line-inactive :background)))
;;   (set-face-attribute 'mode-line nil
;;                       :height (- my/default-font-height 40)
;;                       :box `(:line-width 6 :color ,bg-color))
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :height (- my/default-font-height 40)
;;                       :box `(:line-width 6 :color ,inactive-bg-color))))

;; (my/config-default-modeline)

;; Dealing with pairs (parenthesis, brackets, etc)
(use-package smartparens
  :diminish
  :hook
  (prog-mode . smartparens-strict-mode)
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
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-log-io nil)
  (lsp-restart 'auto-restart)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-auto-activate nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu nil)
  (lsp-enable-snippet nil)
  (lsp-lens-enable nil)
  (lsp-enable-indentation nil)
  (lsp-completion-provider :none) ;; Use corfu as completion
  (lsp-rust-analyzer-cargo-watch-command "clippy"))

;; LSP + Treemacs integration
(use-package lsp-treemacs
  :after lsp)

(use-package clojure-mode
  :hook
  (clojure-mode . lsp-deferred))

(defun my/cider-mode-hook ()
  (smartparens-strict-mode)
  ;; Use CIDER completion when the REPL is on
  (setq-local lsp-enable-completion-at-point nil)
  ;; Temporary fix to use cider completions with corfu
  (setq-local completion-styles '(basic)))

(use-package cider
  :commands cider-jack-in
  :bind
  ("C-c M-b" . cider-repl-clear-buffer)
  :hook
  (cider-mode . my/cider-mode-hook)
  :config
  (unbind-key "M-." cider-mode-map)
  (unbind-key "M-," cider-mode-map)
  :custom
  (cider-prompt-for-symbol nil)
  (cider-test-defining-forms '("deftest"
                               "defspec"
                               "defflow"
                               "init-flow")))

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

(defun my/org-mode-setup ()
  "Custom 'org-mode' setup."
  (org-indent-mode)
  (diminish 'org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook
  (org-mode . my/org-mode-setup)
  :custom
  ;; UI IMPROVEMENTS
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)
  ;; ORG-AGENDA
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(i)" "REVIEW(r)" "BLOCKED(b)" "|" "DONE(d!)")))
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  :config
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Make org link open file in the same buffer
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; ORG-AGENDA
  (defvar org-files-dir "~/dev/org-files")
  (setq org-agenda-files (list (expand-file-name "Tasks.org" org-files-dir)
                               (expand-file-name "Birthdays.org" org-files-dir)))

  ;; ORG-BABEL
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)))

  (setq org-confirm-babel-evaluate nil)

  (defun my/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name) my/config-file)
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  ;; Automatically tangle our Emacs.org config file when we save it
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'my/org-babel-tangle-config))))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :diminish
  :hook (org-mode . my/org-mode-visual-fill))

(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n I" . my/org-roam-node-insert-imediate)
   ("C-c n b" . my/org-roam-capture-inbox))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :custom
  (org-roam-directory "~/dev/org-roam")
  (org-roam-completion-everywhere t)
  ;; Enable marginalia tags
  (org-roam-node-display-template
   (concat "${title:80} " (propertize "${tags:20}" 'face 'org-tag))
   org-roam-node-annotation-function
   (lambda (node) (marginalia--time (org-roam-node-file-mtime node))))
  :config
  (require 'org-roam-dailies)
  (setq org-roam-dailies-directory "Dailies/")
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "project" plain
           (file (expand-file-name "Templates/ProjectTemplate.org" org-roam-directory))
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: Project")
           :unnarrowed t)))
  (org-roam-setup))

(defun my/org-roam-node-insert-imediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* TODO %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n\n\n")))))

(use-package org-roam-ui
  :diminish
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; Start emacs server to enable emacsclient
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; Bring GC threshold back to a more reasonable amount
(setq gc-cons-threshold (* 10 1000 1000))

(provide 'init)

;;; init.el ends here.
