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
(defun package--save-selected-packages (&rest _opt)
  "Avoid writing package-selected-packages on init.el."
  nil)

;;;;;;;;;;;;;;;;;;;;; GENERAL

;; Use ag in projectile search
(use-package ag)

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
  (setq company-selection-wrap-around t))

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

;; Minibuffer completion (alternative to ido and helm)
(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
  (setq-default ivy-use-virtual-buffers t
		ivy-dynamic-exhibit-delay-ms 250
		ivy-re-builders-alist '((t . ivy--regex-fuzzy))
		;; ivy-initial-inputs-alist nil
		)
  ;; Use enter to navigate instead opening dired
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done))

(use-package counsel
  :init
  (counsel-mode t))

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))

;; Paredit (keep parenthesis balanced)
(use-package paredit)

;; Project manager
(use-package projectile
  :init
  (projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package counsel-projectile
  :after projectile
  :init
  (counsel-projectile-mode t))

;; Show each delimiter (parenthesis, brackets, etc) with different colors
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

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

;; Set command as meta key in mac
(setq-default mac-option-key-is-meta nil
              mac-command-key-is-meta t
              mac-command-modifier 'meta
              mac-option-modifier 'none)

;; Ask before exit
(setq confirm-kill-emacs 'y-or-n-p)

;; At last some piece and quiet
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;; UI

;; Show icons
;; Run M-x all-the-icons-install-fonts in the first time
(use-package all-the-icons
  :if (display-graphic-p))

;; Theme
(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

;; Font
(if (memq window-system '(mac ns x))
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco")
    (set-face-attribute 'default nil :height 140))
  (when (member "JetBrains Mono" (font-family-list))
    (set-face-attribute 'default nil :font "JetBrains Mono")
    (set-face-attribute 'default nil :height 130)))

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
  :bind
  ("M-." . lsp-find-definition)
  :hook
  (((go-mode
     clojure-mode
     clojurec-mode
     clojurescript-mode
     java-mode) . lsp-deferred))
  :config
  (setq-default lsp-auto-guess-root t)
  (setq-default lsp-log-io nil)
  (setq-default lsp-restart 'auto-restart)
  (setq-default lsp-enable-symbol-highlighting nil)
  (setq-default lsp-enable-on-type-formatting nil)
  (setq-default lsp-signature-auto-activate nil)
  (setq-default lsp-signature-render-documentation nil)
  (setq-default lsp-eldoc-hook nil)
  (setq-default lsp-modeline-code-actions-enable nil)
  (setq-default lsp-modeline-diagnostics-enable nil)
  (setq-default lsp-headerline-breadcrumb-enable nil)
  (setq-default lsp-semantic-tokens-enable nil)
  (setq-default lsp-enable-folding nil)
  (setq-default lsp-enable-imenu nil)
  (setq-default lsp-enable-snippet nil)
  (setq-default read-process-output-max (* 1024 1024)) ;; 1MB
  (setq-default lsp-idle-delay 0.5)
  (lsp-enable-which-key-integration t)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq-default lsp-ui-doc-enable nil)
  (setq-default lsp-ui-sideline-enable nil)
  (setq-default lsp-ui-doc-header nil)
  (setq-default lsp-ui-doc-include-signuature nil)
  (setq-default lsp-ui-doc-border (face-foreground 'default))
  (setq-default lsp-ui-sideline-delay 0.05))

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
  (setq-default cider-prompt-for-symbol nil)
  (unbind-key "M-." cider-mode-map)
  (unbind-key "M-," cider-mode-map)
  (setq cider-test-defining-forms
        (append cider-test-defining-forms '("defflow"
                                            "defflow-i18n"
                                            "defflow-loopback-false"
                                            "defflow-new-system!")))
  (advice-add 'cider-ansi-color-string-p :override #'custom--cider-ansi-color-string-p)
  (advice-add 'cider-font-lock-as :override #'custom--cider-font-lock-as))

;;;;;;;;;;;;;;;;;;;;; GOLANG

(use-package go-mode)

;;;;;;;;;;;;;;;;;;;;; JAVA

(use-package lsp-java
  :after lsp)

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
