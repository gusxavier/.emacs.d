;;; Package --- summary

;;; Commentary:

;;; Code:

(require 'use-package)
(setq use-package-always-ensure t)

(use-package all-the-icons)

(use-package better-defaults)

(use-package cider
  :bind ("C-c M-b" . cider-repl-clear-buffer)
  :config
  (setq cider-lein-parameters "with-profile prod repl :headless"))

(use-package clj-refactor
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

(use-package clojure-mode)

(use-package company
  :defer t
  :init (global-company-mode))

(use-package company-go)

(use-package dracula-theme
  :init (load-theme 'dracula t))

(use-package elpy
  :init
  (elpy-enable)
  :config
  (setq python-indent 4)
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset 4))

(use-package enh-ruby-mode
  :bind ("TAB" . enh-ruby-indent-exp)
  :config
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (add-to-list 'auto-mode-alist
               '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package feature-mode)

(use-package flycheck
  :defer t
  :config
  (setq flycheck-highlighting-mode "symbols")
  :init (global-flycheck-mode))

(use-package go-autocomplete)

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-guru)

(use-package helm
  :defer t
  ;; :config
  ;; (setq helm-split-window-default-side 'down)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)))

(use-package helm-ag
  :config
  (setq helm-ag-use-grep-ignore-list '("\\node_modules\\'"))
  :defer t)

(use-package helm-projectile
  :defer t
  :init (helm-projectile-on))

(use-package inf-mongo)

(use-package inf-ruby)

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package js2-refactor
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js-mode-map (kbd "M-.") nil))

(use-package magit)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package neotree
  :defer t
  :bind ("C-c p n" . neotree)
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package nim-mode)

(use-package paredit
  :init (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package projectile
  :defer t
  :config
  (projectile-discover-projects-in-directory "~/workspace")
  ;; Workaround to avoid projectile making the editor very slow
  ;; https://github.com/bbatsov/projectile/issues/1183#issuecomment-335569547
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  :init (projectile-mode))

(use-package pyenv-mode
  :config
  (pyenv-mode))

(use-package pyenv-mode-auto)

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :init (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package restclient)

(use-package rubocop)

(use-package ruby-end)

(use-package ruby-mode
  :config (setq ruby-insert-encoding-magic-comment nil))

(use-package rvm)

(use-package slim-mode)

(use-package smartparens
  :defer t
  :init (smartparens-global-mode))

(use-package telephone-line
  :defer t
  :init (telephone-line-mode 1))

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package vue-mode)

(use-package web-mode
  :config
  (defun my-web-mode-hook ()
    "Hooks for web mode"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-indentation nil))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mjml\\'" . web-mode)))


(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'packages)
;;; packages.el ends here
