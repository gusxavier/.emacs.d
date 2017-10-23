;;; Package --- summary

;;; Commentary:

;;; Code:

(require 'use-package)
(setq use-package-always-ensure t)

(use-package all-the-icons)

(use-package better-defaults)

(use-package company
  :defer t
  :init (global-company-mode))

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package enh-ruby-mode
  :bind ("TAB" . enh-ruby-indent-exp)
  :config
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (add-to-list 'auto-mode-alist
               '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package feature-mode)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)))

(use-package helm-ag)

(use-package helm-projectile
  :defer t
  :init (helm-projectile-on))

(use-package inf-ruby)

(use-package magit)

(use-package neotree
  :bind ("C-c p n" . neotree)
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package projectile
  :defer t
  :config (projectile-discover-projects-in-directory "~/workspace")
  :init (projectile-mode))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :init (rainbow-mode))

(use-package rubocop)

(use-package ruby-end)

(use-package ruby-mode
  :config (setq ruby-insert-encoding-magic-comment nil))

(use-package rvm)

(use-package smartparens
  :defer t
  :init (smartparens-global-mode))

(use-package telephone-line
  :init (telephone-line-mode 1))

(use-package windmove
  :defer t
  :init (windmove-default-keybindings))

(provide 'packages)
;;; packages.el ends here
