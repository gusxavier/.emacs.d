;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package enh-ruby-mode
  :bind ("TAB" . enh-ruby-indent-exp)
  :config
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (add-to-list 'auto-mode-alist
               '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package feature-mode)

(use-package inf-ruby)

(use-package ruby-end)

(use-package ruby-mode
  :config (setq ruby-insert-encoding-magic-comment nil))

(use-package rubocop)

(use-package rvm)

(use-package slim-mode)

(provide 'ruby)
;;; ruby.el ends here
