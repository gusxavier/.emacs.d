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

(use-package rvm)

(use-package slim-mode)

(provide 'ruby)
;;; ruby.el ends here
