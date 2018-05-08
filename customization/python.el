;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :config
  (defvar company-backends)
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

(use-package elpy
  :init
  (elpy-enable))

(use-package pyenv-mode
  :config
  (pyenv-mode))

(use-package pyenv-mode-auto)

(provide 'python)
;;; python.el ends here
