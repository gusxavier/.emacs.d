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

(use-package py-isort
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --pprint")

(provide 'python)
;;; python.el ends here
