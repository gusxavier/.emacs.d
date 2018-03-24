;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package elpy
  :init
  (elpy-enable))


(use-package pyenv-mode
  :config
  (pyenv-mode))

(use-package pyenv-mode-auto)

(use-package ruby-end)

(use-package ruby-mode
  :config (setq ruby-insert-encoding-magic-comment nil))

(use-package rubocop)

(provide 'python)
;;; python.el ends here
