;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package haskell-mode)

(use-package intero
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(provide 'haskell)
;;; haskell.el ends here
