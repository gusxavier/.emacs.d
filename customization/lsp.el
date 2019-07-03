;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package company-lsp)

(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil))

(use-package hydra)

(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide 'lsp)
;;; lsp.el ends here
