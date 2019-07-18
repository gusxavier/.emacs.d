;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :hook
  (js-mode . lsp)
  (js2-mode . lsp)
  (rjsx-mode . lsp)
  :commands lsp
  :bind
  (("M-." . lsp-find-definition)
   ("M-," . lsp-find-implementation)
   ("M-ç" . lsp-find-references))
  :config
  (setq lsp-prefer-flymake nil))

(use-package company-lsp
  :after company)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package hydra)

(use-package lsp-ui
  ;; :config
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :commands lsp-ui-mode)

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide 'lsp)
;;; lsp.el ends here
