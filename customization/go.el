;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package company-go
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode))))

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-guru)

(use-package go-projectile)

(provide 'go)
;;; go.el ends here
