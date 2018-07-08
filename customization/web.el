;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package web-mode
  :config
  (defun my-web-mode-hook ()
    "Hooks for web mode"
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-style-padding 4)
    (setq web-mode-script-padding 4)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-indentation nil)
    (setq web-mode-auto-close-style 2)
    (setq-default web-mode-comment-formats
                  '(("javascript" . "//"))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mjml\\'" . web-mode)))

(provide 'web)
;;; web.el ends here
