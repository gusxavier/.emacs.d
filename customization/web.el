;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package web-mode
  :config
  (defun my-web-mode-hook ()
    "Hooks for web mode"
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-indentation nil)
    (setq web-mode-auto-close-style 2)
    (setq-default web-mode-comment-formats
                  '(("javascript" . "//"))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mjml\\'" . web-mode)))

;; better jsx syntax-highlighting in web-mode
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(provide 'web)
;;; web.el ends here
