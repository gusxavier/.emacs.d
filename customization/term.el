;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package multi-term
  :config
  (setq-default multi-term-program "/bin/zsh"))

;; Use emacs term info instead of system term info
(setq system-uses-terminfo nil)

(defun rename-term (name)
  "Rename terminal buffer using the giving NAME."
  (interactive "sRename term buffer to: ")
  (rename-buffer (concat "*term* " name)))

;; Term-mode config
(add-hook 'term-mode-hook
          (lambda ()
            (yas-minor-mode -1)
            (setq-default term-buffer-maximum-size 10000)
            (setq-default show-trailing-whitespace nil)
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            (define-key term-raw-map (kbd "C-y") 'term-paste)))



;; Shell config
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'term)
;;; term.el ends here
