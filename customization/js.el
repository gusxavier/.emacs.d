;;; Package --- summary

;;; Commentary:

;;; Code:

;; (use-package js2-mode
;;   :config
;;   (setq js2-basic-offset 2)
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;   (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

;; (use-package js2-refactor
;;   :config
;;   (add-hook 'js2-mode-hook #'js2-refactor-mode)
;;   (js2r-add-keybindings-with-prefix "C-c C-r")
;;   (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;;   (define-key js-mode-map (kbd "M-.") nil))

;; (use-package rjsx-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

;; (use-package json-mode)

;; ;; use local eslint from node_modules before global
;; ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package tide
  :after (company flycheck)
  :bind (("M-?" . tide-references))
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)

         (javascript-mode . tide-setup)
         (javascript-mode . tide-hl-identifier-mode)

         (js-jsx-mode . tide-setup)
         (js-jsx-mode . tide-hl-identifier-mode)

         (js-mode . tide-setup)
         (js-mode . tide-hl-identifier-mode))
  :config
  (setq-default js-jsx-indent-level 2
                js-indent-level 2
                typescript-indent-level 2))

(provide 'js)
;;; js.el ends here
