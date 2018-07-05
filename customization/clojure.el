;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package cider
  :bind
  ("C-c M-b" . cider-repl-clear-buffer))
  ;; :config
  ;; (setq cider-lein-parameters "with-profile dev repl :headless"))

(use-package clj-refactor
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

(use-package clojure-mode)

(use-package flycheck-joker)

(use-package helm-cider
  :config
  (helm-cider-mode 1))

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode))

(defun cider-jack-in-with-profile (profile)
  "Ask for lein PROFILE before starting cider."
  (interactive "sSet cider lein profiles (default: dev): ")
  (setq-default cider-lein-parameters (format "with-profile %s repl :headless"
                                              (or profile "dev")))
  (cider-jack-in))

(provide 'clojure)
;;; clojure.el ends here
