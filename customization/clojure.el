;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package cider
  :bind
  ("C-." . cider-browse-spec)
  :config
  (setq cider-prompt-for-symbol nil))

(use-package clojure-mode
  :config
  (setq clojure-align-forms-automatically t)
  (require 'flycheck-clj-kondo))

(use-package clj-refactor
  :hook
  (clojure-mode . clj-refactor-mode)
  :config
  (setq cljr-warn-on-eval nil)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package flycheck-clj-kondo)

(use-package smartparens
  :config
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  :init
  (smartparens-global-strict-mode +1))

;; Configure indentation for specific macros
(define-clojure-indent
  (fact 1)
  (facts 1))

(provide 'clojure)
;;; clojure.el ends here
