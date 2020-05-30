;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package cider
  :config
  (setq cider-prompt-for-symbol nil))

(use-package clojure-mode
  :config
  (setq clojure-align-forms-automatically t)
  (require 'flycheck-clj-kondo))

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
