;;; Package --- summary

;;; Commentary:

;;; Code:
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(provide 'docker)
;;; docker.el ends here
