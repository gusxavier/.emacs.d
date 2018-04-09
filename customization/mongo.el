;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package inf-mongo
  :bind
  ("C-c C-c" . mongo-send-buffer)
  :config
  (setq inf-mongo-command "mongo"))

(provide 'mongo)
;;; mongo.el ends here
