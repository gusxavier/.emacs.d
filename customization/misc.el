;;; package --- Summary

;;; Commentary:

;;; Code:

;; Set command as meta key in mac
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Store backup and autosave files in tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Indent with spaces only
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Enable windmove
(windmove-default-keybindings)

;; Reload buffers on disk change
(global-auto-revert-mode t)

(provide 'misc)
;;; misc.el ends here
