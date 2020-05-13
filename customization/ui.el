;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package doom-themes
  :config
  (load-theme 'doom-nord t)
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package nlinum
  :init
  (global-nlinum-mode +1))

;; Fullscreen
(toggle-frame-maximized)

;; Disable startup screen
(setq inhibit-startup-message t)

;; Do not open default buffers on startup
(setq initial-buffer-choice nil)

;; Don't show *Buffer list* when opening multiple files
;; at the same time
(setq inhibit-startup-buffer-menu t)

;; Show cursor position
(column-number-mode 1)

;; Font config
(defun fontify-frame (frame)
  "Define the font size based on FRAME resolution."
  (interactive)
  (if window-system
      (progn
        (set-frame-parameter frame 'font "Fira Code Retina 14"))))
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)

;; Remove scroll bar
(scroll-bar-mode -1)

;; Remove top bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Config whitespace
(global-whitespace-mode)
(setq-default whitespace-style '(face trailing tabs))

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

(provide 'ui)
;;; ui.el ends here
