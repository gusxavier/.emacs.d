;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package dracula-theme
  :init (load-theme 'dracula t))

(use-package spaceline
  :config
  (spaceline-spacemacs-theme))

;; Fullscreen
(toggle-frame-maximized)

;; Enable line numbers
(global-linum-mode t)

;; Disable startup screen
(setq inhibit-startup-message t)

;; Font config
(defun fontify-frame (frame)
  "Define the font size based on FRAME resolution."
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 1900)
            (set-frame-parameter frame 'font "Hack 10")
          (set-frame-parameter frame 'font "Hack 14")))))
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

(provide 'ui)
;;; ui.el ends here
