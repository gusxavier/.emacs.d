;;; package --- Summary

;;; Commentary:

;;; Code:

;; (use-package nofrils-acme-theme
;;   :init
;;   (load-theme 'nofrils-acme t))

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t)
  :config
  ;; use variable-pitch fonts for some headings and titles
  (setq zenburn-use-variable-pitch t)
  ;; scale headings in org-mode
  (setq zenburn-scale-org-headlines t)
  ;; scale headings in outline-mode
  (setq zenburn-scale-outline-headlines t))

;; (use-package spaceline
;;   :config
;;   (spaceline-emacs-theme))

;; Fullscreen
(toggle-frame-maximized)

;; Enable line numbers
(global-linum-mode t)

;; Disable startup screen
(setq inhibit-startup-message t)

;; Truncate lines by default
(setq-default truncate-lines t)

;; Show cursor position
(column-number-mode 1)

;; Font config
(defun fontify-frame (frame)
  "Define the font size based on FRAME resolution."
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 1900)
            (set-frame-parameter frame 'font "Fira Code Retina 12")
          (set-frame-parameter frame 'font "Fira Code Retina 10")))))
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
