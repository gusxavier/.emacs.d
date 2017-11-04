;;; package --- Summary

;;; Commentary:

;;; Code:

;; Fullscreen
(toggle-frame-maximized)

;; Enable line numbers
(global-linum-mode t)

;; Highlight current line
(global-hl-line-mode 1)

;; Disable startup screen
(setq inhibit-startup-message t)

;; Font config
(defun fontify-frame (frame)
  "Define the font size based on FRAME resolution."
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 1900)
            (set-frame-parameter frame 'font "Hack 9")
          (set-frame-parameter frame 'font "Hack 12")))))
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)

;; Remove scroll bar
(scroll-bar-mode -1)

;; Remove top bar
(menu-bar-mode -1)
(tool-bar-mode -1)

(provide 'ui)
;;; ui.el ends here
