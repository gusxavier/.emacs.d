;;; package --- Summary

;;; Commentary:

;;; Code:

;; Current theme
(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

;; Highlight current line on big jumps
(use-package beacon
  :init
  (beacon-mode +1))

;; Disable startup screen
(setq inhibit-startup-message t)

;; Do not open default buffers on startup
(setq initial-buffer-choice nil)

;; Don't show *Buffer list* when opening multiple files
;; at the same time
(setq inhibit-startup-buffer-menu t)

;; Show cursor position
(line-number-mode t)
(column-number-mode t)

;; Font config
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono")
  (set-face-attribute 'default nil :height 120))

;; Remove scroll bar
(scroll-bar-mode -1)

;; Remove top bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Remove blinking cursor
(blink-cursor-mode -1)

;; Highlight parens
(show-paren-mode t)

;; Highlight current line
(global-hl-line-mode t)

(provide 'ui)
;;; ui.el ends here
