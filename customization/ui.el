;;; package --- Summary

;;; Commentary:

;;; Code:

;; (use-package zenburn-theme
;;   :init
;;   (load-theme 'zenburn t)
;;   :config
;;   ;; use variable-pitch fonts for some headings and titles
;;   (setq zenburn-use-variable-pitch t)
;;   ;; scale headings in org-mode
;;   (setq zenburn-scale-org-headlines t)
;;   ;; scale headings in outline-mode
;;   (setq zenburn-scale-outline-headlines t))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

(use-package centaur-tabs
  :demand
  :init (setq centaur-tabs-set-bar 'over)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-icons t)
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("C-x <C-left>" . centaur-tabs-backward)
  ("C-x <C-right>" . centaur-tabs-forward))

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

;; Show cursor position
(column-number-mode 1)

;; Font config
(defun fontify-frame (frame)
  "Define the font size based on FRAME resolution."
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 1900)
            (set-frame-parameter frame 'font "Fira Code Medium 12")
          (set-frame-parameter frame 'font "Fira Code Medium 14")))))
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
