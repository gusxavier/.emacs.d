;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package all-the-icons)

(use-package better-defaults)

(use-package company
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq-default company-dabbrev-downcase nil)
  :init
  (global-company-mode))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package helm
  :defer t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)))

(use-package helm-ag
  :config
  (setq helm-ag-use-grep-ignore-list '("\\node_modules\\'"))
  :defer t)

(use-package helm-projectile
  :defer t
  :init (helm-projectile-on))

(use-package magit)

(use-package neotree
  :defer t
  :bind ("C-c p n" . neotree)
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package projectile
  :defer t
  :config
  (projectile-discover-projects-in-directory "~/workspace")
  ;; Workaround to avoid projectile making the editor very slow
  ;; https://github.com/bbatsov/projectile/issues/1183#issuecomment-335569547
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  :init (projectile-mode))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :init (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package restclient)

(use-package smartparens
  :defer t
  :init (smartparens-global-mode))

(use-package undo-tree
  :init (global-undo-tree-mode))

;; Set command as meta key in mac
(setq-default mac-option-key-is-meta nil
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

;; Enable windmove
(windmove-default-keybindings)

;; Reload buffers on disk change
(global-auto-revert-mode t)

;; Scroll settings
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(provide 'misc)
;;; misc.el ends here
