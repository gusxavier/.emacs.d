;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package all-the-icons)

(use-package avy
  :bind
  (("M-s" . avy-goto-word-1)))

(use-package better-defaults)

(use-package company
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq-default company-dabbrev-downcase nil)
  (defvar company-backends)
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-files))

  :init
  (global-company-mode))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint
                          json-jsinlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

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
  :bind
  ("C-c p n" . neotree)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq-default neo-show-hidden-files t)
  (setq neo-window-fixed-size nil))

(use-package projectile
  :defer t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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

(use-package which-key
  :init
  (which-key-mode))

;; Set command as meta key in mac
(setq-default mac-option-key-is-meta nil
              mac-command-key-is-meta t
              mac-command-modifier 'meta
              mac-option-modifier 'none)

;; At last some piece and quiet
(setq ring-bell-function 'ignore)

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

(setq explicit-shell-file-name "/bin/bash")

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun set-exec-path-from-shell-PATH ()
  "Set PATH env variable from SHELL."
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq-default eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(provide 'misc)
;;; misc.el ends here
