;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package all-the-icons)

(use-package better-defaults)

(use-package company
  :hook (prog-mode . company-mode)
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
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :config
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

(use-package page-break-lines
  :config
  (turn-on-page-break-lines-mode))

(use-package paredit
  :after clojure-mode
  :hook
  (clojure-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode))

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

;; Do not create backup files
(setq make-backup-files nil)

;; Set home as default directory
(setq default-directory "~/")

;; Do not confirm when killing process
(setq confirm-kill-processes nil)

;; Remove mouse highlight
(setq mouse-highlight nil)

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

;; Remove scratch buffer
(setq initial-scratch-message "")

(defun remove-scratch-buffer ()
  "Remove *scratch* from buffer after the mode has been set."
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Just type y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

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

;; Start emacsclient server
(server-start)

(provide 'misc)
;;; misc.el ends here
