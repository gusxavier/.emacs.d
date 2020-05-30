;;; package --- Summary

;;; Commentary:

;;; Code:

;; Use ag inside emacs
(use-package ag)

;; Code auto complete
(use-package company
  :diminish
  :init
  (global-company-mode 1)
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-tooltip-limit 10
        company-idle-delay 0.5
        company-show-numbers t
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  (setq-default company-dabbrev-downcase nil))

(use-package counsel
  :diminish
  :init
  (counsel-mode t))

(use-package counsel-projectile
  :after projectile
  :init
  (counsel-projectile-mode t))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flx
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

(use-package ivy
  :diminish
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  ;; Use enter to navigate instead opening dired
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done))

(use-package magit)

(use-package page-break-lines
  :config
  (page-break-lines-mode))

(use-package projectile
  :diminish
  :init
  (projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-discover-projects-in-directory "~/workspace")
  ;; Workaround to avoid projectile making the editor very slow
  ;; https://github.com/bbatsov/projectile/issues/1183#issuecomment-335569547
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package restclient)

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))

(use-package which-key
  :init
  (which-key-mode +1))

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

;; Better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil)

;; Config whitespace
(global-whitespace-mode)
(setq-default whitespace-style '(face trailing tabs))

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Just type y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Avoid writing package-selected-packages on init.el
(defun package--save-selected-packages (&rest opt) nil)

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

(provide 'misc)
;;; misc.el ends here
