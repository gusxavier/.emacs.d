;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package meghanada
  :config
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  (add-hook 'java-mode-hook
            (lambda ()
              ;; meghanada-mode on
              (meghanada-mode t)
              (flycheck-mode +1)
              (setq c-basic-offset 2)))
  (cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn"))))

(provide 'java)
;;; java.el ends here
