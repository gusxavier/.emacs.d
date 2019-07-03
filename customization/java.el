;;; Package --- summary

;;; Commentary:

;;; Code:

(use-package lsp-java
  :after lsp
  :config
  (add-hook 'java-mode-hook #'lsp)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (setq lsp-java-vmargs
        ;; Add lombok support
        (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              (concat "-javaagent:" "/home/gus/.m2/repository/org/projectlombok/lombok/1.16.18/lombok-1.16.18.jar")
              (concat "-Xbootclasspath/a:" "/home/gus/.m2/repository/org/projectlombok/lombok/1.16.18/lombok-1.16.18.jar"))
        lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules"
          ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
          "build")
        ;; Do not organize imports on save
        lsp-java-save-actions-organize-imports nil))

(provide 'java)
;;; java.el ends here
