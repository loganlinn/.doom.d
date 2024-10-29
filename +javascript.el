;;; +javascript.el -*- lexical-binding: t; -*-

(defun +javascript/locate-package-json ()
  "Find the closest package.json file by traversing up the directory tree."
  (let ((package-json (projectile-locate-dominating-file default-directory "package.json")))
    (when package-json
      (expand-file-name package-json))))

(defun +javascript/find-file-in-package ()
  "Jump to a file in the current JavaScript package directory.
The package directory is determined by locating the nearest package.json file."
  (interactive)
  (let ((package-dir (+javascript/locate-package-json )))
    (if package-dir
        (projectile-find-file-in-directory package-dir)
      (user-error "No package.json found in parent directories"))))

;; configure lsp-eslint because the self-install is unreliable
;; run: npm install -g vscode-langservers-extracted
(when (modulep! +lsp)
  ;; TODO
  ;; (when-let ((exe (executable-find "vscode-eslint-language-server")))
  ;;   (setq lsp-eslint-server-command  '(,exe "--stdio")))
  (setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio")))

(map! (:after typescript-mode
       :map typescript-mode-map
       :localleader
       "SPC" #'+javascript/find-file-in-package)
      (:after lsp-eslint
       :map typescript-mode-map
       :localleader
       "e" #'lsp-eslint-apply-all-fixes))
