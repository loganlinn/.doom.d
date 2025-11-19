;;; +javascript.el -*- lexical-binding: t; -*-

(defun +javascript/locate-package-json ()
  "Find the closest package.json file by traversing up the directory tree."
  (when-let (package-json (projectile-locate-dominating-file default-directory "package.json"))
    (expand-file-name package-json)))

(defun +javascript/find-file-in-package ()
  "Jump to a file in the current JavaScript package directory.
The package directory is determined by locating the nearest package.json file."
  (interactive)
  (if-let (package-dir (+javascript/locate-package-json))
      (doom-project-find-file package-dir)
    (user-error "No package.json found in parent directories")))

(defun +javascript/search-in-package ()
  "Conduct a text search in files under the current package."
  (interactive)
  (if-let (package-dir (+javascript/locate-package-json))
      (+default/search-cwd package-dir)
    (user-error "No package.json found in parent directories")))

(defun +javascript/package-dired ()
  (interactive)
  (dired (+javascript/locate-package-json)))

;; configure lsp-eslint because the self-install is unreliable
;; run: npm install -g vscode-langservers-extracted
(after! lsp-mode
  ;; (setq lsp-eslint-server-command '("px" "vscode-eslint-language-server" "--stdio"))
  )

(map! :after typescript-mode
      :map typescript-mode-map
      :localleader
      "SPC" #'+javascript/find-file-in-package
      "/" #'+javascript/search-in-package
      "f" #'+javascript/package-dired)

(map! :after lsp-eslint
      :map typescript-mode-map
      :localleader
      "e" #'lsp-eslint-apply-all-fixes)

(after! projectile
  ;; Recognize package.json as a project marker, but prefer git root in monorepos
  (add-to-list 'projectile-project-root-files "package.json")
  (add-to-list 'projectile-project-root-files "tsconfig.json"))

;; Configure LSP to detect project roots in monorepo subdirectories
(after! lsp-mode
  (setq lsp-auto-guess-root t)  ;; Attempt to guess the project root
  ;; You can also set a custom root directory detection function
  (setq lsp-before-initialize-hook
        (lambda ()
          (when (and buffer-file-name
                     (or (locate-dominating-file buffer-file-name "tsconfig.json")
                         (locate-dominating-file buffer-file-name "package.json")))
            (setq-local lsp-workspace-root
                        (or (locate-dominating-file buffer-file-name "tsconfig.json")
                            (locate-dominating-file buffer-file-name "package.json")))))))
