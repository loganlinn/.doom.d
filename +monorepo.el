;;; Monorepo navigation helpers -*- lexical-binding: t; -*-

;; 1. Find file from git root (escape hatch)
;; Binds to SPC p G (mnemonic: Git root find file)
(defun +my/find-file-in-git-root ()
  "Find file starting from git repository root, ignoring workspace boundaries."
  (interactive)
  (let ((default-directory (or (vc-root-dir)
                               (locate-dominating-file default-directory ".git")
                               default-directory)))
    (projectile-find-file-in-directory default-directory)))

;; 2. Yank file path relative to git root
;; Replaces SPC f Y behavior
(defun +my/yank-buffer-path-from-git-root ()
  "Copy the current buffer's path relative to git root."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
            (git-root (or (vc-root-dir)
                          (locate-dominating-file filename ".git"))))
      (let ((relative-path (file-relative-name filename git-root)))
        (kill-new relative-path)
        (message "%s" relative-path))
    (error "Couldn't find git root or buffer has no file")))

;; Keybindings
(map! :leader
      (:prefix "p"
       :desc "Find file in git root" "f" #'+my/find-file-in-git-root
       :desc "Yank project relative path" "y" #'+default/yank-buffer-path-relative-to-project)
      (:prefix "f"
       :desc "Yank path from git root" "Y" #'+my/yank-buffer-path-from-git-root))
