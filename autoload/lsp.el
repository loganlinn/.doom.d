;;; autoload/lsp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun loganlinn/lsp-dirs-to-watch (&optional root output)
  "Create recursive file notification watch in DIR.
CALLBACK will be called when there are changes in any of
the monitored files. WATCHES is a hash table directory->file
notification handle which contains all of the watch that
already have been created. Watches will not be created for
any directory that matches any regex in IGNORED-DIRECTORIES.
Watches will not be created for any file that matches any
regex in IGNORED-FILES."
  (interactive)
  (let* ((root (or root (doom-project-root)))
         (root (if (f-symlink? root)
                   (file-truename root)
                 root))
         (output (or output (file-name-concat root ".lsp/watched-directories.txt")))
         (ignored-things (lsp--get-ignored-regexes-for-workspace-root root))
         ;;(ignored-files (car ignored-things))
         (ignored-directories (cadr ignored-things)))
    (when (or (not (f-exists-p output))
              (and (y-or-n-p (format "Overwrite '%s'?" output))
                   (f-delete output)))
      (with-temp-file output
        (dolist (watchable-dir (lsp--all-watchable-directories root ignored-directories))
          (insert (concat watchable-dir "\n"))))
      (message "Wrote %s" output))))
