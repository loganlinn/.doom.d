;;; autoload/clojure.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +clojure/link-cider-session ()
    "Link the current buffer to a running CIDER session."
    (interactive)
    (setq sesman-system 'CIDER)
    (sesman-link-with-buffer))

;;;###autoload
(defun +clojure/clerk-serve ()
  (interactive)
  (cider-interactive-eval
   "((requiring-resolve 'nextjournal.clerk/serve!) {:browse? true})"))

;;;###autoload
(defun +clojure/clerk-show ()
  (interactive)
  (when-let ((filename (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "((requiring-resolve 'nextjournal.clerk/show!) \"" filename "\")"))))

;;;###autoload
(defun +clojure/cider-jack-in-polylith (params)
  "Start an nREPL server for the current Polylith workspace and connect to it."
  (interactive "P")
  (if-let ((ws-dir (locate-dominating-file (pwd) "workspace.edn")))
      (progn
        (message "Starting nREPL server from `%s'" ws-dir)
        (cider-jack-in-clj (plist-put params :project-dir ws-dir)))
    (error "Unable to locate 'workspace.edn' in current directory or parent directory")))
