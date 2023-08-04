;;; autoload/clojure.el -*- lexical-binding: t; -*-
;;;###if (modulep! :lang clojure)

;;;###autoload
(defun +cider/link-session (&optional context-type)
  "Link the current CONTEXT-TYPE to an existing CIDER session.
If only one session exists, it will automatically chosen.
CONTEXT-TYPE may be one of 'buffer, 'project, 'directory, or nil to
use the least-specific context, which is typically project."
  (interactive "P")
  (let* ((session (car (sesman-sessions 'CIDER))))
    (message "%s" session)
    (cond
     ((eq context-type 'buffer) (sesman-link-with-buffer (current-buffer) session))
     ((eq context-type 'directory) (sesman-link-with-directory default-directory session))
     ((eq context-type 'project) (sesman-link-with-project nil session))
     (t (sesman-link-with-project nil session)))))

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
(defun +polylith/projectile-root-dir (dir)
  "Identify a project root in DIR by top-downsearch for Polylith workspace.edn
in dir. Return the first (topmost) matched directory or nil if not found."
  (locate-dominating-file dir "workspace.edn"))

;;;###autoload
(defun +clojure/cider-jack-in-polylith (params)
  "Start an nREPL server for the current Polylith workspace and connect to it."
  (interactive "P")
  (let ((ws-dir (+polylith/projectile-root-dir (pwd))))
    (if ws-dir
        (progn
          (message "Starting nREPL server from `%s'" ws-dir)
          (cider-jack-in-clj (plist-put params :project-dir ws-dir)))
      (error "Unable to locate 'workspace.edn' in current directory or parent directory"))))

;;;###autoload
(defun +cider/open-shortcuts ()
  "Opens shortcut menu, from any buffer"
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-repl-handle-shortcut))

;;;###autoload
(defun +clojure-some-thread-first-all (but-last)
  "Fully thread the form at point using some->.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'."
  (interactive "P")
  (clojure--thread-all "some-> " but-last))

;;;###autoload
(defun +clojure-cond-thread-first-all (but-last)
  "Fully thread the form at point using cond->.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'."
  (interactive "P")
  (clojure--thread-all "cond-> " but-last))

;;;###autoload
(defun clojure-some-thread-last-all (but-last)
  "Fully thread the form at point using some->>.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'."
  (interactive "P")
  (clojure--thread-all "some->> " but-last))

;;;###autoload
(defun clojure-cond-thread-last-all (but-last)
  "Fully thread the form at point using cond->>.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `clojure-thread-all-but-last'."
  (interactive "P")
  (clojure--thread-all "cond->> " but-last))

;; (defun +clojure-cycle-thread ()
;;   (interactive)
;;   (save-excursion
;;     (ignore-errors (forward-char 7))
;;     (search-backward-regexp "(\\(some\\|cond\\)?->")
;;     (if (match-string 1)
;;         (replace-match "" nil nil nil 1)
;;       (goto-char (match-end 0))
;;       (insert (if (or clojure-use-metadata-for-privacy
;;                       (equal (match-string 0) "(def"))
;;                   " ^:private"
;;                 "-"))))
;;   (ignore-errors
;;     (when (looking-at "(")
;;       (forward-char 1)
;;       (forward-sexp 1)))
;;   (search-backward-regexp "([^-]*->")
;;   (down-list)
;;   (when (clojure--threadable-p)
;;     (prog1 (cond
;;             ((looking-at "[^-]*->\\_>")  (clojure--thread-first))
;;             ((looking-at "[^-]*->>\\_>") (clojure--thread-last)))
;;       (clojure--fix-sexp-whitespace 'move-out)))
;;   )

;;;###autoload
(defun +clojure-lsp/register-remote-client (clojure-lsp-executable)
  (interactive)
  (let ((clojure-lsp (or (executable-find "clojure-lsp")
                         (user-error "Couldn't find clojure-lsp installed on your system"))))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection clojure-lsp)
                      :major-modes '(clojure-mode)
                      ;;:language-id "clojure"
                      :remote? t
                      :server-id 'clojure-lsp-remote))))
