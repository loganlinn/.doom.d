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

;;;###autoload
(defun +lsp-clojure-nrepl-connect ()
  "Connect to the running nrepl debug server of clojure-lsp."
  (interactive)
  (let ((port (gethash "port" (lsp-request "clojure/serverInfo/raw" nil))))
    (when (not (or (numberp  port) (and (stringp port) (string-match "[0-9]+" port))))
      (user-error "Unable to determine clojure-lsp nREPL port. (port: %s)" port))
    (cider-connect-clj `(:host "localhost" :port ,port))))

;;;###autoload
(defun +lsp-clojure-workspace-restart-with-debug-cli ()
  (interactive)
  (when (not lsp-clojure-custom-server-command)
    (let* ((clojure-lsp-src-dir (expand-file-name "~/src/github.com/clojure-lsp/clojure-lsp"))
           (clojure-lsp-command (file-name-concat clojure-lsp-src-dir "clojure-lsp")))
      (when (not (file-executable-p clojure-lsp-command))
        (user-error "lsp-clojure-custom-server-command is not est and %s does not exist" clojure-lsp-command))
      (setq lsp-clojure-custom-server-command (list clojure-lsp-command))
      (print! "set lsp-clojure-custom-server-command: %s" lsp-clojure-custom-server-command)
      (print! "restarting LSP workspace...")
      (call-interactively #'lsp-workspace-restart))))

;; (defun +clojure-lsp/register-remote-client (clojure-lsp-command)
;;   (interactive (list (read-string "clojure-lsp command:" (executable-find "clojure-lsp"))))
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-tramp-connection clojure-lsp)
;;                     :major-modes '(clojure-mode)
;;                     :language-id "clojure"
;;                     :remote? t
;;                     :server-id 'clojure-lsp-remote)))

;;;###autoload
(defun +clojure/cursor-info ()
  (interactive)
  (lsp-request "clojure/cursorInfo/raw"
               (lsp-make-clojure-cursor-info-params
                :textDocument (lsp-make-text-document-identifier :uri (lsp--buffer-uri))
                :position (lsp-make-position :line (- (line-number-at-pos) 1)
                                             :character (current-column)))))

;;;###autoload
(defun +clojure/cursor-elements ()
  (interactive)
  (mapcar (lambda (el) (gethash "element" el)) (gethash "elements" (+clojure/cursor-info))))

;;;###autoload
(defun +clojure/cursor-definitions ()
  (interactive)
  (mapcar (lambda (el) (gethash "definition" el)) (gethash "elements" (+clojure/cursor-info))))

;;;###autoload
(defun +clojure/copy-reference ()
  "Yanks qualified reference for definition at cursor"
  (interactive)
  (let* ((definition (or (car (+clojure/cursor-definitions))
                         (user-error "Could find cursor definitions.")))
         (namespace (gethash "ns" definition))
         (name (or (gethash "name" definition)
                   (user-error "Definition does not have name")))
         (ref (if namespace (concat namespace "/" name) name)))
    (kill-new ref)
    (if (string= ref (car kill-ring))
        (message "Copied reference: %s" ref)
      (user-error "Couldn't copy reference in current buffer"))))

;;;###autoload
(defun +cider-doc-interactevely ()
  "Like `cider-doc', but ignores symbol at point. Useful for looking up something specific"
  (interactive)
  (cider-ensure-connected)
  (funcall (cider-prompt-for-symbol-function t)
           "Doc for"
           #'cider-doc-lookup)
  (cider-complete-at-point))
