;;; +org.el -*- lexical-binding: t; -*-

(after! org
  (setq
   org-directory (+org/ensure-trailing-slash (or (getenv "XDG_NOTES_DIR") "~/Notes/")) ;; yes, trailing slash
   org-display-remote-inline-images t)
  (add-to-list 'org-modules 'ol-man))

(defun +org/ensure-trailing-slash (path)
  "Ensure PATH ends with a trailing slash."
  (if (and (stringp path) (not (string-empty-p path)))
      (if (string-suffix-p "/" path)
          path
        (concat path "/"))
    path))

(defun +org/roam-dailies-file-name (&optional time zone)
  "Find and open `org-roam-dailies-directory'."
  (expand-file-name (format-time-string "%Y-%m-%d.org" time zone)
                    (expand-file-name org-roam-dailies-directory org-roam-directory)))

(defun +org/capture-code-snippet ()
  "Capture a code snippet and its file link to the daily org note."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (file-name (file-name-nondirectory file-path))
         (line-number (line-number-at-pos))
         (org-link (format "[[file:%s::%d][%s:%d]]" file-path line-number file-name line-number))
         (selected-text (if (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          (thing-at-point 'line t))))
    (with-current-buffer (find-file-noselect (+org/roam-dailies-file-name))
      (goto-char (point-max))
      (insert (format "\n* Code Snippet from %s\n%s\n#+BEGIN_SRC %s\n%s#+END_SRC\n"
                      org-link
                      (format-time-string "  [%Y-%m-%d %H:%M]")
                      (file-name-extension file-name)
                      selected-text))
      (save-buffer))))
