;;; +org.el -*- lexical-binding: t; -*-

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
    (with-current-buffer (find-file-noselect (format-time-string "~/org/daily/%Y-%m-%d.org"))
      (goto-char (point-max))
      (insert (format "\n* Code Snippet from %s\n%s\n#+BEGIN_SRC %s\n%s#+END_SRC\n"
                      org-link
                      (format-time-string "  [%Y-%m-%d %H:%M]")
                      (file-name-extension file-name)
                      selected-text))
      (save-buffer))))
