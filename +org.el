;;; +org.el -*- lexical-binding: t; -*-

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

(after! org
  (setq org-directory (+org/ensure-trailing-slash (or (getenv "XDG_NOTES_DIR") "~/Notes/"))) ;; yes, trailing slash
  (setq org-display-remote-inline-images t)
  (add-to-list 'org-modules 'ol-man))

(after! org-capture
  (setq! org-capture-templates
         '(("t" "Personal TODO" entry (file+headline +org-capture-todo-file "Inbox")
            "* [ ] %?\n%i\n%a" :prepend t)

           ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox")
            "* %u %?\n%i\n%a" :prepend t)

           ("b" "Bookmark" entry
            (file+headline "bookmarks.org" "Inbox")
            "*** [[%^{URL}][%^{Title}]]\n:PROPERTIES:\n:URL: %\\1\n:ADDED: %U\n:TAGS: %^{Tags}\n:DESCRIPTION: %^{Description}\n:END:\n\n%?"
            :immediate-finish t)

           ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
            "* %U %?\n%i\n%a" :prepend t)

           ("p" "Templates for projects")

           ("pt" "Project-local TODO" entry
            (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%i\n%a"
            :prepend t)

           ("pn" "Project-local notes" entry
            (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%i\n%a"
            :prepend t)

           ;; ("pc" "Project-local changelog" entry
           ;;  (file+headline +org-capture-project-changelog-file "Unreleased")
           ;;  "* %U %?\n%i\n%a" :prepend t)

           ;; ("o" "Centralized templates for projects")

           ("ot" "Project TODO" entry #'+org-capture-central-project-todo-file
            "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)

           ("on" "Project notes" entry #'+org-capture-central-project-notes-file
            "* %U %?\n %i\n %a" :heading "Notes" :prepend t)

           ;; ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file
           ;;  "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
           )))
