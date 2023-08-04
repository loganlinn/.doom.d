;;; autoload/magit.el -*- lexical-binding: t; -*-

;;;###autoload
(defun magit--default-clone-directory-handler (url)
  "Given url like git@github.com/owner/repo.git, returns path like ~/src/github.com/owner/"
  (require 'f)
  (require 'url-parse)
  (let* ((parsed-url (url-generic-parse-url url))
         (host (url-host parsed-url))
         (path (car (url-path-and-query parsed-url)))
         (dir (f-slash (f-dirname (f-no-ext path)))))
    (concat "~/src/" host dir)))
