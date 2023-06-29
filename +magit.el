;;; :tools magit

(defun magit--default-clone-directory-handler (url)
  "Given url like git@github.com/owner/repo.git, returns path like ~/src/github.com/owner/"
  (let* ((parsed-url (url-generic-parse-url url))
         (path (car (url-path-and-query parsed-url))))
    (concat "~/src/" (url-host parsed-url) (f-dirname (file-name-sans-extension path)) "/")))

(after! magit
  (setq magit-diff-refine-hunk 'all
        magit-repository-directories '(("~/src" . 3))
        magit-clone-default-directory #'magit--default-clone-directory-handler
        magit-save-repository-buffers nil
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        transient-values '((magit-rebase "--autosquash" "--autostash")
                           (magit-pull "--rebase" "--autostash")
                           (magit-revert "--autostash")))

  ;; automaticailly refresh magit buffers when files are saved
  (add-hook! 'after-save-hook #'magit-after-save-refresh-status))

(after! forge
  (setq  forge-topic-list-limit '(100 . -10)
         forge-owned-accounts '(("loganlinn"
                                 "patch-tech"
                                 "plumatic"
                                 "omcljs"))))

;; (use-package! magit-delta
;;   :hook (magit-mode . magit-delta-mode))

(use-package! code-review
  ;; ;; https://github.com/wandersoncferreira/code-review#configuration
  ;; (add-hook! 'code-review-mode-hook
  ;;   (lambda ()
  ;;     ;; include *Code-Review* buffer into current workspace
  ;;     (persp-add-buffer (current-buffer))))
  )
