;;; +copilot.el -*- lexical-binding: t; -*-

(use-package! copilot
  :defer t
  :hook (prog-mode . copilot-mode)
  :init
  (setq copilot-indent-offset-warning-disable t)

  :config
;; (defun copilot-install-server ()
;;   "Interactively install server."
;;   (interactive)
;;   (if-let* ((npm-binary (executable-find "npm")))
;;            (progn
;;              (make-directory copilot-install-dir 'parents)
;;              (copilot-async-start-process
;;                nil nil
;;                npm-binary
;;                "-g" "--prefix" copilot-install-dir
;;                "install" (concat copilot-server-package-name
;;                                  (when copilot-version (format "@%s" copilot-version)))))
;;            (copilot--log 'warning "Unable to install %s via `npm' because it is not present" copilot-server-package-name)
;;            nil))

  (map! :map copilot-completion-map
        "<tab>"  #'copilot-accept-completion
        "TAB"  #'copilot-accept-completion
        "C-TAB"  #'copilot-accept-completion-by-word
        "C-<tab>"  #'copilot-accept-completion-by-word
        "C-n"  #'copilot-next-completion
        "C-p"  #'copilot-previous-completion)
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(go-mode 4))
  (add-to-list 'copilot-indentation-alist '(javascript-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(typescript-mode 2)))

(use-package! copilot-chat :defer t)
