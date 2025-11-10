;;; +llm.el -*- lexical-binding: t; -*-

;; Moved to +local.el
;; (after! gptel
;;   (gptel-make-anthropic "Claude"
;;     :stream t
;;     :key gptel-api-key))

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

;; (use-package! claude-code-ide
;;   :defer t
;;   :init
;;   (setq claude-code-ide-cli-path "/Users/logan/.claude/local/claude")
;;   :config
;;   (claude-code-ide-emacs-tools-setup))
;;
;; (map! :when (modulep! :term vterm)
;;       :map vterm-mode-map
;;       "C-c C-c" #'claude-code-ide-send-escape)
;;
;; (map! :leader
;;       :prefix "o"
;;       (:prefix-map ("c" . "Claude Code IDE")
;;        :desc "Menu" "." #'claude-code-ide-menu
;;        :desc "Debug Menu" "d" #'claude-code-ide-debug-menu
;;        :desc "Toggle debug mode" "D" #'claude-code-ide-toggle-debug-mode
;;        :desc "Config Menu" "," #'claude-code-ide-config
;;        :desc "Prompt" "p" #'claude-code-ide-send-prompt
;;        :desc "Open" "C" #'claude-code-ide
;;        :desc "List sessions" "l" #'claude-code-ide-list-sessions
;;        :desc "Toggle" "C" #'claude-code-ide-toggle
;;        :desc "Resume" "r" #'claude-code-ide-resume
;;        :desc "Continue" "'" #'claude-code-ide-continue
;;        :desc "Switch to buffer" "b" #'claude-code-ide-switch-to-buffer
;;        :desc "Stop" "q" #'claude-code-ide-stop
;;        :desc "Insert at mentioned" "i" #'claude-code-ide-insert-at-mentioned))
