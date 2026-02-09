;;; lyl/ai/config.el -*- lexical-binding: t; -*-

(after! chatgpt-shell
  (setq chatgpt-shell-models
        (append chatgpt-shell-models
                (list
                 (chatgpt-shell-openrouter-make-model
                  :version "z-ai/glm-4.5"
                  :short-version "glm-4.5"
                  :label "GLM-4.5"
                  :token-width 4
                  :context-window 131072
                  :other-params '((provider (require_parameters . t))))
                 (chatgpt-shell-openrouter-make-model
                  :version "z-ai/glm-4.7"
                  :short-version "glm-4.7"
                  :label "GLM-4.7"
                  :token-width 4
                  :context-window 202752
                  :other-params '((provider (require_parameters . t))))
                 (chatgpt-shell-openrouter-make-model
                  :label "Qwen3 Coder"
                  :version "qwen/qwen3-coder"
                  :short-version "qwen3-coder"
                  :token-width 4
                  :context-window 256000)
                 (chatgpt-shell-openrouter-make-model
                  :label "Qwen3 Coder Next"
                  :version "qwen/qwen3-coder-next"
                  :short-version "qwen3-coder-next"
                  :token-width 4
                  :context-window 262144))))
  (setq chatgpt-shell-model-version "z-ai/glm-4.5")
  (setq chatgpt-shell-always-create-new nil))

;; (after! dall-e-shell
;;   (setq dall-e-shell-model-version "dall-e-3"))

(defvar llm-refactoring-provider nil)
(after! llm
  (require 'llm-openai)
  (setq llm-refactoring-provider
        (make-llm-openai-compatible
         :url "https://openrouter.ai/api/v1/"
         :key (getenv "OPENROUTER_API_KEY")
         :chat-model "z-ai/glm-4.5"
         :default-chat-non-standard-params
         `((http-referer . "https://github.com/ahyatt/llm")
           (x-title . "Emacs LLM")))
        magit-gptcommit-llm-provider llm-refactoring-provider
        llm-warn-on-nonfree nil))

(after! gptel
  (setq gptel-model 'z-ai/glm-4.5
        gptel-backend (gptel-make-openai "OpenRouter"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream t
                        :key (getenv "OPENROUTER_API_KEY")
                        :models '(z-ai/glm-4.5
                                  qwen/qwen3-coder))))

(after! minuet
  (setq minuet-request-timeout 2.5)
  (setq minuet-auto-suggestion-throttle-delay 1.5) ;; Increase to reduce costs and avoid rate limits
  (setq minuet-auto-suggestion-debounce-delay 0.6) ;; Increase to reduce costs and avoid rate limits

  (setq minuet-provider 'openai-compatible)
  (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
  (plist-put minuet-openai-compatible-options :api-key "OPENROUTER_API_KEY")
  (plist-put minuet-openai-compatible-options :model "moonshotai/kimi-k2")
  ;; Prioritize throughput for faster completion
  (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 56)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))

(defadvice! lyl-magit-gptcommit-save-buffer-a ()
  :after #'magit-gptcommit-commit-accept
  (when-let ((buf (magit-commit-message-buffer)))
    (with-current-buffer buf (save-buffer))))

(use-package! magit-gptcommit
  :after magit
  :init
  :config
  (magit-gptcommit-mode 1)
  (magit-gptcommit-status-buffer-setup))

(after! git-commit
  (map! :map git-commit-mode-map
        "C-c C-g" #'magit-gptcommit-commit-accept))

(use-package! copilot
  :when (and (executable-find "node")
             (modulep! +copilot))
  :defer t :init
  (add-hook 'text-mode-hook #'lyl-copilot-turn-on-safely)
  (add-hook 'prog-mode-hook #'lyl-copilot-turn-on-safely)
  (add-hook 'conf-mode-hook #'lyl-copilot-turn-on-safely)
  (advice-add #'copilot--start-agent :around
              (defun lyl-quiet-coilot-start-agent-a (fn &rest args)
                (quiet!! (apply fn args))))
  (add-hook! 'copilot-disable-predicates
    (defun lyl-disable-copilot-in-gptel-p ()
      (bound-and-true-p gptel-mode))
    (defun lyl-disable-copilot-in-dunnet-p ()
      (derived-mode-p 'dun-mode))
    (defun lyl-multiple-cursors-active-p ()
      (bound-and-true-p multiple-cursors-mode))
    (defun lyl-disable-copilot-in-minibuffer ()
      (minibufferp)))
  (setq copilot-install-dir (concat doom-cache-dir "copilot"))

  (autoload 'copilot-clear-overlay "copilot" nil t)

  (defadvice! lyl-clear-copilot-overlay-a (&rest _)
    :before #'doom/delete-backward-word
    (copilot-clear-overlay))

  :config
  (setq copilot-indent-offset-warning-disable t)

  (map! :map copilot-completion-map
        "<tab>"    #'copilot-accept-completion
        "TAB"      #'copilot-accept-completion
        "C-TAB"    #'copilot-accept-completion-by-word
        "C-<tab>"  #'copilot-accept-completion-by-word
        :ig "C-n"      #'copilot-next-completion
        :ig "C-p"      #'copilot-previous-completion
        :ig "C-f"      #'copilot-accept-completion-by-word
        :ig "C-e"      #'copilot-accept-completion-by-line)

  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(go-mode 4))
  (add-to-list 'copilot-indentation-alist '(javascript-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(typescript-mode 2))

  ;; Assume all Elisp code is formatted with the default indentation style. This
  ;; fixes an error.
  (setf (alist-get 'emacs-lisp-mode copilot-indentation-alist) nil)

  (add-to-list 'copilot-clear-overlay-ignore-commands #'corfu-quit)

  (add-hook! 'doom-escape-hook
    (defun lyl-copilot-clear-overlay-h ()
      "Like `copilot-clear-overlay', but returns `t' if the overlay was visible."
      (when (copilot--overlay-visible)
        (copilot-clear-overlay) t)))

  (setq copilot--base-dir
        (expand-file-name ".local/straight/repos/copilot.el/" doom-emacs-dir)
        copilot-max-char 1000000
        copilot-idle-delay 0)

  (remove-hook 'copilot-enable-predicates 'evil-insert-state-p)
  (add-hook! 'copilot-enable-predicates
    (defun lyl-evil-insert-state-p ()
      (memq (bound-and-true-p evil-state) '(insert emacs nil))))
  (when (modulep! +copilot)
    (after! copilot
      (add-hook 'yas-before-expand-snippet-hook #'copilot-clear-overlay)))
  (after! copilot-balancer
    (add-to-list 'copilot-balancer-lisp-modes 'fennel-mode)
    (after! midnight
      (add-to-list 'clean-buffer-list-kill-never-buffer-names
                   (buffer-name copilot-balancer-debug-buffer)))))

(use-package! minuet
  :when (modulep! -copilot)
  :defer t :init
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  (add-hook 'text-mode-hook #'minuet-auto-suggestion-mode)
  (add-hook 'conf-mode-hook #'minuet-auto-suggestion-mode)
  :config
  (map! (:map minuet-active-mode-map
         :ig "M-y"  #'minuet-complete-with-minibuffer ;; use minibuffer for completion
         :ig "M-i"  #'minuet-show-suggestion ;; use overlay for completion
         :ig "C-c m"  #'minuet-configure-provider)
        (:map minuet-active-mode-map
         ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
         :ig "M-p"  #'minuet-previous-suggestion ;; invoke completion or cycle to next completion
         :ig "M-n"  #'minuet-next-suggestion ;; invoke completion or cycle to previous completion
         :ig "C-f"  #'minuet-accept-suggestion ;; accept whole completion
         ;; Accept the first line of completion, or N lines with a numeric-prefix:
         ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
         :ig "C-e"  #'minuet-accept-suggestion-line
         :ig "M-e"  #'minuet-dismiss-suggestion
         :ig "M-p" #'minuet-previous-suggestion ;; invoke completion or cycle to next completion
         :ig "M-n" #'minuet-next-suggestion ;; invoke completion or cycle to previous completion
         ))

  (add-hook! 'doom-escape-hook :depth -1
    (defun lyl-minuet-dismiss-suggestion-h ()
      (when minuet--current-overlay
        (minuet-dismiss-suggestion)
        t)))

  (add-hook 'evil-insert-state-exit-hook #'minuet-dismiss-suggestion)

  (when (modulep! :editor evil)
    (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)))

;; (use-package! dall-e-shell
;;   :defer t :init
;;   ;;(map! :leader
;;   ;;      :prefix "o"
;;   ;;      :desc "Open DALL-E here" "I" #'dall-e-shell
;;   ;;      :desc "Toggle DALL-E popup" "i" #'lyl-ai-toggle-dall-e-shell
;;   ;;      :desc "Open DALL-E workspace" "C-i" #'lyl-ai-open-dall-e-workspace)
;;   :config
;;   (setq dall-e-shell-display-function #'switch-to-buffer
;;         dall-e-shell-openai-key (getenv "OPENAI_API_KEY")
;;         dall-e-shell-image-quality "hd"
;;         dall-e-shell-image-size "1024x1792"
;;         dall-e-shell-request-timeout 180))

(use-package! gptel
  :defer t :init
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (autoload 'gptel--stream-convert-markdown->org "gptel-org")
  :config
  (setq gpt-openai-key (getenv "OPENAI_API_KEY"))
  (setq gptel-default-mode 'org-mode)
  (after! gptel-context
    (map! :map gptel-context-buffer-mode-map
          :n "q" #'gptel-context-quit
          :n "n" #'gptel-context-next
          :n "p" #'gptel-context-previous
          :n "d" #'gptel-context-flag-deletion
          :n "RET" #'gptel-context-visit
          :map gptel-mode-map
          :n "<return>" #'gptel-send))
  (add-hook! 'gptel-mode-hook
    (defun lyl-gptel-mode-setup-h ()
      (setq-local nobreak-char-display nil)
      (auto-fill-mode -1)
      (doom-mark-buffer-as-real-h))))

(use-package! forge-llm
  :defer t :after forge :config
  (forge-llm-setup))
