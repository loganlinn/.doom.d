;;; +kubernetes.el -*- lexical-binding: t; -*-

(use-package! kubel
  :commands
  kubel
  kubel-quick-edit
  kubel-mode
  kubel-yaml-editing-mode
  kubel-get-resource-details
  kubel-set-kubectl-config-file
  kubel-set-context
  kubel-set-namespace
  kubel-set-resource
  kubel-copy-popup
  kubel-delete-popup
  kubel-exec-popup
  kubel-log-popup
  kubel-port-forward-pod
  :config

  (set-popup-rules!
    '(("^\\*kubel stderr" :size 0.25 :select nil :quit t :ttl 10)
      ("^\\*kubel-process" :size 0.3 :select nil :quit 'current)
      ("^\\*kubel - port-forward" :size 0.3 :select nil :quit 'current :ttl 0)
      ("\\*kubel-drawer\\*" :slot 1 :side right :size 0.5 :select t :quit nil)
      ("^\\*kubel " :ignore t)))

  (defun +kubel/show ()
       (interactive)
       (with-current-buffer (get-buffer-create " *kubel-drawer*")
         (kubel-mode)
         (+popup-buffer-mode 1)
         (pop-to-buffer-same-window (current-buffer))))

  (map! (:leader (:prefix-map ("o" . "open") "k" #'kubel))
        (:localleader
         (:map kubel-mode-map
          :desc "Help Popup" "'" #'kubel-evil-help-popup
          :desc "Port forward" "p" #'kubel-port-forward-pod
          :desc "Logs" "l" #'kubel-log-popup
          :desc "Exec" "e" #'kubel-exec-popup
          :desc "Jab" "j" #'kubel-jab-deployment
          :desc "Scale replicas" "S" #'kubel-scale-replicas
          :desc "Copy to clipboad..." "c" #'kubel-copy-popup
          :desc "Show Process buffer" "$" #'kubel-show-process-buffer
          (:prefix ("a" . "action")
           :desc "Resource details" "RET" #'kubel-describe-popup
           :desc "Quick edit" "E" #'kubel-quick-edit
           :desc "Refresh" "g" #'kubel
           :desc "Delete" "k" #'kubel-delete-popup
           :desc "Rollout" "r" #'kubel-rollout-history)
          (:prefix ("s" . "settings")
           :desc "Set context" "C" #'kubel-set-context
           :desc "Set namespace" "n" #'kubel-set-namespace
           :desc "Set resource" "R" #'kubel-set-resource
           :desc "Set kubectl config file" "K" #'kubel-set-kubectl-config-file
           :desc "Set output format" "F" #'kubel-set-output-format)
          (:prefix ("f" . "filter")
           :desc "Filter" "f" #'kubel-set-filter
           :desc "Next highlight" "M-n" #'kubel-jump-to-next-highlight
           :desc "Previous highlight" "M-p" #'kubel-jump-to-previous-highlight
           :desc "Set label selector" "s" #'kubel-set-label-selector)
          (:prefix ("m" . "marking")
           :desc "Mark item" "m" #'kubel-mark-item
           :desc "Unmark item" "u" #'kubel-unmark-item
           :desc "Mark all items" "M" #'kubel-mark-all
           :desc "Unmark all items" "U" #'kubel-unmark-all)))))

(use-package! kubel-evil
  :when (modulep! :editor evil)
  :hook (kubel-mode . kubel-evil-mode))


(after! (:and kubel vterm)
  (kubel-vterm-setup))
