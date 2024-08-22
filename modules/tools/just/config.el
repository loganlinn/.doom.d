;;; tools/just/config.el -*- lexical-binding: t; -*-

(use-package! just-mode
  :mode ("/[Jj]ustfile\\'" . just-mode)
  :config
  (set-formatter! 'just-fmt `(,just-executable "--dump") :modes '(just-mode))
  (map! :localleader
        :map just-mode-map
        "m" #'justl
        "/" #'justl-help-popup))


(use-package! justl
  :commands justl
  :config
  (set-popup-rules!
    '((,(regexp-quote justl--process-buffer))
      (,(regexp-quote justl--output-process-buffer) :side right :slot 2 :vslot -1 :select nil)
      ("^\\*just " :side right :slot 1 :vslot -1 :size '+popup-shrink-to-fit :select t :quit t :ttl nil)))

  (when (modulep! :term vterm)
    (setq justl-shell 'vterm))

  (map! :map justl-mode-map
        :nv "g" 'justl--refresh-buffer
        :nv "e" 'justl-exec-recipe
        :nv "E" 'justl-exec-shell
        :nv "?" 'justl-help-popup
        :nv "h" 'justl-help-popup
        :nv "w" 'justl--exec-recipe-with-args
        :nv "W" 'justl-no-exec-shell
        :nv "RET" 'justl-go-to-recipe))
