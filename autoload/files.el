;;; autoload/files.el -*- lexical-binding: t; -*-

;;;###autoload
(defun chmod-this-file (&optional path mode)
  "Set executable mode bit of current file"
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         (read-file-modes "File modes (octal or symbolic): "
                          (buffer-file-name (buffer-base-buffer)))))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer)))))
    (unless (file-exists-p path)
     (user-error "Buffer is not visiting any file"))
   (chmod path mode)))

;;;###autoload
(defun load-dir-local-variables ()
  "Apply directory-local variables."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
