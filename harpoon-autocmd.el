;;; harpoon-autocmd.el --- Autocommand setup for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Sets up hooks and autocommands for Harpoon.

;;; Code:

(require 'harpoon-extensions)
(require 'harpoon-config)
(require 'harpoon-list)

(defun harpoon-autocmd-setup (instance)
  "Set up autocommands for Harpoon INSTANCE."
  (add-hook 'kill-emacs-hook (lambda () (harpoon-sync)))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (let ((list (harpoon-get-list instance)))
                (when (and list (buffer-file-name))
                  (let ((buf-leave-fn (plist-get (harpoon-list-config list) :buf-leave)))
                    (when buf-leave-fn
                      (funcall buf-leave-fn nil list))))))))

(provide 'harpoon-autocmd)

;;; harpoon-autocmd.el ends here
