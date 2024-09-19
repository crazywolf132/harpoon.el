
;;; harpoon-autocmd.el --- Autocommand setup for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Sets up hooks and autocommands for Harpoon.

;;; Code:

(require 'harpoon-extensions)
(require 'harpoon-config)

(defun harpoon-autocmd-setup (instance)
  "Set up autocommands for Harpoon INSTANCE."
  (add-hook 'kill-emacs-hook (lambda () (harpoon-sync)))
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (let ((list (harpoon-get-list)))
                (when (and list (buffer-file-name))
                  (harpoon-extensions-emit (harpoon-extensions instance)
                                           'buffer-leave
                                           (list :buffer (current-buffer)
                                                 :list list)))))))

(provide 'harpoon-autocmd)

;;; harpoon-autocmd.el ends here
