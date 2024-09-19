;;; harpoon-buffer.el --- Buffer management for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Provides utilities for buffer handling within Harpoon.

;;; Code:

(require 'harpoon-ui)

(defun harpoon-buffer-setup-keymaps (buffer)
  "Set up keymaps for BUFFER."
  (with-current-buffer buffer
    (local-set-key (kbd "q") 'harpoon-ui-close-menu)
    (local-set-key (kbd "RET") 'harpoon-ui-select-item)))

(defun harpoon-buffer-get-contents (buffer)
  "Get the contents of BUFFER as a list of lines."
  (with-current-buffer buffer
    (split-string (buffer-string) "\n" t)))

(provide 'harpoon-buffer)

;;; harpoon-buffer.el ends here
