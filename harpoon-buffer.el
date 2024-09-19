;;; harpoon-buffer.el --- Buffer management for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Provides utilities for buffer handling within Harpoon.

;;; Code:

(require 'harpoon-ui)

(defun harpoon-buffer-setup-keymaps (_buffer)
  "Set up keymaps for the current Harpoon buffer."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'harpoon-ui-close-menu)
    (define-key map (kbd "RET") 'harpoon-ui-select-item)
    (define-key map (kbd "<escape>") 'harpoon-ui-close-menu)
    (use-local-map map)))

(defun harpoon-buffer-get-contents (buffer)
  "Get the contents of BUFFER as a list of lines."
  (with-current-buffer buffer
    (split-string (buffer-string) "\n" t)))

(provide 'harpoon-buffer)

;;; harpoon-buffer.el ends here
