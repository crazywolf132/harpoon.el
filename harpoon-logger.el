;;; harpoon-logger.el --- Logging utilities for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Provides logging functionality for debugging purposes.

;;; Code:

(defvar harpoon-logger-lines '()
  "List of logged lines.")

(defvar harpoon-logger-max-lines 50
  "Maximum number of lines to keep in the log.")

(defun harpoon-logger-log (&rest args)
  "Log ARGS to the harpoon log."
  (let ((line (mapconcat
               (lambda (arg)
                 (cond
                  ((stringp arg) arg)
                  ((numberp arg) (number-to-string arg))
                  (t (format "%s" arg))))
               args " ")))
    (setq harpoon-logger-lines
          (append harpoon-logger-lines (list line)))
    (when (> (length harpoon-logger-lines) harpoon-logger-max-lines)
      (setq harpoon-logger-lines
            (last harpoon-logger-lines harpoon-logger-max-lines)))))

(defun harpoon-logger-clear ()
  "Clear the harpoon log."
  (interactive)
  (setq harpoon-logger-lines '()))

(defun harpoon-logger-show ()
  "Display the harpoon log in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Harpoon Log*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (string-join harpoon-logger-lines "\n"))
      (goto-char (point-min)))
    (display-buffer (current-buffer))))

(defun harpoon-logger-create ()
  "Create a new Harpoon logger instance."
  ;; In this implementation, the logger is stateless, so we can return nil or self-reference
  'harpoon-logger)

(provide 'harpoon-logger)

;;; harpoon-logger.el ends here
