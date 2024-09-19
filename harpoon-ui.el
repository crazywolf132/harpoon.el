
;;; harpoon-ui.el --- User interface for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Provides the UI components for Harpoon, such as the quick menu.

;;; Code:

(require 'harpoon-list)
(require 'harpoon-logger)

(cl-defstruct harpoon-ui
  buffer
  window
  settings)

(defun harpoon-ui-create ()
  "Create a new Harpoon UI instance."
  (make-harpoon-ui))

(defun harpoon-ui-configure (ui settings)
  "Configure UI with SETTINGS."
  (setf (harpoon-ui-settings ui) settings))

(defun harpoon-ui-toggle-quick-menu (ui list)
  "Toggle the Harpoon quick menu for UI and LIST."
  (if (and (harpoon-ui-window ui)
           (window-live-p (harpoon-ui-window ui)))
      (harpoon-ui-close-menu ui)
    (harpoon-ui-open-menu ui list)))

(defun harpoon-ui-open-menu (ui list)
  "Open the Harpoon quick menu for UI and LIST."
  (let* ((buffer (get-buffer-create "*Harpoon Quick Menu*"))
         (window (display-buffer-in-side-window buffer '((side . bottom)))))
    (with-current-buffer buffer
      (harpoon-ui-mode)
      (setq-local harpoon-ui--list list)
      (harpoon-ui--render-list list))
    (setf (harpoon-ui-buffer ui) buffer
          (harpoon-ui-window ui) window)))

(defun harpoon-ui-close-menu (ui)
  "Close the Harpoon quick menu for UI."
  (when (and (harpoon-ui-window ui)
             (window-live-p (harpoon-ui-window ui)))
    (delete-window (harpoon-ui-window ui)))
  (when (buffer-live-p (harpoon-ui-buffer ui))
    (kill-buffer (harpoon-ui-buffer ui)))
  (setf (harpoon-ui-buffer ui) nil
        (harpoon-ui-window ui) nil))

(defun harpoon-ui--render-list (list)
  "Render LIST in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (item (reverse (harpoon-list-items list)))
      (insert (format "%s\n" (plist-get item :value))))
    (goto-char (point-min))))

(define-derived-mode harpoon-ui-mode special-mode "Harpoon-UI"
  "Major mode for Harpoon's quick menu."
  (setq-local revert-buffer-function #'harpoon-ui--revert-buffer))

(defun harpoon-ui--revert-buffer (_ignore-auto _noconfirm)
  "Revert buffer function for Harpoon UI."
  (harpoon-ui--render-list harpoon-ui--list))

(provide 'harpoon-ui)

;;; harpoon-ui.el ends here
