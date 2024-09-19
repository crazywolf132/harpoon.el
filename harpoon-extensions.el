;;; harpoon-extensions.el --- Extensions system for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Provides an event system for Harpoon to allow extensions.

;;; Code:

(cl-defstruct harpoon-extensions
  listeners)

(defun harpoon-extensions-create ()
  "Create a new Harpoon extensions instance."
  (make-harpoon-extensions :listeners '()))

(defun harpoon-extensions-add-listener (extensions listener)
  "Add LISTENER to EXTENSIONS."
  (push listener (harpoon-extensions-listeners extensions)))

(defun harpoon-extensions-emit (extensions event &rest args)
  "Emit EVENT with ARGS to all listeners in EXTENSIONS."
  (dolist (listener (harpoon-extensions-listeners extensions))
    (let ((func (cdr (assoc event listener))))
      (when func
        (apply func args)))))

(provide 'harpoon-extensions)

;;; harpoon-extensions.el ends here
