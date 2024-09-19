
;;; harpoon-list.el --- List management for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Handles operations on Harpoon lists, such as adding, removing, and selecting items.

;;; Code:

(require 'cl-lib)
(require 'harpoon-config)
(require 'harpoon-logger)
(require 'harpoon-extensions)

(cl-defstruct harpoon-list
  name
  items
  config
  index)

(defun harpoon-list-create (config &optional name)
  "Create a new Harpoon list with CONFIG and optional NAME."
  (make-harpoon-list
   :name (or name harpoon-config-default-list)
   :items '()
   :config config
   :index 0))

(defun harpoon-list-add (list item)
  "Add ITEM to LIST."
  (unless (harpoon-list-contains-p list item)
    (push item (harpoon-list-items list))
    (harpoon-extensions-emit (harpoon-extensions-create) 'add (list :list list :item item))))

(defun harpoon-list-remove (list item)
  "Remove ITEM from LIST."
  (setf (harpoon-list-items list)
        (delete item (harpoon-list-items list)))
  (harpoon-extensions-emit (harpoon-extensions-create) 'remove (list :list list :item item)))

(defun harpoon-list-contains-p (list item)
  "Check if LIST contains ITEM."
  (cl-some (lambda (i) (funcall (plist-get (harpoon-list-config list) :equals) i item))
           (harpoon-list-items list)))

(defun harpoon-list-select (list index)
  "Select the item at INDEX in LIST."
  (let ((item (nth (1- index) (harpoon-list-items list))))
    (when item
      (harpoon-config-default-select item list nil))))

(provide 'harpoon-list)

;;; harpoon-list.el ends here
