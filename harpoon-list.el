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
  (let ((list (make-harpoon-list
               :name (or name harpoon-config-default-list)
               :items '()
               :config config
               :index 0)))
    (harpoon-list-load list)
    list))

(defun harpoon-list-add (list &optional item)
  "Add ITEM to LIST."
  (let ((item (or item
                  (funcall (plist-get (harpoon-list-config list) :create-list-item)
                           (harpoon-list-config list)))))
    (unless (harpoon-list-contains-p list item)
      (push item (harpoon-list-items list))
      (harpoon-extensions-emit (harpoon-extensions-create) 'add (list :list list :item item)))))

(defun harpoon-list-prepend (list &optional item)
  "Prepend ITEM to LIST."
  (let ((item (or item
                  (funcall (plist-get (harpoon-list-config list) :create-list-item)
                           (harpoon-list-config list)))))
    (unless (harpoon-list-contains-p list item)
      (setf (harpoon-list-items list) (append (list item) (harpoon-list-items list)))
      (harpoon-extensions-emit (harpoon-extensions-create) 'add (list :list list :item item)))))

(defun harpoon-list-remove (list item)
  "Remove ITEM from LIST."
  (setf (harpoon-list-items list)
        (cl-remove item (harpoon-list-items list)
                   :test (plist-get (harpoon-list-config list) :equals)))
  (harpoon-extensions-emit (harpoon-extensions-create) 'remove (list :list list :item item)))

(defun harpoon-list-contains-p (list item)
  "Check if LIST contains ITEM."
  (cl-some (lambda (i) (funcall (plist-get (harpoon-list-config list) :equals) i item))
           (harpoon-list-items list)))

(defun harpoon-list-select (list index &optional options)
  "Select the item at INDEX in LIST."
  (let ((items (harpoon-list-items list))
        (config (harpoon-list-config list)))
    (if (or (< index 1) (> index (length items)))
        (when (plist-get config :select-with-nil)
          (funcall (plist-get config :select) nil list options))
      (let ((item (nth (1- index) items)))
        (funcall (plist-get config :select) item list options)))))

(defun harpoon-list-load (list)
  "Load data into LIST from disk."
  (let* ((config (harpoon-list-config list))
         (data (harpoon-data-read config))
         (items (cl-remove-if-not
                 'identity
                 (mapcar (lambda (str)
                           (let ((decode-fn (plist-get config :decode)))
                             (when decode-fn
                               (funcall decode-fn str))))
                         data))))
    (setf (harpoon-list-items list) items)))

(defun harpoon-list-get-by-value (list value)
  "Get the item in LIST that matches VALUE."
  (cl-find-if (lambda (item)
                (equal (plist-get item :value) value))
              (harpoon-list-items list)))

(provide 'harpoon-list)

;;; harpoon-list.el ends here
