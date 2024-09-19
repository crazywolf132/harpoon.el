;;; harpoon-config.el --- Configuration handling for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Provides default configurations and utilities to merge user configurations.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'harpoon-logger)

(defgroup harpoon-config nil
  "Configuration options for Harpoon."
  :group 'harpoon)

(defcustom harpoon-config-default-list "__harpoon_files"
  "Default list name for Harpoon."
  :type 'string
  :group 'harpoon-config)

(defcustom harpoon-config-settings
  '((save-on-toggle . nil)
    (sync-on-ui-close . nil)
    (key . (lambda () default-directory)))
  "Default settings for Harpoon."
  :type 'alist
  :group 'harpoon-config)

(defun harpoon-config-default ()
  "Return the default Harpoon configuration."
  (list
   :settings harpoon-config-settings
   :default (harpoon-config-default-item)))

(defun harpoon-config-default-item ()
  "Return the default configuration item."
  (list
   :select-with-nil nil
   :encode 'harpoon-config-default-encode
   :decode 'harpoon-config-default-decode
   :display 'harpoon-config-default-display
   :select 'harpoon-config-default-select
   :equals 'harpoon-config-default-equals
   :create-list-item 'harpoon-config-default-create-list-item
   :get-root-dir (lambda () default-directory)
   :autocmds '(harpoon-buffer-leave-hook)
   :buf-leave 'harpoon-config-default-buf-leave))

(defun harpoon-config-default-encode (item)
  "Default encode function for ITEM."
  (json-encode item))

(defun harpoon-config-default-decode (str)
  "Default decode function for STR."
  (condition-case nil
      (json-read-from-string str)
    (error nil)))

(defun harpoon-config-default-display (item)
  "Default display function for ITEM."
  (plist-get item :value))

(defun harpoon-config-default-select (item _list _options)
  "Default select function for ITEM."
  (when item
    (let ((file (plist-get item :value))
          (context (plist-get item :context)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- (plist-get context :row)))
      (move-to-column (plist-get context :col)))))

(defun harpoon-config-default-equals (a b)
  "Default equals function comparing A and B."
  (equal (plist-get a :value) (plist-get b :value)))

(defun harpoon-config-default-create-list-item (_config &optional name)
  "Create a list item with optional NAME."
  (let* ((file (or name (buffer-file-name)))
         (row (line-number-at-pos))
         (col (current-column)))
    (when file
      (list :value file
            :context (list :row row :col col)))))

(defun harpoon-config-default-buf-leave (_event list)
  "Default buffer leave function for LIST."
  (let ((file (buffer-file-name)))
    (when file
      (let ((item (harpoon-list-get-by-value list file)))
        (when item
          (setf (plist-get (plist-get item :context) :row) (line-number-at-pos))
          (setf (plist-get (plist-get item :context) :col) (current-column)))))))

(defun harpoon-config-merge (user-config default-config)
  "Merge USER-CONFIG with DEFAULT-CONFIG."
  (if user-config
      (cl-loop for (key . value) in user-config
               do (plist-put default-config key value))
    default-config))

(defun harpoon-config-settings (config)
  "Get settings from CONFIG."
  (plist-get config :settings))

(defun harpoon-config-key (config)
  "Get the key function from CONFIG and execute it."
  (funcall (cdr (assoc 'key (harpoon-config-settings config)))))

(defun harpoon-config-get (config name)
  "Get the configuration item for NAME from CONFIG."
  (let ((default (plist-get config :default)))
    (if name
        (let ((item (plist-get config name)))
          (if item
              (cl-loop for (key value) on default by 'cddr
                       do (unless (plist-member item key)
                            (plist-put item key value)))
            default))
      default)))

(provide 'harpoon-config)

;;; harpoon-config.el ends here
