
;;; harpoon-config.el --- Configuration handling for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Provides default configurations and utilities to merge user configurations.

;;; Code:

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
    :display 'identity
    :select 'harpoon-config-default-select
    :equals 'equal))

(defun harpoon-config-default-encode (item)
  "Default encode function for ITEM."
  (json-encode item))

(defun harpoon-config-default-decode (str)
  "Default decode function for STR."
  (json-read-from-string str))

(defun harpoon-config-default-select (item _list _options)
  "Default select function for ITEM."
  (when item
    (find-file (plist-get item :value))
    (goto-char (point-min))
    (forward-line (1- (plist-get (plist-get item :context) :row)))
    (move-to-column (plist-get (plist-get item :context) :col))))

(defun harpoon-config-merge (user-config default-config)
  "Merge USER-CONFIG with DEFAULT-CONFIG."
  (if user-config
      (deepcopy (merge-alist user-config default-config))
    default-config))

(defun harpoon-config-settings (config)
  "Get settings from CONFIG."
  (plist-get config :settings))

(defun harpoon-config-key (config)
  "Get the key function from CONFIG and execute it."
  (funcall (cdr (assoc 'key (harpoon-config-settings config)))))

(defun harpoon-config-get (config name)
  "Get the configuration item for NAME from CONFIG."
  (or (plist-get config name)
      (plist-get config :default)))

(provide 'harpoon-config)

;;; harpoon-config.el ends here
