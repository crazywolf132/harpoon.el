;;; harpoon.el --- Emacs port of Neovim's Harpoon plugin -*- lexical-binding: t -*-

;; Author: Brayden Moon <crazywolf132@gmail.com>
;; URL: https://github.com/crazywolf132/harpoon
;; Version: 0.1.0
;; Keywords: convenience, bookmarks
;; Package-Requires: ((emacs "29.4"))

;;; Commentary:

;; Harpoon is a navigation tool inspired by ThePrimeagen's Harpoon plugin for Neovim.
;; It allows quick access to frequently used files and buffers.

;;; Code:

(require 'harpoon-config)
(require 'harpoon-logger)
(require 'harpoon-ui)
(require 'harpoon-data)
(require 'harpoon-list)
(require 'harpoon-extensions)
(require 'harpoon-autocmd)

(defgroup harpoon nil
  "Quick navigation and management of files and buffers."
  :group 'convenience
  :prefix "harpoon-")

(defvar harpoon--instance nil
  "Singleton instance of Harpoon.")

(defun harpoon--init ()
  "Initialize Harpoon."
  (unless harpoon--instance
    (setq harpoon--instance
          (make-harpoon
           :config (harpoon-config-default)
           :data (harpoon-data-create)
           :logger (harpoon-logger-create)
           :ui (harpoon-ui-create)
           :extensions (harpoon-extensions-create)
           :lists (make-hash-table :test 'equal)
           :hooks-setup nil)))
  harpoon--instance)

(cl-defstruct harpoon
  config
  data
  logger
  ui
  extensions
  lists
  hooks-setup)

;;;###autoload
(defun harpoon-setup (&optional config)
  "Set up Harpoon with an optional CONFIG."
  (let ((instance (harpoon--init)))
    (setf (harpoon-config instance) (harpoon-config-merge config (harpoon-config instance)))
    (harpoon-ui-configure (harpoon-ui instance) (harpoon-config-settings (harpoon-config instance)))
    (harpoon-extensions-emit (harpoon-extensions instance) 'setup-called (harpoon-config instance))
    (unless (harpoon-hooks-setup instance)
      (harpoon-autocmd-setup instance)
      (setf (harpoon-hooks-setup instance) t))
    instance))

;;;###autoload
(defun harpoon-add-file ()
  "Add the current file to Harpoon's list."
  (interactive)
  (let ((instance (harpoon--init)))
    (harpoon-list-add (harpoon-get-list instance) nil)
    (harpoon-sync)))

;;;###autoload
(defun harpoon-toggle-quick-menu ()
  "Toggle Harpoon's quick menu."
  (interactive)
  (let ((instance (harpoon--init)))
    (harpoon-ui-toggle-quick-menu (harpoon-ui instance) (harpoon-get-list instance))))

(defun harpoon-get-list (&optional instance name)
  "Retrieve or create a Harpoon list by NAME."
  (let* ((instance (or instance (harpoon--init)))
         (key (harpoon-config-key (harpoon-config instance)))
         (lists (harpoon-lists instance))
         (list (gethash key lists)))
    (unless list
      (setq list (harpoon-list-create (harpoon-config-get (harpoon-config instance) name) name))
      (puthash key list lists)
      (harpoon-extensions-emit (harpoon-extensions instance) 'list-created list))
    list))

(defun harpoon-sync ()
  "Sync Harpoon's data to disk."
  (interactive)
  (let ((instance (harpoon--init)))
    (harpoon-data-sync (harpoon-data instance) (harpoon-lists instance))))

(provide 'harpoon)

;;; harpoon.el ends here
