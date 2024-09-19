;;; harpoon-data.el --- Data persistence for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Manages reading and writing Harpoon data to disk.

;;; Code:

(require 'json)
(require 'harpoon-config)
(require 'harpoon-logger)

(defvar harpoon-data-path
  (expand-file-name "harpoon" user-emacs-directory)
  "Directory where Harpoon stores its data.")

(defun harpoon-data--ensure-data-path ()
  "Ensure that `harpoon-data-path` exists."
  (unless (file-exists-p harpoon-data-path)
    (make-directory harpoon-data-path t)))

(defun harpoon-data--filename (key)
  "Generate a filename based on KEY."
  (expand-file-name (concat (secure-hash 'sha256 key) ".json") harpoon-data-path))

(defun harpoon-data-read (config)
  "Read data for CONFIG."
  (harpoon-data--ensure-data-path)
  (let* ((key (harpoon-config-key config))
         (file (harpoon-data--filename key)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (condition-case nil
              (json-read)
            (error (progn
                     (harpoon-logger-log "Error reading data file.")
                     '()))))
      (progn
        (harpoon-data-write config '())
        '()))))

(defun harpoon-data-write (config data)
  "Write DATA for CONFIG."
  (harpoon-data--ensure-data-path)
  (let* ((key (harpoon-config-key config))
         (file (harpoon-data--filename key)))
    (with-temp-file file
      (insert (json-encode data)))))

(defun harpoon-data-create ()
  "Create a new harpoon data instance."
  (harpoon-data-read (harpoon-config-default)))

(defun harpoon-data-sync (data lists)
  "Sync DATA with LISTS."
  (maphash
   (lambda (_key list)
     (let* ((items (harpoon-list-items list))
            (encoded (cl-remove-if-not
                      'identity
                      (mapcar (lambda (item)
                                (let ((encode-fn (plist-get (harpoon-list-config list) :encode)))
                                  (when encode-fn
                                    (funcall encode-fn item))))
                              items))))
       (harpoon-data-write (harpoon-list-config list) encoded)))
   lists))

(provide 'harpoon-data)

;;; harpoon-data.el ends here
