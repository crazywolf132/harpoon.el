;;; harpoon-test.el --- Tests for Harpoon -*- lexical-binding: t -*-

;;; Commentary:

;; Provides unit tests for Harpoon.

;;; Code:

(require 'harpoon)
(require 'ert)

(ert-deftest harpoon-add-file-test ()
  "Test adding a file to Harpoon."
  (with-temp-buffer
    (let ((filename (make-temp-file "harpoon-test")))
      (write-region "" nil filename)
      (find-file filename)
      (harpoon-add-file)
      (should (harpoon-list-contains-p (harpoon-get-list) (harpoon-config-default-create-list-item nil filename))))))

(ert-deftest harpoon-select-file-test ()
  "Test selecting a file from Harpoon."
  (with-temp-buffer
    (let ((filename (make-temp-file "harpoon-test")))
      (write-region "Test content" nil filename)
      (find-file filename)
      (harpoon-add-file)
      (kill-buffer)
      (harpoon-list-select (harpoon-get-list) 1)
      (should (equal (buffer-file-name) filename)))))

(provide 'harpoon-test)

;;; harpoon-test.el ends here
