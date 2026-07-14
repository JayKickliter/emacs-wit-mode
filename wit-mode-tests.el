;;; wit-mode-tests.el --- Tests for wit-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jay Kickliter
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Run with: emacs -Q --batch -l wit-mode.el -l wit-mode-tests.el \
;;             -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'wit-mode)

(defconst wit-tests-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory holding this test file and `example.wit'.")

(defun wit-tests--continue-comment (starter)
  "Type STARTER, then break the line with `M-j' and type text."
  (with-temp-buffer
    (wit-mode)
    (insert starter)
    (default-indent-new-line)
    (insert "cont")
    (buffer-string)))

(ert-deftest wit-tests-doc-comment-continues ()
  "`M-j' inside a `///' doc comment keeps the `///' starter."
  (should (equal (wit-tests--continue-comment "    /// doc")
                 "    /// doc\n    /// cont")))

(ert-deftest wit-tests-line-comment-continues ()
  "`M-j' inside a `//' comment keeps the `//' starter."
  (should (equal (wit-tests--continue-comment "    // plain")
                 "    // plain\n    // cont")))

(ert-deftest wit-tests-indent-rebuild ()
  "Flattening then indenting `example.wit' reproduces the file."
  (let ((example (expand-file-name "example.wit" wit-tests-dir)))
    (skip-unless (file-exists-p example))
    (let ((golden (with-temp-buffer
                    (insert-file-contents example)
                    (buffer-string))))
      (with-temp-buffer
        (insert golden)
        (wit-mode)
        (goto-char (point-min))
        (while (not (eobp)) (delete-horizontal-space) (forward-line 1))
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) golden))))))

(provide 'wit-mode-tests)

;;; wit-mode-tests.el ends here
