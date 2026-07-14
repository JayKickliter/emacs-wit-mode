;;; wit-mode.el --- Major mode for WebAssembly Interface Types (WIT) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jay Kickliter
;; SPDX-License-Identifier: MIT

;; Author: Jay Kickliter <jay@kickliter.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, wasm, webassembly, wit
;; URL: https://github.com/7r-xyz/emacs-wit-mode

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing WIT files, the interface definition language
;; of the WebAssembly Component Model.
;;
;; Features:
;;   - syntax highlighting (keywords, builtin types, gates, docs, versions)
;;   - `//', `///' and `/* */' comment support
;;   - kebab-case and `%'-escaped identifiers treated as words
;;   - brace-aware indentation, 4 spaces by default
;;   - imenu index of packages, worlds, interfaces, types and funcs
;;
;; Enable automatically for `.wit' files by loading this file; the mode
;; adds itself to `auto-mode-alist'.

;;; Code:

(defgroup wit nil
  "Major mode for WebAssembly Interface Types (WIT) files."
  :group 'languages
  :prefix "wit-")

(defcustom wit-indent-offset 4
  "Number of spaces per indentation level in `wit-mode'."
  :type 'integer
  :safe #'integerp
  :group 'wit)

;;; Syntax table

(defvar wit-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; `//' line comments and `/* */' block comments (C-style).
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings.
    (modify-syntax-entry ?\" "\"" table)
    ;; Kebab-case identifiers: hyphen is part of a symbol.
    (modify-syntax-entry ?- "_" table)
    ;; `%' escapes a keyword used as an identifier (e.g. `%interface').
    (modify-syntax-entry ?% "_" table)
    ;; Package/version punctuation.
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?@ "." table)
    ;; Brackets, so `syntax-ppss' can report nesting depth for indentation.
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    ;; Angle brackets are punctuation, not paren pairs: the `->' arrow and
    ;; type params like `list<point>' would otherwise unbalance nesting.
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    table)
  "Syntax table for `wit-mode'.")

;;; Font lock

(defconst wit-keywords
  '("package" "world" "interface" "use" "include" "import" "export"
    "type" "record" "variant" "enum" "flags" "resource" "union"
    "func" "static" "constructor" "from" "as")
  "Structural keywords in WIT.")

(defconst wit-builtin-types
  '("u8" "u16" "u32" "u64" "s8" "s16" "s32" "s64"
    "f32" "f64" "float32" "float64"
    "bool" "char" "string"
    "list" "option" "result" "tuple" "future" "stream"
    "own" "borrow" "error")
  "Built-in types in WIT.")

(defconst wit-mode-font-lock-keywords
  (let ((kw-re (regexp-opt wit-keywords 'symbols))
        (ty-re (regexp-opt wit-builtin-types 'symbols)))
    `(
      ;; Doc comments `///' win over generic keyword faces.
      ("///.*$" 0 font-lock-doc-face t)
      ;; Feature gates: @since, @unstable, @deprecated, ...
      ("@[[:alpha:]][[:alnum:]-]*" 0 font-lock-preprocessor-face)
      ;; Structural keywords.
      (,kw-re . font-lock-keyword-face)
      ;; Built-in types.
      (,ty-re . font-lock-type-face)
      ;; Package / interface / world / type names being declared.
      ("\\_<\\(?:package\\)\\s-+\\([[:alnum:]_-]+:[[:alnum:]_./-]+\\)"
       1 font-lock-constant-face)
      ("\\_<\\(?:world\\|interface\\)\\s-+\\([[:alpha:]%][[:alnum:]_-]*\\)"
       1 font-lock-function-name-face)
      ("\\_<\\(?:record\\|variant\\|enum\\|flags\\|resource\\|type\\)\\s-+\\([[:alpha:]%][[:alnum:]_-]*\\)"
       1 font-lock-type-face)
      ;; Function definitions: `name: func(...)'.
      ("\\([[:alpha:]%][[:alnum:]_-]*\\)\\s-*:\\s-*\\(?:static\\s-+\\)?func\\_>"
       1 font-lock-function-name-face)
      ;; Version pins like `@1.2.3' handled by the gate rule above only if
      ;; alpha; numeric versions get their own colour.
      ("@[0-9][[:alnum:].+-]*" 0 font-lock-constant-face)
      ;; Numeric literals.
      ("\\_<[0-9]+\\_>" . font-lock-constant-face)))
  "Font-lock rules for `wit-mode'.")

;;; Indentation

(defun wit--closing-bracket-line-p ()
  "Return non-nil if the current line's first token is a closing bracket."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[ \t]*[]})]")))

(defun wit-calculate-indent ()
  "Compute the indentation column for the current line."
  (save-excursion
    (beginning-of-line)
    (let* ((ppss (syntax-ppss))
           (depth (nth 0 ppss)))
      (cond
       ;; Leave lines inside block comments and strings alone.
       ((or (nth 3 ppss) (nth 4 ppss)) nil)
       (t
        (when (and (> depth 0) (wit--closing-bracket-line-p))
          (setq depth (1- depth)))
        (max 0 (* wit-indent-offset depth)))))))

(defun wit-indent-line ()
  "Indent the current line as WIT code."
  (interactive)
  (let ((indent (wit-calculate-indent)))
    (when indent
      (let ((offset (save-excursion
                      (back-to-indentation)
                      (- (point) (line-beginning-position)))))
        (if (<= (current-column) offset)
            (indent-line-to indent)
          (save-excursion (indent-line-to indent)))))))

;;; Imenu

(defvar wit-imenu-generic-expression
  '(("Worlds"     "^\\s-*world\\s-+\\([[:alpha:]%][[:alnum:]_-]*\\)" 1)
    ("Interfaces" "^\\s-*interface\\s-+\\([[:alpha:]%][[:alnum:]_-]*\\)" 1)
    ("Types"      "^\\s-*\\(?:record\\|variant\\|enum\\|flags\\|resource\\|type\\)\\s-+\\([[:alpha:]%][[:alnum:]_-]*\\)" 1)
    ("Functions"  "^\\s-*\\([[:alpha:]%][[:alnum:]_-]*\\)\\s-*:\\s-*\\(?:static\\s-+\\)?func\\_>" 1))
  "Imenu configuration for `wit-mode'.")

;;; Mode definition

;;;###autoload
(define-derived-mode wit-mode prog-mode "WIT"
  "Major mode for editing WebAssembly Interface Types (WIT) files.

\\{wit-mode-map}"
  :group 'wit
  (setq-local font-lock-defaults '(wit-mode-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s-*")
  ;; Continue comments on `M-j': line comments reuse their exact starter, so a
  ;; `///' doc comment stays `///' on the next line.
  (setq-local comment-multi-line t)
  (setq-local indent-line-function #'wit-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width wit-indent-offset)
  (setq-local electric-indent-chars
              (append "{}()[]" electric-indent-chars))
  (setq-local imenu-generic-expression wit-imenu-generic-expression))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wit\\'" . wit-mode))

(provide 'wit-mode)

;;; wit-mode.el ends here
