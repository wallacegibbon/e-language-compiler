;;; -*- lexical-binding: t -*-

(define-derived-mode elang-mode prog-mode "E Language"
  "Major mode for editing E Language source files."
  ;; Syntax highlighting
  (setq font-lock-defaults '(elang-font-lock-keywords))
  ;; Comment
  (modify-syntax-entry ?% "< b" elang-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" elang-mode-syntax-table))

(defvar elang-keywords
  '("if" "then" "elif" "else" "while" "do" "fn" "end" "return" "struct"))

(setq elang-font-lock-keywords
      `(
        ;;; Keywords
        (,(regexp-opt elang-keywords 'words) . font-lock-keyword-face)

        ;;; Constants (booleans, null, etc.)
        ;;(,(regexp-opt '("true" "false") 'words) . font-lock-constant-face)

        ;;; Types (assuming types start with a capital letter)
        (":\\s-*\\(\\<[a-zA-Z0-9_]+\\>^*\\)" 1 font-lock-type-face)
	
	; ;; Function names (only highlight the name, not the `(`)
        ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face)

        ;;; Strings
        ("\"[^\"]*\"" . font-lock-string-face)

        ;;; Numbers
        ("[box]?\\b[0-9]+\\b" . font-lock-constant-face)
	))

(defvar elang-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?% "< b" table)  ;; Single-line comments with `%`
    (modify-syntax-entry ?\n "> b" table) ;; End of comment
    (modify-syntax-entry ?\" "\"" table)  ;; Strings
    table)
  "Syntax table for elang-mode.")

(defun elang-indent-line ()
  "Indent current line for Elang mode."
  (interactive)
  (let ((indent-level 2))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[ \t]*}")
          (indent-line-to (- (current-indentation) indent-level))
        (indent-line-to indent-level)))))

(setq elang-mode-hook
      (lambda ()
        (setq-local indent-line-function #'elang-indent-line)))

(add-to-list 'auto-mode-alist '("\\.e\\'" . elang-mode))

(provide 'elang-mode)
