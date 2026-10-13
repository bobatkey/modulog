;; Classes of tokens
(setq modulog-mode-keywords
      '("module" "type" "struct" "sig" "end" "functor" "and" "rec"
	"with" "pred" "define" "constant" "external" "output"))
(setq modulog-mode-logic-keywords
      '()) ;; "true" "false" "forall" "exists"))
(setq modulog-mode-logic-operators
      '("*" ":" ":-" "{" "|" "}"))

(setq modulog-mode-keywords-regexp
      (regexp-opt modulog-mode-keywords 'words))
(setq modulog-mode-logic-keywords-regexp
      (regexp-opt modulog-mode-logic-keywords 'words))
(setq modulog-mode-logic-operators-regexp
      (regexp-opt modulog-mode-logic-operators))

(setq modulog-mode-font-lock-keywords
      `((,modulog-mode-keywords-regexp . font-lock-keyword-face)
	(,modulog-mode-logic-keywords-regexp . font-lock-type-face)
	(,modulog-mode-logic-operators-regexp . font-lock-builtin-face)))

(defvar sott-syntax-table nil "Syntax table for `sott-mode'.")
(setq modulog-mode-syntax-table
  (let ((synTable (make-syntax-table)))

  ;; multiple lines
  (modify-syntax-entry ?\( ". 1" synTable)
  (modify-syntax-entry ?* ". 23" synTable)
  (modify-syntax-entry ?\) ". 4" synTable)

  synTable))

;; define the mode
(define-derived-mode modulog-mode fundamental-mode
  "Modulog mode"
  ;; handling comments
  :syntax-table modulog-mode-syntax-table
  ;; code for syntax highlighting
  (setq font-lock-defaults '((modulog-mode-font-lock-keywords)))
  (setq mode-name "modulog"))

(provide 'modulog-mode)
