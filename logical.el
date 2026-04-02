;; Classes of tokens
(setq logical-mode-keywords
      '("module" "type" "struct" "sig" "end" "functor" "and" "rec"
	"with" "pred" "func" "axiom" "sort" "check" "synth" "display" "case"))
(setq logical-mode-logic-keywords
      '("true" "false" "forall" "exists"))
(setq logical-mode-logic-operators
      '("&" "|" "¬" "->" "="))

(setq logical-mode-keywords-regexp
      (regexp-opt logical-mode-keywords 'words))
(setq logical-mode-logic-keywords-regexp
      (regexp-opt logical-mode-logic-keywords 'words))
(setq logical-mode-logic-operators-regexp
      (regexp-opt logical-mode-logic-operators))

(setq logical-mode-font-lock-keywords
      `((,logical-mode-keywords-regexp . font-lock-keyword-face)
	(,logical-mode-logic-keywords-regexp . font-lock-type-face)
	(,logical-mode-logic-operators-regexp . font-lock-builtin-face)))

(defvar sott-syntax-table nil "Syntax table for `sott-mode'.")
(setq logical-mode-syntax-table
  (let ((synTable (make-syntax-table)))

  ;; multiple lines
  (modify-syntax-entry ?\( ". 1" synTable)
  (modify-syntax-entry ?* ". 23" synTable)
  (modify-syntax-entry ?\) ". 4" synTable)

  synTable))

;; define the mode
(define-derived-mode logical-mode fundamental-mode
  "Logical mode"
  ;; handling comments
  :syntax-table logical-mode-syntax-table
  ;; code for syntax highlighting
  (setq font-lock-defaults '((logical-mode-font-lock-keywords)))
  (setq mode-name "logical"))

(provide 'logical-mode)
