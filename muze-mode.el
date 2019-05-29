(defconst muze-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; string delimiter
    (modify-syntax-entry ?\" "\"" table)

    ;; single line comments
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table) 

    ;; TODO: multi line comments
    table))

;; Font lock
(setq muze-mode-keywords
      (let* (
             ;; define several categories of keywords
             (x-keywords '("break" "continue" "fa" "af" "if" "elif" "else" "fi"
                           "loop" "pool" "mod" "dom" "fun" "nuf" "begin" "end"
                           "var" "type" "const" "case" "esac" "then" "return"))
             (x-types '("real" "string" "integer" "boolean" "array" "list" "hash"))
             (x-operators '("->" "..." ".."))

             ;; Create the regexps
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-operators-regexp (regexp-opt x-operators 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-operators-regexp . font-lock-constant-face)
          )))


(define-derived-mode muze-mode prog-mode "Muze Mode"
  :syntax-table muze-mode-syntax-table
  (setq font-lock-defaults '((muze-mode-keywords)))
  (font-lock-fontify-buffer))

(provide 'muze-mode)
