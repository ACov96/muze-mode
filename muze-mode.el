;;; muze-mode.el --- Major mode for the Muze programming language

;; Copyright (C) 2019-2020 Alex Covington

;; Author: Alex Covington <alex@covington.tech>
;; Maintainer: Alex Covington <alex@covington.tech>
;; Created: May 28, 2019
;; Keywords: languages muze
;; Version: 1.0
;; Homepage: https://github.com/ACov96/muze-mode

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; This is a major mode to go alongside the Muze language project.  None
;; of use are Elisp gurus, so forgive any weirdness you might see.  We don't
;; know why it's like this either...

;; Muze Homepage: https://gitlab.cs.wwu.edu/covinga/language-design-research

;;; Code:

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
(defvar muze-mode-keywords
      (let* (
             ;; define several categories of keywords
             (x-keywords '("break" "continue" "fa" "af" "if" "elif" "else" "fi"
                           "loop" "pool" "mod" "dom" "fun" "nuf" "begin" "end"
                           "var" "type" "const" "case" "esac" "then" "return"
                           "mu" "um" "rec" "cer" "this" "that" "extern"))
             (x-types '("real" "string" "integer" "boolean"
                        "array" "list" "hash"))
             (x-operators '("->" "..." ".."))

             ;; Create the regexps
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-operators-regexp (regexp-opt x-operators 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-operators-regexp . font-lock-constant-face))))

;; Indentation
(defvar muze-indent-value 1)
(defun muze-first-word ()
  "Return the first word on the current line."
  (car (split-string (thing-at-point 'line t))))

(defun muze-prev-line-first-word ()
  "Get and return the first word of the previous line."
  (save-excursion
    (forward-line -1)
    (while (progn (beginning-of-line) (looking-at "[[:space:]]*$"))
      (forward-line -1))
    (muze-first-word)))

(defun muze-prev-line-indent ()
  "Return the indentation of the previous line that isn't whitespace."
  (save-excursion
    (forward-line -1)
    (while (progn (beginning-of-line) (looking-at "[[:space:]]*$"))
      (forward-line -1))
    (current-indentation)))

(defun muze-prev-line-spaces ()
  "Return the number of spaces in the previous line."
  (save-excursion
    (forward-line -1)
    (current-indentation)))

(defun muze-get-last-mod-indent ()
  "Return the indentation for the last line that start with mod."
  (save-excursion
    (let ((dom-count 1))
      (while (> dom-count 0)
        (forward-line -1)
        (when (string-equal (muze-first-word) "dom")
          (setq dom-count (+ dom-count 1)))
        (when (string-equal (muze-first-word) "mod")
          (setq dom-count (- dom-count 1))))
      (current-indentation))))

(defun muze-get-last-fun-indent ()
  "Return the indentation for the last line that start with mod."
  (save-excursion
    (let ((nuf-count 1))
      (while (> nuf-count 0)
        (forward-line -1)
        (when (string-equal (muze-first-word) "nuf")
          (setq nuf-count (+ nuf-count 1)))
        (when (string-equal (muze-first-word) "fun")
          (setq nuf-count (- nuf-count 1))))
      (current-indentation))))

(defun muze-get-last-loop-indent ()
  "Return the indentation for the last line that start with loop."
  (save-excursion
    (let ((pool-count 1))
      (while (> pool-count 0)
        (forward-line -1)
        (when (string-equal (muze-first-word) "pool")
          (setq pool-count (+ pool-count 1)))
        (when (string-equal (muze-first-word) "loop")
          (setq pool-count (- pool-count 1))))
      (current-indentation))))

(defun muze-get-last-if-indent ()
  "Return the indentation for the last line that start with if."
  (save-excursion
    (let ((fi-count 1))
      (while (> fi-count 0)
        (forward-line -1)
        (when (string-equal (muze-first-word) "fi")
          (setq fi-count (+ fi-count 1)))
        (when (string-equal (muze-first-word) "if")
          (setq fi-count (- fi-count 1))))
      (current-indentation))))

(defun muze-nuf-to-fun ()
  "If current line start with nuf token, move to matching fun token."
  (when (string-equal (muze-first-word) "nuf")
    (while (not (string-equal (muze-first-word) "fun"))
      (forward-line -1)
      (forward-whitespace 1)
      (when (string-equal (muze-first-word) "nuf")
        (muze-nuf-to-fun)))))

(defun muze-dom-to-mod ()
  "If current line start with dom token, move to matching mod token."
  (when (string-equal (muze-first-word) "dom")
    (while (not (string-equal (muze-first-word) "mod"))
      (forward-line -1)
      (forward-whitespace 1)
      (when (string-equal (muze-first-word) "dom")
        (muze-dom-to-mod)))))

(defun muze-get-current-scope-indent ()
  "Return the indentation for the current scope (aligned with first line that has a mod or fun token as the first word)."
  (save-excursion
      (while (not (or (string-equal (muze-first-word) "mod")
                      (string-equal (muze-first-word) "fun")))
        (forward-line -1)
        (when (string-equal (muze-first-word) "dom")
          (muze-dom-to-mod)
          (forward-line -1))
        (when (string-equal (muze-first-word) "nuf")
          (muze-nuf-to-fun)
          (forward-line -1)))
      (current-indentation)))

(defun muze-get-indent-offset ()
  "Return the number of spaces to indent the current line."
  (interactive)
  (while (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$"))
    (forward-line -1))
  (let ((prev-first (muze-prev-line-first-word))
        (prev-indent (muze-prev-line-indent)))
    (cond ((bobp) 0)
          ((string-equal (muze-first-word) "dom") (muze-get-last-mod-indent))
          ((string-equal (muze-first-word) "nuf") (muze-get-last-fun-indent))
          ((string-equal (muze-first-word) "pool") (muze-get-last-loop-indent))
          ((string-equal (muze-first-word) "fi") (muze-get-last-if-indent))
          ((string-equal (muze-first-word) "else") (muze-get-last-if-indent))
          ((string-equal (muze-first-word) "begin") (muze-get-current-scope-indent))
          ((or (string-equal prev-first "mod")
               (string-equal prev-first "var")
               (string-equal prev-first "const")
               (string-equal prev-first "type")
               (string-equal prev-first "begin")
               (string-equal prev-first "fun")
               (string-equal prev-first "if")
               (string-equal prev-first "else")
               (string-equal prev-first "loop")
               (string-equal prev-first "extern"))
           (+ (muze-prev-line-spaces) muze-indent-value))
          (t (muze-prev-line-indent)))))

(defun muze-clear-leading-whitespace ()
  "Deletes leading whitespace on the current line."
  (beginning-of-line)
  (when (string= (string (char-after (point))) " ")
      (let ((regex "[ \t]+"))
        (re-search-forward regex nil t)
        (replace-match "" nil nil))))

(defun muze-mode-indent-line ()
  "Indent current line.  This is pretty hacky, accepting any improvement."
  (interactive)
  (save-excursion
  (muze-clear-leading-whitespace)
  (indent-to-column (muze-get-indent-offset))))
  
    

(define-derived-mode muze-mode prog-mode "Muze Mode"
  :syntax-table muze-mode-syntax-table
  (setq font-lock-defaults '((muze-mode-keywords))
        indent-line-function 'muze-mode-indent-line)
  (font-lock-ensure))

(provide 'muze-mode)

;;; muze-mode.el ends here
