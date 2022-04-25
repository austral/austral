;;;; austral-mode.el - Major mode for editing Austral source files.
;;;;
;;;; Copyright (C) 2021 Fernando Borretti
;;;;
;;;; Author: Fernando Borretti <fernando@borretti.me>
;;;; Maintainer: Fernando Borretti <fernando@borretti.me>
;;;; Created: 24 July, 2021
;;;; Version: 0.0.1
;;;; Keywords: languages
;;;;
;;;; Structure
;;;; ---------
;;;;
;;;;     1. Constants
;;;;     1. Syntax Highlighting
;;;;     1. Mode Definition
;;;;

(require 'cl-lib)

;;;; Section 1. Constants

(defconst austral-keywords
  '("and" "or" "not" "module" "is" "body" "import" "as" "end" "constant" "type"
    "function" "generic" "record" "union" "case" "of" "when" "typeclass"
    "instance" "method" "if" "then" "else" "let" "while" "for" "do" "from"
    "to" "borrow" "borrow!" "in" "return" "skip" "Free" "Linear" "Type" "Region"
    "pragma" "nil" "true" "false"))

(defconst austral-default-tab-width 4)

;;;; Section 1. Syntax Highlighting
;;;;

(defvar austral-font-lock-keywords
  (list
   ;; Comments
   (cons "--.*" font-lock-comment-face)
   ;; Keywords
   (cons (regexp-opt austral-keywords 'words) font-lock-keyword-face)
   ;; Identifiers
   (cons "\\<[[:alpha:]][_[:alnum:]]*\\>" font-lock-variable-name-face)
   ))

;;;; Section 1. Indentation
;;;;

(defun austral-find-previous-non-empty-line ()
  "Returns a cons cell, whose car is the indentation of the closest non-blank
line before the current one, and whose cdr is the text of that line.

If there are no blank-lines, returns zero and the empty string."
  (save-excursion
    (if (re-search-backward "^." nil t)
        (let ((text (buffer-substring (line-beginning-position) (line-end-position))))
          (cons (current-indentation) text))
      (cons 0 ""))))

(defun austral-indent-line ()
  "Indent the current line of Austral code."
  ;; previous-indent is the indentation of the closest non-empty line before the
  ;; current line. previous-text is the text of said line.
  (let* ((non-empty (austral-find-previous-non-empty-line))
         (previous-indent (car non-empty))
         (previous-text (cdr non-empty)))
    (cond
     ((string= previous-text "")
      ;; This case represents the beginning of the buffer. So we indent to zero.
      (indent-line-to 0))
     ((string-match "^[ \t]*\\(module\\|import\\|record\\|union\\|interface\\|if\\|else\\|for\\|while\\|borrow\\)" previous-text)
      ;; The previous line opens a new indent block.
      (indent-line-to (+ austral-default-tab-width previous-indent)))
     ((string-match "^[ \t]*\\(end\\|\\\\);\\)" previous-text)
      ;; The previous line decreases indent.
      (indent-line-to (- previous-indent austral-default-tab-width)))
     ((looking-at "^[ \t]*\\(end\\)")
      ;; This line should be indented less.
      (indent-line-to (- previous-indent austral-default-tab-width)))
     (t
      ;; Indent same as the previous line.
      (indent-line-to previous-indent)))))

;;;; Section 1. Mode Definition
;;;;

(define-derived-mode austral-mode fundamental-mode "Austral"
  "Major mode for editing Austral source text."
  (setq font-lock-defaults '((austral-font-lock-keywords)))
  (setq indent-line-function 'austral-indent-line))


(add-to-list 'auto-mode-alist '("\\.aui\\'" . austral-mode))
(add-to-list 'auto-mode-alist '("\\.aum\\'" . austral-mode))

(provide 'austral-mode)
