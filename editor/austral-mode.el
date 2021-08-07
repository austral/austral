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

(require 'cl-lib)

;;;; Constants
;;;;

(defconst austral-keywords
  '("and" "or" "not" "module" "is" "body" "import" "as" "end" "constant" "type"
    "function" "generic" "record" "union" "case" "of" "when" "interface"
    "implementation" "method" "if" "then" "else" "let" "while" "for" "do" "from"
    "to" "borrow" "borrow!" "in" "return" "skip" "Free" "Linear" "Type" "Region"
    "pragma" "nil" "true" "false"))

(defconst austral-default-tab-width 4)

;;;; Syntax Highlighting
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

;;;; Indentation
;;;;

(defun austral-indent-line ()
  "Indent the current line of Austral code."
  ;; From: https://www.emacswiki.org/emacs/ModeTutorial
  ;; This is very much incorrect but works well enough.
  (let ((dedent-regex "^[ \t]*\\(end case\\)")
        (indent-regex "^[ \t]*\\(module\\|record\\|union\\|function\\|interface\\|implementation\\|if\\|for\\|while\\|borrow\\)"))
    (beginning-of-line)
    (if (bobp)
        (indent-line-to 0)
      (let ((not-indented t) cur-indent)
        (if (looking-at dedent-regex)
            (progn
              (save-excursion
                (forward-line -1)
                (setq cur-indent (- (current-indentation) austral-default-tab-width)))
              (if (< cur-indent 0)
                  (setq cur-indent 0)))
          (save-excursion
            (while not-indented
              (forward-line -1)
              (if (looking-at dedent-regex)
                  (progn
                    (setq cur-indent (current-indentation))
                    (setq not-indented nil))
                (if (looking-at indent-regex)
                    (progn
                      (setq cur-indent (+ (current-indentation) austral-default-tab-width))
                      (setq not-indented nil))
                  (if (bobp)
                      (setq not-indented nil)))))))
        (if cur-indent
            (indent-line-to cur-indent)
          (indent-line-to 0))))))

;;;; Mode Definition
;;;;

(define-derived-mode austral-mode fundamental-mode "Austral"
  "Major mode for editing Austral source text."
  (setq font-lock-defaults '((austral-font-lock-keywords)))
  (setq indent-line-function 'austral-indent-line))

(add-to-list 'auto-mode-alist '("\\.aui\\'" . austral-mode))
(add-to-list 'auto-mode-alist '("\\.aum\\'" . austral-mode))

(provide 'austral-mode)
