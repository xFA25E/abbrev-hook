;;; abbrev-hook.el --- Define abbrevs hooks for major modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; URL: https://github.com/xFA25E/abbrev-hook
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: abbrev, convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows easy definition of abbrev hooks for major modes.  It
;; defines a separate abbrev table with regexps that support non word characters
;; in abbrev names.

;;;; Usage

;; (abbrev-hook-define
;;  "if" 'js-mode
;;  (tempo-define-template
;;   "js-if"
;;   '("if (" p ") {" n>
;;     r> n>
;;     "}" >)))

;; To define abbrev hooks you can use `abbrev-hook-define' function.  You just
;; need to provide a name, a major mode and a hook that will be called on abbrev
;; expansion.

;; For automatic expansion to work you have to have `abbrev-mode' enabled, of
;; course.  Otherwise, you can use them semi-automatically with `expand-abbrev'
;; (which is usually bind to "C-x '").

;; You can also call hooks by name with completion with `abbrev-hook-call'.

;;;;; Tips

;;;;;; Example configuration

;; (with-eval-after-load 'abbrev-hook
;;   (define-key global-map "\C-z" 'abbrev-hook-call))

;; (with-eval-after-load 'lisp-mode
;;   (require 'abbrev-hook)
;;   (require 'tempo)
;;   (abbrev-hook-define
;;    "let" 'lisp-mode
;;    (tempo-define-template
;;     "lisp-let"
;;     '("(let (" p ")" n>
;;       r> ")"))))

;;;;;; Solutions for problems with abbrev expansion in Lisp modes

;; Lisp languages are notoriuos for having non-word charcters in their symbols
;; and abbrev is not very well versed in the domain of non-word characters.

;; abbrev-hook tries to mitigate this problem by adding a custom regexp for its
;; abbrev tables and solves the issue of having non-word characters inside the
;; symbols.  The following are the solutions for a problem of having non-word
;; characters at the end of the symbols.

;;;;;;; Disable expansion on non space characters for Lisp modes

;; This solution will enable automatic expansion only on space or explicit call
;; to `expand-abbrev'.  I'm, personally, using this one.  Works pretty well.

;; (cl-flet ((enable () (or (eq this-command 'expand-abbrev)
;;                          (eql ?\s last-command-event))))
;;   (dolist (mode '(lisp-mode emacs-lisp-mode))
;;     (let ((table (abbrev-hook-abbrev-table mode)))
;;       (define-abbrev-table table nil nil :enable-function #'enable))))

;;;;;;; Modify syntax for some characters to make them word characters

;; This solution will treat "some-long-symbol*" as a single word.  You can
;; change only "*" character, but in that case "let-" will trigger "let".

;; (defun modify-lisp-syntax-tables ()
;;   (modify-syntax-entry ?* "w" (syntax-table))
;;   (modify-syntax-entry ?- "w" (syntax-table)))

;; (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
;;   (add-hook hook #'modify-lisp-syntax-tables))

;;;;;;; Bind expand-abbrev on some key

;; See the last section of this link:
;; https://www.emacswiki.org/emacs/AbbrevMode#h5o-11

;;;;;; Definition for multiple mode

;; Since `abbrev-hook-define' is a function, you can do it like this.

;; (dolist (mode '(js-mode c-mode php-mode))
;;   (abbrev-hook-define "if" mode
;;     '("if (" p ") {" n>
;;       r> n>
;;       "}" >)))

;; This is very stupid and simple, but effective.

;;;; Installation

;;;;; Package manager

;; If you've installed it with your package manager, you're done.

;;;;; Manual

;; Put this file in your load-path, and put the following in a file where you
;; want to define abbrev hooks:

;; (require 'abbrev-hook)

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'derived)
(require 'subr-x)

;;;; Commands

(defun ah-call ()
  "Call abbrev-hook hook with completion for current mode."
  (interactive)
  (let ((templates (ah-templates)))
    (if (hash-table-empty-p templates)
        (message "No abbrev-hook hook defined for current mode.")
      (let ((abbrev (completing-read "Template: " templates nil t)))
        (funcall (gethash abbrev templates))))))

;;;; Functions

;;;;; Public

(defun ah-define (name mode hook)
  "Define abbrev-hook hook.
NAME is an abbrev name as in `define-abbrev'.  MODE can be a
symbol of some major mode or nil.  If MODE is nil, a global
template will be defined.  HOOK is an abbrev hook as in
`define-abbrev' and must be a symbol."
  (let ((mode-abbrev-table (ah-mode-abbrev-table mode))
        (abbrev-table (ah-abbrev-table mode)))

    (put hook 'no-self-insert t)
    (put hook 'abbrev-hook-table abbrev-table)

    (define-abbrev-table mode-abbrev-table nil)
    (define-abbrev-table abbrev-table
      `((,name "" ,hook :case-fixed t :system t :abbrev-hook t))
      :case-fixed t :abbrev-hook t)

    (ah-update-table-regexp abbrev-table)
    (ah-set-table-parent mode-abbrev-table abbrev-table)

    hook))

(defun ah-mode-abbrev-table (mode)
  "Get abbrev table for MODE.
Return `global-abbrev-table' if MODE is nil."
  (if mode
      (derived-mode-abbrev-table-name mode)
    'global-abbrev-table))

(defun ah-abbrev-table (mode)
  "Get abbrev table for templates for MODE."
  (intern (concat "abbrev-hook-" (symbol-name (ah-mode-abbrev-table mode)))))

(defun ah-update-table-regexp (table)
  "Update regexp of abbrev table TABLE.
TABLE is a symbol.  The regexp is a combination of all names in
TABLE, so we can match non-word characters in names."
  (let* ((names (ah-abbrev-table-names table))
         (regexp (concat (regexp-opt names "\\_<\\(") " *")))
    (abbrev-table-put (symbol-value table) :regexp regexp)))

(defun ah-abbrev-table-names (table)
  "Return all names in abbrev table TABLE.
TABLE is a symbol."
  (let ((names nil))
    (mapatoms (lambda (abbrev)
                (when (symbol-value abbrev)
                  (push (symbol-name abbrev) names)))
              (symbol-value table))
    names))

(defun ah-set-table-parent (table parent)
  "Set PARENT as on of the parents of abbrev table TABLE.
TABLE is a symbol."
  (let ((parents (abbrev-table-get (symbol-value table) :parents)))
    (cl-pushnew (symbol-value parent) parents :test #'eq)
    (abbrev-table-put (symbol-value table) :parents parents)))

(defun ah-templates ()
  "Gather all abbrev-hook templates for current mode.
Return a hash table where keys are abbrev names and values are
abbrev hooks."
  (let ((templates (make-hash-table :test 'equal))
        (tables (abbrev--active-tables)))
    (while tables
      (let ((table (pop tables)))
        (when (abbrev-table-get table :abbrev-hook)
          (mapatoms
           (lambda (abbrev)
             (when (symbol-value abbrev)
               (let ((name (symbol-name abbrev)))
                 (unless (gethash name templates)
                   (puthash name (symbol-function abbrev) templates)))))
           table))
        (setq tables (append (abbrev-table-get table :parents) tables))))
    templates))

;;;; Footer

(provide 'abbrev-hook)

;; Local Variables:
;; read-symbol-shorthands: (("ah-" . "abbrev-hook-"))
;; End:

;;; abbrev-hook.el ends here
