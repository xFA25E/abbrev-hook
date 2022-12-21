;;; abbrev-hook-test.el --- Tests for abbrev-hook  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Valeriy Litkovskyy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for abbrev-hook

;;; Code:

(require 'ert)
(require 'abbrev-hook)

(ert-deftest abbrev-hook-test-mode-abbrev-table ()
  (should (eq (abbrev-hook-mode-abbrev-table 'lisp-mode)
              'lisp-mode-abbrev-table))
  (should (eq (abbrev-hook-mode-abbrev-table nil)
              'global-abbrev-table)))

(ert-deftest abbrev-hook-test-abbrev-table ()
  (should (eq (abbrev-hook-abbrev-table 'lisp-mode)
              'abbrev-hook-lisp-mode-abbrev-table))
  (should (eq (abbrev-hook-abbrev-table nil)
              'abbrev-hook-global-abbrev-table)))

(ert-deftest abbrev-hook-test-abbrev-table-names ()
  (let ((table 'abbrev-hook-test-table))
    (define-abbrev-table table
      '(("hello" "world")
        ("test1" "test2")
        ("test3" "test4")))
    (should-not
     (cl-set-exclusive-or
      (abbrev-hook-abbrev-table-names table)
      '("hello" "test1" "test3")
      :test #'string=))))

(ert-deftest abbrev-hook-test-update-table-regexp ()
  (let ((table 'abbrev-hook-test-update-regexp))
    (define-abbrev-table table
      '(("hello" "world")
        ("test1" "test2")
        ("test3" "test4")))
    (abbrev-hook-update-table-regexp table)
    (let ((regexp (abbrev-table-get (symbol-value table) :regexp)))
      (cl-every
       (apply-partially #'string-match-p regexp)
       (abbrev-hook-abbrev-table-names table)))))

(ert-deftest abbrev-hook-test-define-hook ()
  (let* ((mode 'test-define-hook-mode)
         (hook 'abbrev-hook-test-define-hook)
         (name "test-define-hook")
         (mode-table (abbrev-hook-mode-abbrev-table mode))
         (table (abbrev-hook-abbrev-table mode)))
    (should (eq (abbrev-hook-define name mode hook) hook))
    (should (get hook 'no-self-insert))
    (should (boundp mode-table))
    (should (boundp table))
    (should (abbrev-table-get (symbol-value table) :case-fixed))
    (should (abbrev-table-get (symbol-value table) :abbrev-hook))
    (let ((abbrev (obarray-get (symbol-value table) name)))
      (should abbrev)
      (should (abbrev-get abbrev :case-fixed))
      (should (abbrev-get abbrev :system))
      (should (abbrev-get abbrev :abbrev-hook))
      (should (equal (symbol-value abbrev) ""))
      (should (eq (symbol-function abbrev) hook)))
    (should (equal (abbrev-table-get (symbol-value table) :regexp)
                   (concat (regexp-opt (list name) "\\_<\\(") " *")))
    (should (memq (symbol-value table)
                  (abbrev-table-get (symbol-value mode-table) :parents)))))

(ert-deftest abbrev-hook-test-templates ()
  (abbrev-hook-define "test1" 'prog-mode 'test-templates-1)
  (abbrev-hook-define "test2" 'prog-mode 'test-templates-2)
  (abbrev-hook-define "test1" nil 'test-1)
  (abbrev-hook-define "test3" nil 'test-3)

  (with-temp-buffer
    (prog-mode)
    (let ((templates (abbrev-hook-templates)))
      (should-not
       (cl-set-exclusive-or
        (hash-table-keys templates)
        '("test1" "test2" "test3")
        :test #'string=))
      (should (eq (gethash "test1" templates) 'test-templates-1))
      (should (eq (gethash "test2" templates) 'test-templates-2))
      (should (eq (gethash "test3" templates) 'test-3)))))

(ert-deftest abbrev-hook-test-call ()
  (let* ((value (gensym))
         (fn (lambda (&rest _) value))
         (hook 'test-call-hook))
    (fset hook fn)
    (abbrev-hook-define "hello" 'fundamental-mode hook)
    (with-temp-buffer
      (fundamental-mode)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "hello")))
        (should (eq value (abbrev-hook-call)))))))

(provide 'abbrev-hook-test)
;;; abbrev-hook-test.el ends here
