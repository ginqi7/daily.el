;;; daily-db-tests.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin  <ginqi7@gmail.com>
;; Keywords:

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

;;

;;; Code:
(require 'daily-db)
(require 'ert)

(ert-deftest test-daily-db-tag-insert-or-update ()
  "Test that inserting or updating one with tags writes both tables correctly and updates existing records without duplicating tag data."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags :uuid "000001" :text "Hello World" :date "00001"
                                             :tags '((:uuid "000001" :name "food" :one-uuid "00001")))
    (should (equal '("000001" "00001" "Hello World" 0) (car (emacsql db [:select [*] :from one :where (= uuid "000001")]))))
    (should (equal '("000001" "food" "000001") (car (emacsql db [:select [*] :from tag :where (= uuid "000001")]))))
    (daily-db-one-insert-or-update-with-tags :uuid "000001" :text "Hello World 1" :date "00001"
                                             :tags '((:uuid "000001" :name "food" :one-uuid "00001")))
    (should (equal '("000001" "00001" "Hello World 1" 0) (car (emacsql db [:select [*] :from one :where (= uuid "000001")]))))
    (should (equal '("000001" "food" "000001") (car (emacsql db [:select [*] :from tag :where (= uuid "000001")]))))
    (delete-file daily-db-path)))

;;; daily-db-tests.el ends here
