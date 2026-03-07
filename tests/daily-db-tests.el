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

(ert-deftest test-daily-db-one-insert-or-update ()
  "Test inserting and updating records in the one table."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    ;; Insert a new record
    (daily-db-one-insert-or-update :uuid "000001" :text "First text" :date "20260307")
    (should (equal '("000001" "20260307" "First text" 0)
                   (car (emacsql db [:select [*] :from one :where (= uuid "000001")]))))
    ;; Update the same record
    (daily-db-one-insert-or-update :uuid "000001" :text "Updated text" :date "20260307")
    (should (equal '("000001" "20260307" "Updated text" 0)
                   (car (emacsql db [:select [*] :from one :where (= uuid "000001")]))))
    ;; Insert another record
    (daily-db-one-insert-or-update :uuid "000002" :text "Second text" :date "20260306")
    (should (= 2 (caar (emacsql db [:select (funcall count 1) :from one]))))
    (delete-file daily-db-path)))

(ert-deftest test-daily-db-tag-insert-or-update-multiple-tags ()
  "Test inserting multiple tags for a single one record."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags
     :uuid "000001" :text "Multi-tag test" :date "20260307"
     :tags '((:uuid "t00001" :name "food" :one-uuid "000001")
             (:uuid "t00002" :name "work" :one-uuid "000001")
             (:uuid "t00003" :name "personal" :one-uuid "000001")))
    (should (equal '("000001" "20260307" "Multi-tag test" 0)
                   (car (emacsql db [:select [*] :from one :where (= uuid "000001")]))))
    (should (= 3 (length (emacsql db [:select [*] :from tag :where (= one-uuid "000001")]))))
    (delete-file daily-db-path)))

(ert-deftest test-daily-db-delete-tags ()
  "Test deleting all tags associated with a one record."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags
     :uuid "000001" :text "Tag test" :date "20260307"
     :tags '((:uuid "t00001" :name "food" :one-uuid "000001")
             (:uuid "t00002" :name "work" :one-uuid "000001")))
    (should (= 2 (length (emacsql db [:select [*] :from tag :where (= one-uuid "000001")]))))
    (daily-db-delete-tags "000001")
    (should (= 0 (length (emacsql db [:select [*] :from tag :where (= one-uuid "000001")]))))
    (should (= 1 (length (emacsql db [:select [*] :from one :where (= uuid "000001")]))))
    (delete-file daily-db-path)))

(ert-deftest test-daily-db-delete-one ()
  "Test soft deleting a one record by setting deleted to 1."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (daily-db-one-insert-or-update :uuid "000001" :text "To delete" :date "20260307")
    (should (equal '("000001" "20260307" "To delete" 0)
                   (car (emacsql db [:select [*] :from one :where (= uuid "000001")]))))
    (daily-db-delete-one "000001")
    (should (equal '("000001" "20260307" "To delete" 1)
                   (car (emacsql db [:select [*] :from one :where (= uuid "000001")]))))
    (delete-file daily-db-path)))

(ert-deftest test-daily-db-delete-one-with-tags ()
  "Test soft deleting a one record and removing all associated tags."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags
     :uuid "000001" :text "Delete with tags" :date "20260307"
     :tags '((:uuid "t00001" :name "food" :one-uuid "000001")
             (:uuid "t00002" :name "work" :one-uuid "000001")))
    (should (= 2 (length (emacsql db [:select [*] :from tag :where (= one-uuid "000001")]))))
    (daily-db-delete-one-with-tags "000001")
    (should (= 0 (length (emacsql db [:select [*] :from tag :where (= one-uuid "000001")]))))
    (should (equal '("000001" "20260307" "Delete with tags" 1)
                   (car (emacsql db [:select [*] :from one :where (= uuid "000001")]))))
    (delete-file daily-db-path)))

(ert-deftest test-daily-db-get-one ()
  "Test retrieving a one record with its associated tags."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags
     :uuid "000001" :text "Get test" :date "20260307"
     :tags '((:uuid "t00001" :name "food" :one-uuid "000001")
             (:uuid "t00002" :name "work" :one-uuid "000001")))
    (let ((result (daily-db-get-one "000001")))
      (should (= 2 (length result)))
      (should (equal "000001" (caar result)))
      (should (equal "20260307" (cadr (car result)))))
    (delete-file daily-db-path)))

(ert-deftest test-daily-db-list-one-uuid ()
  "Test listing uuids from one table with filtering options."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags
     :uuid "000001" :text "First" :date "20260307"
     :tags '((:uuid "t00001" :name "food" :one-uuid "000001")))
    (daily-db-one-insert-or-update-with-tags
     :uuid "000002" :text "Second" :date "20260306"
     :tags '((:uuid "t00002" :name "work" :one-uuid "000002")))
    (daily-db-one-insert-or-update-with-tags
     :uuid "000003" :text "Third" :date "20260307"
     :tags '((:uuid "t00003" :name "food" :one-uuid "000003")))
    ;; Test list all (not deleted)
    (should (= 3 (length (daily-db-list-one-uuid))))
    ;; Test list with limit
    (should (= 2 (length (daily-db-list-one-uuid :limit 2))))
    ;; Test list with offset
    (should (= 1 (length (daily-db-list-one-uuid :offset 2))))
    ;; Test list filtered by tag name
    (should (= 2 (length (daily-db-list-one-uuid :tags '("food")))))
    (delete-file daily-db-path)))

(ert-deftest test-daily-db-one-count ()
  "Test counting non-deleted records in the one table."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (should (= 0 (daily-db-one-count)))
    (daily-db-one-insert-or-update :uuid "000001" :text "First" :date "20260307")
    (daily-db-one-insert-or-update :uuid "000002" :text "Second" :date "20260306")
    (should (= 2 (daily-db-one-count)))
    (daily-db-delete-one "000001")
    (should (= 1 (daily-db-one-count)))
    (delete-file daily-db-path)))

(ert-deftest test-daily-db-no-repeat-tag-names ()
  "Test getting distinct tag names."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags
     :uuid "000001" :text "First" :date "20260307"
     :tags '((:uuid "t00001" :name "food" :one-uuid "000001")
             (:uuid "t00002" :name "work" :one-uuid "000001")))
    (daily-db-one-insert-or-update-with-tags
     :uuid "000002" :text "Second" :date "20260306"
     :tags '((:uuid "t00003" :name "food" :one-uuid "000002")
             (:uuid "t00004" :name "personal" :one-uuid "000002")))
    (let ((names (mapcar #'car (daily-db-no-repeat-tag-names))))
      (should (= 3 (length names)))
      (should (member "food" names))
      (should (member "work" names))
      (should (member "personal" names)))
    (delete-file daily-db-path)))

;;; daily-db-tests.el ends here
