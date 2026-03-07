;;; daily-obj-tests.el --- Tests for daily-obj.el   -*- lexical-binding: t; -*-

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

;; Tests for daily-obj.el classes and functions

;;; Code:
(require 'daily-obj)
(require 'daily-db)
(require 'ert)

;;; Class Instantiation Tests

(ert-deftest test-daily-obj-create ()
  "Test creating a daily-obj base class instance."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db")))
   (let ((obj (daily-obj :uuid "test-uuid-001")))
     (should (object-of-class-p obj 'daily-obj))
     (should (equal "test-uuid-001" (daily-obj-uuid obj))))))

(ert-deftest test-daily-one-create ()
  "Test creating a daily-one instance with all required fields."
  (let ((one (daily-one :uuid "one-uuid-001"
                        :date "2026-03-07 10:00:00"
                        :text "Test content"
                        :tags nil)))
    (should (object-of-class-p one 'daily-one))
    (should (equal "one-uuid-001" (daily-obj-uuid one)))
    (should (equal "2026-03-07 10:00:00" (daily-one-date one)))
    (should (equal "Test content" (daily-one-text one)))
    (should (equal nil (daily-one-tags one)))))

(ert-deftest test-daily-one-create-with-tags ()
  "Test creating a daily-one instance with tags."
  (let ((tag1 (daily-tag :uuid "tag-uuid-001" :name "work" :one-uuid "one-uuid-001"))
        (tag2 (daily-tag :uuid "tag-uuid-002" :name "personal" :one-uuid "one-uuid-001"))
        (one (daily-one :uuid "one-uuid-001"
                        :date "2026-03-07 10:00:00"
                        :text "Test content"
                        :tags (list tag1 tag2))))
    (should (equal 2 (length (daily-one-tags one))))
    (should (equal "work" (daily-tag-name (nth 0 (daily-one-tags one)))))
    (should (equal "personal" (daily-tag-name (nth 1 (daily-one-tags one)))))))

(ert-deftest test-daily-tag-create ()
  "Test creating a daily-tag instance."
  (let ((tag (daily-tag :uuid "tag-uuid-001" :name "test-tag" :one-uuid "one-uuid-001")))
    (should (object-of-class-p tag 'daily-tag))
    (should (equal "tag-uuid-001" (daily-obj-uuid tag)))
    (should (equal "test-tag" (daily-tag-name tag)))
    (should (equal "one-uuid-001" (daily-tag-one-uuid tag)))))

(ert-deftest test-daily-filter-create ()
  "Test creating a daily-filter instance with default values."
  (let ((filter (daily-filter :page-num 1 :page-size 30)))
    (should (equal 1 (daily-filter-page-num filter)))
    (should (equal 30 (daily-filter-page-size filter)))
    (should (equal 'date (daily-filter-sort filter)))
    (should (eq t (daily-filter-reversed filter)))
    (should (equal nil (daily-filter-date filter)))
    (should (equal nil (daily-filter-text filter)))
    (should (equal nil (daily-filter-tags filter)))))

(ert-deftest test-daily-filter-create-with-all-options ()
  "Test creating a daily-filter instance with all options specified."
  (let ((filter (daily-filter :date '(like date "%test%")
                              :text '(like text "%hello%")
                              :tags '("work" "personal")
                              :sort 'date
                              :reversed nil
                              :page-num 2
                              :page-size 50)))
    (should (equal '(like date "%test%") (daily-filter-date filter)))
    (should (equal '(like text "%hello%") (daily-filter-text filter)))
    (should (equal '("work" "personal") (daily-filter-tags filter)))
    (should (equal 'date (daily-filter-sort filter)))
    (should (eq nil (daily-filter-reversed filter)))
    (should (equal 2 (daily-filter-page-num filter)))
    (should (equal 50 (daily-filter-page-size filter)))))

;;; Writer/Setter Tests

(ert-deftest test-daily-one-write-date ()
  "Test writing/updating the date field of a daily-one object."
  (let ((one (daily-one :uuid "one-uuid-001"
                        :date "2026-03-07"
                        :text "Test"
                        :tags nil)))
    (daily-one-write-date one "2026-03-08")
    (should (equal "2026-03-08" (daily-one-date one)))))

(ert-deftest test-daily-one-write-text ()
  "Test writing/updating the text field of a daily-one object."
  (let ((one (daily-one :uuid "one-uuid-001"
                        :date "2026-03-07"
                        :text "Original text"
                        :tags nil)))
    (daily-one-write-text one "Updated text")
    (should (equal "Updated text" (daily-one-text one)))))

(ert-deftest test-daily-one-write-tags ()
  "Test writing/updating the tags field of a daily-one object."
  (let ((one (daily-one :uuid "one-uuid-001"
                        :date "2026-03-07"
                        :text "Test"
                        :tags nil)))
    (daily-one-write-tags one (list (daily-tag :uuid "t1" :name "new-tag" :one-uuid "one-uuid-001")))
    (should (= 1 (length (daily-one-tags one))))
    (should (equal "new-tag" (daily-tag-name (car (daily-one-tags one))))))

  ;; Test writing nil tags
  (let ((one (daily-one :uuid "one-uuid-001"
                        :date "2026-03-07"
                        :text "Test"
                        :tags (list (daily-tag :uuid "t1" :name "old-tag" :one-uuid "one-uuid-001")))))
    (daily-one-write-tags one nil)
    (should (equal nil (daily-one-tags one)))))

;;; Conversion Tests

(ert-deftest test-daily-obj--to-tag ()
  "Test converting a database row to a daily-tag object."
  (let ((row '("tag-uuid-001" "food" "one-uuid-001")))
    (let ((tag (daily-obj--to-tag row)))
      (should (object-of-class-p tag 'daily-tag))
      (should (equal "tag-uuid-001" (daily-obj-uuid tag)))
      (should (equal "food" (daily-tag-name tag)))
      (should (equal "one-uuid-001" (daily-tag-one-uuid tag))))))

(ert-deftest test-daily-obj--to-tag-with-nil-row ()
  "Test that daily-obj--to-tag returns nil for empty/nil rows."
  (should (equal nil (daily-obj--to-tag nil)))
  (should (equal nil (daily-obj--to-tag '(nil nil nil)))))

(ert-deftest test-daily-obj--to-one ()
  "Test converting database rows to a daily-one object."
  (let ((rows '(("one-uuid-001" "2026-03-07" "Test content" "tag-uuid-001" "food" "one-uuid-001")
                ("one-uuid-001" "2026-03-07" "Test content" "tag-uuid-002" "work" "one-uuid-001"))))
    (let ((one (daily-obj--to-one rows)))
      (should (object-of-class-p one 'daily-one))
      (should (equal "one-uuid-001" (daily-obj-uuid one)))
      (should (equal "2026-03-07" (daily-one-date one)))
      (should (equal "Test content" (daily-one-text one)))
      (should (= 2 (length (daily-one-tags one)))))))

(ert-deftest test-daily-obj--to-one-without-tags ()
  "Test converting database rows to a daily-one object without tags."
  (let ((rows '(("one-uuid-001" "2026-03-07" "Test content" nil nil nil))))
    (let ((one (daily-obj--to-one rows)))
      (should (equal "one-uuid-001" (daily-obj-uuid one)))
      (should (equal 0 (length (daily-one-tags one)))))))

(ert-deftest test-daily-tag--to-plist ()
  "Test converting a daily-tag object to a property list."
  (let ((tag (daily-tag :uuid "tag-uuid-001" :name "food" :one-uuid "one-uuid-001")))
    (let ((plist (daily-tag--to-plist tag)))
      (should (equal '(:uuid "tag-uuid-001" :name "food" :one-uuid "one-uuid-001") plist)))))

(ert-deftest test-daily-obj-to-org ()
  "Test converting a daily-one object to org-mode format."
  (let ((one (daily-one :uuid "one-uuid-001"
                        :date "2026-03-07 10:00:00"
                        :text "This is test content."
                        :tags nil)))
    (let ((org-string (daily-obj-to-org one)))
      (should (string-prefix-p "* 2026-03-07 10:00:00" org-string))
      (should (string-match-p ":ID:       one-uuid-001" org-string))
      (should (string-match-p "This is test content\\." org-string)))))

(ert-deftest test-daily-obj-to-printable-tag ()
  "Test converting a daily-tag object to printable string."
  (let ((tag (daily-tag :uuid "tag-uuid-001" :name "food" :one-uuid "one-uuid-001")))
    (should (equal "food" (daily-obj-to-printable tag)))))

(ert-deftest test-daily-obj-to-printable-one ()
  "Test converting a daily-one object to printable list."
  (let ((one (daily-one :uuid "one-uuid-001"
                        :date "2026-03-07"
                        :text "Test content"
                        :tags (list (daily-tag :uuid "t1" :name "work" :one-uuid "one-uuid-001")
                                    (daily-tag :uuid "t2" :name "life" :one-uuid "one-uuid-001")))))
    (let ((printable (daily-obj-to-printable one)))
      (should (equal "2026-03-07" (nth 0 printable)))
      (should (equal "Test content" (nth 1 printable)))
      (should (equal "work,life" (nth 2 printable)))
      (should (equal "one-uuid-001" (nth 3 printable))))))

(ert-deftest test-daily-obj-to-printable-one-with-newline ()
  "Test that newlines in text are replaced with spaces in printable format."
  (let ((one (daily-one :uuid "one-uuid-001"
                        :date "2026-03-07"
                        :text "Line 1\nLine 2"
                        :tags nil)))
    (should (equal "Line 1 Line 2" (nth 1 (daily-obj-to-printable one))))))

;;; Integration Tests

(ert-deftest test-daily-one-get ()
  "Test retrieving a daily-one object from the database."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (db (emacsql-sqlite-open daily-db-path)))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags
     :uuid "one-uuid-001"
     :text "Integration test content"
     :date "2026-03-07"
     :tags '((:uuid "tag-uuid-001" :name "test" :one-uuid "one-uuid-001")
             (:uuid "tag-uuid-002" :name "integration" :one-uuid "one-uuid-001")))
    (let ((one (daily-one-get "one-uuid-001")))
      (should (object-of-class-p one 'daily-one))
      (should (equal "one-uuid-001" (daily-obj-uuid one)))
      (should (equal "2026-03-07" (daily-one-date one)))
      (should (equal "Integration test content" (daily-one-text one)))
      (should (= 2 (length (daily-one-tags one)))))
    (delete-file daily-db-path)))

(ert-deftest test-daily-one-list ()
  "Test listing daily-one objects with a filter."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (filter (daily-filter :page-num 1 :page-size 10)))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags
     :uuid "one-uuid-001"
     :text "First entry"
     :date "2026-03-07"
     :tags '((:uuid "tag-uuid-001" :name "work" :one-uuid "one-uuid-001")))
    (daily-db-one-insert-or-update-with-tags
     :uuid "one-uuid-002"
     :text "Second entry"
     :date "2026-03-06"
     :tags '((:uuid "tag-uuid-002" :name "personal" :one-uuid "one-uuid-002")))
    (let ((ones (daily-one-list filter)))
      (should (= 2 (length ones)))
      (should (object-of-class-p (nth 0 ones) 'daily-one))
      (should (object-of-class-p (nth 1 ones) 'daily-one)))
    (delete-file daily-db-path)))

(ert-deftest test-daily-one-count ()
  "Test counting daily-one objects."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db")))
    (daily-db-init)
    (should (= 0 (daily-one-count)))
    (daily-db-one-insert-or-update :uuid "one-uuid-001" :text "Test" :date "2026-03-07")
    (should (= 1 (daily-one-count)))
    (daily-db-one-insert-or-update :uuid "one-uuid-002" :text "Test 2" :date "2026-03-06")
    (should (= 2 (daily-one-count)))
    (delete-file daily-db-path)))

(ert-deftest test-daily-one-delete ()
  "Test deleting a daily-one object."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db")))
    (daily-db-init)
    (daily-db-one-insert-or-update-with-tags
     :uuid "one-uuid-001"
     :text "To be deleted"
     :date "2026-03-07"
     :tags '((:uuid "tag-uuid-001" :name "test" :one-uuid "one-uuid-001")))
    (should (= 1 (daily-one-count)))
    (daily-one-delete "one-uuid-001")
    (should (= 0 (daily-one-count)))
    (delete-file daily-db-path)))

(ert-deftest test-daily-one-insert-or-update-method ()
  "Test the daily-one-insert-or-update method with a daily-one object."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (one (daily-one :uuid "one-uuid-001"
                         :date "2026-03-07"
                         :text "Original text"
                         :tags (list (daily-tag :uuid "tag-uuid-001" :name "test" :one-uuid "one-uuid-001")))))
    (daily-db-init)
    ;; Insert
    (daily-one-insert-or-update one)
    (should (= 1 (daily-one-count)))
    (let ((retrieved (daily-one-get "one-uuid-001")))
      (should (equal "Original text" (daily-one-text retrieved)))
      (should (= 1 (length (daily-one-tags retrieved)))))
    ;; Update
    (daily-one-write-text one "Updated text")
    (daily-one-insert-or-update one)
    (let ((retrieved (daily-one-get "one-uuid-001")))
      (should (equal "Updated text" (daily-one-text retrieved))))
    (delete-file daily-db-path)))

(ert-deftest test-daily-filter-writers ()
  "Test writer functions for daily-filter fields."
  (let ((filter (daily-filter :page-num 1 :page-size 30)))
    (daily-filter-write-date filter '(like date "%2026%"))
    (should (equal '(like date "%2026%") (daily-filter-date filter)))
    (daily-filter-write-text filter '(like text "%test%"))
    (should (equal '(like text "%test%") (daily-filter-text filter)))
    (daily-filter-write-tags filter '("tag1" "tag2"))
    (should (equal '("tag1" "tag2") (daily-filter-tags filter)))))

;;; daily-obj-tests.el ends here
