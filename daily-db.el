;;; daily-db.el ---                                  -*- lexical-binding: t; -*-

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

(require 'emacsql)
(require 'emacsql-sqlite)
(require 'cl-lib)

;;; Custom Variables

(defcustom daily-db-path "~/daily.db"
  "Path to the daily database file.")

(defcustom daily-db-limit 100
  "Maximum number of records to keep in the daily database.")

;;; Internal Variables

;;; Internal Functions

(defun daily-db--create-table-one ()
  "Create the one table in the daily database if it does not already exist."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql db [:create-table
                 :if :not :exists one
                 ([(uuid :primary-key) date text (deleted integer :not :null :default 0)])])))

(defun daily-db--create-table-tag ()
  "Create the tag table in the daily database if it does not already exist."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql db [:create-table
                 :if :not :exists tag
                 ([(uuid :primary-key) name one-uuid]
                  (:foreign-key [one-uuid] :references one [uuid] :on-delete :cascade))])))

;;; API Functions
(defun daily-db-init ()
  "Initialize the daily database by creating required tables."
  (daily-db--create-table-one)
  (daily-db--create-table-tag))

(defun daily-db-no-repeat-tag-names ()
  "Return distinct tag names from the tag table, limited by daily-db-limit."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql db [:select
                 :distinct
                 [name]
                 :from tag
                 :limit $s1]
             daily-db-limit)))

(cl-defun daily-db-tag-insert-or-update (&key uuid name one-uuid)
  "Insert a tag record or update the existing one with matching uuid using the provided uuid, name, and one-uuid.
The one-uuid must be contained in the `one` table."
  (unless (and uuid name one-uuid)
    (error "UUID, Name and one-uuid must be provided."))
  (let* ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql db
             [:insert :into tag [uuid name one-uuid]
                      :values [$s1 $s2 $s3]
                      :on :conflict [:uuid]
                      :do :update :set
                      [(= name excluded:name)
                       (= one-uuid excluded:one-uuid)]]
             uuid name one-uuid)))

(cl-defun daily-db-one-insert-or-update (&key uuid text date)
  "Insert a one record or update the existing one with matching uuid using the provided uuid, text, and date."
  (unless (and uuid text date)
    (error "UUID, Text and Date must be provided."))
  (let* ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql db
             [:insert :into one [uuid text date]
                      :values [$s1 $s2 $s3]
                      :on :conflict [:uuid]
                      :do :update :set
                      [(= text excluded:text)
                       (= date excluded:date)]]
             uuid text date)))

(cl-defun daily-db-one-insert-or-update-with-tags (&key uuid text date tags)
  "Insert or update a one record, then replace its associated tags within a single transaction."
  (let* ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql-with-transaction db
      (daily-db-one-insert-or-update :uuid uuid :text text :date date)
      (daily-db-delete-tags uuid)
      (when tags
        (mapc (lambda (tag)
                (daily-db-tag-insert-or-update
                  :uuid (plist-get tag :uuid)
                  :name (plist-get tag :name)
                  :one-uuid uuid))
              tags)))))

(cl-defun daily-db-get-one (uuid)
  "Return the one record with the given uuid and its associated tags, limited by daily-db-limit."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql db [:select
                 [one:uuid date text tag:uuid name one-uuid]
                 :from one
                 :left :join tag
                 :on (= one:uuid tag:one-uuid)
                 :where (= one:uuid $s1)
                 :limit $s2]
             uuid
             daily-db-limit)))

(defun daily-db-delete-tags (one-uuid)
  "Delete all tag records associated with the specified one-uuid."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql db [:delete
                 :from tag
                 :where (= one-uuid $s1)]
             one-uuid)))

(defun daily-db-delete-one (uuid)
  "Mark the one record with the given uuid as deleted by setting its deleted field to 1."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql db [:update
                 one
                 :set (= deleted 1)
                 :where (= uuid $s1)]
             uuid)))

(defun daily-db-delete-one-with-tags (uuid)
  "Mark the one record as deleted and remove all its associated tags within a single transaction."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (emacsql-with-transaction db
      (daily-db-delete-one uuid)
      (daily-db-delete-tags uuid))))

(defun daily-db-list-one-uuid ()
  "Return a list of uuid values for records in one that are not marked as deleted, limited by daily-db-limit."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (mapcar #'car (emacsql db [:select
                               uuid
                               :from one
                               :where (!= deleted 1)
                               :limit $s1]
                           daily-db-limit))))

(defun daily-db-one-count ()
  "Return the count of records in one that are not marked as deleted."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (caar (emacsql db [:select
                       (funcall count 1)
                       :from one
                       :where (!= deleted 1)]))))

(provide 'daily-db)
;;; daily-db.el ends here
