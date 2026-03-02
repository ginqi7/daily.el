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
(defun daily-db--check (name value)
  (unless value
    (error (format "%s must be provided." name))))

(cl-defun daily-db--select-sql-exp (&key columns table clauses offset limit order desc)
  (daily-db--check ":table" table)
  (let ((columns (or columns '(*)))
        (offset (or offset 0))
        (limit (or limit daily-db-limit)))
    (vconcat [:select]
             (list (vconcat columns))
             (list :from table)
             (when clauses (list :where clauses))
             (when order (list :order :by order (when desc :desc)))
             (list :limit (vector offset limit)))))

(cl-defun daily-db--insert-or-update-sql-exp (&key columns table values)
  (daily-db--check ":table" table)
  (daily-db--check ":columns" columns)
  (daily-db--check ":values" values)
  (vconcat (vector :insert :into table)
           (list (vconcat columns))
           (list :values (mapcar #'vconcat values))
           (list :on :conflict (vector (intern (format ":%s"(car columns)))))
           (list :do :update :set)
           (list (vconcat (mapcar
                           (lambda (column) (list '= column (intern (format "excluded:%s" column))))
                           (cdr columns))))))

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
  ""
  (daily-db--check ":uuid" uuid)
  (daily-db--check ":name" name)
  (daily-db--check ":one-uuid" one-uuid)
  (let* ((db (emacsql-sqlite-open daily-db-path))
         (sql-exp (daily-db--insert-or-update-sql-exp
                   :table 'tag
                   :columns '(uuid name one-uuid)
                   :values (list (list uuid name one-uuid)))))
    (emacsql db sql-exp)))

(cl-defun daily-db-one-insert-or-update (&key uuid text date)
  ""
  (daily-db--check ":uuid" uuid)
  (daily-db--check ":text" text)
  (daily-db--check ":date" date)
  (let* ((db (emacsql-sqlite-open daily-db-path))
         (sql-exp (daily-db--insert-or-update-sql-exp
                   :table 'one
                   :columns '(uuid text date)
                   :values (list (list uuid text date)))))
    (emacsql db sql-exp)))

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

(cl-defun daily-db-list-one-uuid (&key offset limit date text tags order desc)
  "Return a list of uuid values for records in one that are not marked as deleted, limited by daily-db-limit."
  (let ((db (emacsql-sqlite-open daily-db-path))
        (offset (or offset 0))
        (limit (min (or limit daily-db-limit) daily-db-limit))
        (where-clause '(!= deleted 1))
        (sql))
    (when date
      (setq where-clause (list 'and where-clause date)))
    (when text
      (setq where-clause (list 'and where-clause text)))
    (when tags
      (setq where-clause (list 'and where-clause tags)))
    (setq sql (daily-db--select-sql-exp
               :table 'one
               :columns '(uuid)
               :clauses where-clause
               :limit limit
               :offset offset
               :order order
               :desc desc))
    ;; (print sql)
    (mapcar #'car (emacsql db sql))))

(defun daily-db-one-count ()
  "Return the count of records in one that are not marked as deleted."
  (let ((db (emacsql-sqlite-open daily-db-path)))
    (caar (emacsql db [:select
                       (funcall count 1)
                       :from one
                       :where (!= deleted 1)]))))

(provide 'daily-db)
;;; daily-db.el ends here
