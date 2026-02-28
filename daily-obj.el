;;; daily-obj.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
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

;;; Classes
(defclass daily-obj ()
  ((uuid :initarg :uuid :reader daily-obj-uuid)))

(defclass daily-one (daily-obj)
  ((date :initarg :date :reader daily-one-date)
   (text :initarg :text :reader daily-one-text :writer daily-one-write-text)
   (tags :initarg :tags :reader daily-one-tags :writer daily-one-write-tags)))

(defclass daily-tag (daily-obj)
  ((name :initarg :name :reader daily-tag-name)
   (one-uuid :initarg :one-uuid :reader daily-tag-one-uuid)))

;;; Internal Functions
(defun daily-obj--to-one (rows)
  (let* ((tag-row-length (length (eieio-class-slots 'daily-tag)))
         (one-row-length (length (eieio-class-slots 'daily-one)))
         (one-row (seq-take (first rows) one-row-length))
         (tag-rows (remove nil (mapcar (lambda (row) (remove nil (last row tag-row-length))) rows))))
    (daily-one
     :uuid (nth 0 one-row)
     :date (nth 1 one-row)
     :text (nth 2 one-row)
     :tags (mapcar #'daily-obj--to-tag tag-rows))))

(defun daily-obj--to-tag (row)
  (when (nth 0 row)
    (daily-tag
     :uuid (nth 0 row)
     :name (nth 1 row)
     :one-uuid (nth 2 row))))

(cl-defmethod daily-tag--to-plist ((obj daily-tag))
  (list :uuid
        (daily-obj-uuid obj)
        :name
        (daily-tag-name obj)
        :one-uuid
        (daily-tag-one-uuid obj)))

;;; API Functions
(defun daily-one-get(uuid)
  (daily-obj--to-one (daily-db-get-one uuid)))

(defun daily-one-list()
  (mapcar #'daily-one-get (daily-db-list-one-uuid)))

(defun daily-one-count()
  (daily-db-one-count))

(defun daily-one-delete(uuid)
  (daily-db-delete-one-with-tags uuid))

(cl-defmethod daily-one-insert-or-update ((obj daily-one))
  (daily-db-one-insert-or-update-with-tags
   :uuid (daily-obj-uuid obj)
   :text (daily-one-text obj)
   :date (daily-one-date obj)
   :tags (when (slot-boundp obj 'tags)
           (mapcar #'daily-tag--to-plist (daily-one-tags obj)))))

(provide 'daily-obj)
;;; daily-obj.el ends here
