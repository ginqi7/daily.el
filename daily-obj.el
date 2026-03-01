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
  ((uuid :initarg :uuid :reader daily-obj-uuid))
  "Defines a daily-obj class with a UUID attribute that is set during initialization and accessed using the daily-obj-uuid function.")

(defclass daily-one (daily-obj)
  ((date :initarg :date :reader daily-one-date)
   (text :initarg :text :reader daily-one-text :writer daily-one-write-text)
   (tags :initarg :tags :reader daily-one-tags :writer daily-one-write-tags))
  "Defines the daily-one class, a subclass of daily-obj, with attributes for date, text, and tags. The date attribute is initialized with :date and accessed using daily-one-date. The text attribute is initialized with :text, accessed using daily-one-text, and writable using daily-one-write-text. The tags attribute is initialized with :tags, accessed using daily-one-tags, and writable using daily-one-write-tags.")

(defclass daily-tag (daily-obj)
  ((name :initarg :name :reader daily-tag-name)
   (one-uuid :initarg :one-uuid :reader daily-tag-one-uuid))
  "Defines the daily-tag class as a subclass of daily-obj with attributes for name and one-uuid. The name attribute is initialized with :name and accessed using daily-tag-name. The one-uuid attribute is initialized with :one-uuid and accessed using daily-tag-one-uuid.")

;;; Internal Functions
(defun daily-obj--to-one (rows)
  "Converts a list of rows into a daily-one object by extracting the primary fields (UUID, date, text) from the first row and mapping the remaining rows to daily-tag objects via daily-obj--to-tag, then assembling them as tags for the daily-one."
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
  "Converts a row into a daily-tag object if the first element of the row is non-nil. The function maps the first element to the UUID, the second element to the name, and the third element to the one-uuid attribute."
  (when (nth 0 row)
    (daily-tag
     :uuid (nth 0 row)
     :name (nth 1 row)
     :one-uuid (nth 2 row))))

(cl-defmethod daily-tag--to-plist ((obj daily-tag))
  "Converts a daily-tag object to a property list by mapping the object's UUID, name, and one-uuid attributes to their corresponding keys (:uuid, :name, :one-uuid) in the list."
  (list :uuid
        (daily-obj-uuid obj)
        :name
        (daily-tag-name obj)
        :one-uuid
        (daily-tag-one-uuid obj)))

;;; API Functions
(cl-defmethod daily-obj-to-printable ((obj daily-tag))
  "Converts a daily-tag object into a printable string by formatting its tag name."
  (format "%s" (daily-tag-name obj)))

(cl-defmethod daily-obj-to-printable ((obj daily-one))
  "Converts a daily-one object into a printable list representation using its date, text with newline replaced by space, comma-separated tags, and UUID."
  (list (daily-one-date obj)
        (string-replace "\n" " " (daily-one-text obj))
        (string-join
         (mapcar #'daily-obj-to-printable (daily-one-tags obj))
         ",")
        (daily-obj-uuid obj)))

(defun daily-one-get(uuid)
  "Fetches the daily-one object corresponding to the given UUID by retrieving it from the database and converting it."
  (daily-obj--to-one (daily-db-get-one uuid)))

(defun daily-one-list()
  "Lists all daily-one objects by retrieving their UUIDs from the database and converting each one using daily-one-get."
  (mapcar #'daily-one-get (daily-db-list-one-uuid)))

(defun daily-one-count()
  "Returns the count of daily-one objects by querying the database."
  (daily-db-one-count))

(defun daily-one-delete(uuid)
  "Deletes the daily-one object identified by the UUID along with its associated tags from the database."
  (daily-db-delete-one-with-tags uuid))

(cl-defmethod daily-one-insert-or-update ((obj daily-one))
  "Inserts or updates the daily-one object in the database using its UUID, text, date, and tags (if available)."
  (daily-db-one-insert-or-update-with-tags
   :uuid (daily-obj-uuid obj)
   :text (daily-one-text obj)
   :date (daily-one-date obj)
   :tags (when (slot-boundp obj 'tags)
           (mapcar #'daily-tag--to-plist (daily-one-tags obj)))))

(provide 'daily-obj)
;;; daily-obj.el ends here
