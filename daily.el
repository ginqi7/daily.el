;;; daily.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

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

(require 'ctable)
(require 'daily-obj)

;;; Custom Variables
(defcustom daily-time-format "%Y-%m-%d %H:%M:%S"
  "")

(defcustom daily-page-size 30
  "")

;;; Internal Variables

(defvar-local daily--ctable-component nil)
(defvar-local daily--current-page 1)

(defvar daily--buffer-name "*daily*")
(defvar daily--keymap
  (let ((map (copy-keymap ctbl:table-mode-map)))
    (define-key map (kbd "a") #'daily-add)
    (define-key map (kbd "e") #'daily-edit)
    (define-key map (kbd "d") #'daily-delete)
    map))

;;; Internal Functions

(defun daily--dashboard-width ()
  (let ((margins (window-margins)))
    (- (window-width) (car margins) (cdr margins))))

(cl-defmethod daily--to-ctable-data ((obj daily-tag))
  (format "%s" (daily-tag-name obj)))

(cl-defmethod daily--to-ctable-data ((obj daily-one))
  (list (daily-one-date obj)
        (string-replace
         "\n"
         " "
         (daily-one-text obj))
        (string-join
         (mapcar #'daily--to-ctable-data
                 (daily-one-tags obj))
         ",")
        (daily-obj-uuid obj)))

(defun daily--uuid ()
  "Return string with random (version 4) UUID."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                   (random)
                   (org-time-convert-to-list nil)
                   (user-uid)
                   (emacs-pid)
                   (user-full-name)
                   user-mail-address
                   (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
        (substring rnd 0 8)
        (substring rnd 8 12)
        (substring rnd 13 16)
        (format "%x"
            (logior
                 #b10000000
                 (logand
                      #b10111111
                      (string-to-number
                           (substring rnd 16 18) 16))))
        (substring rnd 18 20)
        (substring rnd 20 32))))

;;; Interactive Functions
(defun daily-add ()
  ""
  (interactive)
  (let* ((one-uuid (daily--uuid))
         (one (daily-one
               :uuid one-uuid
               :date (format-time-string daily-time-format)
               :text (read-string "Input Text: ")
               :tags (mapcar (lambda (tag)
                               (daily-tag
                                :uuid (daily--uuid)
                                :name tag
                                :one-uuid one-uuid))
                             (completing-read-multiple "Add Tags:" (daily-db-no-repeat-tag-names))))))

    (daily-one-insert-or-update one)
    (daily-refresh)))

(defun daily-delete ()
  ""
  (interactive)
  (let* ((row (ctbl:cp-get-selected-data-row (ctbl:cp-get-component)))
         (uuid (car (last row)))
         (date (car row))
         (text (cadr row)))
    (when (yes-or-no-p (format "Are you sure to delete the row? [%s](%s)" date text))
      (daily-one-delete uuid)
      (daily-refresh))))

(defun daily-tag-edit ()
  ""
  (interactive)
  (let* ((row (ctbl:cp-get-selected-data-row (ctbl:cp-get-component)))
         (uuid (car (last row)))
         (one (daily-one-get uuid))
         (tags (string-join (mapcar #'daily-tag-name (daily-one-tags one)) ",")))
    (daily-one-write-tags
     one
     (mapcar (lambda (tag)
               (daily-tag
                :uuid (daily--uuid)
                :name tag
                :one-uuid uuid))
             (completing-read-multiple "Add Tags:"
                                       (daily-db-no-repeat-tag-names)
                                       nil
                                       nil
                                       tags)))
    (daily-one-insert-or-update one)
    (daily-refresh)))

(defun daily-edit ()
  ""
  (interactive)
  (let* ((row (ctbl:cp-get-selected-data-row (ctbl:cp-get-component)))
         (uuid (car (last row)))
         (one (daily-one-get uuid)))
    (daily-one-write-text one (read-string "Update: " (daily-one-text one)))
    (daily-one-insert-or-update one)
    (daily-refresh)))

(defun daily ()
  ""
  (interactive)
  (daily-db-init)
  (daily-refresh)
  (switch-to-buffer daily--buffer-name))

(defun daily-insert-dashboard-text ()
  (let* ((count (daily-one-count))
         (page-count (1+ (/ count daily-page-size)))
         (title (format "Daily Text | Total: [%d] | Page: %d/%d  \n" count daily--current-page page-count))
         (keys (concat "[SPC] view, [RET] open, [a] add, [e] edit, [d] delete, [g] refresh, [+] more, [q] quit\n"
                       "[t] filter tag\n"))
         (eq-char ?═)
         (dash-char ?─)
         (eq-line (concat (make-string (daily--dashboard-width) eq-char) "\n"))
         (dash-line (concat (make-string (daily--dashboard-width) dash-char) "\n")))
   (insert (propertize title 'tiles-header t)
           (propertize eq-line 'face 'font-lock-comment-face 'tiles-header t)
           (propertize keys 'face 'font-lock-comment-face 'tiles-header t)
           ;; (propertize status-line 'face 'font-lock-comment-face 'tiles-header t)
           (propertize dash-line 'face 'font-lock-comment-face 'tiles-header t))))

(defun daily-refresh ()
  (let* ((param (copy-ctbl:param ctbl:default-rendering-param))
         (date-length (length (format-time-string daily-time-format)))
         (tags-length 20)
         (text-length (- (daily--dashboard-width) date-length tags-length))
         (column-model ; column model
          (list (make-ctbl:cmodel
                 :title "Date" :sorter 'ctbl:sort-string-lessp
                 :align 'right
                 :min-width date-length :max-width date-length)
                (make-ctbl:cmodel
                 :title "Text" :align 'center
                 :min-width text-length :max-width date-length)
                (make-ctbl:cmodel
                 :title "Tags" :align 'left
                 :min-width tags-length :max-width date-length)))
         (data (mapcar #'daily--to-ctable-data (daily-one-list)))
         (model (make-ctbl:model :column-model column-model :data data)))
    (with-current-buffer (get-buffer-create daily--buffer-name)
      (let ((buffer-read-only nil))
        (setf (ctbl:param-display-header param) nil)
        (unless daily--ctable-component
          (erase-buffer)
          (daily-insert-dashboard-text)
          (setq-local daily--ctable-component (ctbl:create-table-component-region :model model :param param :keymap daily--keymap))
          (ctbl:cp-add-click-hook daily--ctable-component (lambda () )))
        (ctbl:cp-set-model daily--ctable-component model))
      (setq-local buffer-read-only t))))

(provide 'daily)
;;; daily.el ends here
