;;; daily-ui.el ---                                  -*- lexical-binding: t; -*-

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

(require 'vui)
(require 'daily-obj)

(defvar daily-ui--component nil)

(defun daily-ui--filter (page-count count)
  (vui-vstack
   (vui-hstack
    (vui-heading "Page:")
    (vui-button (format "%s" (daily-filter-page-num daily--filter))
      :on-click #'daily-set-filter-page-num)
    (vui-heading "| Page Size:")
    (vui-button (format "%s" (daily-filter-page-size daily--filter))
      :on-click #'daily-set-filter-page-size)
    (vui-heading (format "Page Count: %s | Count: %s" page-count count)))
   (vui-hstack
    (vui-heading "Order by:")
    (vui-button (format "%s" (daily-filter-sort daily--filter))
      :on-click #'daily-set-filter-sort))
   (vui-hstack
    (vui-heading "Filters: Data:")
    (vui-button (daily--filter-exp-to-str (daily-filter-date daily--filter))
      :on-click #'daily-set-filter-date)
    (vui-heading "| Text:")
    (vui-button (daily--filter-exp-to-str (daily-filter-text daily--filter))
      :on-click #'daily-set-filter-text)
    (vui-heading "| Tags:")
    (vui-button (daily--filter-exp-to-str (daily-filter-tags daily--filter))
      :on-click #'daily-set-filter-tags))))

(vui-defcomponent vui-daily (title current-page page-count count data)
  :render
  (let* ((title (format "Daily Text"))
         (keys (concat "[SPC] view, [RET] open, [a] add, [e] edit, [d] delete, [g] accumulate, [+] more, [q] quit\n"
                       "[f] filter"))
         (eq-char ?═)
         (dash-char ?─)
         (eq-line (make-string (daily-ui--dashboard-width) eq-char))
         (dash-line (make-string (daily-ui--dashboard-width) dash-char))
         (date-length (length (format-time-string daily-time-format)))
         (tags-length 20)
         (text-length (- (daily-ui--dashboard-width) date-length tags-length)))
    (vui-vstack
     (vui-heading title)
     (vui-heading eq-line)
     (vui-heading-1 keys)
     (vui-text dash-line)
     (daily-ui--filter page-count count)
     (vui-table
      :columns (list (list :header "")
                     (list :width date-length :header (buttonize "Date" nil))
                     (list :width text-length :truncate t :header (buttonize "Text" nil))
                     (list :width tags-length :header (buttonize "Tags" nil)))
      :rows (mapcar #'daily-obj-to-row data)
      :border :unicode))))

(cl-defmethod daily-obj-to-row ((obj daily-one))
  (append
   (list (vui-checkbox :checked (member-if (lambda (one) (daily-one-equal one obj)) daily--selected-list)
                       :on-change (lambda (v)
                                    (if v (add-to-list 'daily--selected-list obj)
                                      (setq daily--selected-list
                                            (cl-delete-if (lambda (one) (daily-one-equal one obj)) daily--selected-list)))
                                    (daily-refresh))))
   (daily-obj-to-printable obj)))

(defun daily-ui--dashboard-width ()
  "Calculates the usable dashboard width by subtracting the left and right window margins from the total window width."
  (let ((margins (window-margins)))
    (- (window-width) (or (car margins) 0) (or (cdr margins) 0))))

(defun daily-ui-render (&rest props)
  (let ((vui-width-mode 'pixel))
    (if daily-ui--component
        (vui-update-props daily-ui--component props)
      (setq daily-ui--component
            (vui-mount (apply #'vui-component (append '(vui-daily) props))
                       daily--buffer-name)))))

(provide 'daily-ui)
;;; daily-ui.el ends here
