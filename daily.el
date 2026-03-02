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
  "Format string used to display date and time values in daily.")

(defcustom daily-page-size 30
  "Number of entries to load per page in daily lists.")

;;; Internal Variables

(defvar-local daily--ctable-component nil
  "Buffer-local ctable component instance used to render and manage the daily list view.")

(defvar-local daily--current-page 1
  "Current page number for paginated daily list data in the current buffer")

(defvar-local daily--current-one nil
  "Local variable to store the current daily entry for the buffer.")

(defvar daily--buffer-name "*daily*"
  "Name of the buffer used for daily's main interface.")

(defvar daily--text-buffer-name "*daily-text*"
  "Name of the buffer designated for daily text operations.")

(defvar daily--keymap
  (let ((map (copy-keymap ctbl:table-mode-map)))
    (define-key map (kbd "a") #'daily-add)
    (define-key map (kbd "e") #'daily-edit)
    (define-key map (kbd "d") #'daily-delete)
    (define-key map (kbd "<RET>") #'daily-show)
    (define-key map (kbd "<SPC>") #'daily-preview)
    map)
  "Keymap defining daily commands")

;;; Internal Functions
(defun daily--show-one (one)
  "Displays the content of a given daily entry in the daily text buffer. The function creates or switches to the designated text buffer, enables org mode, sets the current daily entry as a local variable, configures the header line with the entry’s UUID and date, clears the buffer, and inserts the entry's text for editing."
  (with-current-buffer (get-buffer-create daily--text-buffer-name)
    (org-mode)
    (setq-local daily--current-one one)
    (setq header-line-format (format "  [%s][%s]: Press 'C-c C-c' to submit your modifications."
                                     (daily-obj-uuid one)
                                     (daily-one-date one)))
    (erase-buffer)
    (insert (daily-one-text one))
    (daily-text-mode)))

(defun daily--dashboard-width ()
  "Calculates the usable dashboard width by subtracting the left and right window margins from the total window width."
  (let ((margins (window-margins)))
    (- (window-width) (or (car margins) 0) (or (cdr margins) 0))))

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

(cl-defun daily--edit-one (one &key date text tags)
  "Updates a daily entry's components based on provided keyword arguments. If a date is specified, the entry's date is updated; if text is specified, the entry's text is updated; and if tags are specified, the entry's tags are updated."
  (when date
    (daily-one-write-date one date))
  (when text
    (daily-one-write-text one text))
  (when tags
   (daily-one-write-tags one tags)))

(defun daily--edit-one-date (one)
  "Prompts the user to input a new date for the daily entry and updates the entry with the provided value."
  (daily-one-write-date one (read-string "Edit Date: " (daily-one-date one))))

(defun daily--edit-one-text (one)
  "Prompts the user to input new text for the daily entry and updates the entry with the provided value."
  (daily-one-write-text one (read-string "Edit Text: " (daily-one-text one))))

(defun daily--edit-one-tags (one)
  "Prompts the user to input tag names via a multiple-selection completion interface, maps each tag name to a tag object with a generated UUID and the current daily entry's UUID, and updates the entry's tags accordingly."
  (daily-one-write-tags
   one
   (mapcar (lambda (tag)
             (daily-tag
              :uuid (daily--uuid)
              :name tag
              :one-uuid (daily-obj-uuid one)))
           (completing-read-multiple "Edit Tags: "
                                     (daily-db-no-repeat-tag-names)
                                     nil
                                     nil
                                     (string-join (mapcar #'daily-tag-name (daily-one-tags one)) ",")))))

;;; Interactive Functions
(defun daily-show ()
  "Interactively retrieves the selected daily entry by obtaining its unique identifier, fetches the entry, displays its content using an internal display function, and switches to a designated text buffer."
  (interactive)
  (let* ((row (ctbl:cp-get-selected-data-row (ctbl:cp-get-component)))
         (uuid (car (last row)))
         (one (daily-one-get uuid)))
    (daily--show-one one)
    (pop-to-buffer daily--text-buffer-name)))

(defun daily-preview ()
  "Interactively displays the daily entry preview by calling the daily-show function and then selecting the window that contains the designated daily buffer."
  (interactive)
  (daily-show)
  (let ((daily-window (cl-find-if (lambda (window) (string= (buffer-name (window-buffer window)) daily--buffer-name))
                                  (window-list))))
    (when daily-window
     (select-window daily-window))))

(defun daily-add ()
  "Prompts the user to add a new daily entry. It generates a unique identifier for the entry, records the current time with a specified format, and requests user input for the text content. It also allows the user to add multiple tag names from a provided list, creating a daily-tag object for each with its own unique identifier and linking it to the entry via the same UUID. The function then saves the entry by updating or inserting it into the appropriate storage and refreshes the display."
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
  "Deletes the selected daily entry. The function retrieves the currently selected row's data, extracting the unique identifier, date, and text. It then prompts the user for confirmation to delete the entry, displaying the date and text. Upon confirmation, it deletes the entry identified by the unique identifier and refreshes the display."
  (interactive)
  (let* ((row (ctbl:cp-get-selected-data-row (ctbl:cp-get-component)))
         (uuid (car (last row)))
         (date (car row))
         (text (cadr row)))
    (when (yes-or-no-p (format "Are you sure to delete the row? [%s](%s)" date text))
      (daily-one-delete uuid)
      (daily-refresh))))

(defun daily-edit ()
  "Edits a daily entry based on the selected row and column in the daily interface. The function retrieves the current table component, selected row, selected column, and the unique identifier from the row. It then fetches the corresponding daily entry and calls the appropriate editing routine depending on the selected column (date, text, or tags). Finally, it saves the changes to the entry and refreshes the interface."
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp))
         (col-num (cdr (ctbl:cp-get-selected cp)))
         (uuid (car (last row)))
         (one (daily-one-get uuid)))
    (pcase col-num
      (0 (daily--edit-one-date one))
      (1 (daily--edit-one-text one))
      (2 (daily--edit-one-tags one)))
    (daily-one-insert-or-update one)
    (daily-refresh)))

(defun daily-edit-text-submit ()
  "Interactively submits the edited text for the current daily entry. When the user confirms via a yes-no prompt, it captures the buffer's entire content as the new text, updates the current case, persists the changes, and refreshes the display."
  (interactive)
  (let* ((one daily--current-one))
    (when (yes-or-no-p "Are you sure you want to submit the modified text?")
      (daily--edit-one one
                       :text (buffer-substring-no-properties (point-min) (point-max)))
      (setq-local daily--current-one one)
      (daily-one-insert-or-update one)
      (daily-refresh))))

(defun daily ()
  "Initializes the daily database, refreshes the display, and switches to the daily buffer for user interaction."
  (interactive)
  (daily-db-init)
  (daily-refresh)
  (switch-to-buffer daily--buffer-name))

(defun daily-insert-dashboard-text ()
  "Generates and inserts the dashboard header in the daily interface. It calculates the total entry count and determines the page number and total page count. The header includes a title with count and page information, a line of key command instructions, and decorative separator lines created with repeated characters, all styled with designated text properties."
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
  "Refreshes the daily interface. The function calculates dynamic widths for the date, text, and tags columns based on the dashboard width and current date format. It then builds a column model with title, alignment, and width settings, and obtains the data by converting daily entries to printable format. With the daily buffer created or retrieved, it disables the header display, erases the buffer if the table component is not present, inserts the dashboard header text, creates the table component with the specified model and keymap, and finally updates the table model with the new data while setting the buffer to read-only."
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
         (data (mapcar #'daily-obj-to-printable (daily-one-list)))
         (model (make-ctbl:model :column-model column-model :data data)))
    (with-current-buffer (get-buffer-create daily--buffer-name)
      (let ((buffer-read-only nil))
        (setf (ctbl:param-display-header param) nil)
        (unless daily--ctable-component
          (erase-buffer)
          (daily-insert-dashboard-text)
          (setq-local daily--ctable-component (ctbl:create-table-component-region :model model :param param :keymap daily--keymap))
          (ctbl:cp-add-click-hook daily--ctable-component (lambda ())))
        (ctbl:cp-set-model daily--ctable-component model))
      (setq-local buffer-read-only t))))

(define-minor-mode daily-text-mode
  "Minor mode for daily text editing."
  :lighter " Daily"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'daily-edit-text-submit)
    map))

(provide 'daily)
;;; daily.el ends here
