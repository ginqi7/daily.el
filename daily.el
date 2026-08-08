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

(require 'daily-obj)
(require 'daily-ui)

;;; Custom Variables
(defcustom daily-time-format "%Y-%m-%d %H:%M:%S"
  "Format string used to display date and time values in daily.")

(defcustom daily-page-size 30
  "Number of entries to load per page in daily lists.")

;;; Internal Variables

(defvar-local daily--filter (daily-filter
                             :page-num 1
                             :page-size daily-page-size
                             :sort 'date
                             :reversed t)
  "Holds the daily filter instance with default pagination, sorting, and filtering settings for daily entries.")

(defvar-local daily--current-one nil
  "Local variable to store the current daily entry for the buffer.")

(defvar daily--buffer-name "*daily*"
  "Name of the buffer used for daily's main interface.")

(defvar daily--text-buffer-name "*daily-text*"
  "Name of the buffer designated for daily text operations.")

(defvar daily--accumulate-buffer-name "*daily-accumulate*"
  "Defines a variable to store the name of the accumulate buffer.")

(defvar daily--selected-list nil
  "List storing the currently selected daily entries.")

;;; Internal Functions
(defun daily--filter-exp-to-str (exp)
  "Converts a list of filter expressions into a single string representation by iterating over each element. Each element is formatted so that if it is a string, it is enclosed in quotes; otherwise, it is formatted normally."
  (format "%s"
          (mapcar
           (lambda (exp) (format (if (stringp exp) "\"%s\"" "%s") exp))
           exp)))

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

(defun daily-filter-reset ()
  "Reset the daily filter to its default pagination, sorting, and ordering settings."
  (interactive)
  (setq daily--filter (daily-filter
                       :page-num 1
                       :page-size daily-page-size
                       :sort 'date
                       :reversed t)))

(defun daily-set-filter-date (&optional date-filter)
  "Prompts the user to update the date filter expression for the daily filter. If an optional date filter is provided, it uses that; otherwise, it defaults to the current date filter or a generic filter expression. The function converts the filter expression to a string for display, lets the user modify it via an interactive prompt, updates the daily filter with the new value, and then refreshes the interface."
  (interactive)
  (let* ((date-filter-exp (or date-filter (daily-filter-date daily--filter) '(like date "%%")))
         (date-filter-str (daily--filter-exp-to-str date-filter-exp)))
    (daily-filter-write-date daily--filter (read--expression "Set Date Filter: "  date-filter-str)))
  (daily-refresh))

(defun daily-set-filter-text (&optional text-filter)
  "Sets the text filter for the daily filter configuration. It uses an optional text filter argument if provided; otherwise, it defaults to the current text filter or a generic filter expression. The function converts the filter expression to a string for display, prompts the user to modify it interactively, updates the daily filter with the new value, and then refreshes the interface."
  (interactive)
  (let* ((text-filter-exp (or text-filter (daily-filter-text daily--filter) '(like text "%%")))
         (text-filter-str (daily--filter-exp-to-str text-filter-exp)))
    (daily-filter-write-text daily--filter (read--expression "Set Text Filter: "  text-filter-str)))
  (daily-refresh))

(defun daily-set-filter-tags (&optional tags-filter)
  "Sets the tags filter for the daily filter configuration. It uses an optional filter if provided; otherwise, it defaults to the current tags filter or a generic filter expression. The function converts the filter expression to a string, prompts the user to modify it interactively, updates the daily filter with the new value, and then refreshes the interface."
  (interactive)
  (let* ((tags-filter (or tags-filter (daily-filter-tags daily--filter))))
    (daily-filter-write-tags daily--filter (completing-read-multiple
                                            "Set Tags Filter: "
                                            (daily-db-no-repeat-tag-names)
                                            nil
                                            nil
                                            (string-join tags-filter ","))))
  (daily-refresh))

(defun daily-set-filter-sort (&optional sort)
  "Set the sorting criterion for the daily filter by prompting the user to select from date, text, or tags, then refresh the display."
  (interactive)
  (let* ((sort (or sort (daily-filter-sort daily--filter))))
    (daily-filter-write-sort daily--filter (completing-read
                                            "Set Sort: "
                                            '(date text tags))))
  (daily-refresh))

(defun daily-set-filter-sort-reversed (&optional reversed)
  "Toggle the reversed sorting order for the daily filter by prompting the user for confirmation and then refresh the display."
  (interactive)
  (daily-filter-write-reversed daily--filter (yes-or-no-p "Sort Reversed:"))
  (daily-refresh))

(defun daily-set-filter-page-num ()
  "Set the page number for the daily filter by prompting the user for a value and then refresh the display."
  (interactive)
  (daily-filter-write-page-num daily--filter (read-number "Page Number: "))
  (daily-refresh))

(defun daily-set-filter-page-size ()
  "Set the page size for the daily filter by prompting the user for a value and then refresh the display."
  (interactive)
  (daily-filter-write-page-size daily--filter (read-number "Page Size: "))
  (daily-refresh))

(defun daily-set-filter ()
  "Prompt the user to select a filter category and update the daily filter accordingly."
  (interactive)
  (when-let* ((col (completing-read "Filter: " '(date text tags))))
    (pcase col
      ("date" (daily-set-filter-date))
      ("text" (daily-set-filter-text))
      ("tags" (daily-set-filter-tags)))))

(defun daily-accumulate ()
  "Collect selected daily entries into a dedicated Org-mode buffer and display it."
  (interactive)
  (when-let* ((selected-list daily--selected-list))
    (with-current-buffer (get-buffer-create daily--accumulate-buffer-name)
      (erase-buffer)
      (org-mode)
      (mapcar #'insert (mapcar #'daily-obj-to-org selected-list))
      (switch-to-buffer (current-buffer))))
  (setq daily--selected-list nil))

(defun daily-show ()
  "Interactively retrieves the selected daily entry by obtaining its unique identifier, fetches the entry, displays its content using an internal display function, and switches to a designated text buffer."
  (interactive)
  (when-let* ((one (car daily--selected-list)))
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
  (when-let* ((selected-list daily--selected-list))
    (when (yes-or-no-p (format "Are you sure to delete the selected rows? [%s rows]" (length selected-list)))
      (mapcar #'daily-one-delete (mapcar #'daily-obj-uuid selected-list))
      (daily-refresh)
      (setq daily--selected-list nil))))

(defun daily-edit ()
  "Edits a daily entry based on the selected row and column in the daily interface. The function retrieves the current table component, selected row, selected column, and the unique identifier from the row. It then fetches the corresponding daily entry and calls the appropriate editing routine depending on the selected column (date, text, or tags). Finally, it saves the changes to the entry and refreshes the interface."
  (interactive)
  (when-let* ((one (car daily--selected-list))
              (col (completing-read "Edit: " '(date text tags))))
    (pcase col
      ("date" (daily--edit-one-date one))
      ("text" (daily--edit-one-text one))
      ("tags" (daily--edit-one-tags one)))
    (daily-one-insert-or-update one)
    (daily-refresh)
    (setq daily--selected-list nil)))

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
  (daily-filter-reset)
  (daily-refresh)
  (switch-to-buffer daily--buffer-name))

(defun daily-refresh ()
  "Update the daily buffer by recalculating pagination and rendering entries according to the current filter."
  (let* ((count (daily-one-count))
         (page-count (1+ (/ count (daily-filter-page-size daily--filter))))
         (data (daily-one-list daily--filter)))
    (with-current-buffer (get-buffer-create daily--buffer-name)
      (daily-mode 1)
      (daily-ui-render :current-page (daily-filter-page-num daily--filter)  :page-count page-count :count count :data data))))

(define-minor-mode daily-mode
  "Minor mode for daily."
  :lighter " Daily"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'daily-add)
    (define-key map (kbd "e") #'daily-edit)
    (define-key map (kbd "d") #'daily-delete)
    (define-key map (kbd "<RET>") #'daily-show)
    (define-key map (kbd "<SPC>") #'daily-preview)
    (define-key map (kbd "g") #'daily-accumulate)
    (define-key map (kbd "f") #'daily-set-filter)
    map))

(define-minor-mode daily-text-mode
  "Minor mode for daily text editing."
  :lighter " Daily"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'daily-edit-text-submit)
    map))

(provide 'daily)
;;; daily.el ends here
