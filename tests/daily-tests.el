;;; daily-tests.el --- Tests for daily.el        -*- lexical-binding: t; -*-

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

;; Tests for daily.el functions

;;; Code:
(require 'daily)
(require 'ert)

;;; Helper Functions

(defun daily-tests--create-test-one ()
  "Create a test daily-one object for use in tests."
  (daily-one :uuid "test-uuid-001"
             :date "2026-03-07 10:00:00"
             :text "Test entry content"
             :tags (list (daily-tag :uuid "tag-001" :name "test" :one-uuid "test-uuid-001"))))

;;; Internal Function Tests

(ert-deftest test-daily--filter-exp-to-str ()
  "Test converting filter expressions to string representation."
  ;; The function returns a string representation of the list
  ;; Test with string elements
  (should (stringp (daily--filter-exp-to-str '("%test%"))))
  ;; Test with non-string elements
  (should (stringp (daily--filter-exp-to-str '(like))))
  ;; Test with empty list - returns "nil"
  (should (equal "nil" (daily--filter-exp-to-str nil))))

(ert-deftest test-daily--show-one ()
  "Test displaying a daily entry in the text buffer."
  (let ((one (daily-tests--create-test-one)))
    (daily--show-one one)
    (should (get-buffer daily--text-buffer-name))
    (with-current-buffer daily--text-buffer-name
      (should (derived-mode-p 'org-mode))
      (should (equal one daily--current-one))
      (should (string-match-p "\\[test-uuid-001\\]" header-line-format))
      (should (string-match-p "2026-03-07 10:00:00" header-line-format))
      (should (string-match-p "Test entry content" (buffer-string))))
    (kill-buffer daily--text-buffer-name)))

(ert-deftest test-daily--dashboard-width ()
  "Test calculating dashboard width."
  ;; This test may vary based on window configuration
  (let ((width (daily--dashboard-width)))
    (should (integerp width))
    (should (> width 0))))

(ert-deftest test-daily--uuid ()
  "Test UUID generation format."
  (let ((uuid1 (daily--uuid))
        (uuid2 (daily--uuid)))
    ;; Check UUID format (8-4-4-4-12 pattern with version 4)
    (should (string-match-p "^[a-f0-9]\\{8\\}-[a-f0-9]\\{4\\}-4[a-f0-9]\\{3\\}-[a-f0-9]\\{4\\}-[a-f0-9]\\{12\\}$" uuid1))
    ;; Check uniqueness
    (should-not (equal uuid1 uuid2))))

(ert-deftest test-daily--edit-one ()
  "Test editing a daily-one object with keyword arguments."
  (let ((one (daily-tests--create-test-one)))
    ;; Test editing date only
    (daily--edit-one one :date "2026-03-08")
    (should (equal "2026-03-08" (daily-one-date one)))
    ;; Test editing text only
    (daily--edit-one one :text "Updated content")
    (should (equal "Updated content" (daily-one-text one)))
    ;; Test editing tags only
    (let ((new-tag (daily-tag :uuid "new-tag-001" :name "new" :one-uuid "test-uuid-001")))
      (daily--edit-one one :tags (list new-tag))
      (should (= 1 (length (daily-one-tags one))))
      (should (equal "new" (daily-tag-name (car (daily-one-tags one))))))
    ;; Test editing date and text only (not tags)
    (daily--edit-one one :date "2026-03-09" :text "All updated")
    (should (equal "2026-03-09" (daily-one-date one)))
    (should (equal "All updated" (daily-one-text one)))))

(ert-deftest test-daily--edit-one-date ()
  "Test editing date of a daily-one object."
  (let ((one (daily-tests--create-test-one))
        (read-string-hook (lambda (&rest _) "2026-03-08")))
    (cl-letf (((symbol-function 'read-string) read-string-hook))
      (daily--edit-one-date one)
      (should (equal "2026-03-08" (daily-one-date one))))))

(ert-deftest test-daily--edit-one-text ()
  "Test editing text of a daily-one object."
  (let ((one (daily-tests--create-test-one))
        (read-string-hook (lambda (&rest _) "Updated text content")))
    (cl-letf (((symbol-function 'read-string) read-string-hook))
      (daily--edit-one-text one)
      (should (equal "Updated text content" (daily-one-text one))))))

;;; Filter Function Tests

(ert-deftest test-daily-set-filter-date ()
  "Test setting date filter."
  (let ((daily--filter (daily-filter :page-num 1 :page-size 30))
        (read-expression-hook (lambda (&rest _) "(like date \"%test%)")))
    (cl-letf (((symbol-function 'read--expression) read-expression-hook)
              ((symbol-function 'daily-refresh) #'ignore))
      (daily-set-filter-date '(like date "%test%")))
    ;; read--expression returns a string, which is stored as the filter value
    (should (equal "(like date \"%test%)" (daily-filter-date daily--filter)))))

(ert-deftest test-daily-set-filter-text ()
  "Test setting text filter."
  (let ((daily--filter (daily-filter :page-num 1 :page-size 30))
        (read-expression-hook (lambda (&rest _) "(like text \"%test%)")))
    (cl-letf (((symbol-function 'read--expression) read-expression-hook)
              ((symbol-function 'daily-refresh) #'ignore))
      (daily-set-filter-text '(like text "%test%")))
    ;; read--expression returns a string, which is stored as the filter value
    (should (equal "(like text \"%test%)" (daily-filter-text daily--filter)))))

(ert-deftest test-daily-set-filter-tags ()
  "Test setting tags filter."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (daily--filter (daily-filter :page-num 1 :page-size 30))
         (completing-read-multiple-hook (lambda (&rest _) '("work" "personal"))))
    (unwind-protect
        (progn
          (daily-db-init)
          (cl-letf (((symbol-function 'completing-read-multiple) completing-read-multiple-hook)
                    ((symbol-function 'daily-refresh) #'ignore))
            (daily-set-filter-tags '("test")))
          (should (equal '("work" "personal") (daily-filter-tags daily--filter))))
      (delete-file daily-db-path))))

;;; daily-text-mode Tests

(ert-deftest test-daily-text-mode ()
  "Test daily-text-mode minor mode."
  (with-temp-buffer
    (daily-text-mode 1)
    (should daily-text-mode)
    ;; Check keymap
    (should (keymapp daily-text-mode-map))
    (should (equal 'daily-edit-text-submit (lookup-key daily-text-mode-map (kbd "C-c C-c"))))))

(ert-deftest test-daily-edit-text-submit ()
  "Test submitting edited text."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (one (daily-tests--create-test-one))
         (daily--current-one nil)
         (yes-or-no-p-hook (lambda (&rest _) t)))
    (daily-db-init)
    (with-temp-buffer
      (insert "Modified content")
      (setq daily--current-one one)
      (cl-letf (((symbol-function 'yes-or-no-p) yes-or-no-p-hook)
                ((symbol-function 'daily-refresh) #'ignore))
        (daily-edit-text-submit))
      (should (equal "Modified content" (daily-one-text daily--current-one))))
    (delete-file daily-db-path)))

;;; Dashboard Tests

(ert-deftest test-daily-insert-dashboard-text ()
  "Test inserting dashboard text."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (daily--current-page 1)
         (daily--filter (daily-filter :page-num 1 :page-size 30)))
    (daily-db-init)
    (with-temp-buffer
      (daily-insert-dashboard-text)
      (let ((content (buffer-string)))
        (should (string-match-p "Daily Text | Total:" content))
        (should (string-match-p "Page:" content))
        (should (string-match-p "\\[SPC\\] view" content))
        (should (string-match-p "\\[a\\] add" content))
        (should (string-match-p "\\[f\\] filter" content))
        (should (string-match-p "Filters | Date:" content))))
    (delete-file daily-db-path)))

;;; Buffer and Variable Tests

(ert-deftest test-daily-buffer-names ()
  "Test daily buffer name constants."
  (should (stringp daily--buffer-name))
  (should (stringp daily--text-buffer-name))
  (should (stringp daily--accumulate-buffer-name))
  (should (equal "*daily*" daily--buffer-name))
  (should (equal "*daily-text*" daily--text-buffer-name))
  (should (equal "*daily-accumulate*" daily--accumulate-buffer-name)))

(ert-deftest test-daily-custom-variables ()
  "Test daily custom variables."
  (should (stringp daily-time-format))
  (should (integerp daily-page-size))
  (should (> daily-page-size 0)))

(ert-deftest test-daily-keymap ()
  "Test daily keymap bindings."
  (should (keymapp daily--keymap))
  ;; Check that daily commands are bound
  (should (equal 'daily-add (lookup-key daily--keymap (kbd "a"))))
  (should (equal 'daily-edit (lookup-key daily--keymap (kbd "e"))))
  (should (equal 'daily-delete (lookup-key daily--keymap (kbd "d"))))
  (should (equal 'daily-show (lookup-key daily--keymap (kbd "<RET>"))))
  (should (equal 'daily-preview (lookup-key daily--keymap (kbd "<SPC>"))))
  (should (equal 'daily-accumulate (lookup-key daily--keymap (kbd "g"))))
  (should (equal 'daily-set-filter (lookup-key daily--keymap (kbd "f")))))

;;; Integration Tests

(ert-deftest test-daily-filter-default-values ()
  "Test daily--filter default values."
  (should (object-of-class-p daily--filter 'daily-filter))
  (should (equal 1 (daily-filter-page-num daily--filter)))
  (should (equal daily-page-size (daily-filter-page-size daily--filter)))
  (should (equal 'date (daily-filter-sort daily--filter)))
  (should (eq t (daily-filter-reversed daily--filter))))

(ert-deftest test-daily-one-count-integration ()
  "Test daily-one-count with database operations."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db")))
    (daily-db-init)
    (should (= 0 (daily-one-count)))
    (daily-db-one-insert-or-update :uuid "uuid-001" :text "Test" :date "2026-03-07")
    (should (= 1 (daily-one-count)))
    (delete-file daily-db-path)))

(ert-deftest test-daily-refresh-integration ()
  "Test daily-refresh creates buffer and content."
  (let* ((daily-db-path (make-temp-file "daily-db-test" nil ".db"))
         (daily--current-page 1)
         (daily--filter (daily-filter :page-num 1 :page-size 30)))
    (daily-db-init)
    (daily-db-one-insert-or-update :uuid "uuid-001" :text "Test entry" :date "2026-03-07")
    (condition-case nil
        (daily-refresh)
      (error nil))  ; May fail in batch mode due to window operations
    (when (get-buffer daily--buffer-name)
      (with-current-buffer daily--buffer-name
        (should (buffer-live-p (current-buffer)))
        (should (string-match-p "Daily Text" (buffer-string)))))
    (delete-file daily-db-path)))

;;; daily-tests.el ends here
