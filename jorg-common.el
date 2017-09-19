;;; jorg-common.el --- common JOrg functionality

;; Copyright (C) 2017 Jason Church

;; Author: Jason Church <jasonchurch@edeveloper.ca>
;; Version: 1.0.0
;; Keywords: outlines jay jorg common

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Holds functionality likely to be used in other jorg el files but
;; doesn't warrent its own jorg-xxx.el file.

;;; Code:

(require 'org)

;;;;;;;;;;;;;;;;;;
;; User Utility ;;
;;;;;;;;;;;;;;;;;;
(defun jorg-get-date ()
  "Get the current date yyyy-mm-dd."
  (interactive)
  (format-time-string "%Y-%m-%d"))

(defun jorg-common-inactive-time-stamp ()
  "Get the current date as an org inactive timestamp."
  (concat "[" (format-time-string "%Y-%m-%d %a") "]"))

(defun jorg-common-get-datetime ()
  "Get the current date and time yyyy-mm-dd HH:mm:ss."
  (interactive)
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun jorg-common-insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;TODO Don't think this is used, remove it or move to some personal package?
(defun jorg-common-copy-file-path ()
  "Copies the file path of current buffer to kill ring."
  (interactive)
  (message "%s" buffer-file-name)
  (kill-new buffer-file-name))
;; User Utility ends here

;;TODO Ended up using orgs built in ID generated, maybe move this out to my elisp notes
(defun jorg-common-generate-uuid ()
  "Generate a UUID.  Implemented by calling Linux uuidgen."
  (interactive)
  (shell-command-to-string "/usr/bin/uuid"))

(defun jorg-enty-update-tags (tags)
  "Update the TAGS property of the current heading.
Returns t if entry was updated, otherwise nil.

This was required because tags is a special kind of property
that isn't updatable `org-entry-put'."
  (save-excursion
    (let ((is-updated))
      (org-back-to-heading)
      (unless (equal (org-entry-get nil "TAGS") tags)
        (org-set-tags-to (if tags tags ""))
        (setq is-updated t))
      is-updated)))

(defun jorg-enty-update-item (item)
  "Update the ITEM property of the current heading.
Returns t if entry was updated, otherwise nil.

This was required because item is a special kind of property
that isn't updatable `org-entry-put'."
  (save-excursion
    (let ((is-updated))
      (org-back-to-heading)
      (unless (equal (org-entry-get nil "ITEM") item)
        (re-search-forward "^\*+ " (point-at-eol))
        (kill-line)
        (insert item)
        (setq is-updated t))
      is-updated)))

;; TODO not currently used but may use again later
(defun jorg-entry-update-properties (update-keys properties)
  "Update the properties of the current heading.
Uses list of property keys from UPDATE-KEYS to identify which pairs from
PROPERTIES, to update in the current heading.  PROPERTIES is usually obtained
from another heading using `(org-entry-properties).'

It will only update properties if they are different and it will return t if it
did, otherwise nil.  If position is not within an org entry, it will through an
error."
  (let ((is-updated))
    (dolist (elt update-keys)
      (cond
       ((equal "ITEM" elt)
        (when  (jorg-enty-update-item (cdr (assoc-string elt properties)))
          (setq is-updated t)))
       ((equal "TAGS" elt)
        (when  (jorg-enty-update-tags (cdr (assoc-string elt properties)))
          (setq is-updated t))
        )
       (t
        (unless (equal (org-entry-get nil elt) (cdr (assoc-string elt properties)))
          (org-entry-put nil elt (cdr (assoc-string elt properties)))
          (setq is-updated t)))))
    is-updated))

(provide 'jorg-common)
;;; jorg-common.el ends here
