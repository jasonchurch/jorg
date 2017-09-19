;;; jorg-capture.el --- Jason's Org Capture functionality

;; Copyright (C) 2017 Jason Church

;; Author: Jason Church <jasonchurch@edeveloper.ca>
;; Version: 1.0.0
;; Keywords: outlines jay jorg capture

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

;; Sets up org capture functionality with jorg items.
;;; Code:

(require 'org)
(require 'subr-x)
(require 'jorg-common)

(defvar org-capture-templates
  "A global var from org lib, it holds list of org capture templates.")

(defvar org-capture-plist
  "A global var from org lib, it old the plist for current capture.")

(defvar jorg-capture-key-main "j"
  "The key to show the jorg capture templates from org capture templates, `C-c c <key>'.")

(defvar jorg-capture-project-task-target "TASKS"
  "The target heading under which to file tasks in the project file.")

(defvar jorg-capture-project-update-target "UPDATES"
  "The target heading under which to file tasks in the project file.")

(defvar jorg-capture-project-reference-target "REFERENCE"
  "The target heading under which to file reference entries.")

;; Hook up Jorg Capture
(add-hook 'org-capture-before-finalize-hook 'jorg-capture-project-template-finalize-hook)

(add-to-list 'org-capture-templates
             `(,jorg-capture-key-main "JOrg")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "n") "Next (Next Action)" entry (function jorg-select-project-for-capture-entry)
               (function jorg-capture-template-next)
               :empty-lines 1
               :jorg-capture-target ,jorg-capture-project-task-target )
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "s") "Scheduled Next Actions")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "st") "Tomorrow" entry (function jorg-select-project-for-capture-entry)
               (function jorg-capture-template-next)
               :empty-lines 1
               :jorg-capture-target ,jorg-capture-project-task-target
               :jorg-capture-scheduled "+1d")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "sw") "Week)" entry (function jorg-select-project-for-capture-entry)
               (function jorg-capture-template-next)
               :empty-lines 1
               :jorg-capture-target ,jorg-capture-project-task-target
               :jorg-capture-scheduled "+1w")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "sm") "Month" entry (function jorg-select-project-for-capture-entry)
               (function jorg-capture-template-next)
               :empty-lines 1
               :jorg-capture-target ,jorg-capture-project-task-target
               :jorg-capture-scheduled "+1m")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "d") "Deadline Next Actions")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "dt") "Tomorrow" entry (function jorg-select-project-for-capture-entry)
               (function jorg-capture-template-next)
               :empty-lines 1
               :jorg-capture-target ,jorg-capture-project-task-target
               :jorg-capture-deadline "+1d")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "dw") "Week)" entry (function jorg-select-project-for-capture-entry)
               (function jorg-capture-template-next)
               :empty-lines 1
               :jorg-capture-target ,jorg-capture-project-task-target
               :jorg-capture-deadline "+1w")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "dm") "Month" entry (function jorg-select-project-for-capture-entry)
               (function jorg-capture-template-next)
               :empty-lines 1
               :jorg-capture-target ,jorg-capture-project-task-target
               :jorg-capture-deadline "+1m")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "t") "Todo (unsure, maybe)" entry (function jorg-select-project-for-capture-entry)
               "** TODO %?\n\n"
               :empty-lines 1
               :jorg-capture-target ,jorg-capture-project-task-target )
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "u") "Update" plain (function jorg-select-heading-for-list-capture-entry)
               (function jorg-capture-template-update)
               :jorg-capture-target ,jorg-capture-project-update-target
               )
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "r") "Reference" entry (function jorg-select-project-for-capture-entry)
               (function jorg-capture-template-heading)
               :jorg-capture-target ,jorg-capture-project-reference-target
               )
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "p") "JORG Project Template" plain (function jorg-create-temporary-template-buffer)
               (function jorg-project-template)
               :empty-lines 1
               )
             t)

(defun jorg-capture-project-template-finalize-hook ()
  "Finalize the JORG project template."
  (when (equal "JORG Project Template" (plist-get org-capture-plist :description))
    (message "capture type:%s" (plist-get org-capture-plist ':description ))
    (set-buffer (org-capture-get :buffer))
    (message "Finalizing %s " (buffer-name))
    (unless (jorg-goto-project-heading)
      (error "Not a JOrg Project Capture"))
    (let* (
           (id (org-entry-get nil "ID"))
           (priority (org-entry-get nil "PRIORITY"))
           (tags (org-entry-get nil "TAGS"))
           (category (org-entry-get nil "CATEGORY"))
           )
      (unless category (org-entry-put nil "CATEGORY" (substring category 0 10)))
      (jorg-insert-file-properties (org-entry-properties))
      (jorg-insert-additional-headings)
      (jorg-make-buffer-project-file (org-entry-properties))
      (org-agenda-file-to-front))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JOrg Captures Templates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jorg-capture-template-next ()
  "Create capture template for next items.
It will use optional jorg-capture-scheduled and
jorg-capture-deadline properties to set a date with org dates or
date increments, ex +1d."
  (let ((jorg-schedule (plist-get org-capture-plist :jorg-capture-scheduled))
        (jorg-deadline (plist-get org-capture-plist :jorg-capture-deadline)))
    (with-temp-buffer
      (insert "** NEXT %?\n")
      (when jorg-schedule (org-schedule nil jorg-schedule))
      (when jorg-deadline (org-deadline nil jorg-deadline))
      (buffer-string))))

(defun jorg-capture-template-heading ()
  "Create a capture template for adding basic headings.
Primary motivation is to capture headings for the reference section."
  (with-temp-buffer
    (insert "** %?\n")
    (insert "   :PROPERTIES:\n")
    (insert (concat "   :CREATED: " (jorg-common-inactive-time-stamp) "\n"))
    (insert "   :END:\n")
    (insert "%i")
    (buffer-string)))

(defun jorg-capture-template-update ()
  "Create capture template for next items.
It will use optional jorg-capture-scheduled and
jorg-capture-deadline properties to set a date with org dates or
date increments, ex +1d."
  (message "indent: %s" (org-capture-get :jorg-indent))
  (with-temp-buffer
    (insert (concat "   - " (jorg-common-inactive-time-stamp) " %?"))
    (buffer-string)))

(defun jorg-project-template ()
  "Create Project Heading template."
  (message "plist: %s" org-capture-plist)
  (with-temp-buffer
    (insert "* PROJECT [#C] %?\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :CATEGORY: \n")
    (insert "  :CREATED_DATE: %(jorg-common-get-datetime)\n")
    (insert "  :ALT_NAME:\n")
    (insert "  :ID: %(string-trim(org-id-new))\n")
    (insert " :END:\n")
    (buffer-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JOrg find/make capture buffer functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jorg-create-temporary-template-buffer ()
  "Create an empty project buffer that will get its location when capture template completes."
  (set-window-buffer nil (set-buffer (generate-new-buffer "project template")))
  (org-mode))

;; TODO: Replace with function that prompts user to choose project file from list.
;; Defvar jorg-unfiled-project, a project that will contain unfiled tasks. Should have some way to distinguish it.
(defun jorg-select-project-for-capture-entry ()
  "Select the project for the capture entry."
  ;;(message "selected %s" (ido-completing-read "Select JORG Project:" (org-agenda-files)))
  (let ((target (org-capture-get :jorg-capture-target))
        (capture-file-name (jorg-select-project)))
    (set-buffer (find-file capture-file-name))
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote target))
         nil t)
        (goto-char (point-at-eol))
      (error (concat "No project target: " target " in " (buffer-file-name))))
    (org-mode)
    ))

(defun jorg-select-heading-for-list-capture-entry ()
  "Select the project for the capture list.
Locate the target heading and then the first listen within the heading."
  (let ((target (org-capture-get :jorg-capture-target))
        (point-at-end-of-heading)
        (capture-file-name (jorg-select-project)))
    (set-buffer (find-file capture-file-name))
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote target))
         nil t)
        (progn
          (goto-char (point-at-bol))
          (save-excursion
            (org-end-of-subtree)
            (setq point-at-end-of-heading (point)))
          (unless (org-list-search-forward (org-item-beginning-re) point-at-end-of-heading t)
            (error (concat "No list found for target" target " in " (buffer-file-name))))
          (org-capture-put :jorg-indent (- (point) 2 (point-at-bol)))
          (goto-char (point-at-bol))
          )
      (error (concat "No project target: " target " in " (buffer-file-name))))
    (org-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jorg Capture Project Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jorg-make-buffer-project-file (properties)
  "Make the current buffer into a project file using PROPERTIES.
Using PROPERTIES, project heading properties, determine file and folder
names, create folder and parents, and make buffer visit generated filename."
  (let* ((project-heading (cdr (assoc-string "ITEM" properties)))
         (created-date (cdr (assoc-string "CREATED_DATE" properties)))
         (alt-name (cdr (assoc-string "ALT_NAME" properties)))
         (jorg-project-dir (replace-regexp-in-string "[^a-zA-Z0-9.]"
                                                     "_"
                                                     (concat (replace-regexp-in-string "[_/:-]" "" created-date)
                                                             "_"
                                                             (if (or (equal nil alt-name) (string-empty-p alt-name))
                                                                 project-heading
                                                               alt-name))))
         (jorg-project-dirpath (concat jorg-project-base-dir (unless (equal "/" (substring jorg-project-base-dir -1)) "/") jorg-project-dir))
         (jorg-project-filename  (replace-regexp-in-string "[^a-zA-Z0-9.]"
                                                           "_"
                                                           (if (or (equal nil alt-name) (string-empty-p alt-name))
                                                               (concat project-heading ".org")
                                                             (concat alt-name ".org"))))
         (jorg-project-filepath (concat jorg-project-dirpath (unless (equal "/" (substring jorg-project-dirpath -1)) "/") jorg-project-filename))
         )
    (rename-buffer jorg-project-filename)
    (set-visited-file-name jorg-project-filepath)
    (make-directory jorg-project-dirpath 't)))

(defun jorg-insert-file-properties (properties)
  "Add org file properties using PROPERTIES.
PROPERTIES are the properties of the PROJECT heading."
  (save-excursion
    (goto-char (point-min))
    (insert (concat "#+TITLE:" (cdr (assoc-string "ITEM" properties)) "\n"))
    (insert (concat "#+AUTHOR:" jorg-user-name "\n"))
    (insert (concat "#+EMAIL:" jorg-user-email "\n"))
    (insert (concat "#+DATE:" (cdr (assoc-string "CREATED_DATE" properties)) "\n"))
    (insert "#+STARTUP: content\n")))

(defun jorg-insert-additional-headings ()
  "Insert additional headings like update, tasks, reference at end of current buffer."
  (save-excursion
    (goto-char (point-max))
    (insert "** UPDATES\n")
    (insert "   :PROPERTIES:\n")
    (insert "   :VISIBILITY: showall\n")
    (insert "   :END:\n\n")
    (insert (concat "   - " (jorg-common-inactive-time-stamp) " initial update\n"))
    (insert "** TASKS\n\n")
    (insert "** REFERENCE\n")
    (insert "   :PROPERTIES:\n")
    (insert "   :VISIBILITY: folded\n")
    (insert "   :END:\n")))

;;TODO doesn't appear to be used, do we need any more, can we use with current template?
(defun insert-template-text (string)
  "Insert into the current buffer STRING formated as template text."
  (insert (propertize string
                      'font-lock-face '(:foreground "LightSkyBlue" :slant italic))))

(provide 'jorg-capture)

;;; jorg-capture.el ends here
