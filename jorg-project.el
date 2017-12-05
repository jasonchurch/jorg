;;; jorg-project.el --- JOrg project functionality

;; Copyright (C) 2017 Jason Church

;; Author: Jason Church <jasonchurch@edeveloper.ca>
;; Version: 1.0.0
;; Keywords: outlines jay jorg project

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
;; TODO maybe move some of these comments to jorg.el?
;; JORG's Project - an org file that represents a single project:
;; movies I like; books to read;become a rocket scientest;build shiny
;; rocket ship; etc.  Each file consists of a single top level PROJECT
;; TO DO Heading with subheadings:

;; - UPDATES to help keep track of progress or key info either for
;; your own sake (I have a poor memory) or a means to have an easy
;; update on hand to provide to interested parties.

;; - TASKS for related NEXT ACTIONS (if your a GTD fan) or project
;; related TODOs.

;; - RERERENCE a location for anything else related to your project:
;; snippets of info, images/diagrams, notes, important links, etc.

;; Example of Project Structure using perhaps a trivial example, but
;; some of us need all the organization we can get! While this example
;; shows the basic structure of a JOrg Project, there are more
;; attributes like Title, Author, etc and the project and other
;; headings may have various properties.

;; * PROJECT [#A] Read ELisp Manual
;; ** UPDATES
;;    - [2017-09-08] Found manual online, but its more cool reading it inside Emacs!
;; ** TASKS
;; *** NEXT Read Lisp Data Types
;; ** REFERENCE
;; *** NOTES
;; **** 1.2 Lisp History
;; ***** MacLisp created in the 1960s
;; ***** Common Lisp, the result of many MacLisp decendants finally getting together
;;       - Emacs has a cl-lib which implements some common Lisp.

;; DISCLAIMER: I have no justification that this is a useful way of
;; doing things, beyond my own experiences as an individual trying to
;; be more organized in the face of 100s of projects.  By projects I'm
;; thinking from an individuals own creation of projects and not
;; larger professional projects.  For my own experience it has helped
;; me feel more organized and less scattered.  This really started
;; out, and perhaps always will be, just a tool for me.  There may be
;; a little GTD influence here and there, but it may not be so
;; apparent as I would likely just treat the 50k, 40k, 30K, 20K as
;; individual projects that I review periodically and the 10K and
;; runway are really just the project headings + next actions.  For me
;; atleast I find it more organized if I keep tasks grouped near other
;; project info like updates and reference info.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'jorg-common)

(defgroup jorg-capture-project nil
  "A file per project capture add-on for org-mode."
  :group 'org
  :prefix "jorg-")

;;; JOrg defvars
(defvar jorg-project-base-dir "~/jorg/projects"
  "The location jorg-project should create project dirs in.")

(defvar jorg-text-template-color "blue"
  "The color of any template text in the JOrg Project org file.")

(defvar jorg-user-name nil
  "The user's name.")

(defvar jorg-user-email nil
  "The user's email.")

(defvar jorg-recently-saved-projects-to-front t
  "Moves recently saved projects to front of the agenda list if non nil.
This help keeps active projects at the fore front.")

(defcustom jorg-select-project-method 'ido
  "Sets the way users supply input when selecting a project."
  :type '(choice
          (const :tag "ido completing read" ido)
          (const :tag "x-popup menu" x-popup))
  :group 'jorg-capture-project)

(defvar jorg-projects-meta-cache 'nil
  "Hold map of project meta.")

;;Menu
(easy-menu-change
 '("Tools") "JOrg"
 '(["Switch Project" jorg-switch-project])
 "Search Files (Grep)...")
(easy-menu-change '("Tools") "--" nil "Search Files (Grep)...")

;; Hook up save hook after-save-hook?
(add-hook 'after-save-hook 'jorg-project-on-save)

(defun jorg-select-project ()
  "Return path to project that user select."
  (interactive)
  (cond ((eq jorg-select-project-method 'ido) (jorg-select-project-ido))
        ((eq jorg-select-project-method 'x-popup) (jorg-select-project-x-popup))))

(defun jorg-select-project-x-popup ()
  "Return the user selected jorg project from a popup menu.
Uses jorg-all-projects-meta, which relies on org-agenda-files force project
files, to produce the menu items."
  (interactive)
  (let* ((agenda-pairs (mapcar
                        (lambda (x)
                          `(,(cdr (assoc-string "name" x)) . ,(cdr (assoc-string "path" x))))
                        (jorg-all-projects-meta)))
         (capture-file-name))
    (if (jorg-project-p)
        (progn
          (setq agenda-pairs (delq (rassoc (expand-file-name (buffer-file-name)) agenda-pairs) agenda-pairs))
          (push "---" agenda-pairs)
          (let ((proj-meta (jorg-project-meta (buffer-file-name))))
            (push
             `(,(cdr (assoc-string "name" proj-meta)) . ,(cdr (assoc-string "path" proj-meta)))
             agenda-pairs)
            ))
      )
    (push '"List 1" agenda-pairs)
    (x-popup-menu
     (list '(50 50) (selected-frame)) ;; where to popup
     (list "Choose a JOrg Project" ;; the menu itself
           agenda-pairs))))
;;(jorg-select-project-x-popup)

(defun jorg-projects-files ()
  "Return list of project files.
Currently jorg considers all files that relies on the files in org-agenda-files."
  (org-agenda-files))

(defun jorg-select-project-ido ()
  "Select the jorg project using ido."
  (interactive)
  (let* ((agenda-pairs (mapcar
                        (lambda (x)
                          `(,(cdr (assoc-string "name" x)) . ,(cdr (assoc-string "path" x))))
                        (jorg-all-projects-meta)))
         (capture-file-name))
    (if (jorg-project-p)
        (progn
          (setq agenda-pairs (delq (rassoc (expand-file-name (buffer-file-name)) agenda-pairs) agenda-pairs))
          (let ((proj-meta (jorg-project-meta (buffer-file-name))))
            (push
             `(,(cdr (assoc-string "name" proj-meta)) . ,(cdr (assoc-string "path" proj-meta)))
             agenda-pairs)
            )))
    (cdr (assoc-string (ido-completing-read "JOrg Project " (mapcar
                                                             (lambda (x)
                                                               (car x))
                                                             agenda-pairs))
                       agenda-pairs))))
;;(jorg-select-project-ido)

(defun jorg-project-meta (path)
  "Return an alist representing the meta data of the project found at PATH."
  (save-excursion
    (let* ((project-buffer (get-file-buffer path))
           (kill-buffer-after)
           (project-alist ())
           (project-heading-pos))
      (unless project-buffer
        (setq kill-buffer-after t)
        (setq project-buffer (find-file-noselect path)))
      (set-buffer project-buffer)
      (setq project-heading-pos (jorg-find-project-heading))
      (when project-heading-pos
        (goto-char project-heading-pos)
        (push (cons "name" (org-entry-get nil "ITEM")) project-alist)
        (push (cons "created" (org-entry-get nil "CREATED_DATE")) project-alist)
        (push (cons "id" (org-entry-get nil "ID")) project-alist)
        (push (cons "path" (expand-file-name path)) project-alist)
        )
      (when kill-buffer-after (kill-buffer project-buffer))
      project-alist)))
;;(cdr (assoc-string "name" (jorg-project-meta "~/org/2017/projects/20170726_214506_Odyssey/Odyssey.org")))

(defun jorg-all-projects-meta ()
  "Return all jorg projects meta data.
Returns an alist where the path is the key, and the value is an alist of project
meta.  Uses org-agenda-files as source of potential project files."
  (if jorg-projects-meta-cache
      jorg-projects-meta-cache
    (setq jorg-projects-meta-cache (jorg-load-projects-meta))))
;;(cdr (assoc-string "~/org/2017/projects/20170819_233419_Note4Roms/Note4Roms.org" (jorg-all-projects-meta)))

(defun jorg-load-projects-meta ()
  "Load jorg project meta from project files."
  (cl-remove-if 'null (mapcar
                       (lambda (x)
                         (let ((proj-meta (jorg-project-meta x)))
                           (if proj-meta
                               (cons (expand-file-name x) proj-meta)
                             nil)))
                       (jorg-projects-files))))

(defun jorg-projects-meta-cache-add-project (filename &optional replace)
  "Add new meta to cache using FILENAME, push to front of alist, unless optional REPLACE is not nil."
  (if replace
      (setf (cdr (assoc-string filename jorg-projects-meta-cache)) (jorg-project-meta filename))
    (push (jorg-project-meta filename) jorg-projects-meta-cache)))

(defun jorg-projects-meta-cache-remove-project (filename)
  "Remove project from meta cache using FILENAME."
  (setq jorg-projects-meta-cache-remove-project
        (assq-delete-all (car (assoc-string filename jorg-projects-meta-cache)) jorg-projects-meta-cache)))

(defun jorg-switch-project ()
  "Switch buffer to a jorg project."
  (interactive)
  (find-file (jorg-select-project)))

(defun jorg-project-on-save ()
  "Perform any on-save action."
  (interactive)
  (save-window-excursion
    (save-excursion
      (when (jorg-find-project-heading)
        (jorg-add-remove-project-agenda)
        (message "Saved project %s" (buffer-name))))))

(defun jorg-add-remove-project-agenda ()
  "Add current project to agenda if not already one, remove archived ones."
  (save-excursion
    (when (jorg-find-project-heading)
      (if (member "ARCHIVE" (org-get-local-tags))
          (progn
            (org-remove-file)
            (jorg-projects-meta-cache-remove-project (buffer-file-name)))
        (if jorg-recently-saved-projects-to-front
            (progn
              (org-remove-file) ;; remove this file from org
              (org-agenda-file-to-front) ;;add it back to the front
              (jorg-projects-meta-cache-remove-project (buffer-file-name)) ;; remove from meta cache
              (jorg-projects-meta-cache-add-project (buffer-file-name)) ;;add back to front
              )
          (unless (org-agenda-file-p) (org-agenda-file-to-front)) ;;else only add if not in agenda
          (jorg-projects-meta-cache-add-project (buffer-file-name) t)) ;;add meta to cache, or replace if exists
        ))))


(defun jorg-archived-p ()
  "Return non-nil if current heading has archived tag."
  (member "ARCHIVE" (org-get-tags-at)))

(defun jorg-project-p ()
  "Determine whether current buffer is a jorg project or not."
  (save-excursion
    (if (jorg-find-project-heading)
        t
      nil)))

(defun jorg-find-project-heading ()
  "Return the position of project heading or nil."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\*+ PROJECT " nil t)))

(defun jorg-goto-project-heading ()
  "Move point to project heading."
  (interactive)
  (goto-char (jorg-find-project-heading)))

(provide 'jorg-project)

;;; jorg-project.el ends here
