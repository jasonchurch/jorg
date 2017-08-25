;;; jorg-capture-project.el --- Jason's Org Capture Customizations

;;; Commentary:

;; This file contains Jason's Org Capture customizations.  The primary
;; motivation for these customizations was a desire to be able to
;; capture projects and their related tasks into separate org files,
;; while maintaining a master org file with a list of projects.

;; The intention is for a user to create a new project using one
;; capture to create a new project folder, with a prefix date, and add
;; an entry into the global org file.  Then the user should use
;; another capture to add tasks to the project org file they are in.

;;; Code:

(require 'org)
(require 'subr-x)

(defvar org-capture-templates
  "A global var from org lib, it holds list of org capture templates.")

(defvar org-capture-plist
  "A global var from org lib, it old the plist for current capture.")

;; JOrg defvars
(defvar jorg-project-base-dir "~/jorg/projects"
  "The location jorg-project should create project dirs in.")

(defvar jorg-text-template-color "blue"
  "The color of any template text in the JOrg Project org file.")

(defvar jorg-capture-key-main "j"
  "The key to show the jorg capture templates from org capture templates, `C-c c <key>'.")

(defvar jorg-capture-project-task-target "TASKS"
  "The target heading under which to file tasks in the project file.")

(defvar jorg-capture-project-update-target "UPDATES"
  "The target heading under which to file tasks in the project file.")

(defvar jorg-user-name nil
  "The user's name.")

(defvar jorg-user-email nil
  "The user's email.")

(defvar jorg-recently-saved-projects-to-front t
  "Moves recently saved projects to front of the agenda list if non nil.
This help keeps active projects at the fore front.")

;; Hook up Jorg Capture
(add-hook 'org-capture-before-finalize-hook 'jorg-capture-project-template-finalize-hook)

;; Hook up save hook after-save-hook?
(add-hook 'after-save-hook 'jorg-project-on-save)

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
             `(,(concat jorg-capture-key-main "p") "JORG Project Template" plain (function jorg-create-temporary-template-buffer)
               (function jorg-project-template)
               :empty-lines 1
               )
             t)

(defun jorg-capture-project-template-finalize-hook ()
  "Finalize the JORG project template."
  (when (equal "JORG Project Template" (plist-get org-capture-plist :description))
    (message "capture type:%s" (plist-get org-capture-plist ':description ))
    ;; compute filename
    ;; setup OrgFile Properties: Category, Author, Title, etc.
    ;;
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
      (buffer-string)
      )
    )
  )

(defun jorg-capture-template-update ()
  "Create capture template for next items.
It will use optional jorg-capture-scheduled and
jorg-capture-deadline properties to set a date with org dates or
date increments, ex +1d."
  (message "indent: %s" (org-capture-get :jorg-indent))
  (with-temp-buffer
    (insert (concat "   - " (jorg-inactive-time-stamp) " %?"))
    (buffer-string)
    )
  )


(defun jorg-project-template ()
  "Create Project Heading template."
  (message "plist: %s" org-capture-plist)
  (with-temp-buffer
    (insert "* PROJECT [#C] %?\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :CATEGORY: \n")
    (insert "  :CREATED_DATE: %(get-datetime)\n")
    (insert "  :ALT_NAME:\n")
    (insert "  :ID: %(string-trim(org-id-new))\n")
    (insert " :END:\n")
    (buffer-string)
    )
  )

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
        (capture-file-name (jorg-select-project-from-agenda-files)))
    (set-buffer (find-file capture-file-name))
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote target))
         nil t)
        (goto-char (point-at-eol))
      (error (concat "No project target: " target " in " (buffer-file-name))))
    (org-mode)
    )
  )

(defun jorg-select-project-from-agenda-files ()
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
           agenda-pairs
           ))))
;;(jorg-select-project-from-agenda-files)

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
  (cl-remove-if 'null (mapcar
                       (lambda (x)
                         (let ((proj-meta (jorg-project-meta x)))
                           (if proj-meta
                               `(,(expand-file-name x) . ,proj-meta)
                             nil)))
                       (org-agenda-files))))
;;(cdr (assoc-string "~/org/2017/projects/20170819_233419_Note4Roms/Note4Roms.org" (jorg-all-projects-meta)))

(defun jorg-select-heading-for-list-capture-entry ()
  "Select the project for the capture list.
Locate the target heading and then the first listen within the heading."
  (let ((target (org-capture-get :jorg-capture-target))
        (point-at-end-of-heading)
        (capture-file-name (jorg-select-project-from-agenda-files)))
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
    (org-mode)
    )
  )
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
    (insert (concat "   - " (jorg-inactive-time-stamp) " initial update\n"))
    (insert "** TASKS\n\n")
    (insert "** REFERENCE\n")
    (insert "   :PROPERTIES:\n")
    (insert "   :VISIBILITY: folded\n")
    (insert "   :END:\n")))

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
          (org-remove-file)
        (when jorg-recently-saved-projects-to-front (org-remove-file))
        (unless (org-agenda-file-p)
          (org-agenda-file-to-front))))))

(defun jorg-archived-p ()
  "Return non-nil if current heading has archived tag."
  (member "ARCHIVE" (org-get-tags-at))
  )

(defun jorg-project-p ()
  "Determine whether current buffer is a jorg project or not."
  (save-excursion
    (if (jorg-find-project-heading)
        t
      nil)))
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

(defun insert-template-text (string)
  "Insert into the current buffer STRING formated as template text."
  (insert (propertize string
                      'font-lock-face '(:foreground "LightSkyBlue" :slant italic))))

;;;;;;;;;;;;;;;;;;
;; JOrg General ;;
;;;;;;;;;;;;;;;;;;
(defun jorg-find-project-heading ()
  "Return the position of project heading or nil."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\*+ PROJECT " nil t)
    )
  )

(defun jorg-goto-project-heading ()
  "Move point to project heading."
  (interactive)
  (goto-char (jorg-find-project-heading))
  )
;;;;;;;;;;;;;;;;;;
;; User Utility ;;
;;;;;;;;;;;;;;;;;;
(defun get-date ()
  "Get the current date yyyy-mm-dd."
  (interactive)
  (format-time-string "%Y-%m-%d")
  )

(defun jorg-inactive-time-stamp ()
  "Get the current date as an org inactive timestamp."
  (concat "[" (format-time-string "%Y-%m-%d %a") "]")
  )

(defun get-datetime ()
  "Get the current date and time yyyy-mm-dd HH:mm:ss."
  (interactive)
  (format-time-string "%Y-%m-%d %H:%M:%S")
  )

(defun insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun copy-file-path ()
  "Copies the file path of current buffer to kill ring."
  (interactive)
  (message "%s" buffer-file-name)
  (kill-new buffer-file-name))
;; User Utility ends here

(defun generate-uuid ()
  "Generate a UUID.  Implemented by calling Linux uuidgen."
  (interactive)
  (shell-command-to-string "/usr/bin/uuid"))

(provide 'jorg-capture-project)

;;; jorg-capture-project.el ends here
