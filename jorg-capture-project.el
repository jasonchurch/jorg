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

(defvar jorg-capture-summary-file "~/jorg/summary.org"
  "The org file to put the summary of projects.")

(defvar jorg-capture-summary-project-target "Project Summaries"
  "The target heading under which to file project summaries.")

(defvar jorg-capture-summary-task-target "TASKS"
  "The target heading under which to file tasks in the summary file.")

(defvar jorg-capture-project-task-target "TASKS"
  "The target heading under which to file tasks in the project file.")

;; Hook up Jorg Capture
(add-hook 'org-capture-before-finalize-hook 'jorg-capture-finalize-hook)

;; Hook up save hook after-save-hook?
(add-hook 'after-save-hook 'jorg-sync-projects-to-summaries)

;; JOrg Captures Templates
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

(add-to-list 'org-capture-templates
             `(,jorg-capture-key-main "JOrg")
             t)
(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "p") "JOrg Project" entry (file+headline  ,jorg-capture-summary-file ,jorg-capture-summary-project-target)
               "* %?\n  :PROPERTIES:\n  :CATEGORY: PROJ_SUM\n  :CREATED_DATE: %(get-datetime)\n  :ALT_NAME:\n  :ID: %(string-trim(org-id-new \"summary\"))\n :END:\n"
               :empty-lines 1
               )
             t)
(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "n") "Next (Next Action)" entry (function jorg-find-project-or-summary-file)
               (function jorg-capture-template-next)
               :empty-lines 1
               :project-target ,jorg-capture-project-task-target :summary-file ,jorg-capture-summary-file :summary-target ,jorg-capture-summary-task-target)
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "s") "Scheduled Next Actions")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "st") "Tomorrow" entry (function jorg-find-project-or-summary-file)
               (function jorg-capture-template-next)
               :empty-lines 1
               :project-target ,jorg-capture-project-task-target :summary-file ,jorg-capture-summary-file
               :summary-target ,jorg-capture-summary-task-target :jorg-capture-scheduled "+1d")
             t)
(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "sw") "Week)" entry (function jorg-find-project-or-summary-file)
               (function jorg-capture-template-next)
               :empty-lines 1
               :project-target ,jorg-capture-project-task-target :summary-file ,jorg-capture-summary-file
               :summary-target ,jorg-capture-summary-task-target :jorg-capture-scheduled "+1w")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "sm") "Month" entry (function jorg-find-project-or-summary-file)
               (function jorg-capture-template-next)
               :empty-lines 1
               :project-target ,jorg-capture-project-task-target :summary-file ,jorg-capture-summary-file
               :summary-target ,jorg-capture-summary-task-target :jorg-capture-scheduled "+1m")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "d") "Deadline Next Actions")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "dt") "Tomorrow" entry (function jorg-find-project-or-summary-file)
               (function jorg-capture-template-next)
               :empty-lines 1
               :project-target ,jorg-capture-project-task-target :summary-file ,jorg-capture-summary-file
               :summary-target ,jorg-capture-summary-task-target :jorg-capture-deadline "+1d")
             t)
(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "dw") "Week)" entry (function jorg-find-project-or-summary-file)
               (function jorg-capture-template-next)
               :empty-lines 1
               :project-target ,jorg-capture-project-task-target :summary-file ,jorg-capture-summary-file
               :summary-target ,jorg-capture-summary-task-target :jorg-capture-deadline "+1w")
             t)

(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "dm") "Month" entry (function jorg-find-project-or-summary-file)
               (function jorg-capture-template-next)
               :empty-lines 1
               :project-target ,jorg-capture-project-task-target :summary-file ,jorg-capture-summary-file
               :summary-target ,jorg-capture-summary-task-target :jorg-capture-deadline "+1m")
             t)


(add-to-list 'org-capture-templates
             `(,(concat jorg-capture-key-main "t") "Todo (unsure, maybe)" entry (function jorg-find-project-or-summary-file)
               "** TODO %?\n\n"
               :empty-lines 1
               :project-target ,jorg-capture-project-task-target :summary-file ,jorg-capture-summary-file :summary-target ,jorg-capture-summary-task-target)
             t)


(defun jorg-capture-finalize-hook ()
  "Create a new project file after the project is finalized in main org file."
  (message "capture type:%s" (plist-get org-capture-plist ':description ))
  (when (equal "JOrg Project" (plist-get org-capture-plist ':description ))
    (jorg-create-project org-capture-plist (org-heading-components))))

(defun jorg-create-project (capture-plist heading-components)
  "Create a project file within a project location using CAPTURE-PLIST and HEADING-COMPONENTS."

  (let* ((project-heading (nth 4 heading-components))
         (capture-buffer (current-buffer))
         (created-date (org-entry-get nil "CREATED_DATE"))
         (alt-name (org-entry-get nil "ALT_NAME"))
         (id (org-entry-get nil "ID"))
         (priority (org-entry-get nil "PRIORITY"))
         (tags (org-entry-get nil "TAGS"))
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
         (new-project-buffer nil))
    (org-set-property "PROJ_FILE" (concat "file:" jorg-project-filepath))
    (unless (> (length alt-name) 0) (progn
                                      (setq alt-name (substring jorg-project-filename 0 10))
                                      (org-set-property "ALT_NAME" alt-name)))
    (message "project:%s on %s has alt name:'%s'" project-heading created-date alt-name)
    (make-directory jorg-project-dirpath 't)
    (save-excursion
      (let ()
        (setq new-project-buffer (set-buffer (generate-new-buffer jorg-project-filepath)))
        (set-visited-file-name jorg-project-filepath)
        (org-agenda-file-to-front)

        ;;remove summary prefix from id
        (if (equal "summary:" (substring id 0 8))
            (setq id (substring id 8))
          )
        ;; Add project heading, properties and text
        (insert (concat "* PROJECT " project-heading "\n" ))

        (org-set-property "PROJ_FILE" (concat "file:" jorg-project-filepath))
        (org-set-property "CREATED_DATE" created-date )
        (when priority (org-entry-put nil "PRIORITY" priority))
        (when tags (org-set-tags-to tags))
        (when id (org-set-property "ID" id))
        (when alt-name (org-set-property "ALT_NAME" alt-name))
        (insert-template-text "   Enter project description.\n")

        ;; Add UPDATES heading and text
        (insert "\n** UPDATES\n")
        (insert-template-text "   Entry any highlevel updates you have for the project here so that \n   its easier to switch contexts by reading updates.\n\n")
        (insert "   - ")
        (insert-template-text "  <2017-07-02 Sun>  (C-c . to get datestamp) some update for this project.\n")

        ;;Add TASKS heading and text
        (insert "\n** TASKS [/]\n")
        (insert-template-text "   Add the next steps as TODO items under TASKS. Ensure a scheduled\n   date or deadline if the task is being planned and you want it to \n   show up in agenda weekly view.\n")

        ;;Add REFERENCE section and text
        (insert "\n** REFERENCE\n")
        (insert-template-text "   Any reference items like URLs, notes, code clips, rough drafts,\n   ideas, etc. should go into headings under this REFERENCE.\n")

        (save-buffer)
        ))
    (switch-to-buffer-other-window new-project-buffer)
    (set-buffer capture-buffer)
    (message "jorg: created new project at: %s" jorg-project-filepath)))


(defun jorg-find-project-or-summary-file ()
  "If current buffer is a project, use it, otherwise find the summary file.
It relies on 'org-capture' properties:
- project-target: the heading under which to file the capture
- summary-file: the summary file to use if current buffer doesn't contain
  project-target
- summary-target the summary heading under which to file the capture if no
  project found."
  (set-buffer (org-capture-target-buffer (buffer-file-name)))
  (org-capture-put-target-region-and-position)
  (let ((hd (org-capture-get :project-target))
        (summary-file (org-capture-get :summary-file))
        (summary-target (org-capture-get :summary-target)))
    (widen)
    (goto-char (point-min))
    (if (and (derived-mode-p 'org-mode)
             (re-search-forward
              (format org-complex-heading-regexp-format (regexp-quote hd))
              nil t))
        (progn (message "position is: %d %d" (point) (point-at-bol))               )
      (message "Current buffer is either not an org file, %s, or doesn't contain target %s. Attempting to capture to summary file %s and target %s."
               (buffer-file-name)
               hd
               summary-file
               summary-target)
      (set-buffer (org-capture-target-buffer summary-file))
      (org-capture-put-target-region-and-position)
      (unless (derived-mode-p 'org-mode)
        (error
         "Target buffer \"%s\" for file+headline should be in Org mode"
         (current-buffer)))
      (widen)
      (goto-char (point-min))
      (if (and (derived-mode-p 'org-mode)
               (re-search-forward
                (format org-complex-heading-regexp-format (regexp-quote hd))
                nil t))
          (progn
            (message "position is: %d %d" (point) (point-at-bol))
            )
        (error
         "Summary buffer \"%s\" doesn't contain a \"%s\" heading; Capture failed"
         summary-file
         summary-target)
        ))))


(defun jorg-sync-summaries-to-project ()
  "Sync change to summaries to project files."
  (let ((absolute-path-summary (expand-file-name jorg-capture-summary-file))
        (absolute-path-current (expand-file-name (buffer-file-name))))
    (save-window-excursion
      (save-excursion
        (cond
         ((equal absolute-path-summary absolute-path-current)
          ;;on summary, sync each project in jorg-capture-summary-project-target
          (let ((heading)
                (id))

            (goto-char (org-find-olp `(jorg-capture-summary-project-target)))
            ;;Find the summary target (* 10K Projects)
            ;;Iterate over the childen (these are the summaries)
            ;;For each child
            ;;  Save window excursan/buffer
            ;;  get properties of child
            ;;  find heading with id of project (strip off summary from child ID)
            ;;  (jorg-entry-update-properties summary-properties)
            (message "Synced summary to projects for %s %s" heading id))
          )
         )))))

(defun jorg-sync-projects-to-summaries ()
  "Sync project with summary if current buffer is a project.
Sync summary with project if current buffer is summary."
  (interactive)
  (save-window-excursion
    (save-excursion
      (when (find-jorg-project)
        (jorg-add-remove-project-agenda)
        (let ((id (org-entry-get nil "ID"))
              (heading (org-entry-get nil "ITEM"))
              (project-properties (org-entry-properties))
              (is-synced nil)
              )
          (when (> (length id) 0)
            (org-id-goto (concat "summary:" id))
            (if (equal
                 (concat "summary:" id) (org-entry-get nil "ID"))
                (when (jorg-entry-update-properties '("ITEM" "PRIORITY" "TAGS" "PROJ_FILE" "CREATED_DATE" "ALT_NAME") project-properties)
                  (message "Synced Jorg Project to summary."))
              (error "Unable to find Org Summary for %s %s" heading id))))))))

(defun jorg-add-remove-project-agenda ()
  "Add current project to agenda if not already one, remove archived ones."
  (save-excursion
    (ignore-errors
      (outline-up-heading 1000))
    (if (member "ARCHIVE" (org-get-local-tags))
        (org-remove-file)
      (unless (org-agenda-file-p)
        (org-agenda-file-to-front)))))

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


(defun find-jorg-project ()
  "Determine if the `current buffer' is a jorg-project.
Return t if jorg-project, otherwise nil."
  (interactive)
  (let ((is-project nil))
    (save-excursion
      (if (derived-mode-p 'org-mode)
          (progn
            (ignore-errors
              (outline-up-heading 1000))
            (if (equal "PROJECT" (org-entry-get nil "TODO"))
                't
              nil))
        nil
        ))))

(defun get-gmail-entries ()
  "Get entries from gmail account."
  (interactive)
  (print "* hello   :sometag:\nsomecontent")
  )

(defun insert-template-text (string)
  "Insert into the current buffer STRING formated as template text."
  (insert (propertize string
                      'font-lock-face '(:foreground "LightSkyBlue" :slant italic))))

;;;;;;;;;;;;;;;;;;
;; User Utility ;;
;;;;;;;;;;;;;;;;;;
(defun get-date ()
  "Get the current date yyyy-mm-dd."
  (interactive)
  (format-time-string "%Y-%m-%d")
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
