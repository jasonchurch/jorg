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


;; Hook up Jorg Capture
(add-hook 'org-capture-before-finalize-hook 'jorg-capture-finalize-hook)
(add-to-list 'org-capture-templates
             '("p" "JOrg Project" entry (file+headline  "~/org/2017/gtd.org" "10K Projects")
               "* PROJECT %?\n  :PROPERTIES:\n  :CATEGORY: project\n  :CREATED_DATE: %(get-datetime)\n  :ALT_NAME:\n  :END:\n")
             t)

(defvar jorg-project-base-dir ""
  "The location jorg-project should create project dirs in.")

(defvar jorg-text-template-color "blue"
  "The color of any template text in the JOrg Project org file.")

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
    (org-set-property "PROJ_FILE" jorg-project-filepath)
    (message "project:%s on %s has alt name:'%s'" project-heading created-date alt-name)
    (make-directory jorg-project-dirpath 't)
    (save-excursion
      (let ()
        (setq new-project-buffer (set-buffer (generate-new-buffer jorg-project-filepath)))
        (set-visited-file-name jorg-project-filepath)

        (insert (concat "* PROJECT " project-heading "\n" ))
        (org-set-property "PROJ_FILE" (concat "file:" jorg-project-filepath))
        (org-set-property "CREATED_DATE" created-date )
        (when alt-name (org-set-property "ALT_NAME" alt-name))
        (insert-template-text "   Enter project description.\n")

        (insert "\n** UPDATES\n")
        (insert-template-text "   Entry any highlevel updates you have for the project here so that \n   its easier to switch contexts by reading updates.\n\n")
        (insert "   - ")
        (insert-template-text "  <2017-07-02 Sun>  (C-c . to get datestamp) some update for this project.\n")

        (insert "\n** TASKS [/]\n")
        (insert-template-text "   Add the next steps as TODO items under TASKS. Ensure a scheduled\n   date or deadline if the task is being planned and you want it to \n   show up in agenda weekly view.\n")

        (insert "\n** REFERENCE\n")
        (insert-template-text "   Any reference items like URLs, notes, code clips, rough drafts,\n   ideas, etc. should go into headings under this REFERENCE.\n")

        (save-buffer)
        ))
    (switch-to-buffer-other-window new-project-buffer)
    (set-buffer capture-buffer)
    (message "jorg: created new project at: %s" jorg-project-filepath)))

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



(provide 'jorg-capture-project)

;;; jorg-capture-project.el ends here
