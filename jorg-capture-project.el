;;; jorg-capture-project.el --- Jason's Org Capture Project Customizations

;;; Commentary:
;;

;;; Code:

(defun jorg-capture-finalize-hook ()
  "Create a new project file after the project is finalized in main org file."
  (message "capture type:%s" (plist-get org-capture-plist ':description ))
  (when (equal "JOrg Project" (plist-get org-capture-plist ':description ))
    (jorg-create-project (nth 4 (org-heading-components)))))

(defun jorg-create-project (project-heading)
  "Create a project file within a project location using PROJECT-HEADING."
  (let* ((jorg-project-base-dir "/home/red0ck33g/org/2017/projects")
         (jorg-project-dir (replace-regexp-in-string "[^a-zA-Z0-9]" "_" project-heading))
         (jorg-project-dirpath (concat jorg-project-base-dir (unless (equal "/" (substring jorg-project-base-dir -1)) "/") jorg-project-dir))
         (jorg-project-filename (concat (replace-regexp-in-string "[^a-zA-Z0-9]" "_" project-heading) ".org"))
         (jorg-project-filepath (concat jorg-project-dirpath (unless (equal "/" (substring jorg-project-dirpath -1)) jorg-project-filename)))
         )
    (make-directory jorg-project-dirpath 't)
    (save-excursion
      (let ((new-project-buffer (set-buffer (generate-new-buffer (concat jorg-project-dir ".org")))))
        (set-visited-file-name jorg-project-filepath)
        (insert "* PROJECT \n")
        (insert "** UPDATES\n")
        (insert "** TASKS\n")
        (insert "** REFERENCE\n")
        (save-buffer)
        ))
    (message "jorg: created new project at: %s" jorg-project-filepath)))

(defun get-gmail-entries ()
  "Get entries from gmail account."
  (interactive)
  (print "* hello   :sometag:\nsomecontent")
  )
;; User Org Related ends here


;;;;;;;;;;;;;;;;;;
;; User Utility ;;
;;;;;;;;;;;;;;;;;;
(defun get-date ()
  "Get the current date yyyy-mm-dd."
  (interactive)
  (format-time-string "%Y-%m-%d")
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
