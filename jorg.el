;;; jorg.el --- Jay's ORG enhancements

;; Copyright (C) 2017 Jason Church

;; Author: Jason Church <jasonchurch@edeveloper.ca>
;; Version: 1.0.0
;; Keywords: outlines jay jorg

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

;; Jay's ORG enhancements provides an opinionated approach to using
;; ORG for projects.  It takes the approach of treating each project
;; as its own org file, living in its own directory along with any
;; related files, images, etc.  Each project has a PROJECT (custom
;; TODO) with the project name along with 3 main sub heading: updates,
;; tasks and reference.  Updates provide a useful section reminding
;; one, ones boss, and other intersted parties of the current
;; progress.  Tasks is where all your next actions (GTD influence),
;; while reference provides a place for snippets, research, etc.  This
;; may not be useful for everyone, but I have found it has helped me
;; manage a large number of projects both at work and home.  Active
;; projects are any project files whose PROJECT heading is not
;; archived.  This active projects will be part of org-agenda-files
;; (moved to front on each save), while achived projects are removed
;; fromed from org-agenda-files.

;; What should a project be? Well I've been influenced by Getting
;; Things Done; so, I tend to see a project as anything more than a
;; simple standalone task.  It could be a large work related project
;; that could run for months with many updates, tasks and reference
;; material or it could be a very small project with just a few tasks.
;; I might consider creating a project called unfiled to hold one off
;; tasks, or incoming tasks that need to be considered and refiled to
;; a proper project.  The rest of the GTD is in flux for me; I do have
;; the 20K-50K views jotted down in a single file.  I've found moving
;; to a file/folder per project (possibily using a date folder, helps
;; keep a managable structure that doesn't need to be pruned: ex
;; ~/org/projects/2016/someproject/someproject.org.

;;; Code:
(require 'org)
(require 'subr-x)
(require 'jorg-capture)
(require 'jorg-common)
(require 'jorg-project)

(provide 'jorg)
;;; jorg.el ends here
