;;; jorg-test-common.el --- Tests for jorg-common

;; Copyright (C) 2017 Jason Church

;; Author: Jason Church <jasonchurch@edeveloper.ca>
;; Version: 1.0.0
;; Keywords: outlines jay jorg common test

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

;;; Code:

(require 'jorg-commmon)

(ert-deftest jorg-test-get-date ()
  "Tests jorg-get-date returns proper yyyy-mm-dd format"
  (should (string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" (jorg-get-date))))

(provide 'jorg-test-common)
;;; jorg-test-common.el ends here
