;;; klid-export.el --- Helper code for use with the "klid" accounting system -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 Jakub Ječmínek

;; This file is not part of GNU Emacs.

;; Author: Jakub Ječmínek

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Klid is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Klid is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Klid.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; Utility functions for exporting data.

;;; Code:

(require 'org-table)

(defun klid-export-orgtbl-to-table.el (table &optional params)
  "Convert the `orgtbl-mode' TABLE into a table.el.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.

Similar to `orgtbl-to-table.el' except that it fixes Bug#64205.
Comitted as 4c01b0deee1 to master branch."
  (with-temp-buffer
    (insert (orgtbl-to-orgtbl table params))
    (org-table-align)
    (goto-char (point-min))
    (while (search-forward "-|" nil t)
      (replace-match "+|"))
    (goto-char (point-min))
    (while (search-forward "|-" nil t)
      (replace-match "|+"))
    (buffer-string)))

(defalias 'klid-export-table.el-to-list 'org-table-to-lisp)

(provide 'klid-export)

;;; klid-export.el ends here
