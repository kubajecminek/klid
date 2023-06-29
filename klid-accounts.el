;;; klid-accounts.el --- Helper code for use with the "klid" accounting system -*- lexical-binding: t; -*-

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
;; This module offers a range of utilities for sorting, filtering, and
;; outputting accounts utilized in accounting transactions.

;;; Code:

(require 'cl-lib)
(require 'klid-transaction)
(require 'klid-export)

(defun klid-accounts-sort (accounts)
  "Sort ACCOUNTS using `string-lessp' predicate.

ACCOUNTS is a list of strings."
  (cl-sort accounts 'string-lessp :key 'downcase))

(defun klid-accounts-unique (txs)
  "Return unique accounts from TXS that were used on either debit or credit side.

TXS is a list, and each element within the list is itself a list
with the same structure as `klid-transaction'.  Return value is a
list of strings."
  (let ((accounts nil))
    (dolist (tx txs)
      (push (klid-transaction-debit-account tx) accounts)
      (push (klid-transaction-credit-account tx) accounts))
    (klid-accounts-sort (delete-dups accounts))))

(defun klid-accounts-export-to-table.el (accounts &optional params)
  "Export ACCOUNTS to table.el.

ACCOUNTS is a list of strings.  PARAMS is a property list
of parameters that can influence the conversion.  All parameters
from ‘orgtbl-to-generic’ are supported."
  (let ((table nil))
    (push 'hline table)
    (push '("POŘADOVÉ ČÍSLO" "ÚČET") table)
    (push 'hline table)
    (dotimes (i (length accounts))
      (push `(,(format "%s" (number-to-string (1+ i)))
	      ,(nth i accounts))
	    table))
    (push 'hline table)
    (setq table (nreverse table))
    (klid-export-orgtbl-to-table.el table params)))

(defun klid-accounts-export-to-org (accounts &optional params)
  "Export ACCOUNTS to table.el with some additional markup.

ACCOUNTS is a list of strings.  PARAMS is a property list
of parameters that can influence the conversion.  All parameters
from ‘orgtbl-to-generic’ are supported."
  (with-temp-buffer
    (insert
     "* Seznam použitých účtů\n"
     (klid-accounts-export-to-table.el accounts params)
     "\n\n")
    (buffer-string)))

(provide 'klid-accounts)

;;; klid-accounts.el ends here
