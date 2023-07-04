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
(require 'klid-transaction)
(require 'klid-ledger)
(require 'klid-accounts)

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

(defun klid-export-transactions-to-list (txs)
  "Export TXS to list format compatible with `orgtbl-to-generic' function.

TXS is a list, and each element within the list is itself a list
with the same structure as `klid-transaction'."
  (mapcar (lambda (tx)
	    (let ((new-tx (copy-sequence tx)))
	      (setf (klid-transaction-date new-tx)
		    (klid-datetime-csn-01-6910-to-string
		     (klid-transaction-date new-tx)))
	      (setf (klid-transaction-amount new-tx)
		    (number-to-string (klid-transaction-amount new-tx)))
	      new-tx))
	  txs))

(defun klid-export-transactions-to-table.el (txs &optional params)
  "Export TXS to table.el.

TXS is a list, and each element within the list is itself a list
with the same structure as `klid-transaction'.  PARAMS is a property
list of parameters that can influence the conversion.  All parameters
from ‘orgtbl-to-generic’ are supported."
  (let ((table (klid-export-transactions-to-list txs)))
    (if (= (length table) 0)
	""
      (push 'hline table)
      (push '("DATUM" "DOKLAD" "ČÁSTKA" "POPIS" "MD" "DAL" "POZNÁMKA") table)
      (push 'hline table))
    (nconc table (list 'hline))
    (klid-export-orgtbl-to-table.el table params)))

(defun klid-export-transactions-to-org (txs &optional params)
  "Export TXS to table.el with some additional markup.

TXS is a list, and each element within the list is itself a list
with the same structure as `klid-transaction'.  PARAMS is a property
list of parameters that can influence the conversion.  All parameters
from ‘orgtbl-to-generic’ are supported."
  (with-temp-buffer
    (insert
     "* Účetní deník\n"
     (klid-export-transactions-to-table.el txs params)
     "\n\n")
    (buffer-string)))

(defun klid-export-account-subledger-to-table.el (general-ledger account &optional params)
  "Export ACCOUNT subledger from GENERAL-LEDGER to table.el.

GENERAL-LEDGER is a hash-table where each account is mapped to its corresponding
`klid-ledger-account-subledger' structure.  PARAMS is a property list of parameters
that can influence the conversion.  All parameters from ‘orgtbl-to-generic’ are
supported."
  (let ((account-subledger (gethash account general-ledger))
	(table nil))
    (if (null account-subledger)
	""
      (push 'hline table)
      (push '("DATUM" "DOKLAD" "POPIS" "MD [KČ]" "DAL [KČ]" "SALDO [KČ]" "PROTIÚČET") table)
      (push 'hline table)
      (dolist (record (klid-ledger-account-subledger-records account-subledger))
	(push
	 `(,(klid-datetime-csn-01-6910-to-string (klid-ledger-record-date record))
	   ,(klid-ledger-record-document record)
	   ,(klid-ledger-record-description record)
	   ,(number-to-string (klid-ledger-record-debit-amount record))
	   ,(number-to-string (klid-ledger-record-credit-amount record))
	   ,(number-to-string (klid-ledger-record-balance record))
	   ,(klid-ledger-record-counter-account record))
	 table))
      (push 'hline table)
      (push `(""
	      ""
	      "SUMA"
	      ,(number-to-string
		(klid-ledger-account-subledger-total-debit account-subledger))
	      ,(number-to-string
		(klid-ledger-account-subledger-total-credit account-subledger))
	      ,(number-to-string
		(klid-ledger-account-subledger-total-balance account-subledger))
	      "")
	    table)
      (push 'hline table)
      (setq table (nreverse table))
      (klid-export-orgtbl-to-table.el table params))))

(defun klid-export-general-ledger-to-org
    (general-ledger &optional account-prefix params)
  "Export subledgers from GENERAL-LEDGER to table.el with some additional markup.

This function exports subledgers from GENERAL-LEDGER that contain ACCOUNT-PREFIX.
GENERAL-LEDGER is a hash-table where each account is mapped to its corresponding
`klid-ledger-account-subledger' structure.  PARAMS is a property list of parameters
that can influence the conversion.  All parameters from ‘orgtbl-to-generic’ are
supported."
  (let* ((keys nil)
	 (sorted-keys nil)
	 (prefix (or account-prefix "")))
    (maphash
     (lambda (k v) (when (string-prefix-p prefix k) (push k keys)))
     general-ledger)
    (setq sorted-keys (klid-accounts-sort keys))
    (with-temp-buffer
      (insert "* Hlavní kniha\n")
      (dolist (acc sorted-keys)
	(insert
	 (format "** Účet: %s\n\n" acc)
	 (klid-export-account-subledger-to-table.el general-ledger acc params)
	 "\n\n"))
      (buffer-string))))

(defun klid-export-accounts-to-table.el (accounts &optional params)
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

(defun klid-export-accounts-to-org (accounts &optional params)
  "Export ACCOUNTS to table.el with some additional markup.

ACCOUNTS is a list of strings.  PARAMS is a property list
of parameters that can influence the conversion.  All parameters
from ‘orgtbl-to-generic’ are supported."
  (with-temp-buffer
    (insert
     "* Seznam použitých účtů\n"
     (klid-export-accounts-to-table.el accounts params)
     "\n\n")
    (buffer-string)))

(provide 'klid-export)

;;; klid-export.el ends here
