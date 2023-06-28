;;; klid-transaction.el --- Helper code for use with the "klid" accounting system -*- lexical-binding: t; -*-

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
;; This module serves as the entrypoint to the whole "klid" accounting
;; system.  It mainly provides key data structure that represents the
;; transaction and a set of utilities for reading, sorting, parsing and
;; exporting the transactions.

;;; Code:

(require 'cl-lib)
(require 'klid-datetime)
(require 'klid-export)

(cl-defstruct (klid-transaction
               (:type list))
  "A structure representing accounting transaction."
  (date nil :documentation "The date of the transaction (`decoded-time' format).")
  (document nil :documentation "The document associated with the transaction (string).")
  (amount nil :documentation "The amount of the transaction (number).")
  (description nil :documentation "A description of the transaction (string).")
  (debit-account nil :documentation "The debit account involved in the transaction (string).")
  (credit-account nil :documentation "The credit account involved in the transaction (string).")
  (note nil :documentation "Additional notes or comments about the transaction (string)."))

(defun klid-transaction-earlier-p (tx1 tx2)
  "Sorting predicate compatible with sort functions.

Both TX1 and TX2 are `klid-transaction' data structures."
  (<
   (klid-datetime-to-timestamp (klid-transaction-date tx1))
   (klid-datetime-to-timestamp (klid-transaction-date tx2))))

(defun klid-transaction-sort-by-date (txs)
  "Sort TXS chronologically.

TXS is a list of `klid-transaction'."
  (sort txs 'klid-transaction-earlier-p))

(defun klid-transaction-parse-list (tx)
  "Create `klid-transaction' structure from TX.

TX must be a list containing valid transaction values - date
in DD.MM.YYYY format, accounting document ID (string), non-zero
amount (either string or number), brief description (string),
debit account (string), credit account (string) and note (string).
If the list doesn't hold valid transaction data then this function
signals `klid-parse-error'"
  (cond
   ((not (listp tx)) (signal 'klid-parse-error tx))
   ((< (length tx) 7) (signal 'klid-parse-error tx))
   ((not (or (stringp (nth 2 tx)) (numberp (nth 2 tx)))) (signal 'klid-parse-error (nth 2 tx))))
  (let* ((amount-raw (nth 2 tx))
	 (amount (if (stringp amount-raw)
		     (string-to-number amount-raw)
		   (when (numberp amount-raw) amount-raw))))
    (when (= amount 0) (signal 'klid-parse-error tx))
    (make-klid-transaction
     :date (klid-datetime-csn-01-6910-parse (nth 0 tx))
     :document (nth 1 tx)
     :amount amount
     :description (nth 3 tx)
     :debit-account (nth 4 tx)
     :credit-account (nth 5 tx)
     :note (nth 6 tx))))

(defun klid-transaction-parse-lists (data)
  "Parse transactions nested in DATA list.

This function iteratively calls `klid-transaction-parse-list'."
  (let ((txs '())
	(skipped 0))
    (dolist (elt data)
      (condition-case nil
	  (push (klid-transaction-parse-list elt) txs)
	(klid-parse-error (setq skipped (1+ skipped)))))
    (unless (= skipped 0) (message "Skipped %d records due to an error" skipped))
    (klid-transaction-sort-by-date txs)))

(defun klid-transaction-export-transactions-to-list (txs)
  "Export TXS to list format compatible with `org-table-export' function.

TXS is a list of `klid-transaction'."
  (mapcar (lambda (tx)
	    (let ((new-tx (copy-sequence tx)))
	      (setf (klid-transaction-date new-tx)
		    (klid-datetime-csn-01-6910-to-string (klid-transaction-date new-tx)))
	      (setf (klid-transaction-amount new-tx)
		    (number-to-string (klid-transaction-amount new-tx)))
	      new-tx))
	  txs))

(defun klid-transaction-export-transactions-to-table.el (txs &optional params)
  "Export TXS to table.el.

TXS is a list of `klid-transaction'.  PARAMS is a property list
of parameters that can influence the conversion.  All parameters
from ‘orgtbl-to-generic’ are supported."
  (let ((table (klid-transaction-export-transactions-to-list txs)))
    (if (= (length table) 0)
	""
      (push 'hline table)
      (push '("DATUM" "DOKLAD" "ČÁSTKA" "POPIS" "MD" "DAL" "POZNÁMKA") table)
      (push 'hline table))
    (nconc table (list 'hline))
    (klid-export-orgtbl-to-table.el table params)))

(defun klid-transaction-export-transactions-to-org (txs &optional params)
  "Export TXS to table.el with some additional text.

TXS is a list of `klid-transaction'.  PARAMS is a property list
of parameters that can influence the conversion.  All parameters
from ‘orgtbl-to-generic’ are supported."
  (with-temp-buffer
    (insert
     "* Účetní deník\n"
     (klid-transaction-export-transactions-to-table.el txs params)
     "\n\n")
    (buffer-string)))

(provide 'klid-transaction)

;;; klid-transaction.el ends here
