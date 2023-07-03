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
;; transaction and a set of utilities for reading, sorting, and parsing
;; the transactions.

;;; Code:

(require 'cl-lib)
(require 'klid-datetime)

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

TXS is a list, and each element within the list is itself a list
with the same structure as `klid-transaction'."
  (sort txs 'klid-transaction-earlier-p))

(defun klid-transaction-list-parsable-p (list)
  "Check if the provided LIST is parsable as a `klid-transaction'.

LIST is considered parsable if it satisfies the following conditions:
- It is a list.
- It has at least seven elements and all of them are strings.
- The first element is a date string in ČSN 01 6910 format.
- The third element is a non-zero number (string).

If the provided list meets all the criteria, this function returns non-nil.
Otherwise, it returns nil."
  (and
   (listp list)
   (>= (length list) 7)
   (list-of-strings-p list)
   (klid-datetime-csn-01-6910--match
    klid-datetime-csn-01-6910--full-date-match
    (nth 0 list))
   (not (= (string-to-number (nth 2 list)) 0))))

(defun klid-transaction-from-list (list)
  "Create `klid-transaction' structure from LIST.

LIST must contain valid transaction values:
 - Date in DD.MM.YYYY format (string)
 - Accounting document ID (string)
 - Non-zero amount (string)
 - Brief description (string)
 - Debit account (string)
 - Credit account (string)
 - Note (string)

If the LIST does not meet the criteria, this function signals
`klid-parse-error'."
  ;; This function is mainly used in conjunction with `org-table-to-lisp'
  (unless (klid-transaction-list-parsable-p list)
    (signal 'klid-parse-error list))
  (make-klid-transaction
   :date (klid-datetime-csn-01-6910-parse (nth 0 list))
   :document (nth 1 list)
   :amount (string-to-number (nth 2 list))
   :description (nth 3 list)
   :debit-account (nth 4 list)
   :credit-account (nth 5 list)
   :note (nth 6 list)))

(defun klid-transaction-from-list-iter (list-of-lists)
  "Convert LIST-OF-LISTS into list of `klid-transaction' structures.

This function iteratively calls `klid-transaction-from-list' for
every element of LIST-OF-LISTS.  Final list of transactions is also
sorted by date."
  (let ((txs '())
	(skipped 0))
    (dolist (elt list-of-lists)
      (condition-case nil
	  (push (klid-transaction-from-list elt) txs)
	(klid-parse-error (setq skipped (1+ skipped)))))
    (unless (= skipped 0)
      (message "Skipped %d records due to an error" skipped))
    (klid-transaction-sort-by-date txs)))

(provide 'klid-transaction)

;;; klid-transaction.el ends here
