;;; klid-filter.el --- Helper code for use with the "klid" accounting system -*- lexical-binding: t; -*-

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
;; Utility functions for filtering data.

;;; Code:

(require 'seq)
(require 'klid-transaction)

(defun klid-filter-transactions-by-date (txs &optional from to)
  "Filter TXS by date.

TXS is a list, and each element within the list is itself a list
with the same structure as `klid-transaction'.  This function filters out
transactions that occurred before the date specified by FROM and after the
date specified by TO.  Both FROM and TO arguments must be provided in one
of the following formats:
  - Strings in the ČSN 01 6910 standard (i.e., DD.MM.YYYY)
  - Valid `decoded-time' values.

The function returns a new list containing the filtered transactions."
  (let ((start (cond
		((or (null from) (string= "" from)) 0)
		((stringp from) (klid-datetime-to-timestamp (klid-datetime-csn-01-6910-parse from)))
		(t (klid-datetime-to-timestamp from))))
	(end (cond
	      ((or (null to) (string= "" to)) 253402214400) ; 31.12.9999
	      ((stringp to) (klid-datetime-to-timestamp (klid-datetime-csn-01-6910-parse to)))
	      (t (klid-datetime-to-timestamp to)))))
    (seq-filter
     #'(lambda (elt)
	 (and
	  (>= (klid-datetime-to-timestamp (klid-transaction-date elt)) start)
	  (<= (klid-datetime-to-timestamp (klid-transaction-date elt)) end)))
     txs)))

(defun klid-filter-transactions-by-account (txs account-prefix)
  "Filter TXS by ACCOUNT-PREFIX.

TXS is a list, and each element within the list is itself a list
with the same structure as `klid-transaction'.  This function filters
transactions from TXS that do not contain ACCOUNT-PREFIX on either the
debit or credit side.  Account comparison is performed using `string-prefix-p'
internally.

The function returns a new list containing the filtered transactions."
  (seq-filter
   #'(lambda (elt)
       (or
	(string-prefix-p account-prefix (klid-transaction-debit-account elt))
	(string-prefix-p account-prefix (klid-transaction-credit-account elt))))
   txs))

(defun klid-filter-org-hline-symbol (table)
  "Remove hline symbol from TABLE.

TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line."
  (seq-filter #'(lambda (elt) (not (symbolp elt))) table))

(provide 'klid-filter)

;;; klid-filter.el ends here
