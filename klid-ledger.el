;;; klid-ledger.el --- Helper code for use with the "klid" accounting system -*- lexical-binding: t; -*-

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
;; General ledger is a bookkeeping ledger in which accounting data are
;; posted from journals [1].  General ledger is in our case a hash-table
;; where each key is the account (string) and value is
;; `klid-ledger-account-subledger' structure.  This structure contains total
;; debit amount, total credit amount, as well as total balance.  Additionally,
;; this structure contains all records found in the underlying journal
;; (of type `klid-ledger-record') for this given account.  In practice, this is
;; probably most important module.
;; 
;; [1] https://en.wikipedia.org/wiki/General_ledger

;;; Code:

(require 'cl-lib)
(require 'klid-transaction)

(defun klid-ledger-calc (sym &rest numbers)
  "Perform basic arithmetic operations with `calc' precision.

SYM is a symbol representing the arithmetic operation to be performed
\('+', '-', '*', '/').  NUMBERS are the numbers to be used in the
operation.

The function returns the result as a number."
  (require 'calc)
  (let* ((numbers-subseq
	  (cl-subseq numbers 0 (- (length numbers) 1)))
	 (last-elem (car (last numbers)))
	 (expr (string-join
		(append
		 (mapcar
		  #'(lambda (num)
		      (concat (number-to-string num) (symbol-name sym)))
		  numbers-subseq)
		 (list (number-to-string last-elem))))))
    (string-to-number (calc-eval expr))))

(cl-defstruct (klid-ledger-account-subledger
	       (:type list))
  (records '() :documentation "List of ledger records for an account.")
  (total-debit 0 :documentation "Total debit amount for an account.")
  (total-credit 0 :documentation "Total credit amount for an account.")
  (total-balance 0 :documentation "Total balance for an account."))

(cl-defstruct (klid-ledger-record
	       (:type list))
  (date nil :documentation "Date of the ledger record.")
  (document nil :documentation "Document associated with the ledger record.")
  (description nil :documentation "Description of the ledger record.")
  (debit-amount nil :documentation "Debit amount for the ledger record.")
  (credit-amount nil :documentation "Credit amount for the ledger record.")
  (balance nil :documentation "Balance for the ledger record.")
  (counter-account nil :documentation "Counter account for the ledger record."))

;; Hashmap - KEY = account, VAL = account-subledger
(defun klid-ledger-general-ledger (txs)
  "Create the general ledger from TXS.

TXS is a list, and each element within the list is itself a list
with the same structure as `klid-transaction'.  This function returns
a hash-table where each account is mapped to its corresponding
`klid-ledger-account-subledger' structure."
  (let ((general-ledger (make-hash-table :test 'equal)))
    (dolist (tx txs)
      (let ((dflt (make-klid-ledger-account-subledger)))
        ;; Update debit account subledger
        (let ((account-subledger (gethash
				  (klid-transaction-debit-account tx)
				  general-ledger
				  ;; Set default value
				  dflt)))
          (puthash (klid-transaction-debit-account tx)
                   (klid-ledger-update-account-subledger tx account-subledger t)
                   general-ledger))
        ;; Update credit account subledger
        (let ((account-subledger (gethash
				  (klid-transaction-credit-account tx)
				  general-ledger
				  ;; Set default value
				  dflt)))
          (puthash (klid-transaction-credit-account tx)
                   (klid-ledger-update-account-subledger tx account-subledger nil)
                   general-ledger))))
    general-ledger))

(defun klid-ledger-update-account-subledger (tx account-subledger debit-p)
  "Update the account subledger with a new transaction record.

Returns the updated account-subledger of type `klid-ledger-account-subledger'.
TX is transaction to be added to the ledger of type `klid-transaction',
ACCOUNT-SUBLEDGER is the current account-subledger, and DEBIT-P is a boolean
indicating whether the update is a debit or credit."
  (let* ((amount (klid-transaction-amount tx))
         (record (make-klid-ledger-record
                  :date (klid-transaction-date tx)
                  :document (klid-transaction-document tx)
                  :description (klid-transaction-description tx)
                  :debit-amount (if debit-p amount 0)
                  :credit-amount (if (not debit-p) amount 0)
                  :balance (if debit-p amount (- amount))
                  :counter-account (if debit-p (klid-transaction-credit-account tx)
                                     (klid-transaction-debit-account tx))))
         (total-debit
	  (klid-ledger-calc
	   '+
	   (klid-ledger-account-subledger-total-debit account-subledger)
	   (if debit-p amount 0)))
         (total-credit
	  (klid-ledger-calc
	   '+
	   (klid-ledger-account-subledger-total-credit account-subledger)
	   (if (not debit-p) amount 0)))
         (total-balance
	  (klid-ledger-calc
	   '-
	   total-debit
	   total-credit)))
    (make-klid-ledger-account-subledger
     :records (append (klid-ledger-account-subledger-records account-subledger) (list record))
     :total-debit total-debit
     :total-credit total-credit
     :total-balance total-balance)))

(defun klid-ledger-sort (accounts)
  "Sort ACCOUNTS using `string-lessp' predicate.

ACCOUNTS is a list of strings."
  (cl-sort accounts 'string-lessp :key 'downcase))

(defun klid-ledger-get-keys (general-ledger)
  "Get sorted list of all keys found in GENERAL-LEDGER."
  (let* ((keys '()))
    (maphash
     (lambda (k v) (push k keys))
     general-ledger)
    (klid-ledger-sort keys)))

(provide 'klid-ledger)

;;; klid-ledger.el ends here
