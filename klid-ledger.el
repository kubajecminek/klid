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
;; `klid-ledger-account-ledger' structure.  This structure contains total
;; debit amount, total credit amount, as well as total balance.  Additionally,
;; this structure contains all records found in the underlying journal
;; (of type `klid-ledger-record') for this given account.  In practice, this is
;; probably most important module.
;; 
;; [1] https://en.wikipedia.org/wiki/General_ledger

;;; Code:

(require 'cl-lib)
(require 'klid-transaction)
(require 'klid-accounts)

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

(cl-defstruct (klid-ledger-account-ledger
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

;; Hashmap - KEY = account, VAL = account-ledger
(defun klid-ledger-general-ledger (txs)
  "Create the general ledger from TXS.

TXS is a list, and each element within the list is itself a list
with the same structure as `klid-transaction'.  This function returns
a hash-table where each account is mapped to its corresponding
`klid-ledger-account-ledger' structure."
  (let ((general-ledger (make-hash-table :test 'equal)))
    (dolist (tx txs)
      (let ((dflt (make-klid-ledger-account-ledger)))
        ;; Update debit account ledger
        (let ((account-ledger (gethash
			       (klid-transaction-debit-account tx)
			       general-ledger
			       ;; Set default value
			       dflt)))
          (puthash (klid-transaction-debit-account tx)
                   (klid-ledger-update-account-ledger tx account-ledger t)
                   general-ledger))
        ;; Update credit account ledger
        (let ((account-ledger (gethash
			       (klid-transaction-credit-account tx)
			       general-ledger
			       ;; Set default value
			       dflt)))
          (puthash (klid-transaction-credit-account tx)
                   (klid-ledger-update-account-ledger tx account-ledger nil)
                   general-ledger))))
    general-ledger))

(defun klid-ledger-update-account-ledger (tx account-ledger debit-p)
  "Update the account ledger with a new transaction record.

Returns the updated account-ledger of type `klid-ledger-account-ledger'.
TX is transaction to be added to the ledger of type `klid-transaction',
ACCOUNT-LEDGER is the current account-ledger, and DEBIT-P is a boolean
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
	   (klid-ledger-account-ledger-total-debit account-ledger)
	   (if debit-p amount 0)))
         (total-credit
	  (klid-ledger-calc
	   '+
	   (klid-ledger-account-ledger-total-credit account-ledger)
	   (if (not debit-p) amount 0)))
         (total-balance
	  (klid-ledger-calc
	   '-
	   total-debit
	   total-credit)))
    (make-klid-ledger-account-ledger
     :records (append (klid-ledger-account-ledger-records account-ledger) (list record))
     :total-debit total-debit
     :total-credit total-credit
     :total-balance total-balance)))

(defun klid-ledger-export-account-to-table.el (general-ledger account &optional params)
  "Export ACCOUNT subledger from GENERAL-LEDGER to table.el.

GENERAL-LEDGER is a hash-table where each account is mapped to its corresponding
`klid-ledger-account-ledger' structure.  PARAMS is a property list of parameters
that can influence the conversion.  All parameters from ‘orgtbl-to-generic’ are
supported."
  (let ((account-ledger (gethash account general-ledger))
	(table nil))
    (if (null account-ledger)
	""
      (push 'hline table)
      (push '("DATUM" "DOKLAD" "POPIS" "MD [KČ]" "DAL [KČ]" "SALDO [KČ]" "PROTIÚČET") table)
      (push 'hline table)
      (dolist (record (klid-ledger-account-ledger-records account-ledger))
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
		(klid-ledger-account-ledger-total-debit account-ledger))
	      ,(number-to-string
		(klid-ledger-account-ledger-total-credit account-ledger))
	      ,(number-to-string
		(klid-ledger-account-ledger-total-balance account-ledger))
	      "")
	    table)
      (push 'hline table)
      (setq table (nreverse table))
      (klid-export-orgtbl-to-table.el table params))))

(defun klid-ledger-export-general-ledger-to-org
    (general-ledger &optional account-prefix params)
  "Export subledgers from GENERAL-LEDGER to table.el with some additional markup.

This function exports subledgers from GENERAL-LEDGER that contain ACCOUNT-PREFIX.
GENERAL-LEDGER is a hash-table where each account is mapped to its corresponding
`klid-ledger-account-ledger' structure.  PARAMS is a property list of parameters
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
	 (klid-ledger-export-account-to-table.el general-ledger acc params)
	 "\n\n"))
      (buffer-string))))

(provide 'klid-ledger)

;;; klid-ledger.el ends here
