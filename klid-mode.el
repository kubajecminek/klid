;;; klid-mode.el --- Czech accounting software for the hackers -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 Jakub Ječmínek

;; This file is not part of GNU Emacs.

;; Package-Requires: ((emacs "27.1"))

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
;; This Emacs package provides a minor mode for performing common accounting
;; operations.  This program targets Czech audiance.  Klid currently takes
;; input only from `org-mode' tables.  To activate it, just type 'M-x klid-mode RET'.

;; Underlying `org-mode' table must be in a very specific format for Klid to
;; work properly.  It must include these columns:
;; 1. Date in ČSN 01 6910 format (DD.MM.YYYY)
;; 2. Accounting document
;; 3. Non-zero amount (with dot specyfing decimals)
;; 4. Brief description
;; 5. Debit account
;; 6. Credit account
;; 7. Note (can be blank)

;; Example table might look as follows:
;; |      Datum | Doklad      | Částka | Popis                        |     MD |    Dal | Poznámka    |
;; | 01.01.2020 | ID001       | 100000 | Počáteční stav - 411         | 701000 | 411000 |             |
;; | 01.01.2020 | ID001       |  50000 | Počáteční stav - 221         | 221000 | 701000 |             |
;; | 01.01.2020 | ID001       |  50000 | Počáteční stav - 211         | 211000 | 701000 |             |
;; | 02.01.2020 | FAP001      |  15000 | Nájemné na únor              | 518001 | 321000 |             |
;; | 02.01.2020 | BV1         |  15000 | Platba nájemného             | 321000 | 221000 |             |
;; | 15.02.2020 | FAV15022020 | 100000 | VyFa za prodej služeb        | 311000 | 602001 |             |
;; | 20.02.2020 | BV2         | 100000 | Platba faktury od odběratele | 221000 | 311000 | FAV15022020 |
;; | 31.12.2020 | ID002       | 100000 | Konečný stav - 411           | 411000 | 702000 |             |
;; | 31.12.2020 | ID002       |  50000 | Konečný stav - 211           | 702000 | 211000 |             |
;; | 31.12.2020 | ID002       | 135000 | Konečný stav - 221           | 702000 | 221000 |             |
;; | 31.12.2020 | ID002       |  15000 | Uzavření účtu - 518001       | 710000 | 518001 |             |
;; | 31.12.2020 | ID002       | 100000 | Uzavření účtu - 602001       | 602001 | 710000 |             |
;; | 31.12.2020 | ID002       |  85000 | Zaúčtování zisku             | 710000 | 702000 |             |

;; You can invoke any `klid-mode' command with the table at point.  For example,
;; 'M-x klid-mode-filter-by-account' function outputs filtered transactions to the
;; output buffer.


;;; Code:

(require 'klid-export)
(require 'klid-transaction)
(require 'klid-filter)
(require 'klid-ledger)
(require 'klid-accounts)

(defconst klid-mode-version "0.2.0"
  "The version of `klid' package.")

(defcustom klid-mode-output-buffer "*klid-output*"
  "`klid' output buffer name.")

(defcustom klid-mode-output-overwrite t
  "Non-nil means that `klid-output-buffer' will be erased before every operation.")

(define-minor-mode klid-mode
  "Czech accounting software for the hackers."
  :lighter " klid"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c k v") 'klid-mode-version)
	    (define-key map (kbd "C-c k f a") 'klid-mode-filter-by-account)
	    (define-key map (kbd "C-c k f d") 'klid-mode-filter-by-date)
	    (define-key map (kbd "C-c k g") 'klid-mode-general-ledger)
	    (define-key map (kbd "C-c k l") 'klid-mode-list-accounts)
	    (define-key map (kbd "C-c k a") 'klid-mode-all-reports)
            map))

(defun klid-mode-version ()
  "Display `klid-mode' version in the minibuffer."
  (interactive)
  (message "Klid version: %s" klid-mode-version))

(defun klid-mode-filter-by-account (account-prefix)
  "Filter transactions at point by ACCOUNT-PREFIX.

This function takes transactions at point and displays the result in
`klid-output-buffer' so that transactions at point remain unchanged."
  (interactive "sAccount prefix: ")
  (klid-mode--display
   #'(lambda (txs)
       (klid-transaction-export-transactions-to-org
	(klid-filter-transactions-by-account txs account-prefix)))))

(defun klid-mode-filter-by-date (from to)
  "Filter transactions at point by date.

FROM specifies the beginning of the time range and TO the end.  This
function displays the result in `klid-output-buffer' so that transactions
at point remain unchanged."
  (interactive "sFrom (DD-MM-YYYY): \nsTo (DD-MM-YYYY):")
  (klid-mode--display
   #'(lambda (txs)
       (klid-transaction-export-transactions-to-org
	(klid-filter-transactions-by-date txs from to)))))

(defun klid-mode-general-ledger (account-prefix)
  "Display general ledger for each account that has ACCOUNT-PREFIX."
  (interactive "sAccount prefix: ")
  (klid-mode--display
   #'(lambda (txs)
       (klid-ledger-export-general-ledger-to-org
	(klid-ledger-general-ledger txs)
	account-prefix))))

(defun klid-mode-list-accounts ()
  "Display the list of all accounts used on either debit or credit side."
  (interactive)
  (klid-mode--display
   #'(lambda (txs)
       (klid-accounts-export-to-org
	(klid-accounts-unique txs)))))

(defun klid-mode-all-reports ()
  "Generate key accounting reports."
  (interactive)
  (klid-mode--display
   #'(lambda (txs)
       (concat
	(klid-transaction-export-transactions-to-org txs)
	(klid-ledger-export-general-ledger-to-org (klid-ledger-general-ledger txs) "")
	(klid-accounts-export-to-org (klid-accounts-unique txs))))))

(defmacro klid-mode--measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.

Courtesy of https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html"
  `(let ((time (current-time)))
     ,@body
     (message "Klid operation execution time: %.06f"
	      (float-time (time-since time)))))

(defun klid-mode--display (transformation)
  "Apply TRANSFORMATION to the transactions at point and display the result.

This function applies the TRANSFORMATION to the transactions at point (currently
in the form of org-table) and outputs the result as `org-mode' markup to the
`klid-mode-output-buffer' buffer.

TRANSFORMATION is a function that accepts a list of `klid-transaction' and returns
output string in `org-mode' markup."
  ;; TODO: Implement other reader functions other than `klid-export-table.el-to-list'
  (klid-mode--measure-time
   (let ((txs (klid-transaction-parse-lists
	       (klid-export-table.el-to-list))))
     (with-current-buffer
	 (get-buffer-create klid-mode-output-buffer)
       (save-excursion
	 (when klid-mode-output-overwrite (erase-buffer))
	 (goto-char (point-max))
	 (insert (funcall transformation txs)))
       (unless (string= major-mode "org-mode")
	 (org-mode)))
     (pop-to-buffer klid-mode-output-buffer))))

(provide 'klid-mode)

;;; klid-mode.el ends here
