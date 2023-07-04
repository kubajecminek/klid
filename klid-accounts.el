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
;; This module offers a range of utilities for sorting and filtering
;; accounts utilized in accounting transactions.

;;; Code:

(require 'cl-lib)
(require 'klid-transaction)

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

(provide 'klid-accounts)

;;; klid-accounts.el ends here
