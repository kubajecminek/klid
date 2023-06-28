;;; klid-datetime.el --- Helper code for use with the "klid" accounting system -*- lexical-binding: t; -*-

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
;; In Czech Republic, the most common date format is DD.MM.YYYY.
;; See https://en.wikipedia.org/wiki/Date_format_by_country

;; This package (at least for now) implement only parts of
;; ČSN 01 6910 standard.
;; See https://ujc.avcr.cz/expertni-cinnost/csn016910/

;;; Code:

(require 'iso8601)

(define-error 'klid-parse-error "Klid parse error")

(defun klid-datetime-csn-01-6910--match (regexp string)
  (string-match (concat "\\`" regexp "\\'") string))

(defconst klid-datetime-csn-01-6910--full-date-match
  "\\([0-9][0-9]\\).?\\([0-9][0-9]\\).\\([+-]?[0-9][0-9][0-9][0-9]\\)"
  "Regular expression that matches DD.MM.YYYY date format.")

(defun klid-datetime-csn-01-6910-parse (string)
  "Parse the date in STRING (DD.MM.YYYY) into `decode-time' compatible structure."
  (cond
   ;; DD.MM.YYYY
   ((klid-datetime-csn-01-6910--match klid-datetime-csn-01-6910--full-date-match string)
    (iso8601--decoded-time
     :year (string-to-number (match-string 3 string))
     :month (string-to-number (match-string 2 string))
     :day (string-to-number (match-string 1 string))
     ;; FIXME: If we omit hour, minute and second then encode-time does not work
     :hour 0
     :minute 0
     :second 0
     :zone 0
     :dst 0
     ))
   ;; Add other formats ...
   ;; See iso8601.el
   (t (signal 'klid-parse-error string))))

(defun klid-datetime-to-timestamp (datetime)
  "Convert DATETIME to timestamp.

This function is useful mainly for sorting predicates."
  (string-to-number (format-time-string "%s" (time-convert (encode-time datetime)))))

(defun klid-datetime-csn-01-6910-to-string (datetime)
  "Convert DATETIME to ČSN 01 6910 string representation."
  (format-time-string "%d.%m.%Y" (encode-time datetime)))

(provide 'klid-datetime)

;;; klid-datetime.el ends here
