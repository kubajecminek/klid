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

(define-error 'klid-parse-error "Klid parse error")

(defun klid-datetime-csn-01-6910--match (regexp string)
  (string-match (concat "\\`" regexp "\\'") string))

(defun klid--decoded-time (day month year)
  "Return simplified `decoded-time' structure.

All unknown values other than DST are returned as nil, and an
unknown DST value is returned as -1. DAY, MONTH and YEAR are all
integers."
  `(nil nil nil ,day ,month ,year nil -1 nil))

(defconst klid-datetime-csn-01-6910--full-date-match
  "\\([0-3][0-9]\\).?\\([0-1][0-9]\\).\\([+-]?[0-9][0-9][0-9][0-9]\\)"
  "Regular expression that matches DD.MM.YYYY date format.")

(defun klid-datetime-csn-01-6910-parse (string)
  "Parse the date in STRING (DD.MM.YYYY) into `decoded-time' structure."
  (cond
   ;; DD.MM.YYYY
   ((klid-datetime-csn-01-6910--match klid-datetime-csn-01-6910--full-date-match string)
    (klid--decoded-time
     (string-to-number (match-string 1 string))
     (string-to-number (match-string 2 string))
     (string-to-number (match-string 3 string))))
   ;; Add other formats ...
   ;; See iso8601.el
   (t (signal 'klid-parse-error string))))

(defun klid-datetime--encode-time (time)
  "Convert TIME to a timestamp even if it contains unknown values.

TIME is a list (SECOND MINUTE HOUR DAY MONTH YEAR IGNORED DST ZONE)
in the style of ‘decode-time’.  Nil values in TIME are replaced with
0."
  (encode-time
   (mapcar (lambda (elem) (or elem 0)) time)))

(defun klid-datetime-to-timestamp (datetime)
  "Convert DATETIME to timestamp.

This function is useful mainly for sorting predicates."
  (string-to-number
   (format-time-string "%s" (time-convert (klid-datetime--encode-time datetime)))))

(defun klid-datetime-csn-01-6910-to-string (datetime)
  "Convert DATETIME to ČSN 01 6910 string format.

DATETIME is as `decoded-time' structure."
  (format-time-string "%d.%m.%Y" (klid-datetime--encode-time datetime)))

(provide 'klid-datetime)

;;; klid-datetime.el ends here
