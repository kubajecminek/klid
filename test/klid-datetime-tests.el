(require 'ert)
(require 'klid-datetime)

(ert-deftest test-klid-datetime-csn-01-6910--full-date-match ()
  ;; returns non-nil if regex matches the string
  (should-not (equal (klid-datetime-csn-01-6910--match klid-datetime-csn-01-6910--full-date-match "25.04.1965") nil))
  (should (equal (klid-datetime-csn-01-6910--match klid-datetime-csn-01-6910--full-date-match "2022/10/14") nil)))

(ert-deftest test-klid-datetime-csn-01-6910-parse ()
  (should (equal (klid-datetime-csn-01-6910-parse "01.01.2000") '(0 0 0 1 1 2000 nil 0 0)))
  (should (equal (klid-datetime-csn-01-6910-parse "31/12/1999") '(0 0 0 31 12 1999 nil 0 0)))
  (should-error (klid-datetime-csn-01-6910-parse "1.1.2020"))
  (should-error (klid-datetime-csn-01-6910-parse "1993/01/03"))) ; does not match regexp

(ert-deftest test-klid-datetime-to-timestamp ()
  (should (= (klid-datetime-to-timestamp '(0 0 0 1 1 1970 nil 0 0)) 0))
  (should (= (klid-datetime-to-timestamp '(0 0 0 2 1 1970 nil 0 0)) 86400)))

(ert-deftest test-klid-datetime-csn-01-6910-to-string ()
  (should (string= (klid-datetime-csn-01-6910-to-string '(0 0 0 1 1 2020 nil 0 0)) "01.01.2020"))
  (should (string= (klid-datetime-csn-01-6910-to-string '(0 0 0 2 12 1970 nil 0 0)) "02.12.1970"))
  (should (string= (klid-datetime-csn-01-6910-to-string '(0 0 0 31 5 2000 nil 0 0)) "31.05.2000")))

