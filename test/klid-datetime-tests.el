(require 'ert)
(require 'klid-datetime)

(ert-deftest test-klid-datetime-csn-01-6910--full-date-match ()
  ;; returns non-nil if regex matches the string
  (should-not (equal (klid-datetime-csn-01-6910--match klid-datetime-csn-01-6910--full-date-match "25.04.1965") nil))
  (should (equal (klid-datetime-csn-01-6910--match klid-datetime-csn-01-6910--full-date-match "2022/10/14") nil)))

(ert-deftest test-klid-datetime-csn-01-6910-parse ()
  (should (equal (klid-datetime-csn-01-6910-parse "01.01.2000") '(nil nil nil 1 1 2000 nil -1 nil)))
  (should (equal (klid-datetime-csn-01-6910-parse "31/12/1999") '(nil nil nil 31 12 1999 nil -1 nil)))
  (should-error (klid-datetime-csn-01-6910-parse "1.1.2020"))
  (should-error (klid-datetime-csn-01-6910-parse "1993/01/03")))

(ert-deftest test-klid-datetime-to-timestamp ()
  (should (= (klid-datetime-to-timestamp '(nil nil nil 1 1 1970 nil -1 nil)) 0))
  (should (= (klid-datetime-to-timestamp '(nil nil nil 2 1 1970 nil -1 nil)) 86400)))

(ert-deftest test-klid-datetime-csn-01-6910-to-string ()
  (should (string= (klid-datetime-csn-01-6910-to-string '(nil nil nil 1 1 2020 nil -1 nil)) "01.01.2020"))
  (should (string= (klid-datetime-csn-01-6910-to-string '(nil nil nil 2 12 1970 nil -1 nil)) "02.12.1970"))
  (should (string= (klid-datetime-csn-01-6910-to-string '(nil nil nil 31 5 2000 nil -1 nil)) "31.05.2000")))

