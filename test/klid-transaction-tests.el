(require 'ert)
(require 'org-table)
(require 'klid-transaction)

(ert-deftest test-klid-transaction-list-parsable-p ()
  (should-not (klid-transaction-list-parsable-p '()))
  (should-not (klid-transaction-list-parsable-p '("" "" "" "" "" "" "")))
  (should-not (klid-transaction-list-parsable-p '("01.01.2022" "" "" "" "" "" "")))
  (should-not (klid-transaction-list-parsable-p '("01.01.2022" "" "xyz" "" "" "" "")))
  (should-not (klid-transaction-list-parsable-p '("01.01.2022" "" "3" "" "" "")))
  (should-not (klid-transaction-list-parsable-p '("40.01.2022" "" "1" "" "" "" "")))
  (should-not (klid-transaction-list-parsable-p '("01.20.2022" "" "" "" "" "" "")))
  (should-not (klid-transaction-list-parsable-p '("01.01.2022" "" 0 "" "" "" "")))
  (should-not (klid-transaction-list-parsable-p '("01.01.2022" "" 1 "" "" "" "")))
  (should-not (klid-transaction-list-parsable-p '("01.01.2022" "" "" "" "" "" t)))
  (should (klid-transaction-list-parsable-p '("01.01.2022" "" "1" "" "" "" "")))
  (should (klid-transaction-list-parsable-p '("01.01.2022" "" "3.14" "" "" "" ""))))

(ert-deftest test-klid-transaction-from-list ()
  (let ((fixture (klid-transaction-from-list
		  '("01.01.2020" "ID001" "30000" "Popis" "221000" "311000" "Poznámka"))))
    (should (string= (klid-transaction-note fixture) "Poznámka"))
    (should (= (klid-transaction-amount fixture) 30000))
    (should (string= (klid-transaction-description fixture) "Popis"))
    (should (equal (klid-transaction-date fixture) '(nil nil nil 1 1 2020 nil -1 nil)))))

(ert-deftest test-klid-transaction-from-list-iter ()
  (let ((fixture
	 '(("02.01.2020" "BV1" "30000" "Přijatá platba" "221000" "311000" "123")
	   ("01.01.2020" "FAV123" "30000" "VyFa" "311000" "602000" "123"))))
    (should (equal (klid-transaction-from-list-iter fixture)
		   `(,(make-klid-transaction
		       :date '(nil nil nil 1 1 2020 nil -1 nil)
		       :document "FAV123"
		       :amount 30000
		       :description "VyFa"
		       :debit-account "311000"
		       :credit-account "602000"
		       :note "123")
		     ,(make-klid-transaction
		       :date '(nil nil nil 2 1 2020 nil -1 nil)
		       :document "BV1"
		       :amount 30000
		       :description "Přijatá platba"
		       :debit-account "221000"
		       :credit-account "311000"
		       :note "123"))))
    (should (equal
	     (klid-transaction-from-list-iter
	      (org-table-to-lisp "\
|      Datum | Doklad | Částka | Popis |     MD |    Dal | Poznámka |
|------------+--------+--------+-------+--------+--------+----------|
| 01.01.2020 | ID001  |  30000 | Popis | 221000 | 311000 |   Pozn.  |"))
	     `(,(make-klid-transaction
		 :date '(nil nil nil 1 1 2020 nil -1 nil)
		 :document "ID001"
		 :amount 30000
		 :description "Popis"
		 :debit-account "221000"
		 :credit-account "311000"
		 :note "Pozn."))))))

(ert-deftest test-klid-transaction-sort-by-date ()
  (let ((tx1 (make-klid-transaction
	      :date '(nil nil nil 2 1 2020 nil -1 nil)
	      :document "Doc - t1"
	      :amount 10000
	      :description "Desc - t1"
	      :debit-account "221000"
	      :credit-account "311000"
	      :note "Note - t1"))
	(tx2 (make-klid-transaction
	      :date '(nil nil nil 1 1 2020 nil -1 nil)
	      :document "Doc - t2"
	      :amount 20000
	      :description "Desc - t2"
	      :debit-account "311000"
	      :credit-account "602000"
	      :note "Note - t2")))

    (should (equal
	     (klid-transaction-sort-by-date `(,tx1 ,tx2))
	     `(,tx2 ,tx1)))))
