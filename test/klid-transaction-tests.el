(require 'ert)
(require 'org-table)
(require 'klid-transaction)

(ert-deftest test-klid-transaction-parse-list ()
  (let ((fixture (klid-transaction-parse-list
		  '("01.01.2020" "ID001" "30000" "Popis" "221000" "311000" "Poznámka"))))
    (should (string= (klid-transaction-note fixture) "Poznámka"))
    (should (= (klid-transaction-amount fixture) 30000))
    (should (string= (klid-transaction-description fixture) "Popis"))
    (should (equal (klid-transaction-date fixture) '(0 0 0 1 1 2020 nil 0 0)))))

(ert-deftest test-klid-transaction-parse-lists ()
  (let ((fixture
	 '(("02.01.2020" "BV1" "30000" "Přijatá platba" "221000" "311000" "123")
	   ("01.01.2020" "FAV123" 30000 "VyFa" "311000" "602000" "123"))))
    (should (equal (klid-transaction-parse-lists fixture)
		   `(,(make-klid-transaction
		       :date '(0 0 0 1 1 2020 nil 0 0)
		       :document "FAV123"
		       :amount 30000
		       :description "VyFa"
		       :debit-account "311000"
		       :credit-account "602000"
		       :note "123")
		     ,(make-klid-transaction
		       :date '(0 0 0 2 1 2020 nil 0 0)
		       :document "BV1"
		       :amount 30000
		       :description "Přijatá platba"
		       :debit-account "221000"
		       :credit-account "311000"
		       :note "123"))))
    (should (equal
	     (klid-transaction-parse-lists
	      (org-table-to-lisp "\
|      Datum | Doklad | Částka | Popis |     MD |    Dal | Poznámka |
|------------+--------+--------+-------+--------+--------+----------|
| 01.01.2020 | ID001  |  30000 | Popis | 221000 | 311000 |   Pozn.  |"))
	     `(,(make-klid-transaction
		 :date '(0 0 0 1 1 2020 nil 0 0)
		 :document "ID001"
		 :amount 30000
		 :description "Popis"
		 :debit-account "221000"
		 :credit-account "311000"
		 :note "Pozn."))))))

(ert-deftest test-klid-transaction-sort-by-date ()
  (let ((tx1 (make-klid-transaction
	      :date '(0 0 0 2 1 2020 nil 0 0)
	      :document "Doc - t1"
	      :amount 10000
	      :description "Desc - t1"
	      :debit-account "221000"
	      :credit-account "311000"
	      :note "Note - t1"))
	(tx2 (make-klid-transaction
	      :date '(0 0 0 1 1 2020 nil 0 0)
	      :document "Doc - t2"
	      :amount 20000
	      :description "Desc - t2"
	      :debit-account "311000"
	      :credit-account "602000"
	      :note "Note - t2")))

    (should (equal
	     (klid-transaction-sort-by-date `(,tx1 ,tx2))
	     `(,tx2 ,tx1)))))

(ert-deftest test-klid-transaction-export-transactions-to-list ()
  (should (equal
	   (klid-transaction-export-transactions-to-list 
	    `(,(make-klid-transaction
		:date '(0 0 0 2 1 2020 nil 0 0)
		:document "Doc - t1"
		:amount 10000
		:description "Desc - t1"
		:debit-account "221000"
		:credit-account "311000"
		:note "Note - t1")
	      ,(make-klid-transaction
		:date '(0 0 0 3 12 2023 nil 0 0)
		:document "Doc - t2"
		:amount 20000
		:description "Desc - t2"
		:debit-account "311000"
		:credit-account "602000"
		:note "Note - t2")))
	   '(("02.01.2020"
	      "Doc - t1"
	      "10000"
	      "Desc - t1"
	      "221000"
	      "311000"
	      "Note - t1")
	     ("03.12.2023"
	      "Doc - t2"
	      "20000"
	      "Desc - t2"
	      "311000"
	      "602000"
	      "Note - t2")))))

(ert-deftest test-klid-transaction-export-transactions-to-table.el ()
  (should (string=
	   (klid-transaction-export-transactions-to-table.el
	    `(,(make-klid-transaction
		:date '(0 0 0 2 1 2020 nil 0 0)
		:document "Doc - t1"
		:amount 10000
		:description "Desc - t1"
		:debit-account "221000"
		:credit-account "311000"
		:note "Note - t1")))
	   "\
|+-----------+----------+--------+-----------+--------+--------+----------+|
|      DATUM | DOKLAD   | ČÁSTKA | POPIS     |     MD |    DAL | POZNÁMKA  |
|+-----------+----------+--------+-----------+--------+--------+----------+|
| 02.01.2020 | Doc - t1 |  10000 | Desc - t1 | 221000 | 311000 | Note - t1 |
|+-----------+----------+--------+-----------+--------+--------+----------+|"))
  (should (string=
	   (klid-transaction-export-transactions-to-table.el
	    `(,(make-klid-transaction
		:date '(0 0 0 2 1 2020 nil 0 0)
		:document "Doc - t1"
		:amount 10000
		:description "Desc - t1"
		:debit-account "221000"
		:credit-account "311000"
		:note "Note - t1")))
	   "\
|+-----------+----------+--------+-----------+--------+--------+----------+|
|      DATUM | DOKLAD   | ČÁSTKA | POPIS     |     MD |    DAL | POZNÁMKA  |
|+-----------+----------+--------+-----------+--------+--------+----------+|
| 02.01.2020 | Doc - t1 |  10000 | Desc - t1 | 221000 | 311000 | Note - t1 |
|+-----------+----------+--------+-----------+--------+--------+----------+|")))