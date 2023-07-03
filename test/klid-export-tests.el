(require 'ert)
(require 'klid-export)

(ert-deftest test-klid-export-account-subledger-to-table.el ()
  (let ((general-ledger
	 (klid-ledger-general-ledger
	  `(,(make-klid-transaction
	      :date '(nil nil nil 1 1 2020 nil -1 nil)
	      :document "FAV123"
	      :amount 30000.66
	      :description "VyFa"
	      :debit-account "311000"
	      :credit-account "602000"
	      :note "123")
	    ,(make-klid-transaction
	      :date '(nil nil nil 2 1 2020 nil -1 nil)
	      :document "BV1"
	      :amount 30000.34
	      :description "Přijatá platba"
	      :debit-account "221000"
	      :credit-account "311000"
	      :note "123")))))
    (should
     (string=
      (klid-export-account-subledger-to-table.el general-ledger "221000") "\
|+-----------+--------+----------------+----------+----------+------------+----------+|
|      DATUM | DOKLAD | POPIS          |  MD [KČ] | DAL [KČ] | SALDO [KČ] | PROTIÚČET |
|+-----------+--------+----------------+----------+----------+------------+----------+|
| 02.01.2020 | BV1    | Přijatá platba | 30000.34 |        0 |   30000.34 |    311000 |
|+-----------+--------+----------------+----------+----------+------------+----------+|
|            |        | SUMA           | 30000.34 |        0 |   30000.34 |           |
|+-----------+--------+----------------+----------+----------+------------+----------+|"))

    (should
     (string=
      (klid-export-account-subledger-to-table.el general-ledger "602000") "\
|+-----------+--------+-------+---------+----------+------------+----------+|
|      DATUM | DOKLAD | POPIS | MD [KČ] | DAL [KČ] | SALDO [KČ] | PROTIÚČET |
|+-----------+--------+-------+---------+----------+------------+----------+|
| 01.01.2020 | FAV123 | VyFa  |       0 | 30000.66 |  -30000.66 |    311000 |
|+-----------+--------+-------+---------+----------+------------+----------+|
|            |        | SUMA  |       0 | 30000.66 |  -30000.66 |           |
|+-----------+--------+-------+---------+----------+------------+----------+|"))

    (should
     (string=
      (klid-export-account-subledger-to-table.el general-ledger "311000") "\
|+-----------+--------+----------------+----------+----------+------------+----------+|
|      DATUM | DOKLAD | POPIS          |  MD [KČ] | DAL [KČ] | SALDO [KČ] | PROTIÚČET |
|+-----------+--------+----------------+----------+----------+------------+----------+|
| 01.01.2020 | FAV123 | VyFa           | 30000.66 |        0 |   30000.66 |    602000 |
| 02.01.2020 | BV1    | Přijatá platba |        0 | 30000.34 |  -30000.34 |    221000 |
|+-----------+--------+----------------+----------+----------+------------+----------+|
|            |        | SUMA           | 30000.66 | 30000.34 |       0.32 |           |
|+-----------+--------+----------------+----------+----------+------------+----------+|"))))

(ert-deftest test-klid-export-transactions-to-list ()
  (should (equal
	   (klid-export-transactions-to-list 
	    `(,(make-klid-transaction
		:date '(nil nil nil 2 1 2020 nil -1 nil)
		:document "Doc - t1"
		:amount 10000
		:description "Desc - t1"
		:debit-account "221000"
		:credit-account "311000"
		:note "Note - t1")
	      ,(make-klid-transaction
		:date '(nil nil nil 3 12 2023 nil -1 nil)
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

(ert-deftest test-klid-export-transactions-to-table.el ()
  (should (string=
	   (klid-export-transactions-to-table.el
	    `(,(make-klid-transaction
		:date '(nil nil nil 2 1 2020 nil -1 nil)
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
	   (klid-export-transactions-to-table.el
	    `(,(make-klid-transaction
		:date '(nil nil nil 2 1 2020 nil -1 nil)
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

