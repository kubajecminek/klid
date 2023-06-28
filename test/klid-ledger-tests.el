(require 'ert)
(require 'klid-ledger)

(ert-deftest test-klid-ledger-update-account-ledger ()
  (let* ((tx (make-klid-transaction
	      :date '(0 0 0 1 1 2020 nil 0 0)
	      :document "FAV123"
	      :amount 30000
	      :description "VyFa"
	      :debit-account "311000"
	      :credit-account "602000"
	      :note "123"))
	 (new-account-ledger (make-klid-ledger-account-ledger))
	 (account-ledger-debit (klid-ledger-update-account-ledger tx new-account-ledger t))
	 (account-ledger-credit (klid-ledger-update-account-ledger tx new-account-ledger nil)))

    (should (= (klid-ledger-account-ledger-total-debit account-ledger-debit) 30000))
    (should (= (klid-ledger-account-ledger-total-credit account-ledger-debit) 0))
    (should (= (klid-ledger-account-ledger-total-balance account-ledger-debit) 30000))

    (should (= (klid-ledger-account-ledger-total-debit account-ledger-credit) 0))
    (should (= (klid-ledger-account-ledger-total-credit account-ledger-credit) 30000))
    (should (= (klid-ledger-account-ledger-total-balance account-ledger-credit) -30000))

    (should (= (length (klid-ledger-account-ledger-records account-ledger-debit)) 1))
    (should (= (length (klid-ledger-account-ledger-records account-ledger-credit)) 1))

    (should (equal
	     (klid-ledger-record-date
	      (car (klid-ledger-account-ledger-records account-ledger-debit)))
	     '(0 0 0 1 1 2020 nil 0 0)))

    (should (equal
	     (klid-ledger-record-date
	      (car (klid-ledger-account-ledger-records account-ledger-credit)))
	     '(0 0 0 1 1 2020 nil 0 0)))

    (should (=
	     (klid-ledger-record-debit-amount
	      (car (klid-ledger-account-ledger-records account-ledger-debit)))
	     30000))
    (should (=
	     (klid-ledger-record-debit-amount
	      (car (klid-ledger-account-ledger-records account-ledger-credit)))
	     0))
    
    (should (=
	     (klid-ledger-record-credit-amount
	      (car (klid-ledger-account-ledger-records account-ledger-debit)))
	     0))
    
    (should (=
	     (klid-ledger-record-credit-amount
	      (car (klid-ledger-account-ledger-records account-ledger-credit)))
	     30000))
    (should (=
	     (klid-ledger-record-balance
	      (car (klid-ledger-account-ledger-records account-ledger-debit)))
	     30000))
    (should (=
	     (klid-ledger-record-balance
	      (car (klid-ledger-account-ledger-records account-ledger-credit)))
	     -30000))

    (should (string=
	     (klid-ledger-record-counter-account
	      (car (klid-ledger-account-ledger-records account-ledger-debit)))
	     "602000"))
    (should (string=
	     (klid-ledger-record-counter-account
	      (car (klid-ledger-account-ledger-records account-ledger-credit)))
	     "311000"))

    (should (string=
	     (klid-ledger-record-document
	      (car (klid-ledger-account-ledger-records account-ledger-debit)))
	     (klid-ledger-record-document
	      (car (klid-ledger-account-ledger-records account-ledger-credit)))))

    (should (string=
	     (klid-ledger-record-description
	      (car (klid-ledger-account-ledger-records account-ledger-debit)))
	     (klid-ledger-record-description
	      (car (klid-ledger-account-ledger-records account-ledger-credit)))))))

(ert-deftest test-klid-ledger-general-ledger ()
  (let* ((txs `(,(make-klid-transaction
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
		  :note "123")))
	 (general-ledger (klid-ledger-general-ledger txs))
	 (acc-221 (gethash "221000" general-ledger))
	 (acc-311 (gethash "311000" general-ledger))
	 (acc-602 (gethash "602000" general-ledger)))

    (should (= (length (klid-ledger-account-ledger-records acc-221)) 1))
    (should (= (length (klid-ledger-account-ledger-records acc-311)) 2))
    (should (= (length (klid-ledger-account-ledger-records acc-602)) 1))

    (should (= (klid-ledger-account-ledger-total-debit acc-221) 30000))
    (should (= (klid-ledger-account-ledger-total-debit acc-311) 30000))
    (should (= (klid-ledger-account-ledger-total-debit acc-602) 0))
    
    (should (= (klid-ledger-account-ledger-total-credit acc-221) 0))
    (should (= (klid-ledger-account-ledger-total-credit acc-311) 30000))
    (should (= (klid-ledger-account-ledger-total-credit acc-602) 30000))))

(ert-deftest test-klid-ledger-export-account-to-table.el ()
  (let ((general-ledger
	 (klid-ledger-general-ledger
	  `(,(make-klid-transaction
	      :date '(0 0 0 1 1 2020 nil 0 0)
	      :document "FAV123"
	      :amount 30000.66
	      :description "VyFa"
	      :debit-account "311000"
	      :credit-account "602000"
	      :note "123")
	    ,(make-klid-transaction
	      :date '(0 0 0 2 1 2020 nil 0 0)
	      :document "BV1"
	      :amount 30000.34
	      :description "Přijatá platba"
	      :debit-account "221000"
	      :credit-account "311000"
	      :note "123")))))
    (should
     (string=
      (klid-ledger-export-account-to-table.el general-ledger "221000") "\
|+-----------+--------+----------------+----------+----------+------------+----------+|
|      DATUM | DOKLAD | POPIS          |  MD [KČ] | DAL [KČ] | SALDO [KČ] | PROTIÚČET |
|+-----------+--------+----------------+----------+----------+------------+----------+|
| 02.01.2020 | BV1    | Přijatá platba | 30000.34 |        0 |   30000.34 |    311000 |
|+-----------+--------+----------------+----------+----------+------------+----------+|
|            |        | SUMA           | 30000.34 |        0 |   30000.34 |           |
|+-----------+--------+----------------+----------+----------+------------+----------+|"))

    (should
     (string=
      (klid-ledger-export-account-to-table.el general-ledger "602000") "\
|+-----------+--------+-------+---------+----------+------------+----------+|
|      DATUM | DOKLAD | POPIS | MD [KČ] | DAL [KČ] | SALDO [KČ] | PROTIÚČET |
|+-----------+--------+-------+---------+----------+------------+----------+|
| 01.01.2020 | FAV123 | VyFa  |       0 | 30000.66 |  -30000.66 |    311000 |
|+-----------+--------+-------+---------+----------+------------+----------+|
|            |        | SUMA  |       0 | 30000.66 |  -30000.66 |           |
|+-----------+--------+-------+---------+----------+------------+----------+|"))

    (should
     (string=
      (klid-ledger-export-account-to-table.el general-ledger "311000") "\
|+-----------+--------+----------------+----------+----------+------------+----------+|
|      DATUM | DOKLAD | POPIS          |  MD [KČ] | DAL [KČ] | SALDO [KČ] | PROTIÚČET |
|+-----------+--------+----------------+----------+----------+------------+----------+|
| 01.01.2020 | FAV123 | VyFa           | 30000.66 |        0 |   30000.66 |    602000 |
| 02.01.2020 | BV1    | Přijatá platba |        0 | 30000.34 |  -30000.34 |    221000 |
|+-----------+--------+----------------+----------+----------+------------+----------+|
|            |        | SUMA           | 30000.66 | 30000.34 |       0.32 |           |
|+-----------+--------+----------------+----------+----------+------------+----------+|"))))
