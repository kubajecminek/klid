(require 'ert)
(require 'klid-ledger)

(ert-deftest test-klid-ledger-update-account-subledger ()
  (let* ((tx (make-klid-transaction
	      :date '(nil nil nil 1 1 2020 nil -1 nil)
	      :document "FAV123"
	      :amount 30000
	      :description "VyFa"
	      :debit-account "311000"
	      :credit-account "602000"
	      :note "123"))
	 (new-account-subledger (make-klid-ledger-account-subledger))
	 (account-subledger-debit (klid-ledger-update-account-subledger tx new-account-subledger t))
	 (account-subledger-credit (klid-ledger-update-account-subledger tx new-account-subledger nil)))

    (should (= (klid-ledger-account-subledger-total-debit account-subledger-debit) 30000))
    (should (= (klid-ledger-account-subledger-total-credit account-subledger-debit) 0))
    (should (= (klid-ledger-account-subledger-total-balance account-subledger-debit) 30000))

    (should (= (klid-ledger-account-subledger-total-debit account-subledger-credit) 0))
    (should (= (klid-ledger-account-subledger-total-credit account-subledger-credit) 30000))
    (should (= (klid-ledger-account-subledger-total-balance account-subledger-credit) -30000))

    (should (= (length (klid-ledger-account-subledger-records account-subledger-debit)) 1))
    (should (= (length (klid-ledger-account-subledger-records account-subledger-credit)) 1))

    (should (equal
	     (klid-ledger-record-date
	      (car (klid-ledger-account-subledger-records account-subledger-debit)))
	     '(nil nil nil 1 1 2020 nil -1 nil)))

    (should (equal
	     (klid-ledger-record-date
	      (car (klid-ledger-account-subledger-records account-subledger-credit)))
	     '(nil nil nil 1 1 2020 nil -1 nil)))

    (should (=
	     (klid-ledger-record-debit-amount
	      (car (klid-ledger-account-subledger-records account-subledger-debit)))
	     30000))
    (should (=
	     (klid-ledger-record-debit-amount
	      (car (klid-ledger-account-subledger-records account-subledger-credit)))
	     0))
    
    (should (=
	     (klid-ledger-record-credit-amount
	      (car (klid-ledger-account-subledger-records account-subledger-debit)))
	     0))
    
    (should (=
	     (klid-ledger-record-credit-amount
	      (car (klid-ledger-account-subledger-records account-subledger-credit)))
	     30000))
    (should (=
	     (klid-ledger-record-balance
	      (car (klid-ledger-account-subledger-records account-subledger-debit)))
	     30000))
    (should (=
	     (klid-ledger-record-balance
	      (car (klid-ledger-account-subledger-records account-subledger-credit)))
	     -30000))

    (should (string=
	     (klid-ledger-record-counter-account
	      (car (klid-ledger-account-subledger-records account-subledger-debit)))
	     "602000"))
    (should (string=
	     (klid-ledger-record-counter-account
	      (car (klid-ledger-account-subledger-records account-subledger-credit)))
	     "311000"))

    (should (string=
	     (klid-ledger-record-document
	      (car (klid-ledger-account-subledger-records account-subledger-debit)))
	     (klid-ledger-record-document
	      (car (klid-ledger-account-subledger-records account-subledger-credit)))))

    (should (string=
	     (klid-ledger-record-description
	      (car (klid-ledger-account-subledger-records account-subledger-debit)))
	     (klid-ledger-record-description
	      (car (klid-ledger-account-subledger-records account-subledger-credit)))))))

(ert-deftest test-klid-ledger-general-ledger ()
  (let* ((txs `(,(make-klid-transaction
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
		  :note "123")))
	 (general-ledger (klid-ledger-general-ledger txs))
	 (acc-221 (gethash "221000" general-ledger))
	 (acc-311 (gethash "311000" general-ledger))
	 (acc-602 (gethash "602000" general-ledger)))

    (should (= (length (klid-ledger-account-subledger-records acc-221)) 1))
    (should (= (length (klid-ledger-account-subledger-records acc-311)) 2))
    (should (= (length (klid-ledger-account-subledger-records acc-602)) 1))

    (should (= (klid-ledger-account-subledger-total-debit acc-221) 30000))
    (should (= (klid-ledger-account-subledger-total-debit acc-311) 30000))
    (should (= (klid-ledger-account-subledger-total-debit acc-602) 0))
    
    (should (= (klid-ledger-account-subledger-total-credit acc-221) 0))
    (should (= (klid-ledger-account-subledger-total-credit acc-311) 30000))
    (should (= (klid-ledger-account-subledger-total-credit acc-602) 30000))))
