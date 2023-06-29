(require 'ert)
(require 'klid-filter)

(ert-deftest test-klid-filter-transactions-by-date ()
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
    (should (equal (klid-filter-transactions-by-date `(,tx2 ,tx1) "31.12.2020") nil))
    (should (equal (klid-filter-transactions-by-date `(,tx2 ,tx1) nil "31.12.2019") nil))
    (should (equal (klid-filter-transactions-by-date `(,tx2 ,tx1) "01.01.2020") `(,tx2 ,tx1)))
    (should (equal (klid-filter-transactions-by-date `(,tx2 ,tx1) "01.01.2020" "01.01.2020") `(,tx2)))
    (should (equal (klid-filter-transactions-by-date `(,tx2 ,tx1) "01.01.2020" "02.01.2020") `(,tx2 ,tx1)))
    (should (equal (klid-filter-transactions-by-date `(,tx2 ,tx1) "02.01.2020" "02.01.2020") `(,tx1)))))

(ert-deftest test-klid-filter-transactions-by-account ()
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
    (should (equal (klid-filter-transactions-by-account `(,tx2 ,tx1) "311") `(,tx2 ,tx1)))
    (should (equal (klid-filter-transactions-by-account `(,tx2 ,tx1) "2") `(,tx1)))
    (should (equal (klid-filter-transactions-by-account `(,tx2 ,tx1) "602000") `(,tx2)))))

(ert-deftest test-klid-filter-org-hline-symbol ()
  (should (equal
	   (klid-filter-org-hline-symbol (org-table-to-lisp "|a|b|\n|-|-|\n|1|2|"))
	   '(("a" "b") ("1" "2")))))
