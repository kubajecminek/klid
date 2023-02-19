package klid

import (
	"encoding/csv"
	"errors"
	"io"
	"math/big"
	"sort"
	"time"
)

// DateLayout is date format used for parsing transactions and printing.
// It represents DD.MM.YYYY.
const (
	date = iota
	document
	amount
	description
	debitAccount
	creditAccount
	note

	DateLayout = "02.01.2006"
)

/*
From wikipedia:
Each financial transaction is lineed in at least two different nominal ledger
accounts within the financial accounting system, so that the total debits equals
the total credits in the general ledger, i.e. the accounts balance. This is a
partial check that each and every transaction has been correctly lineed. The
transaction is lineed as a "debit entry" (Dr) in one account, and a "credit
entry" (Cr) in a second account. The debit entry will be lineed on the debit
side (left-hand side) of a general ledger account, and the credit entry will be
lineed on the credit side (right-hand side) of a general ledger account. If
the total of the entries on the debit side of one account is greater than the
total on the credit side of the same nominal account, that account is said to
have a debit balance.
*/

// Transaction is the main work horse. It describes how the overall financial transactions
// are stored and manipulated.
type Transaction struct {
	// Date on which financial transaction happened.
	Date time.Time

	// Document refer to ID of the accounting document.
	Document string

	// Amount represents monetary value of the financial transaction.
	Amount *big.Rat

	// Description of the financial transaction.
	Description string

	// DebitAccount is the ID of the account that holds debit value of the transaction.
	DebitAccount string

	// CreditAccount is the ID of the account that holds credit value of the transaction.
	CreditAccount string

	// Note is a place for a user to add arbitrary text.
	Note string
}

// Transactions is a shortcut for []*Transaction.
type Transactions []*Transaction

// ParseTransactions reads from r and parses CSV data and returns
// a slice of Transaction.
func ParseTransactions(r io.Reader, skipFirst bool) (Transactions, error) {
	var txs Transactions
	reader := csv.NewReader(r)
	if skipFirst { // move reader onto the next line
		reader.FieldsPerRecord = -1
		reader.Read()
	}
	for {
		// read CSV line
		record, err := reader.Read()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		tx, err := parseRecord(record)
		if err != nil {
			return nil, err
		}
		txs = append(txs, tx)

	}
	sort.Sort(byDate(txs))
	return txs, nil
}

// parseRecord takes one line of CSV, parses some fields and
// returns *Transaction.
func parseRecord(record []string) (*Transaction, error) {
	if len(record) < 7 {
		return nil, errors.New("not enough CSV columns")
	}
	// parse date
	parsedDate, err := time.Parse(DateLayout, record[date])
	if err != nil {
		return nil, err
	}

	// parse amount
	parsedAmount := new(big.Rat)
	if _, ok := parsedAmount.SetString(record[amount]); !ok {
		return nil, err
	}

	return &Transaction{
		Date:          parsedDate,
		Document:      record[document],
		Amount:        parsedAmount,
		Description:   record[description],
		DebitAccount:  record[debitAccount],
		CreditAccount: record[creditAccount],
		Note:          record[note],
	}, nil
}

// byDate sorts the transactions, well, by date...
type byDate Transactions

func (a byDate) Len() int      { return len(a) }
func (a byDate) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a byDate) Less(i, j int) bool {
	return a[i].Date.Before(a[j].Date)
}
