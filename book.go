package klid

import (
	"math/big"
	"time"
)

// Account represents ledger account.
// Account holds all records of this account as well as total
// amount on either debit and credit side.
type Account struct {
	Records      []*Record
	TotalDebit   *big.Rat
	TotalCredit  *big.Rat
	TotalBalance *big.Rat
}

// Key is custom type for calculating monthly balances.
type Key struct {
	Month time.Month
	Year  int
}

// Record represents financial transaction that were assigned to
// this account either on debit or credit side.
type Record struct {
	Date           time.Time
	Document       string
	Description    string
	DebitAmount    *big.Rat
	CreditAmount   *big.Rat
	Balance        *big.Rat
	CounterAccount string
}

// GeneralLedger serves as a central repository for accounting data.
// GeneralLedger takes all transactions as an argument and returns a hash table where
// accounts ID serves as key and Account type as value.
func GeneralLedger(txs Transactions) map[string]Account {
	m := make(map[string]Account)
	for _, tx := range txs {
		// receive the records -> appendRecord new record -> assign it back to the map
		// this is a workaround, see this issue https://github.com/golang/go/issues/3117
		// Append the record on debit side to the account's records.
		a := m[tx.DebitAccount]
		a.appendRecord(tx.Date, tx.Document, tx.Description, tx.CreditAccount, tx.Amount, new(big.Rat), tx.Amount)
		a.updateBalance(tx.Amount, true)
		m[tx.DebitAccount] = a

		// Append the record on credit side to the account's records.
		b := m[tx.CreditAccount]
		b.appendRecord(tx.Date, tx.Document, tx.Description, tx.DebitAccount, new(big.Rat), tx.Amount, new(big.Rat).Neg(tx.Amount))
		b.updateBalance(tx.Amount, false)
		m[tx.CreditAccount] = b
	}
	return m
}

func (a *Account) updateBalance(amount *big.Rat, onDebit bool) {
	if a.TotalDebit == nil {
		a.TotalDebit = new(big.Rat)
	}
	if a.TotalCredit == nil {
		a.TotalCredit = new(big.Rat)
	}
	if a.TotalBalance == nil {
		a.TotalBalance = new(big.Rat)
	}
	if onDebit {
		a.TotalDebit.Add(a.TotalDebit, amount)
	} else {
		a.TotalCredit.Add(a.TotalCredit, amount)

	}
	a.TotalBalance.Sub(a.TotalDebit, a.TotalCredit)
}

// MonthlyBalance computes monthly balance for a given account
// and returns a map where key is represented with month and year.The values
// are three rat numbers representing monthly balance on debit and credit side
// as well as the difference between the two.
func (a *Account) MonthlyBalance() map[Key][3]*big.Rat {
	// TODO: there has to be better way...
	m := make(map[Key][3]*big.Rat)
	for _, rec := range a.Records {
		a := m[Key{rec.Date.Month(), rec.Date.Year()}]
		for i := 0; i < 3; i++ {
			if a[i] == nil {
				a[i] = new(big.Rat)
			}
		}
		a[0].Add(a[0], rec.DebitAmount)
		a[1].Add(a[1], rec.CreditAmount)
		a[2].Sub(a[0], a[1])
		m[Key{rec.Date.Month(), rec.Date.Year()}] = a
	}
	return m
}

// Append appendRecords record to the account's records. idk how to write this better.
func (a *Account) appendRecord(date time.Time, doc, desc, acc string, debit, credit, balance *big.Rat) {
	a.Records = append(a.Records, &Record{
		Date:           date,
		Document:       doc,
		Description:    desc,
		DebitAmount:    debit,
		CreditAmount:   credit,
		Balance:        balance,
		CounterAccount: acc,
	})
}
