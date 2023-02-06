package klid

import (
	"strings"
	"time"
)

// FilterTo removes transactions that occured after endDate
func (txs *Transactions) FilterTo(endDate time.Time) {
	oldSlice := *txs
	var newSlice Transactions
	end := endDate.Add(time.Second)

	for _, tx := range oldSlice {
		if tx.Date.Before(end) {
			newSlice = append(newSlice, tx)
		}
	}
	*txs = newSlice
}

// FilterFrom removes transactions that occured before startDate
func (txs *Transactions) FilterFrom(startDate time.Time) {
	oldSlice := *txs
	var newSlice Transactions
	start := startDate.Add(-1 * time.Second)

	for _, tx := range oldSlice {
		if tx.Date.After(start) {
			newSlice = append(newSlice, tx)
		}
	}
	*txs = newSlice
}

// FilterByAccount overwrites the slice only with transactions with a given account prefix.
func (txs *Transactions) FilterByAccount(account string) {
	oldSlice := *txs
	var newSlice Transactions

	for _, tx := range oldSlice {
		if strings.HasPrefix(tx.DebitAccount, account) || strings.HasPrefix(tx.CreditAccount, account) {
			newSlice = append(newSlice, tx)
		}
	}
	*txs = newSlice
}
