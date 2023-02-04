package klid

import (
	"strings"
	"time"
)

// FilterByDate overwrites the slice only with transactions that occured in
// a specified time range.
func (txs *Transactions) FilterByDate(startDate, endDate time.Time) {
	oldSlice := *txs
	var newSlice Transactions
	start := startDate.Add(-1 * time.Second)
	end := endDate.Add(time.Second)

	for _, tx := range oldSlice {
		if tx.Date.After(start) && tx.Date.Before(end) {
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
