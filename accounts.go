package klid

import (
	"sort"
)

// UniqueAccounts returns slice of all unique accounts used in financial transactions.
func UniqueAccounts(txs Transactions) []string {
	var accounts []string
	m := make(map[string]struct{})
	for _, tx := range txs {
		if _, ok := m[tx.DebitAccount]; !ok {
			accounts = append(accounts, tx.DebitAccount)
			m[tx.DebitAccount] = struct{}{}
		}
		if _, ok := m[tx.CreditAccount]; !ok {
			accounts = append(accounts, tx.CreditAccount)
			m[tx.CreditAccount] = struct{}{}
		}
	}
	sort.Strings(accounts)
	return accounts
}
