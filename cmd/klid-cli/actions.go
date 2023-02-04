package main

import (
	"fmt"
	"klid"
	"os"

	"github.com/urfave/cli/v2"
)

func journalAction(c *cli.Context) error {
	txs := c.App.Metadata["txs"].(klid.Transactions)
	if c.Bool("u") { // filter by account
		account := pickAccount()
		txs.FilterByAccount(account)
	}
	if c.Bool("o") { // filter by date
		startDate, endDate := pickDate()
		txs.FilterByDate(startDate, endDate)
	}
	klid.FormattedJournal(txs, os.Stdout)
	return nil
}

func bookAction(c *cli.Context) error {
	txs := c.App.Metadata["txs"].(klid.Transactions)
	switch c.Bool("v") {
	case true: // return book for all accounts
		accounts := klid.UniqueAccounts(txs)
		for _, acc := range accounts {
			accLedger := klid.GeneralLedger(txs)
			fmt.Printf("Hlavní kniha účtu: %s\n", acc)
			klid.FormattedBook(accLedger[acc], os.Stdout)
		}
	default: // return book for selected account
		acc := pickAccount()
		accLedger := klid.GeneralLedger(txs)
		if val, ok := accLedger[acc]; !ok {
			fmt.Printf("   Zadejte celý účet.\n")
			for k := range accLedger {
				fmt.Printf("     - %s\n", k)
			}

		} else {
			klid.FormattedBook(val, os.Stdout)
		}

	}
	return nil
}

func accountsAction(c *cli.Context) error {
	txs := c.App.Metadata["txs"].(klid.Transactions)
	accounts := klid.UniqueAccounts(txs)
	klid.FormattedAccounts(accounts, os.Stdout)
	return nil
}
