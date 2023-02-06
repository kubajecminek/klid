package main

import (
	"fmt"
	"klid"
	"log"
	"os"
	"time"

	"github.com/urfave/cli/v2"
)

func journalAction(c *cli.Context) error {
	txs := c.App.Metadata["txs"].(klid.Transactions)
	account := c.String("u")    // account value
	startDate := c.String("od") // start date value
	endDate := c.String("do")   // end date value

	// FIXME: Is there some more aestetic way of accomplishing the same thing?
	if account != "" {
		txs.FilterByAccount(account)
	}

	if startDate != "" {
		start, err := time.Parse(klid.DateLayout, startDate)
		if err != nil {
			log.Fatalln("Chyba příkazu: Datum bylo zadané ve špatném formátu")
		}
		txs.FilterFrom(start)
	}

	if endDate != "" {
		end, err := time.Parse(klid.DateLayout, endDate)
		if err != nil {
			log.Fatalln("Chyba příkazu: Datum byo zadané ve špatném formátu")
		}
		txs.FilterTo(end)
	}

	klid.FormattedJournal(txs, os.Stdout)
	return nil
}

func bookAction(c *cli.Context) error {
	txs := c.App.Metadata["txs"].(klid.Transactions)
	allAccounts := c.Bool("v")
	account := c.String("u")
	accLedger := klid.GeneralLedger(txs)

	switch {
	case account != "":
		if val, ok := accLedger[account]; !ok {
			fmt.Printf("   Zadejte celý účet.\n")
			for k := range accLedger {
				fmt.Printf("     - %s\n", k)
			}

		} else {
			fmt.Printf("Hlavní kniha účtu: %s\n", account)
			klid.FormattedBook(val, os.Stdout)
		}
	case allAccounts == true:
		fallthrough
	default:
		accounts := klid.UniqueAccounts(txs)
		for _, acc := range accounts {
			accLedger := klid.GeneralLedger(txs)
			fmt.Printf("Hlavní kniha účtu: %s\n", acc)
			klid.FormattedBook(accLedger[acc], os.Stdout)
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
