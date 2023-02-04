package main

import (
	"fmt"
	"klid"
	"log"
	"os"
	"strings"

	"github.com/jroimartin/gocui"
)

var (
	fileName string
	nv       *gocui.View
	err      error
	txs      klid.Transactions
)

func init() {
	if len(os.Args) <= 1 {
		log.Fatalf("Zřejmě jste zapomněli zadat název deníku.")
	} else {
		filename := os.Args[1]
		csvFile, err := os.Open(filename)
		if err != nil {
			log.Fatal(err)
		}
		txs, err = klid.ParseTransactions(csvFile, true)
		if err != nil {
			log.Fatalf("Chyba při čtení souboru: %s", err)
		}
	}
}

// TODO: Remove redundancy in the functions down below
func deleteView(g *gocui.Gui, v *gocui.View) error {
	if err := g.DeleteView(v.Name()); err != nil {
		return err
	}
	if _, err := g.SetCurrentView("Main"); err != nil {
		return err
	}
	return nil
}

func journalHandler(g *gocui.Gui, v *gocui.View) error {
	if nv, err = g.SetCurrentView("Main"); err != nil {
		return err
	}
	nv.Clear()
	klid.FormattedJournal(txs, nv)
	return nil
}

func accountsHandler(g *gocui.Gui, v *gocui.View) error {
	if nv, err = g.SetCurrentView("Main"); err != nil {
		return err
	}
	nv.Clear()
	accounts := klid.UniqueAccounts(txs)
	klid.FormattedAccounts(accounts, nv)
	return nil
}

func bookHandler(g *gocui.Gui, v *gocui.View) error {
	account := v.Buffer()
	// *view.Buffer adds trailing newline that needs to be trimmed
	account = strings.Trim(account, "\n")
	if nv, err = g.SetCurrentView("Main"); err != nil {
		return err
	}
	nv.Clear()
	accLedger := klid.GeneralLedger(txs)
	if _, ok := accLedger[account]; ok {
		klid.FormattedBook(accLedger[account], nv)
	} else {
		fmt.Fprintf(nv, "   Zadejte celý účet.\n")
		for k := range accLedger {
			fmt.Fprintf(nv, "     - %s\n", k)
		}
	}
	deleteView(g, v)
	return nil
}
