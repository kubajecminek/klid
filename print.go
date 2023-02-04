package klid

import (
	"fmt"
	"io"
	"strconv"

	"github.com/olekukonko/tablewriter"
)

// FormattedJournal formats the journal using tablewriter package
func FormattedJournal(txs Transactions, w io.Writer) {
	tableData := [][]string{}
	for _, tx := range txs {
		col := []string{
			tx.Date.Format(DateLayout),
			tx.Document,
			tx.Amount.FloatString(2),
			tx.Description,
			tx.DebitAccount,
			tx.CreditAccount,
			tx.Note,
		}
		tableData = append(tableData, col)
	}
	// Set and render tabular data
	table := tablewriter.NewWriter(w)
	table.SetHeader([]string{"Datum", "Doklad", "Částka", "Popis", "MD", "Dal", "Poznámka"})
	table.AppendBulk(tableData)
	table.Render()
}

// FormattedBook ...
func FormattedBook(a Account, w io.Writer) {
	tableData := [][]string{}
	for _, r := range a.Records {
		col := []string{
			r.Date.Format(DateLayout),
			r.Document,
			r.Description,
			r.DebitAmount.FloatString(2),
			r.CreditAmount.FloatString(2),
			r.Balance.FloatString(2),
			r.CounterAccount,
		}
		tableData = append(tableData, col)
	}
	// Set and render tabular data
	table := tablewriter.NewWriter(w)
	table.SetHeader([]string{"Datum", "Doklad", "Popis", "MD [Kč]", "Dal [Kč]", "Saldo [Kč]", "Protiúčet"})
	table.AppendBulk(tableData)
	table.SetFooter([]string{
		" ",
		" ",
		"Suma",
		a.TotalDebit.FloatString(2),
		a.TotalCredit.FloatString(2),
		a.TotalBalance.FloatString(2),
		" ",
	})
	table.Render()

	fmt.Printf("\n")
}

// FormattedAccounts ...
func FormattedAccounts(accounts []string, w io.Writer) {
	tableData := [][]string{}
	for i, acc := range accounts {
		col := []string{
			strconv.Itoa(i),
			acc,
		}
		tableData = append(tableData, col)
	}
	// Set and render tabular data
	table := tablewriter.NewWriter(w)
	table.SetHeader([]string{"Pořadové číslo", "Číslo účtu"})
	table.AppendBulk(tableData)
	table.Render()
}
