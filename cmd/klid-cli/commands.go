package main

import (
	"fmt"
	"klid"
	"log"
	"os"

	"github.com/urfave/cli/v2"
)

func argTest(c *cli.Context) error {
	if c.NArg() < 1 {
		return fmt.Errorf("Nebyl zadán název deníku")
	}
	filename := c.Args().Get(0)
	csvFile, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}

	c.App.Metadata["txs"], err = klid.ParseTransactions(csvFile, true)
	if err != nil {
		log.Fatalln("Chyba:", err)
	}
	return nil
}

var (
	cmdJournal cli.Command = cli.Command{
		Name:    "denik",
		Aliases: []string{"d", "den"},
		Usage:   "Vrátí formátovaný a seřazený účetní deník",
		Action:  journalAction,
		Flags: []cli.Flag{
			&cli.BoolFlag{
				Name:    "ucet",
				Usage:   "Zobrazí deník pro daný účet",
				Aliases: []string{"u"},
			},

			&cli.BoolFlag{
				Name:    "obdobi",
				Usage:   "Zobrazí deník pro zvolené období",
				Aliases: []string{"o"},
			},
		},
		Before:             argTest,
		CustomHelpTemplate: MyCommandHelpTemplate,
	}

	cmdBook cli.Command = cli.Command{
		Name:    "kniha",
		Aliases: []string{"k"},
		Usage:   "Vrátí hlavní knihu pro daný účet",
		Action:  bookAction,
		Flags: []cli.Flag{
			&cli.BoolFlag{Name: "vse",
				Usage:   "Zobrazí hlavní knihu pro všechny dostupné účty",
				Aliases: []string{"v"},
			},
		},
		Before:             argTest,
		CustomHelpTemplate: MyCommandHelpTemplate,
	}

	cmdAccounts cli.Command = cli.Command{
		Name:               "ucty",
		Aliases:            []string{"u"},
		Usage:              "Vrátí seznam všech použitých účtů",
		Action:             accountsAction,
		Before:             argTest,
		CustomHelpTemplate: MyCommandHelpTemplate,
	}
)
