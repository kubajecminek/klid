package main

import (
	"klid"
	"log"
	"os"

	"github.com/urfave/cli/v2"
)

func argTest(c *cli.Context) error {
	if c.NArg() < 1 {
		log.Fatalln("Chyba příkazu: Nebyl zadán název deníku")
	}
	filename := c.Args().Get(0)
	csvFile, err := os.Open(filename)
	if err != nil {
		log.Fatalln("Interní chyba:", err)
	}

	c.App.Metadata["txs"], err = klid.ParseTransactions(csvFile, true)
	if err != nil {
		log.Fatalln("Interní chyba:", err)
	}
	return nil
}

var (
	cmdJournal cli.Command = cli.Command{
		Name:    "denik",
		Aliases: []string{"d"},
		Usage:   "Účetní deník",
		Action:  journalAction,
		Flags: []cli.Flag{
			&cli.StringFlag{
				Name:    "ucet",
				Usage:   "pouze jeden konkrétní účet",
				Aliases: []string{"u"},
			},
			&cli.StringFlag{
				Name:  "od",
				Usage: "časový filtr - začátek",
			},
			&cli.StringFlag{
				Name:  "do",
				Usage: "časový filtr - konec",
			},
		},
		Before:             argTest,
		CustomHelpTemplate: MyCommandHelpTemplate,
	}

	cmdBook cli.Command = cli.Command{
		Name:    "kniha",
		Aliases: []string{"k"},
		Usage:   "Hlavní kniha",
		Action:  bookAction,
		Flags: []cli.Flag{
			&cli.BoolFlag{Name: "vse",
				Usage:   "všechny účty",
				Aliases: []string{"v"},
			},
			&cli.StringFlag{
				Name:    "ucet",
				Usage:   "pouze jeden konkrétní účet",
				Aliases: []string{"u"},
			},
		},
		Before:             argTest,
		CustomHelpTemplate: MyCommandHelpTemplate,
	}

	cmdAccounts cli.Command = cli.Command{
		Name:               "ucty",
		Aliases:            []string{"u"},
		Usage:              "Seznam všech použitých účtů",
		Action:             accountsAction,
		Before:             argTest,
		CustomHelpTemplate: MyCommandHelpTemplate,
	}
)
