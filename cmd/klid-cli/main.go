package main

import (
	"log"
	"os"
	"time"

	"github.com/urfave/cli/v2"
)

func main() {
	app := cli.NewApp()
	app.Name = "klid"
	app.Usage = "Účetní software pro tvůj terminál"
	app.Version = "0.1.1"
	app.Compiled = time.Now()
	app.Copyright = "klid Copyright (C) 2023"
	app.CustomAppHelpTemplate = MyAppHelpTemplate
	app.Authors = []*cli.Author{{
		Email: "k@jcmnk.xyz",
	}}

	app.Commands = []*cli.Command{
		&cmdJournal,
		&cmdBook,
		&cmdAccounts,
	}

	err := app.Run(os.Args)
	if err != nil {
		log.Fatal(err)
	}
}
