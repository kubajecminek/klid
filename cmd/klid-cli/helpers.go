package main

import (
	"fmt"
	"klid"
	"log"
	"time"
)

func pickAccount() string {
	var account string
	fmt.Print("Účet: ")
	fmt.Scanln(&account)
	return account
}

func pickDate() (time.Time, time.Time) {
	var startDate, endDate string
	fmt.Print("Počáteční období [DD.MM.YYYY]: ")
	fmt.Scanln(&startDate)

	fmt.Print("Konečné období [DD.MM.YYYY]: ")
	fmt.Scanln(&endDate)

	start, err := time.Parse(klid.DateLayout, startDate)
	if err != nil {
		log.Fatal("Zřejmě jste zadali datum ve špatném formátu")
	}

	end, err := time.Parse(klid.DateLayout, endDate)
	if err != nil {
		log.Fatal("Zřejmě jste zadali datum ve špatném formátu")
	}

	return start, end
}
