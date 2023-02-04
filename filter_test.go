package klid

import (
	"math/big"
	"os"
	"reflect"
	"testing"
	"time"
)

func TestFilterByDate(t *testing.T) {
	csvFile, err := os.Open("docs/example.csv")
	if err != nil {
		t.Fatal(err)
	}
	txs, err := ParseTransactions(csvFile, true)
	if err != nil {
		t.Fatal(err)
	}
	startDate := time.Date(2020, time.February, 1, 0, 0, 0, 0, time.UTC)
	endDate := time.Date(2020, time.February, 28, 0, 0, 0, 0, time.UTC)
	txs.FilterByDate(startDate, endDate)
	expected := Transactions{
		{
			time.Date(2020, time.February, 15, 0, 0, 0, 0, time.UTC),
			"FAV15022020",
			big.NewRat(100000, 1),
			"VyFa za prodej služeb",
			"311000",
			"602001",
			"",
		},
		{
			time.Date(2020, time.February, 20, 0, 0, 0, 0, time.UTC),
			"BV2",
			big.NewRat(100000, 1),
			"Platba faktury od odběratele",
			"221000",
			"311000",
			"FAV15022020",
		},
	}
	if reflect.DeepEqual(txs, expected) != true {
		t.Error("Slices of structs are not deeply equal")
	}
}

func TestFilterByAccount(t *testing.T) {
	csvFile, err := os.Open("docs/example.csv")
	if err != nil {
		t.Fatal(err)
	}
	txs, err := ParseTransactions(csvFile, true)
	if err != nil {
		t.Fatal(err)
	}
	testAccount := "221"
	txs.FilterByAccount(testAccount)
	expected := Transactions{
		{
			time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC),
			"ID001",
			big.NewRat(50000, 1),
			"Počáteční stav - 221",
			"221000",
			"701000",
			"",
		},
		{
			time.Date(2020, time.January, 2, 0, 0, 0, 0, time.UTC),
			"BV1",
			big.NewRat(15000, 1),
			"Platba nájemného",
			"321000",
			"221000",
			"",
		},
		{
			time.Date(2020, time.February, 20, 0, 0, 0, 0, time.UTC),
			"BV2",
			big.NewRat(100000, 1),
			"Platba faktury od odběratele",
			"221000",
			"311000",
			"FAV15022020",
		},
		{
			time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC),
			"ID002",
			big.NewRat(135000, 1),
			"Konečný stav - 221",
			"702000",
			"221000",
			"",
		},
	}
	if reflect.DeepEqual(txs, expected) != true {
		t.Error("Slices of structs are not deeply equal")
	}
}
