package klid

import (
	"math/big"
	"reflect"
	"testing"
	"time"
)

func txsFixture() Transactions {
	return Transactions{
		{time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC), "ID001", big.NewRat(100000, 1), "Počáteční stav - 411", "701000", "411000", ""},
		{time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC), "ID001", big.NewRat(50000, 1), "Počáteční stav - 221", "221000", "701000", ""},
		{time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC), "ID001", big.NewRat(50000, 1), "Počáteční stav - 211", "211000", "701000", ""},
		{time.Date(2020, time.January, 2, 0, 0, 0, 0, time.UTC), "FAP001", big.NewRat(15000, 1), "Nájemné na únor", "518000", "321000", ""},
		{time.Date(2020, time.January, 2, 0, 0, 0, 0, time.UTC), "BV1", big.NewRat(15000, 1), "Platba nájemného", "321000", "221000", ""},
		{time.Date(2020, time.February, 15, 0, 0, 0, 0, time.UTC), "FAV15022020", big.NewRat(100000, 1), "VyFa za prodej služeb", "311000", "602001", ""},
		{time.Date(2020, time.February, 20, 0, 0, 0, 0, time.UTC), "BV2", big.NewRat(100000, 1), "Platba faktury od odběratele", "221000", "311000", "FAV15022020"},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(100000, 1), "Konečný stav - 411", "411000", "702000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(50000, 1), "Konečný stav - 211", "702000", "211000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(135000, 1), "Konečný stav - 221", "702000", "221000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(15000, 1), "Uzavření účtu - 518000", "710000", "518000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(100000, 1), "Uzavření účtu - 602001", "602001", "710000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(85000, 1), "Zaúčtování zisku", "710000", "702000", ""},
	}
}

func TestFilterFrom(t *testing.T) {
	txs := txsFixture()
	startDate := time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC)
	txs.FilterFrom(startDate)
	expected := Transactions{
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(100000, 1), "Konečný stav - 411", "411000", "702000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(50000, 1), "Konečný stav - 211", "702000", "211000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(135000, 1), "Konečný stav - 221", "702000", "221000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(15000, 1), "Uzavření účtu - 518000", "710000", "518000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(100000, 1), "Uzavření účtu - 602001", "602001", "710000", ""},
		{time.Date(2020, time.December, 31, 0, 0, 0, 0, time.UTC), "ID002", big.NewRat(85000, 1), "Zaúčtování zisku", "710000", "702000", ""},
	}
	if reflect.DeepEqual(txs, expected) != true {
		t.Error("Slices are not deeply equal")
	}
}

func TestFilterTo(t *testing.T) {
	txs := txsFixture()
	endDate := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
	txs.FilterTo(endDate)

	expected := Transactions{
		{time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC), "ID001", big.NewRat(100000, 1), "Počáteční stav - 411", "701000", "411000", ""},
		{time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC), "ID001", big.NewRat(50000, 1), "Počáteční stav - 221", "221000", "701000", ""},
		{time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC), "ID001", big.NewRat(50000, 1), "Počáteční stav - 211", "211000", "701000", ""},
	}
	if reflect.DeepEqual(txs, expected) != true {
		t.Error("Slices are not deeply equal")
	}
}

func TestFilterByAccount(t *testing.T) {
	txs := txsFixture()
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
