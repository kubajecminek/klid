package klid

import (
	"math/big"
	"reflect"
	"strconv"
	"testing"
	"time"

	"github.com/campoy/unique"
)

func TestUniqueAccounts1(t *testing.T) {
	txs := txsFixture()
	got := UniqueAccounts(txs)
	expected := []string{"211000", "221000", "311000", "321000", "411000", "518000", "602001", "701000", "702000", "710000"}

	if reflect.DeepEqual(got, expected) != true {
		t.Error("Slices are not deeply equal")
	}
}

func TestUniqueAccounts2(t *testing.T) {
	var txs Transactions
	var expected []string
	for i := 0; i < 1000; i++ {
		txs = append(txs, &Transaction{
			time.Now(),
			"",
			big.NewRat(0, 1),
			"",
			strconv.Itoa(i),
			strconv.Itoa(i),
			"",
		})
	}
	got := UniqueAccounts(txs)
	for i := 0; i < 1000; i++ {
		expected = append(expected, strconv.Itoa(i))
	}

	// TODO: fix sorting
	if len(got) != 1000 {
		t.Errorf("expected 1000 entries, got: %d", len(got))
	}
}

func BenchmarkUniqueAccounts(b *testing.B) {
	txs := txsFixture()
	for n := 0; n < b.N; n++ {
		UniqueAccounts(txs)
	}
}

func BenchmarkUniquePackage(b *testing.B) {
	txs := txsFixture()
	for n := 0; n < b.N; n++ {
		func() []string {
			var accounts []string
			// append all accounts into the slice
			for _, tx := range txs {
				accounts = append(accounts, tx.DebitAccount, tx.CreditAccount)
			}
			// sort accounts
			less := func(i, j int) bool { return accounts[i] < accounts[j] }
			// remove duplicates and sort
			unique.Slice(&accounts, less)

			return accounts
		}()
	}
}
