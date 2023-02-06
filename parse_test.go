package klid

import (
	"encoding/csv"
	"io"
	"math/big"
	"os"
	"reflect"
	"sort"
	"strings"
	"testing"
	"time"
)

func TestParseTransactions(t *testing.T) {
	tests := []struct {
		Name   string
		Input  string
		Output Transactions
	}{{
		Name: "Test Case 1",
		Input: `Header,jslfjlsf,blah,---||||, some other field
01.01.2018,ID001,0.5,Description - Simple Test,221000,701000,,`,
		Output: Transactions{&Transaction{
			Date:          time.Date(2018, time.January, 1, 0, 0, 0, 0, time.UTC),
			Document:      "ID001",
			Amount:        big.NewRat(2, 4),
			Description:   "Description - Simple Test",
			DebitAccount:  "221000",
			CreditAccount: "701000",
			Note:          "",
		},
		},
	}, {
		Name: "Test Case 2",
		Input: `Header,,,,,,,
15.12.2009,ABC123,0.75,Description - Simple Test 2,123.123,321.321,Random Note,`,
		Output: Transactions{&Transaction{
			Date:          time.Date(2009, time.December, 15, 0, 0, 0, 0, time.UTC),
			Document:      "ABC123",
			Amount:        big.NewRat(3, 4),
			Description:   "Description - Simple Test 2",
			DebitAccount:  "123.123",
			CreditAccount: "321.321",
			Note:          "Random Note",
		}},
	}}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			out, err := ParseTransactions(strings.NewReader(tt.Input), true)
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(out, tt.Output) {
				t.Errorf("ParseTransactions error: case %s", tt.Name)
			}
		})
	}
}

func BenchmarkParseTransactions(b *testing.B) {
	csvFile, err := os.Open("docs/example.csv")
	if err != nil {
		b.Fatal(err)
	}
	for n := 0; n < b.N; n++ {
		ParseTransactions(csvFile, true)
	}
}

// I've tried to implement ParseTransactionsAsync which does exactly the same thing as ParseTransactions
// but concurrently. Untfortunately, this implementation is roughly 2x slower than basic non-concurrent parsing.
// TODO: I really need to learn how concurrency works.
// goos: linux
// goarch: amd64
// pkg: klid
// BenchmarkParseTransactions-6              400705              2841 ns/op            4192 B/op          2 allocs/op
// BenchmarkParseTransactionsAsync-6         169406              7094 ns/op            4480 B/op          5 allocs/op
// BenchmarkLoadExample-6                    419197              2857 ns/op            4192 B/op          2 allocs/op
// TODO: return error as well
func BenchmarkParseTransactionsAsync(b *testing.B) {
	csvFile, err := os.Open("docs/example.csv")
	if err != nil {
		b.Fatal(err)
	}
	for n := 0; n < b.N; n++ {
		func(r io.Reader, skipFirst bool) Transactions {
			var txs Transactions
			e := make(chan error)
			results := make(chan *Transaction)
			records := make(chan []string)
			// Goroutine 1: read the CSV file and send []string over the channel.
			go func(out chan []string, e chan<- error) {
				reader := csv.NewReader(r)
				for i := 1; true; i++ {
					record, err := reader.Read()
					if skipFirst && i == 1 { // skip the first iteration
						continue
					}
					if err == io.EOF {
						break
					}
					if err != nil {
						e <- err
					}
					out <- record
				}
				close(out)
			}(records, e)

			// Goroutine 2: parse []string received over the channel and send
			// parsed Transaction forward.
			go func(in chan []string, out chan *Transaction, e chan<- error) {
				for i := range in {
					tx, err := parseRecord(i)
					if err != nil {
						e <- err
					}
					out <- tx
				}
				close(out)
			}(records, results, e)

			// Receive the results.
			for tx := range results {
				txs = append(txs, tx)
			}
			// Sort and return the results.
			sort.Sort(byDate(txs))
			return txs
		}(csvFile, true)
	}
}

// BenchmarkReadExample benchmarks read example from go documentation
// https://golang.org/pkg/encoding/csv/#example_Reader
func BenchmarkLoadExample(b *testing.B) {
	csvFile, err := os.Open("docs/example.csv")
	if err != nil {
		b.Fatal(err)
	}
	for n := 0; n < b.N; n++ {
		func(reader io.Reader) (s [][]string) {
			r := csv.NewReader(csvFile)
			for {
				record, err := r.Read()
				if err == io.EOF {
					break
				}
				if err != nil {
					b.Fatal(err)
				}
				s = append(s, record)
			}
			return s
		}(csvFile)
	}
}
