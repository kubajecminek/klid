package klid

import (
	"encoding/csv"
	"io"
	"math/big"
	"os"
	"reflect"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"
)

func TestCSVParser(t *testing.T) {
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

	csvParser := CSVParser{SkipFirst: true}
	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			out, err := csvParser.Parse(strings.NewReader(tt.Input))
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(out, tt.Output) {
				t.Errorf("CSVParser error: case %s", tt.Name)
			}
		})
	}
}

func BenchmarkCSVParser(b *testing.B) {
	csvFile, err := os.Open("docs/example.csv")
	if err != nil {
		b.Fatal(err)
	}
	csvParser := CSVParser{SkipFirst: true}
	for n := 0; n < b.N; n++ {
		csvParser.Parse(csvFile)
	}
}

// I've tried to implement CSVParse.AsyncParse() which does exactly the same thing as CSVParser.Parse()
// but concurrently. Untfortunately, this implementation is roughly 2x slower than basic non-concurrent parsing.
// TODO: I really need to learn more about concurrency.
type CSVReaderMessage struct {
	row []string
	e   error
}

type KlidParserMessage struct {
	tx *Transaction
	e  error
}

func ParseCSVAsync(r io.Reader, skipFirst bool) (Transactions, error) {
	var txs Transactions
	csvReaderMessage := make(chan CSVReaderMessage)
	klidParserMessage := make(chan KlidParserMessage)
	var wg sync.WaitGroup

	// Goroutine 1
	wg.Add(1)
	go func(out chan CSVReaderMessage) {
		reader := csv.NewReader(r)
		if skipFirst { // move reader onto the next line
			reader.FieldsPerRecord = -1
			reader.Read()
		}
		for {
			row, err := reader.Read()
			if err == io.EOF {
				break
			}
			if err != nil {
				out <- CSVReaderMessage{nil, err}
			}
			out <- CSVReaderMessage{row, nil}
		}
		wg.Done()
		close(out)
	}(csvReaderMessage)

	// Goroutine 2
	// We can spawn more goroutines here, but it doesn't speed up the execution
	wg.Add(1)
	go func(in chan CSVReaderMessage, out chan KlidParserMessage) {
		for {
			message, ok := <-in
			if !ok {
				break
			}
			// forward error message
			if message.e != nil {
				out <- KlidParserMessage{nil, message.e}
			}

			parsedDate, err := time.Parse(DateLayout, message.row[date])
			if err != nil {
				out <- KlidParserMessage{nil, err}
			}

			// parse amount
			parsedAmount := new(big.Rat)
			if _, ok := parsedAmount.SetString(message.row[amount]); !ok {
				out <- KlidParserMessage{nil, err}
			}

			out <- KlidParserMessage{&Transaction{
				Date:          parsedDate,
				Document:      message.row[document],
				Amount:        parsedAmount,
				Description:   message.row[description],
				DebitAccount:  message.row[debitAccount],
				CreditAccount: message.row[creditAccount],
				Note:          message.row[note],
			}, nil}
		}
		wg.Done()
		close(out)
	}(csvReaderMessage, klidParserMessage)

	// Receive the results.
	for {
		message, ok := <-klidParserMessage
		if !ok {
			break
		}
		if message.e != nil {
			return nil, message.e
		}
		txs = append(txs, message.tx)
	}
	wg.Wait()

	// Sort and return the results.
	sort.Sort(byDate(txs))
	return txs, nil
}

func BenchmarkParseCSVAsync(b *testing.B) {
	csvFile, err := os.Open("docs/example.csv")
	if err != nil {
		b.Fatal(err)
	}
	for n := 0; n < b.N; n++ {
		ParseCSVAsync(csvFile, true)
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
