all: build test
 
build:
	mkdir -p out/bin
	go build -o out/bin/klid-cli cmd/klid-cli/*.go
	go build -o out/bin/klid-cui cmd/klid-cui/*.go
 
test:
	go test

bench:
	go test -bench=.
 
clean:
	go clean
	rm out/bin/klid-cli out/bin/klid-cui
