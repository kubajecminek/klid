all: build test
 
build:
	mkdir -p out/bin
	go build -o out/bin/klid-cli cmd/klid-cli/*.go
	go build -o out/bin/klid-cui cmd/klid-cui/*.go
 
test:
	go test
 
clean:
	go clean
	rm -rf out/bin/klid-cli out/bin/klid-cui
