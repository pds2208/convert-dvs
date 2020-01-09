package main

import (
	"convert/parser"
	"flag"
	"fmt"
	"os"
)

var importFile string

func main() {

	flag.StringVar(&importFile, "file", "", "excel file containing DVs")
	flag.Parse()

	if importFile == "" {
		fmt.Println("-file not set")
		os.Exit(-1)
	}

	parser.ParseExcel(importFile)
}
