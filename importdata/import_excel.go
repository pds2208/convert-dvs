package importdata

import (
	"fmt"
	"github.com/tealeg/xlsx"
)

type Header struct {
	VariableName        string
	VariableDescription string
	VariableType        string
	VariableLength      int
	VariablePrecision   int
	LabelName           string
	Drop                bool
}

type Rows struct {
	RowData []string
}

type ImportData struct {
	Header      []Header
	HeaderCount int
	Rows        []Rows
	RowCount    int
}

type Sheet struct {
	Name string
	Data ImportData
}

type ExcelImportData struct {
	Sheets     []Sheet
	SheetCount int
}

type ImportExcel struct {
	sheets []*xlsx.Sheet
	excel  *xlsx.File
}

func NewExcelImport(fileName string) (ImportExcel, error) {
	excel, err := xlsx.OpenFile(fileName)
	if err != nil {
		fmt.Println(err)
		return ImportExcel{}, err
	}
	sheetsMap := excel.Sheets
	return ImportExcel{sheetsMap, excel}, nil
}

func (ex ImportExcel) GetSheetNames() []string {
	sheets := make([]string, len(ex.sheets))
	for i, j := range ex.sheets {
		sheets[i] = j.Name
	}
	return sheets
}

func (ex ImportExcel) ImportSheet(sheet string) (ImportData, error) {
	if ex.excel == nil {
		return ImportData{}, fmt.Errorf("excel file is not open")
	}

	r := ex.excel.Sheet[sheet].Rows
	headerCol := r[0].Cells
	headerCount := 0
	sheetRows := r[1:]
	rowsCount := len(sheetRows)

	header := make([]Header, 0)
	for _, j := range headerCol {
		if j.Value == "" {
			continue
		}
		h := Header{
			VariableName:        j.Value,
			VariableDescription: "",
			VariableType:        "",
			VariableLength:      0,
			VariablePrecision:   0,
			LabelName:           "",
			Drop:                false,
		}
		header = append(header, h)
		headerCount++
	}

	allRows := make([]Rows, rowsCount)
	for i, j := range sheetRows {
		a := make([]string, 0)
		for k, l := range j.Cells {
			if k >= headerCount {
				break
			}
			a = append(a, l.Value)
		}
		allRows[i] = Rows{RowData: a}
	}

	id := ImportData{}
	id.Header = header
	id.HeaderCount = headerCount
	id.Rows = allRows
	id.RowCount = rowsCount

	return id, nil
}

func (ex ImportExcel) Import() (ExcelImportData, error) {
	if ex.excel == nil {
		return ExcelImportData{}, fmt.Errorf("excel file is not open")
	}

	numSheets := len(ex.excel.Sheets)

	sheets := make([]Sheet, 0)
	importData := make([]ImportData, 0)

	for _, value := range ex.excel.Sheets {
		id, err := ex.ImportSheet(value.Name)
		if err != nil {
			return ExcelImportData{}, err
		}
		importData = append(importData, id)
		sheets = append(sheets, Sheet{
			Name: value.Name,
			Data: id,
		})
	}

	return ExcelImportData{
		Sheets:     sheets,
		SheetCount: numSheets,
	}, nil
}
