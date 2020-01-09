package parser

import (
	"convert/importdata"
	"fmt"
	"os"
	"strings"
)

func ParseExcel(fileName string) {

	importExcel, err := importdata.NewExcelImport(fileName)
	if err != nil {
		fmt.Printf("Cannot import excel file: %s\n", err.Error())
		return
	}

	data, err := importExcel.Import()
	if err != nil {
		fmt.Printf("Cannot import excel file: %s\n", err.Error())
		return
	}

	if len(data.Sheets) < 1 {
		fmt.Printf("Excel file: %s is empty\n", fileName)
		return
	}

	for _, j := range data.Sheets {
		sheet := j.Name
		CreateDirIfNotExists("output/" + sheet)
		for i := 0; i < j.Data.RowCount; i++ {
			if len(j.Data.Rows[i].RowData) < 2 {
				continue
			}
			s := j.Data.Rows[i].RowData[0]
			var st = ""
			for _, char := range s {
				st = st + string(char)
				if char == ' ' || char == '(' {
					break
				}
			}

			fileName := "output/" + sheet + "/" + strings.ToLower(st) + ".R"
			dv := j.Data.Rows[i].RowData[1]
			if dv == "" {
				continue
			}
			err := outputToFile(fileName, dv)
			if err != nil {
				return
			}
		}
	}

}

func outputToFile(path, data string) error {

	_ = os.Remove(path)
	// create file if not exists
	var file, err = os.OpenFile(path, os.O_WRONLY|os.O_CREATE, 0644)
	if err != nil {
		fmt.Printf("Error creating file: %s, error: %s\n", path, err.Error())
		return err
	}
	defer func() {
		_ = file.Close()
	}()
	_, err = file.WriteString(data)
	if err != nil {
		fmt.Printf("Error writing to file: %s, error: %s\n", path, err.Error())
	}
	return nil
}

func CreateDirIfNotExists(dir string) {
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		err = os.MkdirAll(dir, 0755)
		if err != nil {
			panic(err)
		}
	}
}
