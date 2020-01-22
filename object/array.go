package object

import "fmt"

type Array struct {
	Name     string
	Elements []Object
}

func (i *Array) Inspect() string { return fmt.Sprintf("%s", i.Name) }

func (i *Array) Type() Type { return ArrayObj }
