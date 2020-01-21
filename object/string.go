package object

import "fmt"

type String struct {
	Value string
}

func (i *String) Inspect() string { return fmt.Sprintf("%s", i.Value) }

func (i *String) Type() Type { return StringObj }
