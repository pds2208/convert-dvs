package object

import "fmt"

type Boolean struct {
	Value bool
}

func (i *Boolean) Inspect() string { return fmt.Sprintf("%t", i.Value) }

func (i *Boolean) Type() Type { return BooleanObj }
