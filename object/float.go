package object

import "fmt"

type Float struct {
	Value float64
}

func (i *Float) Inspect() string { return fmt.Sprintf("%f", i.Value) }

func (i *Float) Type() Type { return FloatObj }
