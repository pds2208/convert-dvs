package object

import (
	"bytes"
)

type Indexes struct {
	Name     string
	Elements []Object
}

func (i *Indexes) Inspect() string {
	var b bytes.Buffer
	b.WriteString("[")
	for k, v := range i.Elements {
		b.WriteString(v.Inspect())
		if k < len(i.Elements)-1 {
			b.WriteString(",")
		}
	}
	b.WriteString("]")
	return b.String()
}

func (i *Indexes) Type() Type { return IndexObj }
