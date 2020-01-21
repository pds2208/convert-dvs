package object

type Type string

type Object interface {
	Type() Type
	Inspect() string
}

const (
	StringObj  = "STRING"
	IntegerObj = "INTEGER"
	FloatObj   = "FLOAT"
	NullObj    = "NULL"
	ArrayObj   = "ARRAY"
	BooleanObj = "BOOLEAN"
	ErrorObj   = "ERROR"
)

type Error struct {
	Message string
}

func (e *Error) Type() Type      { return ErrorObj }
func (e *Error) Inspect() string { return "ERROR: " + e.Message }
