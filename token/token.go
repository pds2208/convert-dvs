package token

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

const (
	Illegal = "ILLEGAL"
	Eof     = "EOF"
	// Identifiers + literals
	Ident   = "IDENT" // add, foobar, x, y, ...
	Integer = "INT"
	// Operators
	Assign = "="
	Minus  = "-"
	Plus   = "+"
	// Delimiters
	Comma      = ","
	Semicolon  = ";"
	LeftParen  = "("
	RightParen = ")"
	LeftBrace  = "{"
	RightBrace = "}"
	Stop       = "."
	Not        = "^"

	NotEqual = "^="

	// Keywords

	IfKeyword     = "IF"
	ElseKeyword   = "ELSE"
	InKeyword     = "IN"
	ThenKeyword   = "THEN"
	AndKeyword    = "AND"
	DoKeyword     = "DO"
	EndKeyword    = "END"
	StringKeyword = "STRING"
	ReturnKeyword = "RETURN"
)

var keywords = map[string]TokenType{
	"if":     IfKeyword,
	"else":   ElseKeyword,
	"in":     InKeyword,
	"then":   ThenKeyword,
	"and":    AndKeyword,
	"do":     DoKeyword,
	"end":    EndKeyword,
	"return": ReturnKeyword,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return Ident
}
