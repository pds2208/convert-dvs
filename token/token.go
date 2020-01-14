package token

type TokenType string

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"

	// Identifiers + literals
	IDENT  = "IDENT"  // add, foobar, x, y, ...
	INT    = "INT"    // 1343456
	FLOAT  = "FLOAT"  // 123.4567
	STRING = "STRING" // "foobar"

	// Operators
	ASSIGN   = "="
	PLUS     = "+"
	MINUS    = "-"
	BANG     = "!"
	ASTERISK = "*"
	SLASH    = "/"
	LT       = "<"
	GT       = ">"
	EQ       = "="
	NOT_EQ   = "^="

	// Delimiters
	COMMA     = ","
	SEMICOLON = ";"

	LPAREN   = "("
	RPAREN   = ")"
	LBRACE   = "{"
	RBRACE   = "}"
	LBRACKET = "["
	RBRACKET = "]"
	STOP     = "."

	// Keywords

	IF          = "IF"
	ELSE        = "ELSE"
	IN          = "IN"
	THEN        = "THEN"
	AND         = "AND"
	DO          = "DO"
	END         = "END"
	ARRAY       = "ARRAY"
	LET         = "LET"
	FUNCTION    = "%MACRO"
	ENDFUNCTION = "%MEND"
)

type Token struct {
	Type    TokenType
	Literal string
}

var keywords = map[string]TokenType{
	"if":     IF,
	"else":   ELSE,
	"in":     IN,
	"then":   THEN,
	"and":    AND,
	"do":     DO,
	"end":    END,
	"array":  ARRAY,
	"let":    LET,
	"%macro": FUNCTION,
	"%mend":  ENDFUNCTION,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}
