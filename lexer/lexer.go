package lexer

import (
	"fmt"
	"strings"
	"unicode"

	"convert/token"
)

type Lexer struct {
	position     int
	readPosition int
	ch           rune
	characters   []rune
	prevToken    token.Token
	orig         string
}

// New a Lexer instance from string input.
func New(input string) *Lexer {
	l := &Lexer{characters: []rune(input)}
	l.orig = input
	l.readChar()
	return l
}

// GetLine returns the rough line-number of our current position.
func (l *Lexer) GetLine() int {
	line := 0
	chars := len(l.characters)
	i := 0

	for i < l.readPosition && i < chars {

		if l.characters[i] == '\n' {
			line++
		}
		i++
	}
	return line
}

// read one forward character
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.characters) {
		l.ch = rune(0)
	} else {
		l.ch = l.characters[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token
	l.skipWhitespace()

	if l.ch == '/' && l.peekChar() == '/' {
		l.skipComment()
		return l.NextToken()
	}

	if l.ch == '#' && l.peekChar() == '!' && l.position == 0 {
		l.skipComment()
		return l.NextToken()
	}

	if l.ch == '/' && l.peekChar() == '*' {
		l.skipMultiLineComment()
	}

	switch l.ch {
	case '&':
		if l.peekChar() == '&' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.AND, Literal: string(ch) + string(l.ch)}
		}
	case '|':
		if l.peekChar() == '|' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.OR, Literal: string(ch) + string(l.ch)}
		}

	case '=':
		tok = newToken(token.ASSIGN, l.ch)
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '?':
		tok = newToken(token.QUESTION, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case '-':
		tok = newToken(token.MINUS, l.ch)
	case '/':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.SLASH_EQUALS, Literal: string(ch) + string(l.ch)}
		} else {
			// slash is mostly division, but could
			// be the start of a regular expression

			// We exclude:
			//   a[b] / c       -> RBRACKET
			//   ( a + b ) / c   -> RPAREN
			//   a / c           -> IDENT
			//   3.2 / c         -> FLOAT
			//   1 / c           -> IDENT
			//
			if l.prevToken.Type == token.RBRACKET ||
				l.prevToken.Type == token.RPAREN ||
				l.prevToken.Type == token.IDENT ||
				l.prevToken.Type == token.INT ||
				l.prevToken.Type == token.FLOAT {

				tok = newToken(token.SLASH, l.ch)
			} else {
				str, err := l.readRegexp()
				if err == nil {
					tok.Type = token.REGEXP
					tok.Literal = str
				} else {
					fmt.Printf("%s\n", err.Error())
					tok.Type = token.REGEXP
					tok.Literal = str
				}
			}
		}
	case '*':
		if l.peekChar() == '*' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.POW, Literal: string(ch) + string(l.ch)}
		} else if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.ASTERISK_EQUALS, Literal: string(ch) + string(l.ch)}
		} else {
			tok = newToken(token.ASTERISK, l.ch)
		}
	case '<':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.LT_EQUALS, Literal: string(ch) + string(l.ch)}
		} else {
			tok = newToken(token.LT, l.ch)
		}
	case '>':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.GT_EQUALS, Literal: string(ch) + string(l.ch)}
		} else {
			tok = newToken(token.GT, l.ch)
		}
	case '~':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.CONTAINS, Literal: string(ch) + string(l.ch)}
		}

	case '^':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.NOT_EQ, Literal: string(ch) + string(l.ch)}
		} else {
			tok = newToken(token.BANG, l.ch)

		}
	case '"':
		tok.Type = token.STRING
		tok.Literal = l.readString()
	case '\'':
		tok.Type = token.STRING
		tok.Literal = l.readString()
	case '`':
		tok.Type = token.BACKTICK
		tok.Literal = l.readBacktick()
	case '[':
		tok = newToken(token.LBRACKET, l.ch)
	case ']':
		tok = newToken(token.RBRACKET, l.ch)
	case ':':
		tok = newToken(token.COLON, l.ch)
	case rune(0):
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isDigit(l.ch) {
			tok := l.readDecimal()
			l.prevToken = tok
			return tok
		}

		tok.Literal = l.readIdentifier()
		tok.Type = token.LookupIdentifier(tok.Literal)
		// SAS has two ways of doing one thing e.g. ge and >=. Nice one SAS

		switch strings.ToLower(tok.Literal) {
		case "ge":
			tok = token.Token{Type: token.GT_EQUALS, Literal: ">="}
		case "gt":
			tok = token.Token{Type: token.GT, Literal: ">"}
		case "lt":
			tok = token.Token{Type: token.LT, Literal: "<"}
		case "se":
			tok = token.Token{Type: token.LT_EQUALS, Literal: "<="}
		case "ne":
			tok = token.Token{Type: token.NOT_EQ, Literal: "~="}
		case "eq":
			tok = token.Token{Type: token.EQ, Literal: "=="}
		}

		l.prevToken = tok

		return tok
	}
	l.readChar()
	l.prevToken = tok
	return tok
}

// return new token
func newToken(tokenType token.Type, ch rune) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

func (l *Lexer) readIdentifier() string {

	id := ""

	for isIdentifier(l.ch) {
		id += string(l.ch)
		l.readChar()
	}

	return id
}

// skip white space
func (l *Lexer) skipWhitespace() {
	for isWhitespace(l.ch) {
		l.readChar()
	}
}

// skip comment (until the end of the line).
func (l *Lexer) skipComment() {
	for l.ch != '\n' && l.ch != rune(0) {
		l.readChar()
	}
	l.skipWhitespace()
}

// Consume all tokens until we've had the close of a multi-line
// comment.
func (l *Lexer) skipMultiLineComment() {
	found := false

	for !found {
		if l.ch == rune(0) {
			found = true
		}

		if l.ch == '*' && l.peekChar() == '/' {
			found = true
			l.readChar()
		}

		l.readChar()
	}
	l.skipWhitespace()
}

// read number - this handles 0x1234 and 0b101010101 too.
func (l *Lexer) readNumber() string {
	str := ""

	// We usually just accept digits.
	accept := "0123456789"

	// But if we have `0x` as a prefix we accept hexadecimal instead.
	if l.ch == '0' && l.peekChar() == 'x' {
		accept = "0x123456789abcdefABCDEF"
	}

	// If we have `0b` as a prefix we accept binary digits only.
	if l.ch == '0' && l.peekChar() == 'b' {
		accept = "b01"
	}

	for strings.Contains(accept, string(l.ch)) {
		str += string(l.ch)
		l.readChar()
	}
	return str
}

// read until white space
func (l *Lexer) readUntilWhitespace() string {
	position := l.position
	for !isWhitespace(l.ch) {
		l.readChar()
	}
	return string(l.characters[position:l.position])
}

// read decimal
func (l *Lexer) readDecimal() token.Token {

	integer := l.readNumber()
	if l.ch == rune('.') && isDigit(l.peekChar()) {
		l.readChar()
		fraction := l.readNumber()
		return token.Token{Type: token.FLOAT, Literal: integer + "." + fraction}
	}
	return token.Token{Type: token.INT, Literal: integer}
}

// read string
func (l *Lexer) readString() string {
	out := ""

	for {
		l.readChar()
		if l.ch == '"' || l.ch == '\'' || l.ch == 0 {
			break
		}
		if l.ch == '\\' {
			l.readChar()

			if l.ch == rune('n') {
				l.ch = '\n'
			}
			if l.ch == rune('r') {
				l.ch = '\r'
			}
			if l.ch == rune('t') {
				l.ch = '\t'
			}
			if l.ch == rune('"') {
				l.ch = '"'
			}
			if l.ch == rune('\\') {
				l.ch = '\\'
			}
		}
		out = out + string(l.ch)
	}

	return out
}

func (l *Lexer) readRegexp() (string, error) {
	out := ""

	for {
		l.readChar()

		if l.ch == rune(0) {
			return "unterminated regular expression", fmt.Errorf("unterminated regular expression")
		}
		if l.ch == '/' {

			l.readChar()
			flags := ""
			for l.ch == rune('i') || l.ch == rune('m') {

				if !strings.Contains(flags, string(l.ch)) {
					tmp := strings.Split(flags, "")
					tmp = append(tmp, string(l.ch))
					flags = strings.Join(tmp, "")
				}
				l.readChar()
			}
			if len(flags) > 0 {
				out = "(?" + flags + ")" + out
			}
			break
		}
		out = out + string(l.ch)
	}

	return out, nil
}

func (l *Lexer) readBacktick() string {
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == '`' {
			break
		}
	}
	out := string(l.characters[position:l.position])
	return out
}

func (l *Lexer) peekChar() rune {
	if l.readPosition >= len(l.characters) {
		return rune(0)
	}
	return l.characters[l.readPosition]
}

func isIdentifier(ch rune) bool {

	if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_' {
		return true
	}

	return false
}

func isWhitespace(ch rune) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '.' || ch == '$'
}

func isDigit(ch rune) bool {
	return rune('0') <= ch && ch <= rune('9')
}
