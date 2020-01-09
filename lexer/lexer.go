package lexer

import "convert/token"

type Lexer struct {
	input        string
	position     int
	readPosition int
	ch           byte
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition += 1
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace()

	switch l.ch {

	case '=':
		tok = newToken(token.Assign, l.ch)

	case ';':
		tok = newToken(token.Semicolon, l.ch)

	case '(':
		tok = newToken(token.LeftParen, l.ch)

	case ')':
		tok = newToken(token.RightParen, l.ch)

	case ',':
		tok = newToken(token.Comma, l.ch)

	case '+':
		tok = newToken(token.Plus, l.ch)

	case '{':
		tok = newToken(token.LeftBrace, l.ch)

	case '}':
		tok = newToken(token.RightBrace, l.ch)

	case '.':
		tok = newToken(token.Stop, l.ch)

	case '-':
		if isDigit(l.peekChar()) {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.Integer, Literal: string(ch) + string(l.ch)}
		} else {
			tok = newToken(token.Minus, l.ch)
		}

	case '"':
		tok.Type = token.StringKeyword
		tok.Literal = l.readString()

	case '^':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = token.Token{Type: token.NotEqual, Literal: string(ch) + string(l.ch)}
		} else {
			tok = newToken(token.Not, l.ch)
		}

	case 0:
		tok.Literal = ""
		tok.Type = token.Eof

	default:
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			return tok
		} else if isDigit(l.ch) {
			tok.Type = token.Integer
			tok.Literal = l.readNumber()
			return tok
		} else {
			tok = newToken(token.Illegal, l.ch)
		}
	}

	l.readChar()
	return tok

}

func (l *Lexer) readString() string {
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 {
			break
		}
	}
	return l.input[position:l.position]
}

func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]

}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}
