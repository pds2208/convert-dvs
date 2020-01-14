package lexer

import (
	"convert/token"
	"testing"
)

func TestNextToken(t *testing.T) {
	input := `
	
    found = 0;
	array uniq {31} uniqual01 - uniqual31;

 	if IOUTCOME ^=3 and thiswv=1  then do;
		 if worse=1 and whpnumbr in ("1","2") then do;
			  if eaffect in(1,4,-8) then WHPTYPEP="00";
			  else if eaffect=2 then WHPTYPEP ="01";
			  else WHPTYPEP ="99";
		  end;
  		else WHPTYPEP ="99";
	end;
	else WHPTYPEP ="99";

`

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
	}{
		{token.IDENT, "found"},
		{token.ASSIGN, "="},
		{token.INT, "0"},
		{token.SEMICOLON, ";"},

		{token.ARRAY, "array"},
		{token.IDENT, "uniq"},
		{token.LBRACE, "{"},
		{token.INT, "31"},
		{token.RBRACE, "}"},
		{token.IDENT, "uniqual01"},
		{token.MINUS, "-"},
		{token.IDENT, "uniqual31"},
		{token.SEMICOLON, ";"},

		{token.IF, "if"},
		{token.IDENT, "IOUTCOME"},
		{token.NOT_EQ, "^="},
		{token.INT, "3"},
		{token.AND, "and"},
		{token.IDENT, "thiswv"},
		{token.ASSIGN, "="},
		{token.INT, "1"},
		{token.THEN, "then"},
		{token.DO, "do"},
		{token.SEMICOLON, ";"},

		{token.IF, "if"},
		{token.IDENT, "worse"},
		{token.ASSIGN, "="},
		{token.INT, "1"},
		{token.AND, "and"},
		{token.IDENT, "whpnumbr"},
		{token.IN, "in"},
		{token.LPAREN, "("},
		{token.STRING, "1"},
		{token.COMMA, ","},
		{token.STRING, "2"},
		{token.RPAREN, ")"},
		{token.THEN, "then"},
		{token.DO, "do"},
		{token.SEMICOLON, ";"},

		{token.IF, "if"},
		{token.IDENT, "eaffect"},
		{token.IN, "in"},
		{token.LPAREN, "("},
		{token.INT, "1"},
		{token.COMMA, ","},
		{token.INT, "4"},
		{token.COMMA, ","},
		{token.MINUS, "-"},
		{token.INT, "8"},
		{token.RPAREN, ")"},
		{token.THEN, "then"},
		{token.IDENT, "WHPTYPEP"},
		{token.ASSIGN, "="},
		{token.STRING, "00"},
		{token.SEMICOLON, ";"},

		{token.ELSE, "else"},
		{token.IF, "if"},
		{token.IDENT, "eaffect"},
		{token.ASSIGN, "="},
		{token.INT, "2"},
		{token.THEN, "then"},
		{token.IDENT, "WHPTYPEP"},
		{token.ASSIGN, "="},
		{token.STRING, "01"},
		{token.SEMICOLON, ";"},

		{token.ELSE, "else"},
		{token.IDENT, "WHPTYPEP"},
		{token.ASSIGN, "="},
		{token.STRING, "99"},
		{token.SEMICOLON, ";"},

		{token.END, "end"},
		{token.SEMICOLON, ";"},

		{token.ELSE, "else"},
		{token.IDENT, "WHPTYPEP"},
		{token.ASSIGN, "="},
		{token.STRING, "99"},
		{token.SEMICOLON, ";"},

		{token.END, "end"},
		{token.SEMICOLON, ";"},

		{token.ELSE, "else"},
		{token.IDENT, "WHPTYPEP"},
		{token.ASSIGN, "="},
		{token.STRING, "99"},
		{token.SEMICOLON, ";"},

		{token.EOF, ""},
	}

	l := New(input)

	for i, tt := range tests {
		tok := l.NextToken()
		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}
		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}
