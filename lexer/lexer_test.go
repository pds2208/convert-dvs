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
		{token.Ident, "found"},
		{token.Assign, "="},
		{token.Integer, "0"},
		{token.Semicolon, ";"},

		{token.ArrayKeyword, "array"},
		{token.Ident, "uniq"},
		{token.LeftBrace, "{"},
		{token.Integer, "31"},
		{token.RightBrace, "}"},
		{token.Ident, "uniqual01"},
		{token.Minus, "-"},
		{token.Ident, "uniqual31"},
		{token.Semicolon, ";"},

		{token.IfKeyword, "if"},
		{token.Ident, "IOUTCOME"},
		{token.NotEqual, "^="},
		{token.Integer, "3"},
		{token.AndKeyword, "and"},
		{token.Ident, "thiswv"},
		{token.Assign, "="},
		{token.Integer, "1"},
		{token.ThenKeyword, "then"},
		{token.DoKeyword, "do"},
		{token.Semicolon, ";"},

		{token.IfKeyword, "if"},
		{token.Ident, "worse"},
		{token.Assign, "="},
		{token.Integer, "1"},
		{token.AndKeyword, "and"},
		{token.Ident, "whpnumbr"},
		{token.InKeyword, "in"},
		{token.LeftParen, "("},
		{token.StringKeyword, "1"},
		{token.Comma, ","},
		{token.StringKeyword, "2"},
		{token.RightParen, ")"},
		{token.ThenKeyword, "then"},
		{token.DoKeyword, "do"},
		{token.Semicolon, ";"},

		{token.IfKeyword, "if"},
		{token.Ident, "eaffect"},
		{token.InKeyword, "in"},
		{token.LeftParen, "("},
		{token.Integer, "1"},
		{token.Comma, ","},
		{token.Integer, "4"},
		{token.Comma, ","},
		{token.Integer, "-8"},
		{token.RightParen, ")"},
		{token.ThenKeyword, "then"},
		{token.Ident, "WHPTYPEP"},
		{token.Assign, "="},
		{token.StringKeyword, "00"},
		{token.Semicolon, ";"},

		{token.ElseKeyword, "else"},
		{token.IfKeyword, "if"},
		{token.Ident, "eaffect"},
		{token.Assign, "="},
		{token.Integer, "2"},
		{token.ThenKeyword, "then"},
		{token.Ident, "WHPTYPEP"},
		{token.Assign, "="},
		{token.StringKeyword, "01"},
		{token.Semicolon, ";"},

		{token.ElseKeyword, "else"},
		{token.Ident, "WHPTYPEP"},
		{token.Assign, "="},
		{token.StringKeyword, "99"},
		{token.Semicolon, ";"},

		{token.EndKeyword, "end"},
		{token.Semicolon, ";"},

		{token.ElseKeyword, "else"},
		{token.Ident, "WHPTYPEP"},
		{token.Assign, "="},
		{token.StringKeyword, "99"},
		{token.Semicolon, ";"},

		{token.EndKeyword, "end"},
		{token.Semicolon, ";"},

		{token.ElseKeyword, "else"},
		{token.Ident, "WHPTYPEP"},
		{token.Assign, "="},
		{token.StringKeyword, "99"},
		{token.Semicolon, ";"},

		{token.Eof, ""},
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
