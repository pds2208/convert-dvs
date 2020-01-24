package parser

import (
	"fmt"
	"strconv"
	"strings"

	"convert/ast"
	"convert/lexer"
	"convert/token"
)

// prefix Parse function
// infix parse function
// postfix parse function
type (
	prefixParseFn  func() ast.Expression
	infixParseFn   func(ast.Expression) ast.Expression
	postfixParseFn func() ast.Expression
)

// precedence order
const (
	_ int = iota
	LOWEST
	COND // OR or AND

	ASSIGN       // =
	TERNARY      // ? :
	EQUALS       // == or !=
	REGEXP_MATCH // !~ ~=
	LESSGREATER  // > or <
	NOT
	IN      // in(x,y,z)
	SUM     // + or -
	PRODUCT // * or /
	POWER   // **
	MOD     // %
	PREFIX  // -X or !X
	CALL    // myFunction(X)
	INDEX   // array[index], map[key]
)

// each token precedence
var precedences = map[token.Type]int{
	token.QUESTION:     TERNARY,
	token.ASSIGN:       ASSIGN,
	token.NOT:          NOT,
	token.EQ:           EQUALS,
	token.NOT_EQ:       EQUALS,
	token.LT:           LESSGREATER,
	token.LT_EQUALS:    LESSGREATER,
	token.GT:           LESSGREATER,
	token.GT_EQUALS:    LESSGREATER,
	token.IN:           IN,
	token.CONTAINS:     REGEXP_MATCH,
	token.NOT_CONTAINS: REGEXP_MATCH,

	token.PLUS:            SUM,
	token.PLUS_EQUALS:     SUM,
	token.MINUS:           SUM,
	token.MINUS_EQUALS:    SUM,
	token.SLASH:           PRODUCT,
	token.SLASH_EQUALS:    PRODUCT,
	token.ASTERISK:        PRODUCT,
	token.ASTERISK_EQUALS: PRODUCT,
	token.POW:             POWER,
	token.MOD:             MOD,
	token.AND:             COND,
	token.OR:              COND,
	token.LPAREN:          CALL,
	token.PERIOD:          CALL,
	token.LBRACKET:        INDEX,
}

// Parser object
type Parser struct {
	l *lexer.Lexer

	prevToken       token.Token
	curToken        token.Token
	peekToken       token.Token
	errors          []string
	prefixParseFns  map[token.Type]prefixParseFn
	infixParseFns   map[token.Type]infixParseFn
	postfixParseFns map[token.Type]postfixParseFn
	tern            bool
}

// New returns our new parser-object.
func New(l *lexer.Lexer) *Parser {

	// Create the parser, and prime the pump
	p := &Parser{l: l, errors: []string{}}
	p.nextToken()
	p.nextToken()

	// Register prefix-functions
	p.prefixParseFns = make(map[token.Type]prefixParseFn)
	p.registerPrefix(token.BACKTICK, p.parseBacktickLiteral)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.DEFINE_FUNCTION, p.parseFunctionDefinition)
	p.registerPrefix(token.EOF, p.parsingBroken)
	p.registerPrefix(token.FALSE, p.parseBoolean)
	p.registerPrefix(token.FLOAT, p.parseFloatLiteral)
	p.registerPrefix(token.FOR, p.parseForLoopExpression)
	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.ILLEGAL, p.parsingBroken)
	p.registerPrefix(token.IF, p.parseIfExpression)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.LBRACE, p.parseHashLiteral)
	//p.registerPrefix(token.LBRACKET, p.parseArrayLiteral)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.REGEXP, p.parseRegexpLiteral)
	p.registerPrefix(token.REGEXP, p.parseRegexpLiteral)
	p.registerPrefix(token.STRING, p.parseStringLiteral)
	p.registerPrefix(token.TRUE, p.parseBoolean)
	p.registerPrefix(token.ARRAY, p.parseArrayLiteral)
	p.registerPrefix(token.DO, p.parseDo)
	p.registerPrefix(token.LENGTH, p.parseLengthLiteral)
	p.registerPrefix(token.PUT, p.parsePutExpression)

	// Register infix functions
	p.infixParseFns = make(map[token.Type]infixParseFn)
	p.registerInfix(token.AND, p.parseInfixExpression)
	p.registerInfix(token.ASSIGN, p.parseAssignExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK_EQUALS, p.parseAssignExpression)
	p.registerInfix(token.CONTAINS, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)
	p.registerInfix(token.GT_EQUALS, p.parseInfixExpression)
	p.registerInfix(token.LBRACKET, p.parseIndexExpression)
	p.registerInfix(token.LPAREN, p.parseCallExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.LT_EQUALS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS_EQUALS, p.parseAssignExpression)
	p.registerInfix(token.MOD, p.parseInfixExpression)
	p.registerInfix(token.NOT_CONTAINS, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT, p.parseNotLiteral)
	p.registerInfix(token.OR, p.parseInfixExpression)
	p.registerInfix(token.PERIOD, p.parseMethodCallExpression)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.PLUS_EQUALS, p.parseAssignExpression)
	p.registerInfix(token.POW, p.parseInfixExpression)
	p.registerInfix(token.QUESTION, p.parseTernaryExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.SLASH_EQUALS, p.parseAssignExpression)
	p.registerInfix(token.IN, p.parseInExpression)

	// Register postfix functions.
	p.postfixParseFns = make(map[token.Type]postfixParseFn)
	p.registerPostfix(token.MINUS_MINUS, p.parsePostfixExpression)
	p.registerPostfix(token.PLUS_PLUS, p.parsePostfixExpression)

	// All done
	return p
}

// registerPrefix registers a function for handling a prefix-based statement
func (p *Parser) registerPrefix(tokenType token.Type, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// registerInfix registers a function for handling a infix-based statement
func (p *Parser) registerInfix(tokenType token.Type, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// registerPostfix registers a function for handling a postfix-based statement
func (p *Parser) registerPostfix(tokenType token.Type, fn postfixParseFn) {
	p.postfixParseFns[tokenType] = fn
}

// Errors return stored errors
func (p *Parser) Errors() []string {
	return p.errors
}

// peekError raises an error if the next token is not the expected type.
func (p *Parser) peekError(t token.Type) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead around line %d", t, p.curToken.Type, p.l.GetLine())
	p.errors = append(p.errors, msg)
}

// nextToken moves to our next token from the lexer.
func (p *Parser) nextToken() {
	p.prevToken = p.curToken
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// ParseProgram used to parse the whole program
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}
	for p.curToken.Type != token.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}
	return program
}

// parseStatement parses a single statement.
func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	//case token.LET:
	case token.IDENT:
		return p.parseLetStatement()
	case token.ARRAY:
		return p.parseArrayStatement()
	case token.CONST:
		return p.parseConstStatement()
	default:
		return p.parseExpressionStatement()
	}
}

// parseLetStatement parses a let-statement.
func (p *Parser) parseLetStatement() *ast.LetStatement {
	t := token.Token{
		Type:    token.LET,
		Literal: "let",
	}
	stmt := &ast.LetStatement{Token: t}

	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if p.peekTokenIs(token.LBRACE) { // index
		stmt.Value = p.parseExpression(LOWEST)
		stmt.Indent = true
		p.nextToken()
		return stmt
	}

	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	p.nextToken()

	stmt.Value = p.parseExpression(LOWEST)

	p.nextToken()

	return stmt
}

// parseLetStatement parses a let-statement.
func (p *Parser) parseArrayStatement() *ast.ArrayStatement {

	stmt := &ast.ArrayStatement{Token: p.curToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	p.nextToken() // eat the {
	stmt.Value = append(stmt.Value, p.parseIntegerLiteral())

	for p.peekTokenIs(token.COMMA) { // multi dimensional array
		if p.peekTokenIs(token.COMMA) {
			p.nextToken()
			p.nextToken()
			stmt.Value = append(stmt.Value, p.parseIntegerLiteral())
		}
	}

	if !p.expectPeek(token.RBRACE) {
		return nil
	}

	for !p.curTokenIs(token.SEMICOLON) {
		if p.curTokenIs(token.EOF) {
			p.errors = append(p.errors, "unterminated array statement")
			return nil
		}

		p.nextToken()
	}

	return stmt
}

// parseConstStatement parses a constant declaration.
func (p *Parser) parseConstStatement() *ast.ConstStatement {
	stmt := &ast.ConstStatement{Token: p.curToken}
	if !p.expectPeek(token.IDENT) {
		return nil
	}
	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectPeek(token.ASSIGN) {
		return nil
	}
	p.nextToken()
	stmt.Value = p.parseExpression(LOWEST)
	for !p.curTokenIs(token.SEMICOLON) {

		if p.curTokenIs(token.EOF) {
			p.errors = append(p.errors, "unterminated const statement")
			return nil
		}

		p.nextToken()
	}
	return stmt
}

// parseReturnStatement parses a return-statement.
func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}
	p.nextToken()
	stmt.ReturnValue = p.parseExpression(LOWEST)
	for !p.curTokenIs(token.SEMICOLON) {

		if p.curTokenIs(token.EOF) {
			p.errors = append(p.errors, "unterminated return statement")
			return nil
		}

		p.nextToken()
	}
	return stmt
}

// no prefix parse function error
func (p *Parser) noPrefixParseFnError(t token.Type) {
	msg := fmt.Sprintf("no prefix parse function for %s found around line %d", t, p.l.GetLine())
	p.errors = append(p.errors, msg)
}

// parse Expression Statement
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Expression = p.parseExpression(LOWEST)

	for p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	postfix := p.postfixParseFns[p.curToken.Type]
	if postfix != nil {
		return postfix()
	}
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()
	if leftExp == nil {
		return nil
	}
	if p.curToken.Type == token.SEMICOLON {

		return leftExp
	}

	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		// check for = and replace with ==
		if p.peekToken.Type == token.ASSIGN {
			p.peekToken = token.Token{Type: token.EQ, Literal: "="}
		}
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}
		p.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}

// parsingBroken is hit if we see an EOF in our input-stream
// this means we're screwed
func (p *Parser) parsingBroken() ast.Expression {
	return nil
}

// parseIdentifier parses an identifier.
func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

// parseIntegerLiteral parses an integer literal.
func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	var value int64
	var err error

	if strings.HasPrefix(p.curToken.Literal, "0b") {
		value, err = strconv.ParseInt(p.curToken.Literal[2:], 2, 64)
	} else if strings.HasPrefix(p.curToken.Literal, "0x") {
		value, err = strconv.ParseInt(p.curToken.Literal[2:], 16, 64)
	} else {
		value, err = strconv.ParseInt(p.curToken.Literal, 10, 64)
	}

	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer around line %d", p.curToken.Literal, p.l.GetLine())
		p.errors = append(p.errors, msg)
		return nil
	}
	lit.Value = value
	return lit
}

// parseFloatLiteral parses a float-literal
func (p *Parser) parseFloatLiteral() ast.Expression {
	flo := &ast.FloatLiteral{Token: p.curToken}
	value, err := strconv.ParseFloat(p.curToken.Literal, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as float around line %d", p.curToken.Literal, p.l.GetLine())
		p.errors = append(p.errors, msg)
		return nil
	}
	flo.Value = value
	return flo
}

// parseBoolean parses a boolean token.
func (p *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

// parsePrefixExpression parses a prefix-based expression.
func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	p.nextToken()
	expression.Right = p.parseExpression(PREFIX)
	return expression
}

// parsePostfixExpression parses a postfix-based expression.
func (p *Parser) parsePostfixExpression() ast.Expression {
	expression := &ast.PostfixExpression{
		Token:    p.prevToken,
		Operator: p.curToken.Literal,
	}
	return expression
}

// parseInfixExpression parses an infix-based expression.
func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}

// parseInfixExpression parses an infix-based expression.
func (p *Parser) parseNotLiteral(left ast.Expression) ast.Expression {
	expression := &ast.NotLiteral{
		Token: p.curToken,
		Name:  left,
	}

	return expression
}

// parseTernaryExpression parses a ternary expression
func (p *Parser) parseTernaryExpression(condition ast.Expression) ast.Expression {

	if p.tern {
		msg := fmt.Sprintf("nested ternary expressions are illegal, around line %d", p.l.GetLine())
		p.errors = append(p.errors, msg)
		return nil
	}

	p.tern = true
	defer func() { p.tern = false }()

	expression := &ast.TernaryExpression{
		Token:     p.curToken,
		Condition: condition,
	}
	p.nextToken() //skip the '?'
	precedence := p.curPrecedence()
	expression.IfTrue = p.parseExpression(precedence)

	if !p.expectPeek(token.COLON) { //skip the ":"
		return nil
	}

	// Get to next token, then parse the else part
	p.nextToken()
	expression.IfFalse = p.parseExpression(precedence)

	p.tern = false
	return expression
}

// parseGroupedExpression parses a grouped-expression.
func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	return exp
}

// parseIfCondition parses an if-expression.
func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.curToken}

	p.nextToken()

	expression.Condition = p.parseExpression(LOWEST)
	if !p.expectPeek(token.THEN) {
		if !p.curTokenIs(token.THEN) {
			return nil
		}
	}

	if p.peekTokenIs(token.DO) {
		p.nextToken() // eat DO
		p.nextToken()
		// check for then
		expression.Consequence = p.parseBlockStatement()
		if !p.curTokenIs(token.END) {
			return nil
		}
		p.nextToken() // eat ;

	} else {
		if !p.curTokenIs(token.THEN) {
			return nil
		}
		p.nextToken()
		expression.Consequence = p.parseSingleBlockStatement()
	}

	if p.peekTokenIs(token.ELSE) {
		p.nextToken()
		if p.peekTokenIs(token.DO) {
			p.nextToken()
			expression.Alternative = p.parseBlockStatement()
		} else {
			p.nextToken()
			expression.Alternative = p.parseSingleBlockStatement()
		}
	}
	return expression
}

// parseForLoopExpression parses a for-loop.
func (p *Parser) parseForLoopExpression() ast.Expression {
	expression := &ast.ForLoopExpression{Token: p.curToken}
	if !p.expectPeek(token.LPAREN) {
		return nil
	}
	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	expression.Consequence = p.parseBlockStatement()
	return expression
}

// parseBlockStatement parsea a block.
func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}
	block.Statements = []ast.Statement{}
	p.nextToken()
	for !p.curTokenIs(token.END) {

		// Don't loop forever
		if p.curTokenIs(token.EOF) {
			p.errors = append(p.errors, "unterminated block statement")
			return nil
		}

		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}
	return block
}

func (p *Parser) parseSingleBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}
	block.Statements = []ast.Statement{}
	// check for = and replace with ==
	if p.peekToken.Type == token.ASSIGN {
		p.peekToken = token.Token{Type: token.EQ, Literal: "=="}
	}
	stmt := p.parseExpressionStatement()
	if stmt != nil {
		block.Statements = append(block.Statements, stmt)
	}
	return block
}

// parseFunctionLiteral parses a function-literal.
func (p *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: p.curToken}
	if !p.expectPeek(token.LPAREN) {
		return nil
	}
	lit.Defaults, lit.Parameters = p.parseFunctionParameters()
	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	lit.Body = p.parseBlockStatement()
	return lit
}

// parseFunctionDefinition parses the definition of a function.
func (p *Parser) parseFunctionDefinition() ast.Expression {
	p.nextToken()
	lit := &ast.FunctionDefineLiteral{Token: p.curToken}
	if !p.expectPeek(token.LPAREN) {
		return nil
	}
	lit.Defaults, lit.Parameters = p.parseFunctionParameters()
	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	lit.Body = p.parseBlockStatement()
	return lit
}

// parseFunctionParameters parses the parameters used for a function.
func (p *Parser) parseFunctionParameters() (map[string]ast.Expression, []*ast.Identifier) {

	// Any default parameters.
	m := make(map[string]ast.Expression)

	// The argument-definitions.
	identifiers := make([]*ast.Identifier, 0)

	// Is the next parameter ")" ?  If so we're done. No args.
	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return m, identifiers
	}
	p.nextToken()

	// Keep going until we find a ")"
	for !p.curTokenIs(token.RPAREN) {

		if p.curTokenIs(token.EOF) {
			p.errors = append(p.errors, "unterminated function parameters")
			return nil, nil
		}

		// Get the identifier.
		ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
		identifiers = append(identifiers, ident)
		p.nextToken()

		// If there is "=xx" after the name then that's
		// the default parameter.
		if p.curTokenIs(token.ASSIGN) {
			p.nextToken()
			// Save the default value.
			m[ident.Value] = p.parseExpressionStatement().Expression
			p.nextToken()
		}

		// Skip any comma.
		if p.curTokenIs(token.COMMA) {
			p.nextToken()
		}
	}

	return m, identifiers
}

// parseStringLiteral parses a string-literal.
func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

// parseRegexpLiteral parses a regular-expression.
func (p *Parser) parseRegexpLiteral() ast.Expression {

	flags := ""

	val := p.curToken.Literal
	if strings.HasPrefix(val, "(?") {
		val = strings.TrimPrefix(val, "(?")

		i := 0
		for i < len(val) {

			if val[i] == ')' {

				val = val[i+1:]
				break
			} else {
				flags += string(val[i])
			}

			i++
		}
	}
	return &ast.RegexpLiteral{Token: p.curToken, Value: val, Flags: flags}
}

// parseBacktickLiteral parses a backtick-expression.
func (p *Parser) parseBacktickLiteral() ast.Expression {
	return &ast.BacktickLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseLengthLiteral() ast.Expression {
	length := &ast.LengthLiteral{Token: p.curToken}
	if !p.expectPeek(token.IDENT) {
		return nil
	}
	length.Name = p.parseIdentifier()
	if !p.expectPeek(token.DOLLAR) {
		return nil
	}
	p.nextToken()
	length.Value = p.parseIntegerLiteral()
	return length
}

// parseArrayLiteral parses an array literal.
func (p *Parser) parseArrayLiteral() ast.Expression {
	array := &ast.ArrayLiteral{Token: p.curToken}
	array.Elements = p.parseExpressionList(token.SEMICOLON)
	return array
}

// parse array elements literal
func (p *Parser) parseExpressionList(end token.Type) []ast.Expression {
	var list []ast.Expression

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	p.nextToken()

	if !p.expectPeek(token.INT) {
		return nil
	}

	list = append(list, p.parseIntegerLiteral())

	p.nextToken()
	if !p.expectPeek(token.IDENT) {
		return nil
	}

	a := p.parseExpression(LOWEST)
	list = append(list, a)

	if !p.expectPeek(end) {
		return nil
	}

	return list
}

// parseArrayLiteral parses an array literal.
func (p *Parser) parseDo() ast.Expression {
	do := &ast.DoLiteral{Token: p.curToken}
	p.nextToken()

	v := p.parseIdentifier()
	if n, ok := v.(*ast.Identifier); ok {
		do.Variable = n
	} else {
		msg := fmt.Sprintf("expected do expresion to be IDENT, got %s instead around line %d",
			v.TokenLiteral(), p.l.GetLine())
		p.errors = append(p.errors, msg)
	}

	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	p.nextToken()

	if p.curTokenIs(token.IDENT) {
		do.From = p.parseIdentifier()
	} else {
		f := p.parseIntegerLiteral()
		if f == nil {
			return nil
		}
		if n, ok := f.(*ast.IntegerLiteral); ok {
			do.From = n
		} else {
			msg := fmt.Sprintf("expected in expresion to be IntegerLiteral, got %s instead around line %d",
				f.String(), p.l.GetLine())
			p.errors = append(p.errors, msg)
		}
	}

	if !p.expectPeek(token.TO) {
		return nil
	}

	p.nextToken()

	if p.curTokenIs(token.IDENT) {
		do.To = p.parseIdentifier()
	} else {
		t := p.parseIntegerLiteral()
		if n, ok := t.(*ast.IntegerLiteral); ok {
			do.To = n
		} else {
			msg := fmt.Sprintf("expected in expresion to be IntegerLiteral, got %s instead around line %d",
				t.TokenLiteral(), p.l.GetLine())
			p.errors = append(p.errors, msg)
		}
	}

	p.nextToken()
	//p.nextToken()
	do.Statements = p.parseBlockStatement()
	//p.nextToken()
	if !p.curTokenIs(token.END) {
		return nil
	}
	p.nextToken() // eat ;

	return do
}

// parse in values elements literal
func (p *Parser) parseInList(end token.Type) []ast.Expression {
	var list []ast.Expression

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()

	for p.curToken.Type != end {

		if p.curTokenIs(token.EOF) {
			p.errors = append(p.errors, "unterminated in statement")
			return nil
		}

		switch p.curToken.Type {
		case token.INT:
			list = append(list, p.parseIntegerLiteral())
		case token.STRING:
			list = append(list, p.parseStringLiteral())
		case token.FLOAT:
			list = append(list, p.parseFloatLiteral())
		case token.MINUS:
			if !p.peekTokenIs(token.INT) && !p.peekTokenIs(token.FLOAT) {
				return nil
			}
			p.nextToken()
			continue
		default:
			return nil
		}

		p.nextToken()

		if !p.curTokenIs(token.COMMA) && !p.curTokenIs(end) {
			return nil
		}

		if p.curTokenIs(token.COMMA) {
			p.nextToken()
		}

	}

	if !p.curTokenIs(token.RPAREN) {
		return nil
	}

	return list
}

func (p *Parser) parsePutExpression() ast.Expression {
	exp := &ast.PutExpression{Token: p.curToken}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}
	if !p.expectPeek(token.IDENT) {
		return nil
	}

	t := p.parseIdentifier()
	if n, ok := t.(*ast.Identifier); ok {
		exp.Name = n
	} else {
		msg := fmt.Sprintf("expected put expresion to contain (Identifier, Integer), got %s instead around line %d",
			t.TokenLiteral(), p.l.GetLine())
		p.errors = append(p.errors, msg)
	}
	p.nextToken()
	if !p.curTokenIs(token.COMMA) {
		return nil
	}
	p.nextToken()
	t = p.parseIntegerLiteral()
	if n, ok := t.(*ast.IntegerLiteral); ok {
		exp.Value = n
	} else {
		msg := fmt.Sprintf("expected put expresion to contain  (Identifier, Integer), got %s instead around line %d",
			t.TokenLiteral(), p.l.GetLine())
		p.errors = append(p.errors, msg)
	}
	p.nextToken()
	if p.curTokenIs(token.PERIOD) {
		p.nextToken()
	}
	if !p.curTokenIs(token.RPAREN) {
		return nil
	}
	return exp
}

// parseInfixExpression parses an in expression.
func (p *Parser) parseInExpression(name ast.Expression) ast.Expression {
	exp := &ast.InExpression{Token: p.curToken}

	switch name.(type) {

	case *ast.NotLiteral:
		n := name.(*ast.NotLiteral)
		exp.Not = true
		switch n.Name.(type) {
		case *ast.IndexExpression:
			t := n.Name.(*ast.IndexExpression)
			exp.Name = t.Left.(*ast.Identifier)

		case *ast.Identifier:
			exp.Name = n.Name.(*ast.Identifier)
		}

	case *ast.Identifier:
		exp.Name = name.(*ast.Identifier)

	case *ast.IndexExpression:
		n := name.(*ast.IndexExpression)
		exp.Name = n.Left.(*ast.Identifier)

	default:
		msg := fmt.Sprintf("expected in expresion to be IDENT, got %s instead around line %d",
			name.TokenLiteral(), p.l.GetLine())
		p.errors = append(p.errors, msg)

	}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	exp.Values = p.parseInList(token.RPAREN)

	return exp
}

// parseInfixExpression parses an array index expression.
func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}
	p.nextToken()
	exp.Index = append(exp.Index, p.parseExpression(LOWEST))
	for p.peekTokenIs(token.COMMA) { // multi dimensional array
		if p.peekTokenIs(token.COMMA) {
			p.nextToken()
			p.nextToken()
			exp.Index = append(exp.Index, p.parseExpression(LOWEST))
		}
	}
	if !p.expectPeek(token.RBRACKET) {
		return nil
	}
	return exp
}

func (p *Parser) parseAssignExpression(name ast.Expression) ast.Expression {
	stmt := &ast.AssignStatement{Token: p.curToken}
	if n, ok := name.(*ast.Identifier); ok {
		stmt.Name = n
	} else {
		msg := fmt.Sprintf("expected assign token to be IDENT, got %s instead around line %d", name.TokenLiteral(), p.l.GetLine())
		p.errors = append(p.errors, msg)
	}

	oper := p.curToken
	p.nextToken()

	switch oper.Type {
	case token.PLUS_EQUALS:
		stmt.Operator = "+="
	case token.MINUS_EQUALS:
		stmt.Operator = "-="
	case token.SLASH_EQUALS:
		stmt.Operator = "/="
	case token.ASTERISK_EQUALS:
		stmt.Operator = "*="
	default:
		stmt.Operator = "="
	}
	stmt.Value = p.parseExpression(LOWEST)
	return stmt
}

// parseCallExpression parses a function-call expression.
func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.curToken, Function: function}
	exp.Arguments = p.parseExpressionList(token.RPAREN)
	return exp
}

// parseHashLiteral parses a hash literal.
func (p *Parser) parseHashLiteral() ast.Expression {
	hash := &ast.HashLiteral{Token: p.curToken}
	hash.Pairs = make(map[ast.Expression]ast.Expression)
	for !p.peekTokenIs(token.RBRACE) {
		p.nextToken()
		key := p.parseExpression(LOWEST)
		if !p.expectPeek(token.COLON) {
			return nil
		}
		p.nextToken()
		value := p.parseExpression(LOWEST)
		hash.Pairs[key] = value
		if !p.peekTokenIs(token.RBRACE) && !p.expectPeek(token.COMMA) {
			return nil
		}
	}
	if !p.expectPeek(token.RBRACE) {
		return nil
	}
	return hash
}

// parseMethodCallExpression parses an object-based method-call.
func (p *Parser) parseMethodCallExpression(obj ast.Expression) ast.Expression {
	methodCall := &ast.ObjectCallExpression{Token: p.curToken, Object: obj}
	p.nextToken()
	name := p.parseIdentifier()
	p.nextToken()
	methodCall.Call = p.parseCallExpression(name)
	return methodCall
}

// curTokenIs tests if the current token has the given type.
func (p *Parser) curTokenIs(t token.Type) bool {
	return p.curToken.Type == t
}

// peekTokenIs tests if the next token has the given type.
func (p *Parser) peekTokenIs(t token.Type) bool {
	return p.peekToken.Type == t
}

// expectPeek validates the next token is of the given type,
// and advances if so.  If it is not an error is stored.
func (p *Parser) expectPeek(t token.Type) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	}

	p.peekError(t)
	return false
}

// peekPrecedence looks up the next token precedence.
func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

// curPrecedence looks up the current token precedence.
func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}
