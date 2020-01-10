package ast

import "convert/token"

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

type Identifier struct {
	Token token.Token // the token.IDENT token Value string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }

type AssignmentStatement struct {
	Token token.Token // the token.LET token Name *Identifier
	Value Expression
}

func (ls *AssignmentStatement) statementNode()       {}
func (ls *AssignmentStatement) TokenLiteral() string { return ls.Token.Literal }
