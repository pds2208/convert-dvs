package ast

import (
	"bytes"
	"fmt"
	"strings"

	"convert/token"
)

type Node interface {
	TokenLiteral() string
	String() string
}

type Statement interface {
	Node

	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Indentation struct {
	Indent   bool
	DeIndent bool
}

type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

func (p *Program) String() string {
	var out bytes.Buffer
	for _, stmt := range p.Statements {
		out.WriteString(stmt.String())
	}
	return out.String()
}

type LetStatement struct {
	Token token.Token

	Name *Identifier

	Value Expression

	Indentation
}

func (ls *LetStatement) statementNode() {}

// TokenLiteral returns the literal token.
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

// String returns this object as a string.
func (ls *LetStatement) String() string {
	var out bytes.Buffer
	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.TokenLiteral())
	out.WriteString(" = ")
	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}
	out.WriteString(";")
	return out.String()
}

// LetStatement holds a let-statement
type ArrayStatement struct {
	// Token holds the token
	Token token.Token

	// Name is the name of the variable to which we're assigning
	Name *Identifier

	// Value is the thing we're storing in the variable.
	Value []Expression
}

func (ls *ArrayStatement) statementNode() {}

// TokenLiteral returns the literal token.
func (ls *ArrayStatement) TokenLiteral() string { return ls.Token.Literal }

// String returns this object as a string.
func (ls *ArrayStatement) String() string {
	var out bytes.Buffer
	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.TokenLiteral())
	out.WriteString(" = ")
	if ls.Value != nil {
		for k, v := range ls.Value {
			out.WriteString(v.String())
			if k != len(ls.Value) {
				out.WriteString(", ")
			}
		}
	}
	out.WriteString(";")
	return out.String()
}

// ConstStatement is the same as let-statement, but the value
// can't be changed later.
type ConstStatement struct {
	// Token is the token
	Token token.Token

	// Name is the name of the variable we're setting
	Name *Identifier

	// Value contains the value which is to be set
	Value Expression
}

func (ls *ConstStatement) statementNode() {}

// TokenLiteral returns the literal token.
func (ls *ConstStatement) TokenLiteral() string { return ls.Token.Literal }

// String returns this object as a string.
func (ls *ConstStatement) String() string {
	var out bytes.Buffer
	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.TokenLiteral())
	out.WriteString(" = ")
	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}
	out.WriteString(";")
	return out.String()
}

// Identifier holds a single identifier.
type Identifier struct {
	// Token is the literal token
	Token token.Token

	// Value is the name of the identifier
	Value string
}

func (i *Identifier) expressionNode() {}

// TokenLiteral returns the literal token.
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }

// String returns this object as a string.
func (i *Identifier) String() string {
	return i.Value
}

// ReturnStatement stores a return-statement
type ReturnStatement struct {
	// Token contains the literal token.
	Token token.Token

	// ReturnValue is the value whichis to be returned.
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode() {}

// TokenLiteral returns the literal token.
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }

// String returns this object as a string.
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer
	out.WriteString(rs.TokenLiteral() + " ")
	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.TokenLiteral())
	}
	out.WriteString(";")
	return out.String()
}

// ExpressionStatement is an expression
type ExpressionStatement struct {
	// Token is the literal token
	Token token.Token

	// Expression holds the expression
	Expression Expression
}

func (es *ExpressionStatement) statementNode() {}

// TokenLiteral returns the literal token.
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }

// String returns this object as a string.
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// IntegerLiteral holds an integer
type IntegerLiteral struct {
	// Token is the literal token
	Token token.Token

	// Value holds the integer.
	Value int64
}

func (il *IntegerLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }

// String returns this object as a string.
func (il *IntegerLiteral) String() string { return il.Token.Literal }

// FloatLiteral holds a floating-point number
type FloatLiteral struct {
	// Token is the literal token
	Token token.Token

	// Value holds the floating-point number.
	Value float64
}

func (fl *FloatLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (fl *FloatLiteral) TokenLiteral() string { return fl.Token.Literal }

// String returns this object as a string.
func (fl *FloatLiteral) String() string { return fl.Token.Literal }

// PrefixExpression holds a prefix-based expression
type PrefixExpression struct {
	// Token holds the token.  e.g. "!"
	Token token.Token

	// Operator holds the operator being invoked (e.g. "!" ).
	Operator string

	// Right holds the thing to be operated upon
	Right Expression
}

func (pe *PrefixExpression) expressionNode() {}

// TokenLiteral returns the literal token.
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }

// String returns this object as a string.
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")
	return out.String()
}

// InfixExpression stores an infix expression.
type InfixExpression struct {
	// Token holds the literal expression
	Token token.Token

	// Left holds the left-most argument
	Left Expression

	// Operator holds the operation to be carried out (e.g. "+", "-" )
	Operator string

	// Right holds the right-most argument
	Right Expression
}

func (ie *InfixExpression) expressionNode() {}

// TokenLiteral returns the literal token.
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }

// String returns this object as a string.
func (ie *InfixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")
	return out.String()
}

// PostfixExpression holds a postfix-based expression
type PostfixExpression struct {
	// Token holds the token we're operating upon
	Token token.Token
	// Operator holds the postfix token, e.g. ++
	Operator string
}

func (pe *PostfixExpression) expressionNode() {}

// TokenLiteral returns the literal token.
func (pe *PostfixExpression) TokenLiteral() string { return pe.Token.Literal }

// String returns this object as a string.
func (pe *PostfixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(pe.Token.Literal)
	out.WriteString(pe.Operator)
	out.WriteString(")")
	return out.String()
}

// Boolean holds a boolean type
type Boolean struct {
	// Token holds the actual token
	Token token.Token

	// Value stores the bools' value: true, or false.
	Value bool
}

func (b *Boolean) expressionNode() {}

// TokenLiteral returns the literal token.
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }

// String returns this object as a string.
func (b *Boolean) String() string { return b.Token.Literal }

// BlockStatement holds a group of statements, which are treated
// as a block.  (For example the body of an `if` expression.)
type BlockStatement struct {
	// Token holds the actual token
	Token token.Token

	// Statements contain the set of statements within the block
	Statements []Statement

	Indentation
}

func (bs *BlockStatement) statementNode() {}

// TokenLiteral returns the literal token.
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }

// String returns this object as a string.
func (bs *BlockStatement) String() string {
	var out bytes.Buffer
	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}

// IfExpression holds an if-statement
type IfExpression struct {
	// Token is the actual token
	Token token.Token

	// Condition is the thing that is evaluated to determine
	// which block should be executed.
	Condition Expression

	// Consequence is the set of statements executed if the
	// condition is true.
	Consequence *BlockStatement

	// Alternative is the set of statements executed if the
	// condition is not true (optional).
	Alternative *BlockStatement
}

func (ie *IfExpression) expressionNode() {}

// TokenLiteral returns the literal token.
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }

// String returns this object as a string.
func (ie *IfExpression) String() string {
	var out bytes.Buffer
	out.WriteString("if")
	out.WriteString(ie.Condition.String())
	out.WriteString(" ")
	out.WriteString(ie.Consequence.String())
	if ie.Alternative != nil {
		out.WriteString("else")
		out.WriteString(ie.Alternative.String())
	}
	return out.String()
}

// TernaryExpression holds a ternary-expression.
type TernaryExpression struct {
	// Token is the actual token.
	Token token.Token

	// Condition is the thing that is evaluated to determine
	// which expression should be returned
	Condition Expression

	// IfTrue is the expression to return if the condition is true.
	IfTrue Expression

	// IFFalse is the expression to return if the condition is not true.
	IfFalse Expression
}

func (te *TernaryExpression) expressionNode() {}

// TokenLiteral returns the literal token.
func (te *TernaryExpression) TokenLiteral() string { return te.Token.Literal }

// String returns this object as a string.
func (te *TernaryExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(te.Condition.String())
	out.WriteString(" ? ")
	out.WriteString(te.IfTrue.String())
	out.WriteString(" : ")
	out.WriteString(te.IfFalse.String())
	out.WriteString(")")

	return out.String()
}

// ForLoopExpression holds a for-loop
type ForLoopExpression struct {
	// Token is the actual token
	Token token.Token

	// Condition is the expression used to determine if the loop
	// is still running.
	Condition Expression

	// Consequence is the set of statements to be executed for the
	// loop body.
	Consequence *BlockStatement
}

func (fle *ForLoopExpression) expressionNode() {}

// TokenLiteral returns the literal token.
func (fle *ForLoopExpression) TokenLiteral() string { return fle.Token.Literal }

// String returns this object as a string.
func (fle *ForLoopExpression) String() string {
	var out bytes.Buffer
	out.WriteString("for (")
	out.WriteString(fle.Condition.String())
	out.WriteString(" ) {")
	out.WriteString(fle.Consequence.String())
	out.WriteString("}")
	return out.String()
}

// FunctionLiteral holds a function-definition
//
// See-also FunctionDefineLiteral.
type FunctionLiteral struct {
	// Token is the actual token
	Token token.Token

	// Parameters is the list of parameters the function receives.
	Parameters []*Identifier

	// Defaults holds any default values for arguments which aren't
	// specified
	Defaults map[string]Expression

	// Body contains the set of statements within the function.
	Body *BlockStatement
}

func (fl *FunctionLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (fl *FunctionLiteral) TokenLiteral() string { return fl.Token.Literal }

// String returns this object as a string.
func (fl *FunctionLiteral) String() string {
	var out bytes.Buffer
	params := make([]string, 0)
	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}
	out.WriteString(fl.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(fl.Body.String())
	return out.String()

}

type FormatExpression struct {
	Token token.Token
	From  *Identifier
	To    *Identifier
}

func (put *FormatExpression) expressionNode() {}

func (put *FormatExpression) TokenLiteral() string {
	return put.Token.Literal
}

func (put *FormatExpression) String() string {
	var out bytes.Buffer
	out.WriteString("format ")
	out.WriteString(put.From.String())
	out.WriteString(put.To.String())
	return out.String()
}

type InExpression struct {
	Token  token.Token
	Name   *Identifier
	Values []Expression
	Not    bool
}

func (in *InExpression) expressionNode() {}

func (in *InExpression) TokenLiteral() string {
	return in.Token.Literal
}

func (in *InExpression) String() string {
	var out bytes.Buffer
	params := make([]string, 0)
	for _, p := range in.Values {
		params = append(params, p.String())
	}
	if in.Not {
		out.WriteString("not ")
	}
	out.WriteString(in.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	return out.String()
}

// FunctionDefineLiteral holds a function-definition.
//
// See-also FunctionLiteral.
type FunctionDefineLiteral struct {
	// Token holds the token
	Token token.Token

	// Parameters holds the function parameters.
	Parameters []*Identifier

	// Defaults holds any default-arguments.
	Defaults map[string]Expression

	// Body holds the set of statements in the functions' body.
	Body *BlockStatement
}

func (fl *FunctionDefineLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (fl *FunctionDefineLiteral) TokenLiteral() string {
	return fl.Token.Literal
}

// String returns this object as a string.
func (fl *FunctionDefineLiteral) String() string {
	var out bytes.Buffer
	params := make([]string, 0)
	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}
	out.WriteString(fl.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(fl.Body.String())
	return out.String()

}

// CallExpression holds the invokation of a method-call.
type CallExpression struct {
	// Token stores the literal token
	Token token.Token

	// Function is the function to be invoked.
	Function Expression

	// Arguments are the arguments to be applied
	Arguments []Expression
}

func (ce *CallExpression) expressionNode() {}

// TokenLiteral returns the literal token.
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }

// String returns this object as a string.
func (ce *CallExpression) String() string {
	var out bytes.Buffer
	args := make([]string, 0)
	for _, a := range ce.Arguments {
		args = append(args, a.String())
	}
	out.WriteString(ce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")
	return out.String()
}

// ObjectCallExpression is used when calling a method on an object.
type ObjectCallExpression struct {
	// Token is the literal token
	Token token.Token

	// Object is the object against which the call is invoked.
	Object Expression

	// Call is the method-name.
	Call Expression
}

func (oce *ObjectCallExpression) expressionNode() {}

// TokenLiteral returns the literal token.
func (oce *ObjectCallExpression) TokenLiteral() string {
	return oce.Token.Literal
}

// String returns this object as a string.
func (oce *ObjectCallExpression) String() string {
	var out bytes.Buffer
	out.WriteString(oce.Object.String())
	out.WriteString(".")
	out.WriteString(oce.Call.String())

	return out.String()
}

type NotLiteral struct {
	Token token.Token
	Name  Expression
}

func (sl *NotLiteral) expressionNode() {}

func (sl *NotLiteral) TokenLiteral() string { return sl.Token.Literal }

func (sl *NotLiteral) String() string { return sl.Token.Literal }

// StringLiteral holds a string
type StringLiteral struct {
	// Token is the token
	Token token.Token

	// Value is the value of the string.
	Value string
}

func (sl *StringLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }

// String returns this object as a string.
func (sl *StringLiteral) String() string { return sl.Token.Literal }

// RegexpLiteral holds a regular-expression.
type RegexpLiteral struct {
	// Token is the token
	Token token.Token

	// Value is the value of the regular expression.
	Value string

	// Flags contains any flags associated with the regexp.
	Flags string
}

func (rl *RegexpLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (rl *RegexpLiteral) TokenLiteral() string { return rl.Token.Literal }

// String returns this object as a string.
func (rl *RegexpLiteral) String() string {

	return fmt.Sprintf("/%s/%s", rl.Value, rl.Flags)
}

// BacktickLiteral holds details of a command to be executed
type BacktickLiteral struct {
	// Token is the actual token
	Token token.Token

	// Value is the name of the command to execute.
	Value string
}

func (bl *BacktickLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (bl *BacktickLiteral) TokenLiteral() string { return bl.Token.Literal }

// String returns this object as a string.
func (bl *BacktickLiteral) String() string { return bl.Token.Literal }

// DoLiteral holds a do
type DoLiteral struct {
	// Token is the token
	Token token.Token

	Variable Expression
	From     Expression
	To       Expression

	Statements Statement
}

func (al *DoLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (al *DoLiteral) TokenLiteral() string { return al.Token.Literal }

// String returns this object as a string.
func (al *DoLiteral) String() string {
	var out bytes.Buffer
	out.WriteString("do ")
	out.WriteString(al.Variable.String() + " = ")
	out.WriteString(al.From.String())
	out.WriteString("to ")
	out.WriteString(al.To.String())
	out.WriteString(al.Statements.String())
	out.WriteString("end ")
	return out.String()
}

// ArrayLiteral holds an inline array
type ArrayLiteral struct {
	// Token is the token
	Token token.Token

	// Elements holds the members of the array.
	Elements []Expression
}

func (al *ArrayLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (al *ArrayLiteral) TokenLiteral() string { return al.Token.Literal }

// String returns this object as a string.
func (al *ArrayLiteral) String() string {
	var out bytes.Buffer
	elements := make([]string, 0)
	for _, el := range al.Elements {
		elements = append(elements, el.String())
	}
	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")
	return out.String()
}

// LengthLiteral holds a LENGTH statement
type LengthLiteral struct {
	// Token is the token
	Token token.Token

	Name  Expression
	Value Expression
}

func (al *LengthLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (al *LengthLiteral) TokenLiteral() string { return al.Token.Literal }

// String returns this object as a string.
func (al *LengthLiteral) String() string {
	var out bytes.Buffer
	out.WriteString("[")
	out.WriteString(al.Name.String() + " = " + al.Value.String())
	out.WriteString("]")
	return out.String()
}

// IndexExpression holds an index-expression
type IndexExpression struct {
	// Token is the actual token
	Token token.Token

	// Left is the thing being indexed.
	Left Expression

	// Index is the value we're indexing
	Index []Expression
}

func (ie *IndexExpression) expressionNode() {}

// TokenLiteral returns the literal token.
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }

// String returns this object as a string.
func (ie *IndexExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString("{")
	elements := make([]string, 0)
	for _, el := range ie.Index {
		elements = append(elements, el.String())
	}
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]})")
	return out.String()
}

// HashLiteral holds a hash definition
type HashLiteral struct {
	// Token holds the token
	Token token.Token // the '{' token

	// Pairs stores the name/value sets of the hash-content
	Pairs map[Expression]Expression
}

func (hl *HashLiteral) expressionNode() {}

// TokenLiteral returns the literal token.
func (hl *HashLiteral) TokenLiteral() string { return hl.Token.Literal }

// String returns this object as a string.
func (hl *HashLiteral) String() string {
	var out bytes.Buffer
	pairs := make([]string, 0)
	for key, value := range hl.Pairs {
		pairs = append(pairs, key.String()+":"+value.String())
	}
	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ", "))
	out.WriteString("}")
	return out.String()
}

// AssignStatement is generally used for a (let-less) assignment,
// such as "x = y", however we allow an operator to be stored ("=" in that
// example), such that we can do self-operations.
//
// Specifically "x += y" is defined as an assignment-statement with
// the operator set to "+=".  The same applies for "+=", "-=", "*=", and
// "/=".
type AssignStatement struct {
	Token    token.Token
	Name     *Identifier
	Operator string
	Value    Expression
}

func (as *AssignStatement) expressionNode() {}

// TokenLiteral returns the literal token.
func (as *AssignStatement) TokenLiteral() string { return as.Token.Literal }

// String returns this object as a string.
func (as *AssignStatement) String() string {
	var out bytes.Buffer
	out.WriteString(as.Name.String())
	out.WriteString(as.Operator)
	out.WriteString(as.Value.String())
	return out.String()
}
