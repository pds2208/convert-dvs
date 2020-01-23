package generator

import (
	"bytes"
	"convert/ast"
	"fmt"
	"regexp"
)

type Generator struct {
	program       *ast.Program
	buffer        *bytes.Buffer
	programName   string
	indent        int
	inLet         bool
	inIfCondition bool
}

func NewGenerator(program *ast.Program, programName string) Generator {
	return Generator{program: program, programName: programName, buffer: &bytes.Buffer{}}
}

func (g Generator) TargetCodeRepresentation() string {
	// remove extra newlines
	regex, err := regexp.Compile("\n\n")
	if err != nil {
		return ""
	}
	s := regex.ReplaceAllString(g.buffer.String(), "\n")
	return s
}

func (g Generator) Generate() Generator {
	g.eval(g.program)
	return g
}

func (g Generator) GeneratePreamble() Generator {

	elements := make([]string, 0)
	for _, statement := range g.program.Statements {
		if s, ok := statement.(*ast.ArrayStatement); ok {
			elements = append(elements, s.Name.String())
		}
	}

	if len(elements) == 0 {
		g.buffer.WriteString("def " + g.programName + ":")
		return g
	}

	g.buffer.WriteString("def " + g.programName + "(")

	for k, v := range elements {
		g.buffer.WriteString(v)
		if k < len(elements)-1 {
			g.buffer.WriteString(", ")
		}
	}

	g.buffer.WriteString("):\n")
	return g
}

func (g Generator) eval(node ast.Node) {

	switch node := node.(type) {

	case *ast.Program:
		g.evalProgram(node)

	case *ast.BlockStatement:
		g.evalBlockStatement(node)

	case *ast.IfExpression:
		g.evalIfExpression(node)

	case *ast.ExpressionStatement:
		g.buffer.WriteString(g.indentString())
		g.eval(node.Expression)
		//g.buffer.WriteString("\n")

	case *ast.LetStatement:
		g.buffer.WriteString(g.indentString())
		g.inLet = true
		if s, ok := node.Value.(*ast.InfixExpression); ok {
			g.eval(s)
		} else {
			g.buffer.WriteString(node.Name.Value + " = ")
			g.eval(node.Value)
		}
		g.inLet = false
		g.buffer.WriteString("\n")

	case *ast.InExpression:
		g.buffer.WriteString("(")
		g.buffer.WriteString(node.Name.Value)
		g.buffer.WriteString(" in (")
		for i, v := range node.Values {
			g.buffer.WriteString(v.String())
			if i < len(node.Values)-1 {
				g.buffer.WriteString(", ")
			}
		}
		g.buffer.WriteString("))")

	case *ast.IndexExpression:
		g.eval(node.Left)
		g.buffer.WriteString("[")

		for _, v := range node.Index {
			g.eval(v)
		}
		g.buffer.WriteString("]")

		// ignore these
	case *ast.LengthLiteral:

	case *ast.ArrayStatement:
		g.buffer.WriteString(node.Name.Value + " = []\n")

	case *ast.IntegerLiteral:
		g.buffer.WriteString(fmt.Sprintf("%d", node.Value))

	case *ast.FloatLiteral:
		g.buffer.WriteString(fmt.Sprintf("%g", node.Value))

	case *ast.Boolean:
		g.buffer.WriteString(fmt.Sprintf("%t", node.Value))

	case *ast.PrefixExpression:
		g.buffer.WriteString(g.indentString())
		g.buffer.WriteString(fmt.Sprintf("%s", node.Operator))
		g.eval(node.Right)

	case *ast.Identifier:
		g.buffer.WriteString(fmt.Sprintf("%s", node.Value))

	case *ast.DoLiteral:
		g.indentString()
		g.buffer.WriteString(fmt.Sprintf("for %s in range(%s, %s):\n",
			node.Variable.String(), node.From.String(), node.To.String()))
		g.indent++
		g.eval(node.Statements)

	case *ast.InfixExpression:
		g.indentString()
		if g.inIfCondition {
			g.buffer.WriteString("(")
			if node.Operator == "=" {
				node.Operator = "=="
			}
		}
		g.indentString()
		g.eval(node.Left)

		g.buffer.WriteString(fmt.Sprintf(" %s ", node.Operator))
		g.eval(node.Right)
		if g.inIfCondition {
			g.buffer.WriteString(")")
		}
	}
}

func (g Generator) evalProgram(program *ast.Program) {
	for _, statement := range program.Statements {
		g.eval(statement)
	}

}

func (g Generator) evalBlockStatement(block *ast.BlockStatement) {
	if block == nil {
		return
	}
	for _, statement := range block.Statements {
		g.eval(statement)
	}
}

func (g Generator) evalStatements(stmts []ast.Statement) {
	if stmts == nil {
		return
	}
	for _, statement := range stmts {
		g.eval(statement)
		g.buffer.WriteString("\n")
	}

}

func (g Generator) evalIfExpression(ie *ast.IfExpression) {
	g.buffer.WriteString("if ")
	g.inIfCondition = true
	g.eval(ie.Condition)
	g.inIfCondition = false
	g.buffer.WriteString(":\n")
	g.indent++
	g.eval(ie.Consequence)
	g.indent--
	if ie.Alternative != nil {
		g.buffer.WriteString("\n")
		g.buffer.WriteString(g.indentString())
		g.buffer.WriteString("else:\n")
		g.indent++
		g.eval(ie.Alternative)
		g.indent--
		g.buffer.WriteString("\n")
	}
}

func (g Generator) indentString() string {
	b := &bytes.Buffer{}
	for i := 0; i < g.indent; i++ {
		b.WriteString("    ")
	}
	return b.String()
}
