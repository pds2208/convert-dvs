package generator

import (
	"bytes"
	"convert/ast"
	"convert/object"
	"fmt"
)

var (
	NULL  = &object.Null{}
	TRUE  = &object.Boolean{Value: true}
	FALSE = &object.Boolean{Value: false}
)

type Generator struct {
	program     *ast.Program
	buffer      *bytes.Buffer
	programName string
	object      object.Object
	indent      int
}

func NewGenerator(program *ast.Program, programName string) Generator {
	return Generator{program: program, programName: programName, buffer: &bytes.Buffer{}}
}

func (g Generator) TargetCodeRepresentation() string {
	return g.buffer.String()
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
		g.buffer.WriteString("\n")

	case *ast.LetStatement:
		//g.buffer.WriteString(node.Name.Value)
		g.buffer.WriteString(g.indentString())
		g.eval(node.Value)

	case *ast.InExpression:
		g.buffer.WriteString(node.Name.Value)

	case *ast.IndexExpression:
		//g.buffer.WriteString(" ")
		g.eval(node.Left)
		g.buffer.WriteString("[")

		for _, v := range node.Index {
			g.eval(v)
		}
		//for _, v := range node.Index {
		//	g.buffer.WriteString(v.String())
		//}
		g.buffer.WriteString("]")

		// ignore these
	case *ast.LengthLiteral:

	case *ast.ArrayStatement:
		g.buffer.WriteString(node.Name.Value + " = []\n")

	case *ast.IntegerLiteral:
		g.buffer.WriteString(fmt.Sprintf("%d", node.Value))

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
		g.buffer.WriteString("(")
		g.eval(node.Left)
		if node.Operator == "==" {
			node.Operator = "="
		}
		g.buffer.WriteString(fmt.Sprintf(" %s ", node.Operator))
		g.eval(node.Right)
		g.buffer.WriteString(")")
	}
}

func (g Generator) evalProgram(program *ast.Program) {
	for _, statement := range program.Statements {
		g.eval(statement)
	}

}

//
//func (g Generator) evaluateIndex(node ast.Node) string {
//	v := node.Value.(*ast.InfixExpression)
//
//	switch v.Right.(type) {
//	case *ast.IndexExpression:
//		leftIndex := v.Left.(*ast.IndexExpression)
//		rightIndex := v.Right.(*ast.IndexExpression)
//		g.buffer.WriteString(fmt.Sprintf("%s %s [%s]", leftIndex.Left.String(), v.Operator, rightIndex.))
//	default:
//		g.buffer.WriteString(fmt.Sprintf("%s = %s", leftIndex.Left.String(), v.Operator, rightIndex.))
//	}
//
//	b := bytes.Buffer{}
//	rightIndex := v.Left.(*ast.IndexExpression)
//
//	for i, j := range rightIndex.Index {
//		b.WriteString(j.String())
//		if i < len(rightIndex.Index) -1 {
//			b.WriteString(", ")
//		}
//	}
//
//	g.buffer.WriteString(fmt.Sprintf("%s %s [%s]", leftIndex.Left.String(), v.Operator, rightIndex.))
//	g.eval(node.Value)
//
//}

func (g Generator) evalBlockStatement(block *ast.BlockStatement) {
	for _, statement := range block.Statements {
		g.eval(statement)
		g.buffer.WriteString("\n")
	}
}

func (g Generator) evalStatements(stmts []ast.Statement) {

	for _, statement := range stmts {
		g.eval(statement)
		g.buffer.WriteString("\n")
	}

}

func (g Generator) evalIfExpression(ie *ast.IfExpression) {
	g.indentString()
	g.buffer.WriteString("if ")
	g.eval(ie.Condition)
	g.buffer.WriteString(":")
	g.indent++
}

func (g Generator) indentString() string {
	b := &bytes.Buffer{}
	for i := 0; i < g.indent; i++ {
		b.WriteString("    ")
	}
	return b.String()
}
