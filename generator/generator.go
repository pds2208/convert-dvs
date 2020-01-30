package generator

import (
	"bufio"
	"bytes"
	"convert/ast"
	"convert/token"
	"fmt"
	"strings"
)

type Generator struct {
	program        *ast.Program
	buffer         *bytes.Buffer
	programName    string
	indent         int
	inIfCondition  bool
	identifiers    map[string]string
	nonIdentifiers map[string]string
	sasInput       bool
	sasIntck       bool
}

func NewGenerator(program *ast.Program, programName string) Generator {
	return Generator{
		program:        program,
		programName:    programName,
		buffer:         &bytes.Buffer{},
		indent:         1,
		identifiers:    map[string]string{},
		nonIdentifiers: map[string]string{},
	}
}

func (g Generator) TargetCodeRepresentation() string {
	// remove extra newlines
	lines, err := StringToLines(g.buffer.String())
	if err != nil {
		return ""
	}

	var b bytes.Buffer
	b.WriteString("def " + strings.ToLower(g.programName) + "_dv(")

	var ids = map[string]string{}
	for k, v := range g.identifiers {
		if _, ok := g.nonIdentifiers[v]; !ok {
			ids[k] = v
		}
	}
	var i = 0
	for _, v := range ids {
		b.WriteString(strings.ToLower(v))
		if i < len(ids)-1 {
			b.WriteString(", ")
		}
		i++
	}

	b.WriteString("):\n")
	if g.sasInput {
		b.WriteString(`
    def sas_input(a, b):
        return a
	`)
		b.WriteString("\n")
	}
	if g.sasIntck {
		b.WriteString(
			`
    def sas_intck(period, d1, d2):
        # you need the dateutils package installed
        from dateutil.parser import parse
        return (parse(d2).year - parse(d1).year) * 12 + parse(d2).month - parse(d1).month
	`)
		b.WriteString("\n")
	}

	g.buffer.WriteString("\n" + g.indentString())

	for _, v := range lines {
		a := strings.TrimRight(v, " ")
		if a != "" {
			b.WriteString(strings.ToLower(v) + "\n")
		}
	}

	b.WriteString(fmt.Sprintf("    return %s\n", strings.ToLower(g.programName)))

	return b.String()
}

func StringToLines(s string) (lines []string, err error) {
	scanner := bufio.NewScanner(strings.NewReader(s))
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	err = scanner.Err()
	return
}

func (g Generator) Generate() Generator {
	g.eval(g.program)
	return g
}

func (g *Generator) eval(node ast.Node) {

	switch node := node.(type) {

	case *ast.Program:
		g.evalProgram(node)

	case *ast.BlockStatement:
		g.evalBlockStatement(node)

	case *ast.IfExpression:
		g.evalIfExpression(node)

	case *ast.ExpressionStatement:
		g.eval(node.Expression)

	case *ast.LetStatement:
		g.buffer.WriteString("\n" + g.indentString())
		g.identifiers[node.Name.Value] = node.Name.Value

		if s, ok := node.Value.(*ast.InfixExpression); ok {
			g.buffer.WriteString(node.Name.Value + " = ")
			if s.Token.Type == token.OR {
				if t, ok := s.Right.(*ast.CallExpression); ok {
					if t.Function.String() == "put" {
						g.eval(s.Left)
						//g.buffer.WriteString(" + str(")
						g.buffer.WriteString(" + ")
						g.eval(s.Right)
						//g.buffer.WriteString(")")
						break
					}
				}
			} else {
				if node.Value == nil {
					g.buffer.WriteString("\"\"")
				}
				g.eval(node.Value)
			}
		} else {
			g.buffer.WriteString(node.Name.Value + " = ")
			if node.Value == nil {
				g.buffer.WriteString("\"\"")
			}
			g.eval(node.Value)
		}

		g.buffer.WriteString("\n")

	case *ast.InExpression:
		g.buffer.WriteString("(")
		g.buffer.WriteString(node.Name.Value)
		g.identifiers[node.Name.Value] = node.Name.Value

		if node.Not {
			g.buffer.WriteString(" not")
		}

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

	case *ast.CallExpression:
		switch node.Function.String() {
		case "put":
			g.generatePut(node)
		case "input":
			g.generateInput(node)
		case "intck":
			g.generateIntck(node)
		default:
			g.buffer.WriteString(node.Function.String() + "(")
			for i, v := range node.Arguments {
				g.eval(v)
				if i < len(node.Arguments)-1 {
					g.buffer.WriteString(",")
				}
			}
			g.buffer.WriteString(")")
		}

	case *ast.ArrayStatement:

	case *ast.StringLiteral:
		g.buffer.WriteString(fmt.Sprintf("\"%s\"", node.Value))

	case *ast.IntegerLiteral:
		g.buffer.WriteString(fmt.Sprintf("%d", node.Value))

	case *ast.FloatLiteral:
		g.buffer.WriteString(fmt.Sprintf("%g", node.Value))

	case *ast.Boolean:
		g.buffer.WriteString(fmt.Sprintf("%t", node.Value))

	case *ast.PrefixExpression:
		g.buffer.WriteString(fmt.Sprintf("%s", node.Operator))
		g.eval(node.Right)

	case *ast.Identifier:
		g.identifiers[node.Value] = node.Value
		g.buffer.WriteString(fmt.Sprintf("%s", node.Value))

	case *ast.DoLiteral:
		g.buffer.WriteString(g.indentString() + fmt.Sprintf("for %s in range(%s, %s):\n",
			node.Variable.String(), node.From.String(), node.To.String()))
		g.indent++

		// we don't want these showing up as function parameters
		g.nonIdentifiers[node.Variable.String()] = node.Variable.String()

		g.eval(node.Statements)
		g.indent--
		g.buffer.WriteString("\n")

		// ignore
	case *ast.FormatExpression:

	case *ast.InfixExpression:
		if g.inIfCondition {
			g.buffer.WriteString("(")
			switch node.Operator {
			case "=":
				node.Operator = "=="
			case "^=":
				node.Operator = "!="
			}
		}
		if node.Operator == "||" { // concat operator
			node.Operator = "+"
		}

		g.eval(node.Left)

		g.buffer.WriteString(fmt.Sprintf(" %s ", node.Operator))
		g.eval(node.Right)
		if g.inIfCondition {
			g.buffer.WriteString(")")
		}
	}
}

func (g *Generator) evalProgram(program *ast.Program) {
	for _, statement := range program.Statements {
		g.eval(statement)
	}
}

func (g *Generator) evalBlockStatement(block *ast.BlockStatement) {
	if block == nil {
		return
	}
	g.buffer.WriteString("\n")
	for _, statement := range block.Statements {
		g.eval(statement)
	}
	g.buffer.WriteString("\n")
}

func (g *Generator) evalStatements(stmts []ast.Statement) {
	if stmts == nil {
		return
	}
	for _, statement := range stmts {
		g.eval(statement)
		g.buffer.WriteString("\n")
	}
}

func (g *Generator) evalIfExpression(ie *ast.IfExpression) {
	g.buffer.WriteString(g.indentString() + "if ")
	g.inIfCondition = true

	g.eval(ie.Condition)
	g.inIfCondition = false
	g.buffer.WriteString(":\n")
	g.indent++
	g.eval(ie.Consequence)
	g.indent--
	if ie.Alternative != nil {
		g.buffer.WriteString(g.indentString())
		g.buffer.WriteString("else:\n")
		g.indent++
		g.eval(ie.Alternative)
		g.indent--
		g.buffer.WriteString("\n")
	}
}

func (g *Generator) indentString() string {
	b := &bytes.Buffer{}
	for i := 0; i < g.indent; i++ {
		b.WriteString("    ")
	}
	return b.String()
}

func (g *Generator) generatePut(node *ast.CallExpression) {
	if len(node.Arguments) != 2 {
		g.buffer.WriteString(" # Cannot parse - put() function does not contain 2 arguments\n")
	}
	// if we are in a let statement, don't convert to an int as we may need to concat - ugly
	if g.inIfCondition {
		g.buffer.WriteString("int(str(")
	} else {
		g.buffer.WriteString("str(")
	}
	g.eval(node.Arguments[0])
	g.buffer.WriteString(")[:")
	g.eval(node.Arguments[1])
	if g.inIfCondition {
		g.buffer.WriteString("])")
	} else {
		g.buffer.WriteString("]")
	}
}

func (g *Generator) generateInput(node *ast.CallExpression) {
	g.buffer.WriteString("sas_input(")
	for i, v := range node.Arguments {
		g.eval(v)
		if i < len(node.Arguments)-1 {
			g.buffer.WriteString(", ")
		}
	}
	g.buffer.WriteString(")")
	g.sasInput = true
}

func (g *Generator) generateIntck(node *ast.CallExpression) {
	g.buffer.WriteString("sas_intck(")
	for i, v := range node.Arguments {
		g.eval(v)
		if i < len(node.Arguments)-1 {
			g.buffer.WriteString(", ")
		}
	}
	g.buffer.WriteString(")")
	g.sasIntck = true
}
