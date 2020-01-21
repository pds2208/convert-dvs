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
}

func NewGenerator(program *ast.Program, programName string) Generator {
	return Generator{program: program, programName: programName, buffer: &bytes.Buffer{}}
}

func (g Generator) TargetCodeRepresentation() string {
	return g.buffer.String()
}

func (g Generator) Generate() Generator {
	g.object = g.eval(g.program, object.NewEnvironment())
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

func (g Generator) eval(node ast.Node, env *object.Environment) object.Object {

	switch node := node.(type) {

	case *ast.Program:
		return g.evalProgram(node, env)

	case *ast.BlockStatement:
		return g.evalBlockStatement(node, env)

	case *ast.IfExpression:
		return g.evalIfExpression(node, env)

	case *ast.ExpressionStatement:
		return g.eval(node.Expression, env)

	case *ast.LetStatement:
		val := g.eval(node.Value, env)
		if isError(val) {
			return val
		}
		env.Set(node.Name.Value, val)
		return val

	case *ast.IndexExpression:
		left := g.eval(node.Left, env)
		if isError(left) {
			return left
		}
		index := g.eval(node.Index, env)
		if isError(index) {
			return index
		}
		return g.evalIndexExpression(left, index)

		// ignore these
	case *ast.LengthLiteral:

	case *ast.ArrayStatement:
		return &object.Array{Name: node.Name.Value} // we don't need the array size in python

	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}

	case *ast.Boolean:
		return &object.Boolean{Value: node.Value}

	case *ast.PrefixExpression:
		right := g.eval(node.Right, env)
		if isError(right) {
			return right
		}
		return g.evalPrefixExpression(node.Operator, right)

	case *ast.Identifier:
		return g.evalIdentifier(node, env)

	case *ast.DoLiteral:

	}

	return nil
}

func (g Generator) evalProgram(program *ast.Program, env *object.Environment) object.Object {
	var result object.Object
	for _, statement := range program.Statements {
		result = g.eval(statement, env)

		switch result := result.(type) {
		//case *object.ReturnValue:
		//	return result.Value
		case *object.Error:
			return result
		}

		if result == nil {
			println(statement.TokenLiteral())
		}
		if result != nil {
			a := result.Inspect()
			print(a)
		}
	}
	return result
}

func (g Generator) evalIndexExpression(left, index object.Object) object.Object {
	switch {
	case left.Type() == object.ArrayObj && index.Type() == object.IntegerObj:
		return g.evalArrayIndexExpression(left, index)
	case left.Type() == object.ArrayObj
		return g.evalArrayIdentIndexExpression(left, index)
	default:
		return newError("index operator not supported: %s", left.Type())
	}
}

func (g Generator) evalArrayIndexExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.Array)
	idx := index.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)

	if idx < 0 || idx > max {
		return NULL
	}

	return arrayObject.Elements[idx]
}

func (g Generator) evalArrayIdentIndexExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.Array)
	idx := index.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)

	if idx < 0 || idx > max {
		return NULL
	}

	return arrayObject.Elements[idx]
}

func (g Generator) evalIdentifier(node *ast.Identifier, env *object.Environment) object.Object {
	val, ok := env.Get(node.Value)
	if !ok {
		return newError("identifier not found: " + node.Value)
	}
	return val
}

func (g Generator) evalBlockStatement(block *ast.BlockStatement, env *object.Environment) object.Object {
	var result object.Object
	for _, statement := range block.Statements {
		result = g.eval(statement, env)
		if result != nil {
			rt := result.Type()
			if rt == object.ErrorObj {
				return result
			}
		}
	}
	return result
}

func (g Generator) evalPrefixExpression(operator string, right object.Object) object.Object {
	switch operator {
	case "-":
		return g.evalMinusPrefixOperatorExpression(right)
	default:
		return newError("unknown operator: %s%s", operator, right.Type())
	}
}

func (g Generator) evalMinusPrefixOperatorExpression(right object.Object) object.Object {
	switch right.Type() {

	case object.IntegerObj:
		value := right.(*object.Integer).Value
		return &object.Integer{Value: -value}

	case object.FloatObj:
		value := right.(*object.Float).Value
		return &object.Float{Value: -value}
	}

	return NULL
}

func (g Generator) evalStatements(stmts []ast.Statement, env *object.Environment) object.Object {
	var result object.Object

	for _, statement := range stmts {
		result = g.eval(statement, env)
	}

	return result
}

func (g Generator) evalIfExpression(ie *ast.IfExpression, env *object.Environment) object.Object {
	condition := g.eval(ie.Condition, env)
	if isError(condition) {
		return condition
	}

	if isTruthy(condition) {
		return g.eval(ie.Consequence, env)
	} else if ie.Alternative != nil {
		return g.eval(ie.Alternative, env)
	} else {
		return NULL
	}
}

func newError(format string, a ...interface{}) *object.Error {
	return &object.Error{Message: fmt.Sprintf(format, a...)}
}

func isError(obj object.Object) bool {
	if obj != nil {
		return obj.Type() == object.ErrorObj
	}
	return false
}

func isTruthy(obj object.Object) bool {
	switch obj {
	case NULL:
		return false
	case TRUE:
		return true
	case FALSE:
		return false
	default:
		return true
	}
}
