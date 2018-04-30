package ru.justagod.checklist

sealed class VariableType
object StringType extends VariableType {
  override def toString = "Строка"
}
object NumberType extends VariableType {
  override def toString = "Число"
}

sealed class Expression
sealed class CompletedExpression extends Expression
case class ExpressionNumber(value: Double) extends CompletedExpression {
  override def toString = value.toString
}
case class ExpressionWord(value: String) extends CompletedExpression {
  override def toString = '"' + value + '"'
}
case class ExpressionBoolean(value: Boolean) extends CompletedExpression {
  override def toString = value.toString
}
case class VariableInsn(value: String) extends Expression {
  override def toString = value
}
case class FunctionInsn(name: String, args: List[Expression]) extends Expression
case class BinaryOperation(left: Expression, mark: String, right: Expression) extends Expression {
  override def toString = s"($left $mark $right)"
}



sealed class OutLineParticipant
case class OutLineWord(value: String) extends OutLineParticipant
case class ExpressionCall(value: Expression) extends OutLineParticipant

sealed class Line
final case class LineWrap(number: Int, line: Line)
sealed class LineWithInstructions extends Line {
  var instructions = None: Option[List[LineWrap]]
}

case class OutLine(participants: List[OutLineParticipant]) extends Line
case class ReturnExpression(expression: Option[Expression]) extends Line
case class Assignation(variable: String, expression: Expression) extends Line
case class Declaration(variable: String, expression: Expression) extends Line
case class ChecklistFunction(name: String, args: List[String]) extends LineWithInstructions
case class VariableBlock(tip: OutLine, varType: VariableType, name: String) extends LineWithInstructions
case class WhileStatement(expression: Expression) extends LineWithInstructions
case class VariableRequest(tip: OutLine, varType: VariableType, name: String) extends Line
case class IfStatement(expression: Expression) extends LineWithInstructions {
  var onFalse = None: Option[List[LineWrap]]
}
case class ElseStatement() extends LineWithInstructions