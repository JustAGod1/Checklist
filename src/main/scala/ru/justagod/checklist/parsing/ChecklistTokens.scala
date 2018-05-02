package ru.justagod.checklist.parsing

import ru.justagod.checklist.VariableType

import scala.util.parsing.combinator.token.Tokens


trait ChecklistTokens extends Tokens {

  case class FrontArrow() extends Token {
    override def chars: String = "->"
  }

  case class Dollar() extends Token {
    override def chars: String = "$"
  }

  case class DoubleDollar() extends Token {
    override def chars: String = "$$"
  }

  case class ReverseCurlyBrace() extends Token {
    override def chars = "}"
  }

  case class Word(value: String) extends Token {
    override def chars: String = "\"" + value + "\""
  }

  case class Identifier(value: String) extends Token {
    override def chars: String = value
  }

  case class Indent(value: Int) extends Token {
    override def chars: String = s"\\t x $value"
  }

  case class Variable(value: String) extends Token {
    override def chars = s"Variable: $value"
  }

  case class VariableTypeDeclaration(value: VariableType) extends Token {
    override def chars = value.toString
  }

  case class Number(value: Double) extends Token {
    override def chars = "Number: " + value.toString
  }

  case class Mark(value: String) extends Token {
    override def chars = "Mark: " + value.toString
  }

  case class OpeningCurlyBrace() extends Token {
    override def chars = "{"
  }

  case class ClosingCurlyBrace() extends Token {
    override def chars = "}"
  }

  case class OpeningBracket() extends Token {
    override def chars = "("
  }

  case class ClosingBracket() extends Token {
    override def chars = ")"
  }

  case class Comma() extends Token {
    override def chars = ","
  }

  case class Colon() extends Token {
    override def chars = ":"
  }

  case class DoubleQuote() extends Token {
    override def chars = "\""
  }

  case class DoubleFloor() extends Token {
    override def chars = "^^"
  }

  case class QuestionMark() extends Token {
    override def chars = "?"
  }

  case class ExclamationMark() extends Token {
    override def chars = "!"
  }

  abstract class LogicValue(val value: Boolean) extends Token {
  }

  case class LogicValueTrue() extends LogicValue(true) {
    override def chars = "true"
  }

  case class LogicValueFalse() extends LogicValue(false) {
    override def chars = "false"
  }

  case class Dog() extends Token {
    override def chars = "@"
  }

  case class Percent() extends Token {
    override def chars = "%"
  }

  case class Assignation() extends Token {
    override def chars = "="
  }

  case class DoubleDog() extends Token {
    override def chars = "@@"
  }
  case class LessMore() extends Token {
    override def chars = "<>"
  }
  case class Hash() extends Token {
    override def chars = "#"
  }

}
