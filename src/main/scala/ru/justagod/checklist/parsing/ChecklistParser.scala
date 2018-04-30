package ru.justagod.checklist.parsing

import ru.justagod.checklist._

import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}


object ChecklistParser extends TokenParsers with ImplicitConversions with PackratParsers {

  type Tokens = ChecklistLexer

  val lexical = new Tokens


  def checklist: Parser[List[(Int, Option[Line])]] = rep(lineWithIndent)

  lazy val lineWithIndent = (indent ~ opt(line)) ^^ {
    case ind ~ valu => (ind, valu)
  }


  lazy private val line = whileStatement | declaration | variableRequest | assignation | elseStatement | ifStatement | variableBlock | returnValue | function | outLine


  lazy private val function = ((doubleDollar ~> identifier) <~ openBracket) ~ repsep(identifier, comma) <~ closeBracket ^^ {
    case name ~ args =>
      ChecklistFunction(name, args)
  }
  lazy private val outLine = rep1(word ^^ (e => OutLineWord(e.value)) | expressionCall) ^^ OutLine
  lazy private val variableBlock = (forwardArrow ~> tip <~ colon) ~ (variableType <~ colon) ~ identifierFromStr ^^ {
    e => VariableBlock(e._1._1, e._1._2, e._2)
  }
  lazy private val variableRequest = (questionMark ~> tip <~ colon) ~ (variableType <~ colon) ~ identifierFromStr ^^ {
    e => VariableRequest(e._1._1, e._1._2, e._2)
  }
  lazy private val ifStatement = (dog ~> (expression withFailureMessage "Condition expected")) ^^ IfStatement
  lazy private val elseStatement = doubleDog ^^^ ElseStatement()
  lazy private val assignation = (percent ~> identifier) ~ ((assign ~> expression) withFailureMessage "New value expected") ^^ {
    case name ~ expression =>
      Assignation(name, expression)
  }
  lazy private val declaration = (exclamation ~> identifier) ~ (assign ~> expression withFailureMessage "Initial value expected") ^^ {
    case name ~ expression =>
      Declaration(name, expression)
  }
  lazy private val whileStatement = lessMore ~> (expression withFailureMessage "Expected condition") ^^ WhileStatement

  lazy private val tip = expressionCall ^^ {
    e =>
      OutLine(List(e))
  } | word ^^ {
    e =>
      OutLine(List(OutLineWord(e.value)))
  } | variableType ^^ {
    e =>
      OutLine(List(OutLineWord(e.toString)))
  }

  lazy private val identifierFromStr: Parser[String] = acceptIf({
    case lexical.Word(stra) =>
      val str = stra.trim
      if (!str.charAt(0).isLetter) false
      else if (str.length > 0) {
        str.substring(1).chars().toArray.forall {
          e =>
            e.toChar.isLetter || e.toChar.isDigit
        }
      } else {
        true
      }
  })({
    e => "Identifier expected but " + e.toString + " found"
  }) ^^ {
    case lexical.Word(str) =>
      str.trim
  }

  lazy private val expressionCall: ChecklistParser.Parser[ExpressionCall] = ((dollar ~> variable) | (dollar ~> openBrace ~> expression <~ closeBrace)) ^^ ExpressionCall
  lazy private val returnValue = doubleFloor ~> opt(expression) ^^ ReturnExpression

  private val functionCall = (identifier <~ openBracket) ~ (repsep(expression, comma) <~ closeBracket) ^^ {
    case name ~ args =>
      FunctionInsn(name, args)
  }

  lazy private val indent = accept("New line", { case lexical.Indent(count) => count })
  lazy private val variable: Parser[VariableInsn] = identifier ^^ {
    e => VariableInsn(e)
  }
  lazy private val dollar = accept("Dollar", { case d: lexical.Dollar => d.chars })
  lazy private val doubleDollar = accept("Double Dollar", { case d: lexical.DoubleDollar => d.chars })
  lazy private val word = accept("Word", { case w: lexical.Word => w })
  lazy private val identifier = accept("Identifier", { case d: lexical.Identifier => d.value })
  lazy private val number = accept("Number", { case lexical.Number(d) => ExpressionNumber(d) })
  lazy private val mark = accept("Mark", { case lexical.Mark(value) => value })
  lazy private val colon = accept(":", { case lexical.Colon() => ":" })
  lazy private val comma = accept("Word", { case _: lexical.Comma => value })
  lazy private val openBrace = accept("{", { case lexical.OpeningCurlyBrace() => "{" })
  lazy private val closeBrace = accept("}", { case lexical.ClosingCurlyBrace() => "}" })
  lazy private val openBracket = accept("(", { case lexical.OpeningBracket() => "(" })
  lazy private val closeBracket = accept(")", { case lexical.ClosingBracket() => ")" })
  lazy private val forwardArrow = accept("->", { case lexical.FrontArrow() => "->" })
  lazy private val doubleFloor = accept("^^", { case lexical.DoubleFloor() => "^^" })
  lazy private val questionMark = accept("?", { case lexical.QuestionMark() => "?" })
  lazy private val dog = accept("@", { case lexical.Dog() => "@" })
  lazy private val lessMore = accept("@", { case lexical.LessMore() => "<>" })
  lazy private val doubleDog = accept("@@", { case lexical.DoubleDog() => "@@" })
  lazy private val bool = accept("true or false", { case v: lexical.LogicValue => v.value }) ^^ ExpressionBoolean
  lazy private val variableType = accept("Variable type", { case lexical.VariableTypeDeclaration(v) => v })
  lazy private val assign = accept("=", { case lexical.Assignation() => "=" })
  lazy private val percent = accept("%", { case lexical.Percent() => "%" })
  lazy private val exclamation = accept("!", { case lexical.ExclamationMark() => "!" })


  lazy private val highPriorityMark = mark withFilter {
    e =>
      (
        e == "*"
          || e == "=="
          || e == "/"
          || e == "%"
          || e == ">="
          || e == "<="
          || e == ">"
          || e == "<"
          || e == "!="
        )
  }

  lazy private val lowPriorityMark = mark withFilter {
    e =>
      (
        e == "+"
          || e == "-"
          || e == "&&"
          || e == "||"
          || e == "!="
        )
  }

  lazy private val expression: PackratParser[Expression] = (expression ~ lowPriorityMark ~ term) ^^ toBinary | term
  lazy private val term: PackratParser[Expression] = (
    term ~ highPriorityMark ~ term ^^ toBinary
      | value
      | (openBracket ~> expression <~ closeBracket)
    )
  lazy private val value: Parser[Expression] =
    (functionCall
      | variable
      | number
      | bool
      | word ^^ (e => ExpressionWord(e.value))
      )


  def toBinary(value: Expression ~ String ~ Expression) = {
    BinaryOperation(value._1._1, value._1._2, value._2)
  }

  implicit class IterableOp[T](self: Array[T]) {

    def forall(block: T => Boolean): Boolean = {
      for (item <- self) {
        if (!block(item)) {
          return false
        }
      }
      true
    }
  }

}

