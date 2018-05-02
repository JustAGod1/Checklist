package ru.justagod.checklist.parsing

import java.util

import ru.justagod.checklist.{NumberType, StringType}

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.CharArrayReader.EofCh

class ChecklistLexer extends Lexical with ChecklistTokens {

  private val status: util.Stack[Status] = {
    val result = new util.Stack[Status]()
    result.add(RootStatus)
    result
  }

  override def token: Parser[Token] = {
    opt(comment) ~> (eol | (currentStatus() match {
      case VariableBlockStatus => variableBlock
      case RootStatus => root
      case ValueStatus => value
      case ExpressionStatus => expression
      case FunctionStatus => function
      case AssignationStatus => assign
    })) <~ opt(comment)
  }

  private val function = identifier | bracket | comma
  private val value = logicValue | identifier ^^ {
    e =>
      popStatus()
      e
  } | openingBrace ^^ {
    e =>
      popStatus()
      pushStatus(ExpressionStatus)
      e
  }
  private val root = hash | lessMore | exclamationMark | percent | frontArrow | doubleDog | dog | doubleDollar | doubleFloor | questionMark | line
  private val variableBlock = variableType | colon | dollar | string
  private val expression = logicValue | identifier | number | mark | exclamationMark | openingBrace | closingBrace ^^ {
    e =>
      popStatus()
      e
  } | bracket | comma | (doubleQuote ~> string <~ doubleQuote)
  private val assign = identifier | assignation

  lazy private val openingBrace = '{' ^^^ OpeningCurlyBrace()
  lazy private val closingBrace = '}' ^^^ ClosingCurlyBrace()
  lazy private val bracket = '(' ^^^ OpeningBracket() | ')' ^^^ ClosingBracket()
  lazy private val comma = ',' ^^^ Comma()
  lazy private val doubleFloor = ('^' ~ '^') ^^ {
    e =>
      pushStatus(ExpressionStatus)
      DoubleFloor()
  }
  lazy private val doubleQuote = '"' ^^^ DoubleQuote()
  lazy private val questionMark = '?' ^^ {
    _ =>
      pushStatus(VariableBlockStatus)
      QuestionMark()
  }
  lazy private val exclamationMark = '!' ^^ {
    _ =>
      pushStatus(AssignationStatus)
    ExclamationMark()
  }
  lazy private val comment = '#' ~> '#' ~> rep(chrExcept('\n'))
  lazy private val hash = '#' ^^^ Hash()
  lazy private val dog = '@' ^^ {
    _ =>
      pushStatus(ExpressionStatus)
      new Dog
  }
  lazy private val lessMore = ('<' ~ '>') ^^ {
    _ =>
      pushStatus(ExpressionStatus)
      new LessMore
  }
  lazy private val doubleDog = ('@' ~ '@') ^^^ DoubleDog()
  lazy private val dollar: Parser[Dollar] = '$' ^^ {
    _ =>
      pushStatus(ValueStatus)
      new Dollar
  }
  lazy private val percent: Parser[Percent] = '%' ^^ {
    _ =>
      pushStatus(AssignationStatus)
      new Percent
  }
  lazy private val assignation: Parser[Assignation] = '=' ^^ {
    _ =>
      if (currentStatus == AssignationStatus) {
        popStatus()
        pushStatus(ExpressionStatus)
      }
      Assignation()
  }

  private val eol: Parser[Indent] = '\n' ~> rep('\t' ^^^ "    " | (' ' ^^^ " ")).withFilter(_.mkString.length % 4 == 0) ^^ {
    e =>
      val properly = e.mkString
      resetStatus()
      Indent(if (properly.length > 1) {
        (properly.length - properly.lastIndexOf('\n') - 1) / 4
      } else {
        0
      }
      )
  }
  lazy private val number =  opt(accept('+') | '-') ~ rep1(digit) ~ opt('.' ~> rep1(digit)) ^^ {
    case m ~ a ~ None => Number((m.getOrElse("") + a.mkString).toDouble)
    case m ~ a ~ Some(frac) =>
      Number((m.getOrElse("").toString + a.mkString + '.' + frac.mkString).toDouble)
  }

  lazy private val doubleDollar = '$' ~ '$' ^^ {
    e =>
      pushStatus(FunctionStatus)
      DoubleDollar()
  }

  lazy private val variableType = identifier.withFilter {
    case Identifier(str) => str == "Строка" || str == "Число"
    case _ => false
  } ^^ {
    e =>
      VariableTypeDeclaration(
        e.value match {
          case "Строка" => StringType
          case "Число" => NumberType
        }
      )
  }

  private def pushStatus(status: Status) = this.status.push(status)

  private def popStatus() = this.status.pop

  private def currentStatus() = this.status.peek

  private def resetStatus() = {
    status.clear()
    status.add(RootStatus)
  }


  lazy private val line = string | dollar


  lazy private val mark = (
    accept('+') ^^^ "+"
      | '-' ^^^ "-"
      | '*' ^^^ "*"
      | '/' ^^^ "/"
      | '%' ^^^ "%"
      | ('=' ~ '=') ^^^ "=="
      | ('!' ~ '=') ^^^ "!="
      | ('<' ~ '=') ^^^ "<="
      | ('>' ~ '=') ^^^ ">="
      | '<' ^^^ "<"
      | '>' ^^^ ">"
      | ('&' ~ '&') ^^^ "&&"
      | ('|' ~ '|') ^^^ "||"
    ) ^^ Mark


  private lazy val frontArrow = ('-' ~ '>') ^^ { _ =>
    pushStatus(VariableBlockStatus)
    new FrontArrow
  }

  private lazy val colon = ':' ^^^ Colon()


  private lazy val identifier: Parser[Identifier] = elem("Identifier", { e => e.isLetter }) ~ rep(elem("Identifier", { e => e.isLetter || (e >= '0' && e <= '9') })) ^^ {
    case start ~ end => Identifier(start + end.mkString)
  }

  lazy private val string = rep1(charSeq | chrExcept('\"', '\n', ':', '$', '#', EofCh)) ^^ {
    e =>
      Word(e.mkString)
  }

  lazy private val charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
      | '\\' ~ '\\' ^^^ "\\"
      | '\\' ~ '/' ^^^ "/"
      | '\\' ~ 'b' ^^^ "\b"
      | '\\' ~ 'f' ^^^ "\f"
      | '\\' ~ 'n' ^^^ "\n"
      | '\\' ~ 'r' ^^^ "\r"
      | '\\' ~ 't' ^^^ "\t"
      | '\\' ~ '$' ^^^ "$"
      | '\\' ~ ':' ^^^ ":"
      | '\\' ~ '\\' ^^^ "\\"
      | '\\' ~ ' ' ^^^ ""
      | '\\' ~ '#' ^^^ "#"
      | '\\' ~> 'u' ~> unicodeBlock)

  lazy private val logicValue = (
    ('t' ~ 'r' ~ 'u' ~ 'e') ^^^ LogicValueTrue()
      | ('f' ~ 'a' ~ 'l' ~ 's' ~ 'e') ^^^ LogicValueFalse()
    )
  private val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  private val hexDigit = elem("hex digit", hexDigits.contains)

  private val unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
    case a ~ b ~ c ~ d =>
      new String(Array(Integer.parseInt(List(a, b, c, d).mkString, 16)), 0, 1)
  }

  override def whitespace: Parser[Any] =
    rep(
      elem("space char", ch => ch <= ' ' && ch != '\n'))

  sealed trait Status

  case object RootStatus extends Status

  case object VariableBlockStatus extends Status

  case object ExpressionStatus extends Status

  case object ValueStatus extends Status

  case object FunctionStatus extends Status

  case object AssignationStatus extends Status

}


