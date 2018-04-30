package ru.justagod.checklist.runtime

import ru.justagod.checklist._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ChecklistEvaluator(val data: DataRequester) extends MarkProcessors {

  val globalVariables = new mutable.HashMap[String, CompletedExpression]()
  val globalFunctions = new ListBuffer[ChecklistFunction]()

  private val markProcessors = Map[String, MarkProcessor](
    ("+", new PlusProcessor),
    ("-", new MinusProcessor),
    ("/", new DividerProcessor),
    ("%", new PercentProcessor),
    ("*", new AsteriskProcessor),
    ("&&", new AndProcessor),
    ("||", new OrProcessor),
    ("<=", new LessOrEqualProcessor),
    (">=", new MoreOrEqualProcessor),
    (">", new MoreProcessor),
    ("<", new LessProcessor),
    ("==", new EqualProcessor),
    ("!=", new NotEqualProcessor)
  )

  def evaluateGlobal(lines: List[LineWrap]): Unit = {
    for (wrap <- lines) {
      try {
        wrap.line match {
          case OutLine(participants) =>
            println("-> " + evaluateLine(participants, globalVariables.toMap, globalFunctions.toList))
          case e: VariableBlock =>
            val evaluatedTip = evaluateLine(e.tip.participants, globalVariables.toMap, globalFunctions.toList)
            print(evaluatedTip + ": ")
            val variable = getVariable(e.varType)
            evaluate(e.instructions.get, mutable.HashMap((e.name, variable)), new ListBuffer[ChecklistFunction])
          case f: ChecklistFunction =>
            if (findFunction(f.name, f.args.size, globalFunctions.toList).isDefined) {
              sys.error(s"Already defined function ${f.name}${f.args}")
            }
            globalFunctions += f
          case VariableRequest(tip, varType, name) =>
            if (globalVariables.contains(name)) {
              sys.error(s"Already defined variable $name")
            }
            val evaluatedTip = evaluateLine(tip.participants, globalVariables.toMap, globalFunctions.toList)
            print(evaluatedTip + ": ")
            val variable = getVariable(varType)
            globalVariables += ((name, variable))
          case ReturnExpression(expression) =>
            if (expression.isDefined) {
              evaluateExpression(
                expression.get,
                globalVariables.toMap,
                globalFunctions.toList
              )

            }
            return
          case i: IfStatement =>
            val evaluated = evaluateExpression(
              i.expression,
              globalVariables.toMap,
              globalFunctions.toList
            )
            evaluated match {
              case Some(expression) =>
                expression match {
                  case ExpressionBoolean(bool) =>
                    if (bool) {
                      evaluate(i.instructions.get, globalVariables, globalFunctions)
                    } else if (i.onFalse.isDefined) {
                      evaluate(i.onFalse.get, globalVariables, globalFunctions)
                    }
                  case another =>
                    sys.error(s"Выражение неожиданно вернуло $another")
                }
              case None => sys.error("Неожиданно пустое выражение")
            }
          case Assignation(variable, expression) =>
            if (globalVariables contains variable) {
              val evaluated = evaluateExpression(expression, globalVariables.toMap, globalFunctions.toList)
              globalVariables(variable) = evaluated.get
            } else {
              sys.error(s"Not defined variable $variable")
            }
          case Declaration(variable, expression) =>
            if (globalVariables.contains(variable)) {
              sys.error(s"Already defined variable $variable")
            } else {
              val evaluated = evaluateExpression(expression, globalVariables.toMap, globalFunctions.toList)
              globalVariables(variable) = evaluated.get

            }
          case w: WhileStatement =>
            var evaluated = evaluateExpression(
              w.expression,
              globalVariables.toMap,
              globalFunctions.toList
            )
            while (
              evaluated match {
                case Some(expression) =>
                  expression match {
                    case ExpressionBoolean(bool) =>
                      bool
                    case another =>
                      sys.error(s"Выражение неожиданно вернуло $another")
                  }
                case None => sys.error("Неожиданно пустое выражение")
              }
            ){
              evaluate(
                w.instructions.get,
                globalVariables,
                globalFunctions
              )
              evaluated = evaluateExpression(
                w.expression,
                globalVariables.toMap,
                globalFunctions.toList
              )
            }
        }
      } catch {
        case e: Exception =>
          throw new RuntimeException(s"Ошибка на строке ${wrap.number}", e)
          //System.err.println(s"Ошибка на строке ${wrap.number}\n\t${e.getClass.getSimpleName}: ${e.getMessage}")
          return
      }
    }
  }

  private def getVariable(varType: VariableType): CompletedExpression = {
    val str = data.nextString()
    val expression =
      varType match {
        case NumberType =>
          try {
            ExpressionNumber(str.toDouble)
          } catch {
            case _: NumberFormatException =>
              sys.error(s"Expected ${varType.toString}. Actual: $str")
          }
        case StringType =>
          ExpressionWord(str)
      }
    expression
  }

  private def evaluate(lines: List[LineWrap],
                       variables: mutable.HashMap[String, CompletedExpression],
                       functions: mutable.ListBuffer[ChecklistFunction] = new ListBuffer[ChecklistFunction]
                      ): Option[CompletedExpression] = {
    for (wrap <- lines) {
      try {
        wrap.line match {
          case OutLine(participants) =>
            val line = evaluateLine(participants, concatMaps(variables.toMap, globalVariables.toMap), concatLists(functions.toList, globalFunctions.toList))
            if (!line.isEmpty) {
              println("-> " + line)
            }
          case e: VariableBlock =>
            val evaluatedTip = evaluateLine(e.tip.participants, concatMaps(variables.toMap, globalVariables.toMap), concatLists(globalFunctions.toList, functions.toList))
            print(evaluatedTip + ": ")
            val variable = getVariable(e.varType)
            evaluate(e.instructions.get, mutable.HashMap((e.name, variable)), new ListBuffer[ChecklistFunction])
          case f: ChecklistFunction =>
            if (findFunction(f.name, f.args.size, concatLists(globalFunctions.toList, functions.toList)).isDefined) {
              sys.error(s"Already defined function ${f.name}${f.args}")
            }
            functions += f
          case VariableRequest(tip, varType, name) =>
            if (concatMaps(variables.toMap, globalVariables.toMap).contains(name)) {
              sys.error(s"Already defined variable $name")
            }
            val evaluatedTip = evaluateLine(tip.participants, concatMaps(variables.toMap, globalVariables.toMap), concatLists(globalFunctions.toList, functions.toList))
            print(evaluatedTip + ": ")
            val variable = getVariable(varType)
            variables += ((name, variable))
          case ReturnExpression(expression) =>
            if (expression.isDefined) {
              return evaluateExpression(
                expression.get,
                concatMaps(variables.toMap, globalVariables.toMap),
                concatLists(functions.toList, globalFunctions.toList)
              )
            } else {
              return None
            }
          case i: IfStatement =>
            val evaluated = evaluateExpression(
              i.expression,
              concatMaps(variables.toMap, globalVariables.toMap),
              concatLists(functions.toList, globalFunctions.toList)
            )
            evaluated match {
              case Some(expression) =>
                expression match {
                  case ExpressionBoolean(bool) =>
                    if (bool) {
                      evaluate(
                        i.instructions.get,
                        {
                          val buffer = new mutable.HashMap[String, CompletedExpression]()
                          buffer ++= globalVariables
                          buffer ++= variables
                          buffer
                        },
                        {
                          val buffer = new ListBuffer[ChecklistFunction]()
                          buffer ++= globalFunctions
                          buffer ++= functions
                          buffer
                        })
                    } else if (i.onFalse.isDefined) {
                      evaluate(i.onFalse.get, globalVariables, globalFunctions)
                    }
                  case another =>
                    sys.error(s"Выражение неожиданно вернуло $another")
                }
              case None => sys.error("Неожиданно пустое выражение")
            }
          case Assignation(variable, expression) =>
            if (variables.contains(variable) || globalVariables.contains(variable)) {
              val evaluated = evaluateExpression(expression,
                concatMaps(variables.toMap, globalVariables.toMap),
                concatLists(functions.toList, globalFunctions.toList)
              )
              if (variables.contains(variable)) {
                variables(variable) = evaluated.get
              } else if (globalVariables.contains(variable)) {
                globalVariables(variable) = evaluated.get
              }
            } else {
              sys.error(s"Not defined variable $variable")
            }
          case Declaration(variable, expression) =>
            if (variables.contains(variable) || globalVariables.contains(variable)) {
              sys.error(s"Already defined variable $variable")
            } else {
              val evaluated = evaluateExpression(
                expression,
                concatMaps(variables.toMap, globalVariables.toMap),
                concatLists(functions.toList, globalFunctions.toList)
              )
              variables(variable) = evaluated.get

            }
          case w: WhileStatement =>
            val evaluated = evaluateExpression(
              w.expression,
              concatMaps(variables.toMap, globalVariables.toMap),
              concatLists(functions.toList, globalFunctions.toList)
            )
            while (
            evaluated match {
              case Some(expression) =>
                expression match {
                  case ExpressionBoolean(bool) =>
                    bool
                  case another =>
                    sys.error(s"Выражение неожиданно вернуло $another")
                }
              case None => sys.error("Неожиданно пустое выражение")
            }
            ){
              evaluate(
                w.instructions.get,
                {
                  val buffer = new mutable.HashMap[String, CompletedExpression]()
                  buffer ++= globalVariables
                  buffer ++= variables
                  buffer
                },
                {
                  val buffer = new ListBuffer[ChecklistFunction]()
                  buffer ++= globalFunctions
                  buffer ++= functions
                  buffer
                })
            }

        }
      } catch {
        case e: Exception =>
          throw new RuntimeException(s"Ошибка на строке ${wrap.number}", e)
          //System.err.println(s"Ошибка на строке ${wrap.number}\n\t${e.getClass.getSimpleName}: ${e.getMessage}")
          return None
      }
    }
    None
  }

  def concatMaps[U, V](first: Map[U, V], second: Map[U, V]): Map[U, V] = {
    val builder = mutable.Map[U, V]()
    builder ++= second
    builder ++= first
    builder.toMap
  }

  def concatLists[U](first: List[U], second: List[U]): List[U] = {
    val builder = new ListBuffer[U]()
    builder ++= first
    builder ++= second
    builder.toList
  }

  private def evaluateLine(participants: List[OutLineParticipant],
                           variables: Map[String, CompletedExpression] = Map.empty,
                           functions: List[ChecklistFunction] = List.empty
                          ): String = {
    val builder = new mutable.StringBuilder()
    for (participant <- participants) {
      participant match {
        case OutLineWord(word) =>
          builder append word
        case ExpressionCall(expression) =>
          val evaluatedOpt = evaluateExpression(expression, variables, functions)
          evaluatedOpt match {
            case Some(evaluated) =>
              evaluated match {
                case ExpressionNumber(number) =>
                  if ((number % 1.0).abs <= 0.000001) {
                    builder append number.toInt
                  } else {
                    builder append number
                  }
                case ExpressionWord(word) =>
                  builder append word
                case ExpressionBoolean(bool) =>
                  builder append bool
              }
              builder append ' '
            case None => ()
          }

      }
    }
    builder.mkString
  }

  private def evaluateExpression(expression: Expression,
                                 variables: Map[String, CompletedExpression] = Map.empty,
                                 functions: List[ChecklistFunction] = List.empty
                                ): Option[CompletedExpression] = {
    expression match {
      case BinaryOperation(left, mark, right) =>
        Some(evaluateBinary(evaluateExpression(left, variables, functions).get, mark, evaluateExpression(right, variables, functions).get))

      case n: ExpressionNumber => Some(n)

      case w: ExpressionWord => Some(w)

      case VariableInsn(name) =>
        if (variables contains name) {
          Some(variables(name))
        } else {
          sys.error(s"Not defined variable ${'"' + name + '"'}")
        }

      case FunctionInsn(name, args) =>
        val func = findFunction(name, args.size, functions)
        if (func.isDefined) {
          val argsMap = mutable.HashMap[String, CompletedExpression]()
          argsMap ++= variables
          for (i <- args.indices) {
            argsMap += ((func.get.args(i), evaluateExpression(args(i), variables, functions).get))
          }
          evaluate(func.get.instructions.get, argsMap)
        } else {
          sys.error(s"Can't find function $name")
        }

      case e: ExpressionBoolean => Some(e)
    }
  }

  private def findFunction(name: String, argsCount: Int, functions: List[ChecklistFunction]): Option[ChecklistFunction] = {
    for (function <- functions) {
      if (function.name == name && function.args.size == argsCount) {
        return Some(function)
      }
    }
    None
  }

  private def evaluateBinary(left: CompletedExpression, mark: String, right: CompletedExpression): CompletedExpression = {
    if (markProcessors contains mark) {
      markProcessors(mark).eval(left, right)
    } else {
      sys.error(s"Unsupported mark $mark")
    }
  }

}
