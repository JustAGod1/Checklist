package ru.justagod.checklist.runtime

import ru.justagod.checklist.{CompletedExpression, ExpressionBoolean, ExpressionNumber, ExpressionWord}

trait MarkProcessors {

  abstract class MarkProcessor {
    def eval(left: CompletedExpression, right: CompletedExpression): CompletedExpression
  }

  class PlusProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionNumber(lvalue + rvalue)
            case ExpressionWord(rvalue) =>
              ExpressionWord(lvalue + rvalue)
          }

        case ExpressionWord(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionWord(lvalue + rvalue)
            case ExpressionWord(rvalue) =>
              ExpressionWord(lvalue + rvalue)
          }
      }
    }
  }

  class MinusProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionNumber(lvalue - rvalue)
            case ExpressionWord(rvalue) =>
              sys.error("Can't substrate word from number")
          }

        case ExpressionWord(lvalue) =>
          right match {
            case _ =>
              sys.error("Can't substrate from word")

          }
      }
    }
  }

  class PercentProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionNumber(lvalue % rvalue)
            case ExpressionWord(rvalue) =>
              sys.error("Can't find remainder from number by word")
          }
        case ExpressionWord(lvalue) =>
          right match {
            case _ =>
              sys.error("Can't find remainder from word")

          }
      }
    }
  }

  class AsteriskProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionNumber(lvalue * rvalue)
            case ExpressionWord(rvalue) =>
              sys.error("Can't multiply number by word")
          }

        case ExpressionWord(lvalue) =>
          right match {
            case _ =>
              sys.error("Can't multiply word")

          }
      }
    }
  }

  class DividerProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionNumber(lvalue / rvalue)
            case ExpressionWord(rvalue) =>
              sys.error("Can't divide number by word")
          }

        case ExpressionWord(lvalue) =>
          right match {
            case _ =>
              sys.error("Can't divide word")

          }
      }
    }
  }

  class LessProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionBoolean(lvalue < rvalue)
            case ExpressionWord(rvalue) =>
              sys.error("Can't compare number and word")
          }

        case ExpressionWord(lvalue) =>
          right match {
            case _ =>
              sys.error("Can't divide word")

          }
      }
    }
  }

  class MoreProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionBoolean(lvalue > rvalue)
            case ExpressionWord(rvalue) =>
              sys.error("Can't compare number and word")
          }

        case ExpressionWord(lvalue) =>
          right match {
            case _ =>
              sys.error("Can't divide word")

          }
      }
    }
  }

  class MoreOrEqualProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionBoolean(lvalue >= rvalue)
            case ExpressionWord(rvalue) =>
              sys.error("Can't compare number and word")
          }

        case ExpressionWord(lvalue) =>
          right match {
            case _ =>
              sys.error("Can't divide word")

          }
      }
    }
  }

  class LessOrEqualProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionBoolean(lvalue <= rvalue)
            case ExpressionWord(rvalue) =>
              sys.error("Can't compare number and word")
          }

        case ExpressionWord(lvalue) =>
          right match {
            case _ =>
              sys.error("Can't divide word")

          }
      }
    }
  }

  class NotEqualProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionBoolean(lvalue != rvalue)
            case ExpressionWord(_) =>
              sys.error("Can't compare word and number")
            case ExpressionBoolean(_) =>
              sys.error("Can't compare boolean and number")
          }
        case ExpressionWord(lvalue) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't compare word and number")
            case ExpressionWord(rvalue) =>
              ExpressionBoolean(rvalue != lvalue)
            case ExpressionBoolean(_) =>
              sys.error("Can't compare boolean and word")
          }
        case ExpressionBoolean(lvalue) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't compare boolean and number")
            case ExpressionWord(_) =>
              sys.error("Can't compare boolean and word")
            case ExpressionBoolean(rvalue) =>
              ExpressionBoolean(rvalue != lvalue)
          }
      }
    }
  }

  class EqualProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(lvalue) =>
          right match {
            case ExpressionNumber(rvalue) =>
              ExpressionBoolean(lvalue == rvalue)
            case ExpressionWord(_) =>
              sys.error("Can't compare word and number")
            case ExpressionBoolean(_) =>
              sys.error("Can't compare boolean and number")
          }
        case ExpressionWord(lvalue) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't compare word and number")
            case ExpressionWord(rvalue) =>
              ExpressionBoolean(rvalue == lvalue)
            case ExpressionBoolean(_) =>
              sys.error("Can't compare boolean and word")
          }
        case ExpressionBoolean(lvalue) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't compare boolean and number")
            case ExpressionWord(_) =>
              sys.error("Can't compare boolean and word")
            case ExpressionBoolean(rvalue) =>
              ExpressionBoolean(rvalue == lvalue)
          }
      }
    }
  }

  class AndProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(_) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't perform and between numbers")
            case ExpressionWord(_) =>
              sys.error("Can't compare word and number")
            case ExpressionBoolean(_) =>
              sys.error("Can't compare boolean and number")
          }
        case ExpressionWord(_) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't compare word and number")
            case ExpressionWord(_) =>
              sys.error("Can't perform and between strings")
            case ExpressionBoolean(_) =>
              sys.error("Can't compare boolean and word")
          }
        case ExpressionBoolean(lvalue) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't compare boolean and number")
            case ExpressionWord(_) =>
              sys.error("Can't compare boolean and word")
            case ExpressionBoolean(rvalue) =>
              ExpressionBoolean(rvalue && lvalue)
          }
      }
    }
  }

  class OrProcessor extends MarkProcessor {
    override def eval(left: CompletedExpression, right: CompletedExpression) = {
      left match {
        case ExpressionNumber(_) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't perform or between numbers")
            case ExpressionWord(_) =>
              sys.error("Can't compare word and number")
            case ExpressionBoolean(_) =>
              sys.error("Can't compare boolean and number")
          }
        case ExpressionWord(_) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't compare word and number")
            case ExpressionWord(_) =>
              sys.error("Can't perform or between strings")
            case ExpressionBoolean(_) =>
              sys.error("Can't compare boolean and word")
          }
        case ExpressionBoolean(lvalue) =>
          right match {
            case ExpressionNumber(_) =>
              sys.error("Can't compare boolean and number")
            case ExpressionWord(_) =>
              sys.error("Can't compare boolean and word")
            case ExpressionBoolean(rvalue) =>
              ExpressionBoolean(rvalue || lvalue)
          }
      }
    }
  }

}
