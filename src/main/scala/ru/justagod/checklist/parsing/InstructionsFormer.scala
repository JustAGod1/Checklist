package ru.justagod.checklist.parsing

import ru.justagod.checklist._

import scala.collection.mutable.ListBuffer

object InstructionsFormer {

  def form(lines: List[(Int, Option[Line])]): List[LineWrap] = {
    val buffer = new ListBuffer[LineWrap]
    doForm(lines, buffer = buffer)
    buffer.toList
  }

  private def doForm(
            lines: List[(Int, Option[Line])],
            offset: Int = 0,
            indent: Int = 0,
            buffer: ListBuffer[LineWrap] = new ListBuffer[LineWrap]
          ): Int = {
    if (lines.size <= offset) {
      return offset
    }
    var off = offset

    while (off < lines.size) {

      val preOffset = off
      val (indentation, lineOpt) = lines(off)
      if (lineOpt.isDefined) {
        val line = lineOpt.get
        if (indentation < indent) return off
        if (indentation > indent) throw new FormingException(s"Invalid indentation at line ${off + 1}")
        line match {
          case withInstructions: LineWithInstructions =>
            val instructions = new ListBuffer[LineWrap]
            off = doForm(lines, off + 1, indent + 1, instructions) - 1
            withInstructions.instructions = Some(instructions.toList)
            withInstructions match {
              case elseStatement: ElseStatement =>
                if (buffer.isEmpty) {
                  throw new FormingException(s"Unexpected else statement at line ${off + 1}")
                }
                buffer.last.line match {
                  case i: IfStatement =>
                    i.onFalse = Some(elseStatement.instructions.get)
                  case _ => ()
                }
              case _ =>
            }
          case _ =>
        }
        if (!line.isInstanceOf[ElseStatement]) {
          buffer += LineWrap(preOffset + 1, line)
        }

      }
      off += 1
    }
    off
  }

}


