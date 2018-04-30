package ru.justagod.checklist

import java.nio.file.{Files, Path, Paths}

import joptsimple.OptionParser
import ru.justagod.checklist.parsing.{ChecklistParser, InstructionsFormer}
import ru.justagod.checklist.parsing.ChecklistParser.lexical
import ru.justagod.checklist.runtime.{ChecklistEvaluator, ConsoleDataRequester, LinesDataRequester}

import scala.collection.JavaConverters._

object Main {

  private val parser = {
    val parser = new OptionParser()
    parser.accepts("lines")
    parser.accepts("formed")
    val commandsOption = parser.accepts("commands")
    val interactiveOption = parser.accepts("interactive")
    commandsOption.requiredUnless("interactive").withRequiredArg()
    interactiveOption.requiredUnless("commands")
    parser.accepts("f").withRequiredArg()
    parser
  }

  def main(args: Array[String]): Unit = {
    val options = parser.parse(args: _*)
    assert(options.has("f"))
    val file = Paths.get(options.valueOf("f").toString)
    val in = if (options.has("interactive")) {
      ConsoleDataRequester
    } else {
      val commands = Paths.get(options.valueOf("commands").toString)
      new LinesDataRequester(Files.readAllLines(commands).asScala.toList)
    }
    val parsed = ChecklistParser.phrase(ChecklistParser.checklist)(new lexical.Scanner('\n' + readFileFully(file).trim))
    if(!parsed.successful) {
      println(parsed)
    } else {
      var number = 1
      if (options.has("lines")) {
        for ((indentation, line) <- parsed.get) {
          println(s"$number: $indentation -> ${line.getOrElse(None)}")
          number += 1
        }
        println()
      }
      val formed = try {
        InstructionsFormer.form(parsed.get)
      } catch {
        case e: Exception =>
          e.printStackTrace()
          return
      }
      if (options.has("formed")) {
        println("----------FORMED----------")
        printFormed(formed)
        println()
      }
      new ChecklistEvaluator(in).evaluateGlobal(formed)
    }

  }

  def printFormed(lines: List[LineWrap], offset: String = ""): Unit = {
    for (line <- lines) {
      println(s"$offset${line.number}: ${line.line}")
      line.line match {
        case i: LineWithInstructions =>
          if (i.instructions.isDefined) {
            printFormed(i.instructions.get, offset + '\t')
          }
          i match {
            case i: IfStatement =>

              if (i.onFalse.isDefined) {
                println(offset + "  or")
                printFormed(i.onFalse.get, offset + '\t')
              }
            case _ => ()
          }
        case _ => ()
       }
    }
  }

  def readFileFully(path: Path): String = {
    val builder = new StringBuilder()
    Files.readAllLines(path).asScala.foreach({
      l =>
        builder.append(l)
        builder.append('\n')
    })
    builder.toString()
  }


}
