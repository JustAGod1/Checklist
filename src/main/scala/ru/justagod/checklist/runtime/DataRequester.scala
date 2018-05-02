package ru.justagod.checklist.runtime

import java.util.Scanner

sealed trait DataRequester {
  def nextString(): String
}

object ConsoleDataRequester extends DataRequester {
  private val scanner = new Scanner(System.in)

  override def nextString(): String = scanner.nextLine()
}

class LinesDataRequester(lines: List[String]) extends DataRequester {
  private var pos = 0

  override def nextString(): String = {
    if (pos >= lines.size) {
      sys.error("Недостаточно комманд в файле")
    }
    val s = lines(pos)
    println(s)
    pos += 1
    s
  }
}
