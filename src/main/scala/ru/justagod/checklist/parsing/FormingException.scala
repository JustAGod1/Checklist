package ru.justagod.checklist.parsing
import java.io.PrintStream

class FormingException(msg: String) extends Exception(msg) {
  override def printStackTrace(s: PrintStream): Unit = {
    s.println(msg)
  }
}
