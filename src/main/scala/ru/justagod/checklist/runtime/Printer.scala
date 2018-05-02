package ru.justagod.checklist.runtime

import java.util

object Printer {

  val stack = new util.Stack[String]()

  def pushSection(name: String): Unit = {
    printOffset()
    println(name + ":")
    stack.push(name)
  }
  def popSection(): Unit = {
    stack.pop()
  }
  def print(msg: String): Unit = {
    printOffset()
    Predef.print(msg)
  }

  def printOffset(): Unit = {
    for (_ <- 0 until stack.size()) {
      Predef.print("\t")
    }
  }

}
