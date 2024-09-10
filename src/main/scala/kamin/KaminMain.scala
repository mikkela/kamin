package kamin

import org.jline.reader.{LineReaderBuilder, LineReader}
import org.jline.terminal.TerminalBuilder

object KaminMain extends App {
  val terminal = TerminalBuilder.terminal()
  val reader: LineReader = LineReaderBuilder.builder().terminal(terminal).build()

  var continue = true
  while (continue) {
    val input = reader.readLine("> ")

    if (input == "exit") continue = false
    else println(s"Received input: $input")
  }
}
