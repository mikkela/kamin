package kamin

import org.jline.reader.{LineReaderBuilder, LineReader}
import org.jline.terminal.TerminalBuilder

object KaminMain extends App:
  given parserContext: BasicLanguageFamilyParserContext = BasicParserContext

  val terminal = TerminalBuilder.terminal()
  val reader: LineReader = LineReaderBuilder.builder().terminal(terminal).build()
  val lexer = Lexer(Seq.empty, Seq.empty)

  var continue = true
  while (continue) do
    val input = reader.readLine("> ")

    if (input == "exit") continue = false
    else
      println(s"Received input: $input")
      val ast = BasicParser.parse(PeekingIterator(lexer.tokens(input)))
      println(ast)

