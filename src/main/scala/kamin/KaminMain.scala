package kamin

import org.jline.reader.{LineReaderBuilder, LineReader}
import org.jline.terminal.TerminalBuilder

object KaminMain extends App:
  private def error(e: String): Unit =
    println("Error: " + e)

  private def evaluateExpression(e: ExpressionNode): Unit =
    e.evaluate match
      case Left(e) => error(e)
      case Right(value) => println(value)

  private def registerFunction(f: FunctionDefinitionNode): Unit =
    functionDefinitionTable.register(f)
    println(f.function)

  given parserContext: BasicLanguageFamilyParserContext = BasicParserContext
  given environment: Environment = GlobalAndLocalScopeEnvironment()
  given functionDefinitionTable: FunctionDefinitionTable = FunctionDefinitionTable()

  val terminal = TerminalBuilder.terminal()
  val reader: LineReader = LineReaderBuilder.builder().terminal(terminal).build()
  val lexer = BasicLexer
  val parser = BasicParser

  var continue = true
  while (continue) do
    val input = reader.readLine("> ")

    if (input == "exit") continue = false
    else
      parser.parse(PeekingIterator(lexer.tokens(input))) match
        case Left(e) => error(e)
        case Right(e:ExpressionNode) =>
          evaluateExpression(e)
        case Right(f: FunctionDefinitionNode) =>
          registerFunction(f)

