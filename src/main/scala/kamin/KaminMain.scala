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

  private def readline(firslLine: Boolean): String =
    reader.readLine(if firslLine then "-> " else " > ").replaceAll(";.*", "") // removes the comments

  def isBalanced(input: String): Boolean =
    var stack = List[Char]()

    for (char <- input)
      char match
        case '(' => stack = char :: stack // Push opening parenthesis to stack
        case ')' =>
          if (stack.isEmpty || stack.head != '(') return false // Unmatched closing parenthesis
          stack = stack.tail // Pop the matching opening parenthesis
        case _ => // Ignore other characters

    stack.isEmpty // If stack is empty, all parentheses were balanced

  given parserContext: BasicLanguageFamilyParserContext = BasicParserContext
  given environment: Environment = GlobalAndLocalScopeEnvironment()
  given functionDefinitionTable: FunctionDefinitionTable = FunctionDefinitionTable()

  val terminal = TerminalBuilder.terminal()
  val reader: LineReader = LineReaderBuilder.builder().terminal(terminal).build()
  val lexer = BasicLexer
  val parser = BasicParser

  var continue = true
  while continue do
    var input = readline(true)

    if (input == "exit") continue = false
    else
      while !isBalanced(input) do
        input = input + " " + readline(false)
      parser.parse(PeekingIterator(lexer.tokens(input))) match
        case Left(e) => error(e)
        case Right(e:ExpressionNode) =>
          evaluateExpression(e)
        case Right(f: FunctionDefinitionNode) =>
          registerFunction(f)


