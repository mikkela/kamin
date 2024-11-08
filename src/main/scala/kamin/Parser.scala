package kamin

import kamin.BasicLexer.{
  AsteriskToken, MinusToken, PlusToken, SlashToken, EqualToken, GreaterThanToken, LessThanToken, PrintToken
}
import kamin.InputNode
import kamin.TokenType.{If, LeftParenthesis, Name, RightParenthesis, While}

import scala.util.Right

trait ParserContext

trait BasicLanguageFamilyParserContext extends ParserContext:
  def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode]

private def invalidToken(token: Token): Left[String, Nothing] =
  Left(s"${token.literal} is an unexpected token")

private def invalidEndOfProgram: Either[String, Nothing] =
  Left("Invalid end of program")

private def invalidArity(expectedOperator: Token, expectedArity: Int): Left[String, Nothing] =
  Left(s"${expectedOperator.literal} requires $expectedArity arguments")

private def unexpectedError: Either[String, Nothing] =
  Left("Unexpected error")

private def checkTokensForPresence(tokens: PeekingIterator[Token], expected: TokenType*): Either[String, Seq[Token]] =
  val peeks = tokens.peek(expected.length)
  if peeks.length < expected.length then
    return invalidEndOfProgram
  expected.zip(peeks).foldLeft[Either[String, List[Token]]](Right(Nil)) {
    case (Right(acc), (expectedType, peek)) =>
      if expectedType == peek.tokenType then Right(acc :+ peek)
      else invalidToken(peek)
    case (left, _) => left // Early termination if error found
  }

private def parseListOfElements[ElementType](tokens: PeekingIterator[Token], elementParser: PeekingIterator[Token] => Either[String, ElementType]): Either[String, Seq[ElementType]] =
  var list = List.empty[ElementType]
  var peek = tokens.peek(1)
  while (peek.nonEmpty && peek.head.tokenType != RightParenthesis)
    elementParser(tokens) match
      case Left(value) => return Left(value)
      case Right(element) =>
        list = list :+ element
    peek = tokens.peek(1)
  Right(list)

private def parseFixedNumberOfElements[ElementType <: Node](tokens: PeekingIterator[Token], count: Int,
                                                                elementParser: PeekingIterator[Token] => Either[String, ElementType]):
  Either[String, Seq[ElementType]] =
    (1 to count).foldLeft(Right(Nil): Either[String, List[ElementType]]) { (acc, _) =>
      acc.flatMap { elements =>
        elementParser(tokens).map(elements :+ _)
      }
    }

trait Parser[ResultType <: InputNode, ParserContextType <: ParserContext]:
  def parse(tokens: PeekingIterator[Token])(using context: ParserContextType): Either[String, ResultType] =
    val peeking = tokens.peek(1)
    if peeking.isEmpty then
      invalidEndOfProgram
    else
      invalidToken(peeking.head)



trait FunctionDefinitionNodeParser extends Parser[FunctionDefinitionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, FunctionDefinitionNode] =
    checkTokensForPresence(tokens, LeftParenthesis, TokenType.Define) match
      case Left(_) => super.parse(tokens) // Handle fallback case directly
      case Right(_) =>
        tokens.consumeTokens(2) // Skip '(' and 'define'
        checkTokensForPresence(tokens, TokenType.Name) match
          case Right(Seq(Token(TokenType.Name, name))) =>
            tokens.consumeTokens(1)
            checkTokensForPresence(tokens, LeftParenthesis) match
              case Right(_) =>
                tokens.consumeTokens(1)
                parseListOfElements(tokens, t =>
                  t.peek(1) match
                    case Seq(Token(TokenType.Name, literal)) =>
                      tokens.consumeTokens(1)
                      Right(literal)
                    case Seq(token) => invalidToken(token)
                    case _ => invalidEndOfProgram
                ) match
                  case Right(args) =>
                    checkTokensForPresence(tokens, TokenType.RightParenthesis) match
                      case Right(_) =>
                        tokens.consumeTokens(1)
                        parseFixedNumberOfElements(tokens, 1, context.parseExpression) match
                          case Right(Seq(expression)) =>
                            checkTokensForPresence(tokens, TokenType.RightParenthesis) match
                            case Right(_) =>
                              tokens.consumeTokens(1)
                              Right(FunctionDefinitionNode(name, args, expression))
                            case Left(value) => Left(value)
                          case Right(_) => unexpectedError
                          case Left(value) => Left(value)
                      case Left(value) => Left(value)
                  case Left(value) => Left(value)
              case Left(value) => Left(value)
          case Right(_) => unexpectedError
          case Left(value) => Left(value)

trait IntegerValueExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.Integer) match
      case Right(Seq(value)) =>
        tokens.consumeTokens(1)
        Right(IntegerExpressionNode(value.literal.toInt))
      case _ => super.parse(tokens)


trait VariableExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.Name) match
      case Right(Seq(value)) =>
        tokens.consumeTokens(1)
        Right(VariableExpressionNode(value.literal))
      case _ => super.parse(tokens)

trait IfExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.If) match
      case Right(_) =>
        tokens.consumeTokens(2)
        parseFixedNumberOfElements(tokens, 3, context.parseExpression) match
          case Left(value) => Left(value)
          case Right(Seq(testExpression, consequenceExpression, alternativeExpression)) =>
            checkTokensForPresence(tokens, TokenType.RightParenthesis) match
              case Left(value) => Left(value)
              case Right(_) =>
                tokens.consumeTokens(1)
                Right(kamin.IfExpressionNode(testExpression, consequenceExpression, alternativeExpression))
          case Right(_) => unexpectedError
      case _ => super.parse(tokens)

trait WhileExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.While) match
      case Right(_) =>
        tokens.consumeTokens(2)
        parseFixedNumberOfElements(tokens, 2, context.parseExpression) match
          case Left(value) => Left(value)
          case Right(Seq(testExpr, bodyExpr)) =>
            checkTokensForPresence(tokens, TokenType.RightParenthesis) match
              case Left(value) => Left(value)
              case Right(_) =>
                tokens.consumeTokens(1)
                Right(kamin.WhileExpressionNode(testExpr, bodyExpr))
          case Right(_) => unexpectedError
      case _ => super.parse(tokens)

trait SetExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.Set) match
      case Right(_) =>
        tokens.consumeTokens(2)
        checkTokensForPresence(tokens, TokenType.Name) match
          case Right(Seq(Token(TokenType.Name, variable))) =>
            tokens.consumeTokens(1)
            parseFixedNumberOfElements(tokens, 1, context.parseExpression) match
              case Left(value) => Left(value)
              case Right(Seq(valueExpression)) =>
                checkTokensForPresence(tokens, TokenType.RightParenthesis) match
                  case Left(value) => Left(value)
                  case Right(_) =>
                    tokens.consumeTokens(1)
                    Right(kamin.SetExpressionNode(variable, valueExpression))
              case Right(_) => unexpectedError
          case Right(_) => unexpectedError
          case Left(value) => Left(value)
      case _ => super.parse(tokens)

trait BeginExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.Begin) match
      case Right(_) =>
        tokens.consumeTokens(2)
        parseListOfElements(tokens, context.parseExpression) match
          case Left(value) => Left(value)
          case Right(expressions) if expressions.nonEmpty =>
            checkTokensForPresence(tokens, TokenType.RightParenthesis) match
              case Left(value) => Left(value)
              case Right(_) => Right(BeginExpressionNode(expressions))
          case Right(_) =>
            tokens.peek(1) match
              case List(token) => invalidToken(token)
              case _ => invalidEndOfProgram
      case _ => super.parse(tokens)

def parseOperator(tokens: PeekingIterator[Token],
                  expectedOperator: Token,
                  expectedArity: Option[Int],
                  producer: (String, Seq[ExpressionNode]) => ExpressionNode,
                  context: BasicLanguageFamilyParserContext,
                  continueChain: PeekingIterator[Token] => Either[String, ExpressionNode]): Either[String, ExpressionNode] =
  checkTokensForPresence(tokens, TokenType.LeftParenthesis, expectedOperator.tokenType) match
    case Left(_) => continueChain(tokens)
    case Right(Seq(Token(_, _), Token(_, literal))) =>
      tokens.consumeTokens(2)
      parseListOfElements(tokens, t => context.parseExpression(t)) match
        case Left(value) => Left(value)
        case Right(expressions) if expressions.length == expectedArity.getOrElse(expressions.length) =>
          checkTokensForPresence(tokens, TokenType.RightParenthesis) match
            case Left(value) => Left(value)
            case Right(_) => Right(producer(literal, expressions))
        case Right(expressions) => invalidArity(expectedOperator, expectedArity.get)
    case Right(_) => unexpectedError

trait AdditionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, PlusToken, Some(2), (_, expressions) => AdditionExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait SubtractionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, MinusToken, Some(2), (_, expressions) => SubtractionExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait MultiplicationExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, AsteriskToken, Some(2), (_, expressions) => MultiplicationExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait DivisionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, SlashToken, Some(2), (_, expressions) => DivisionExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait EqualityExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, EqualToken, Some(2), (_, expressions) => EqualityExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait LessThanExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, LessThanToken, Some(2), (_, expressions) => LessThanExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait GreaterThanExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, GreaterThanToken, Some(2), (_, expressions) => GreaterThanExpressionNode(expressions.head, expressions.tail.head),
      context, tokens => super.parse(tokens)(using context))

trait PrintExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, PrintToken, Some(1), (_, expressions) => PrintExpressionNode(expressions.head),
      context, tokens => super.parse(tokens)(using context))

trait FunctionCallExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, Token(TokenType.Name, ""), None, (name, expressions) => FunctionCallExpressionNode(name, expressions),
      context, tokens => super.parse(tokens)(using context))
