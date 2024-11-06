package kamin

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

trait Parser[ResultType <: Node, ParserContextType <: ParserContext]:
  def parse(tokens: PeekingIterator[Token])(using context: ParserContextType): Either[String, ResultType] =
    val peeking = tokens.peek(1)
    if peeking.isEmpty then
      invalidEndOfProgram
    else
      invalidToken(peeking.head)



trait FunDefNodeParser extends Parser[FunDefNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, FunDefNode] =
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
                              Right(FunDefNode(name, args, expression))
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
        Right(ValueExpressionNode(IntegerValueNode(value.literal.toInt)))
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

def parseOperator(tokens: PeekingIterator[Token], expectedOptrTokenType: TokenType, context: BasicLanguageFamilyParserContext, continueChain: PeekingIterator[Token] => Either[String, ExpressionNode]): Either[String, ExpressionNode] =
  checkTokensForPresence(tokens, TokenType.LeftParenthesis, expectedOptrTokenType) match
    case Left(_) => continueChain(tokens)
    case Right(Seq(Token(LeftParenthesis, _), Token(expectedOptrTokenType, literal))) =>
      tokens.consumeTokens(2)
      parseListOfElements(tokens, t => context.parseExpression(t)) match
        case Left(value) => Left(value)
        case Right(expressions) =>
          checkTokensForPresence(tokens, TokenType.RightParenthesis) match
            case Left(value) => Left(value)
            case Right(_) => Right(FunctionCallExpressionNode(literal, expressions))
    case Right(_) => unexpectedError

trait PlusExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, TokenType.Plus, context, tokens => super.parse(tokens)(using context))

trait MinusExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, TokenType.Minus, context, tokens => super.parse(tokens)(using context))

trait MultiplicationExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, TokenType.Asterisk, context, tokens => super.parse(tokens)(using context))

trait DivisionExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, TokenType.Slash, context, tokens => super.parse(tokens)(using context))

trait EqualExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, TokenType.Equal, context, tokens => super.parse(tokens)(using context))

trait LessThanExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, TokenType.LessThan, context, tokens => super.parse(tokens)(using context))

trait GreaterThanExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, TokenType.GreaterThan, context, tokens => super.parse(tokens)(using context))

trait PrintExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, TokenType.Print, context, tokens => super.parse(tokens)(using context))

trait FunctionCallExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parseOperator(tokens, TokenType.Name, context, tokens => super.parse(tokens)(using context))
