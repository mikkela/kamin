package kamin

import kamin.InputNode
import kamin.TokenType.{If, LeftParenthesis, Name, RightParenthesis, While}

import scala.util.Right

trait ParserContext

trait BasicLanguageFamilyParserContext extends ParserContext:
  def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode]

trait Parser[ResultType <: Node, TParserContext <: ParserContext]:
  def parse(tokens: PeekingIterator[Token])(using context: TParserContext): Either[String, ResultType] =
    if tokens.hasNext then
      invalidToken(tokens.next())
    else
      invalidEndOfProgram

  protected def invalidToken(token: Token) =
    Left(s"${token.literal} is an unexpected token")

  protected def invalidEndOfProgram: Either[String, Nothing] =
    Left("Invalid end of program")

trait OptrNodeParser extends Parser[OptrNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, OptrNode] =
    tokens.peek(1) match
      case List(Token(TokenType.Plus, literal)) => tokens.next(); Right(ASTPlusValueOperationNode())
      case List(Token(TokenType.Minus, literal)) => tokens.next(); Right(ASTMinusValueOperationNode())
      case List(Token(TokenType.Asteriks, literal)) => tokens.next(); Right(ASTMultiplicationValueOperationNode())
      case List(Token(TokenType.Slash, literal)) => tokens.next(); Right(ASTDivisionValueOperationNode())
      case List(Token(TokenType.Equal, literal)) => tokens.next(); Right(ASTEqualValueOperationNode())
      case List(Token(TokenType.LessThan, literal)) => tokens.next(); Right(ASTLessThanValueOperationNode())
      case List(Token(TokenType.GreaterThan, literal)) => tokens.next(); Right(ASTGreaterThanValueOperationNode())
      case List(Token(TokenType.Print, literal)) => tokens.next(); Right(ASTPrintValueOperationNode())
      case List(Token(Name, literal)) => tokens.next(); Right(ASTFunctionOperationNode(ASTFunctionNode(literal)))
      case _ => super.parse(tokens)


trait ValueExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    tokens.peek(1) match
      case List(Token(TokenType.Integer, literal)) =>
        tokens.next();
        Right(ASTValueExpressionNode(ASTIntegerValueNode(literal.toInt)))
      case _ => super.parse(tokens)


trait VariableExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    tokens.peek(1) match
      case List(Token(TokenType.Name, literal)) =>
        tokens.next()
        Right(ASTVariableExpressionNode(ASTVariableNode(literal)))
      case _ => super.parse(tokens)

trait CompoundExpressionParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  private def validateAndConsumeTokens(tokens: PeekingIterator[Token], expected: TokenType*): Either[String, Unit] =
    var peeks = tokens.peek(expected.length)
    if peeks.length < expected.length then
      return invalidEndOfProgram
    expected.foreach { t =>
      if t != peeks.head.tokenType then
        return invalidToken(peeks.head)
      peeks = peeks.tail
    }

    expected.foreach { _ =>
      tokens.next()
    }
    Right(())

  protected def checkOpeningParenthesis(tokens: PeekingIterator[Token], keyword: TokenType): Either[String, Unit] =
    validateAndConsumeTokens(tokens = tokens, expected = LeftParenthesis, keyword)

  protected def parseExpressions(tokens: PeekingIterator[Token], count: Int, context: BasicLanguageFamilyParserContext):
  Either[String, List[ExpressionNode]] =
    (1 to count).foldLeft(Right(Nil): Either[String, List[ExpressionNode]]) { (acc, _) =>
      acc.flatMap { exprs =>
        context.parseExpression(tokens).map(exprs :+ _)
      }
    }

  protected def parseExpressions(tokens: PeekingIterator[Token], context: BasicLanguageFamilyParserContext):
  Either[String, List[ExpressionNode]] =
    var expressions = List.empty[ExpressionNode]
    var peek = tokens.peek(1)
    while (peek.length > 0 && peek(0).tokenType != RightParenthesis)
      context.parseExpression(tokens) match
        case Left(value) => return Left(value)
        case Right(expression) =>
          expressions = expressions :+ expression
      tokens.next()
      peek = tokens.peek(1)
    Right(expressions)
  
  protected def checkClosingParenthesis(tokens: PeekingIterator[Token]): Either[String, Unit] =
    validateAndConsumeTokens(tokens = tokens, expected = RightParenthesis)

trait IfExpressionNodeParser extends CompoundExpressionParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkOpeningParenthesis(tokens, TokenType.If) match
      case Left(value) => super.parse(tokens)
      case Right(()) =>
        parseExpressions(tokens, 3, context) match
          case Left(value) => Left(value)
          case Right(List(testExpression, consequenceExpression, alternativeExpression)) =>
            checkClosingParenthesis(tokens) match
              case Left(value) => Left(value)
              case Right(()) => Right(kamin.ASTIfExpressionNode(testExpression, consequenceExpression, alternativeExpression))

trait WhileExpressionNodeParser extends CompoundExpressionParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkOpeningParenthesis(tokens, TokenType.While) match
      case Left(value) => super.parse(tokens)
      case Right(()) =>
        parseExpressions(tokens, 2, context) match
          case Left(value) => Left(value)
          case Right(List(testExpression, bodyExpression)) =>
            checkClosingParenthesis(tokens) match
              case Left(value) => Left(value)
              case Right(()) => Right(kamin.ASTWhileExpressionNode(testExpression, bodyExpression))

trait SetExpressionNodeParser extends CompoundExpressionParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkOpeningParenthesis(tokens, TokenType.Set) match
      case Left(value) => super.parse(tokens)
      case Right(()) =>
        tokens.peek(1) match
          case List(Token(TokenType.Name, variable)) =>
            tokens.next() // Consume variable
            parseExpressions(tokens, 1, context) match
              case Left(value) => Left(value)
              case Right(List(valueExpression)) =>
                checkClosingParenthesis(tokens) match
                  case Left(value) => Left(value)
                  case Right(()) => Right(kamin.ASTSetExpressionNode(ASTVariableNode(variable), valueExpression))
          case List(token) => invalidToken(token)
          case _ => invalidEndOfProgram

trait BeginExpressionNodeParser extends CompoundExpressionParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkOpeningParenthesis(tokens, TokenType.Begin) match
      case Left(value) => super.parse(tokens)
      case Right(()) =>
        parseExpressions(tokens, context) match
          case Left(value) => Left(value)
          case Right(expressions) if expressions.length > 0 =>
            checkClosingParenthesis(tokens) match
              case Left(value) => Left(value)
              case Right(()) => Right(ASTBeginExpressionNode(expressions))
          case Right(_) =>
            tokens.peek(1) match
              case List(token) => invalidToken(token)
              case _ => invalidEndOfProgram


