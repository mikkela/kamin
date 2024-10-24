package kamin

import kamin.InputNode
import kamin.TokenType.{If, LeftParenthesis, Name, RightParenthesis, While}

import scala.util.Right

trait ParserContext

trait BasicLanguageFamilyParserContext extends ParserContext:
  def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode]

  def parseFixedNumberOfExpressions(tokens: PeekingIterator[Token], count: Int):
  Either[String, Seq[ExpressionNode]] =
    (1 to count).foldLeft(Right(Nil): Either[String, List[ExpressionNode]]) { (acc, _) =>
      acc.flatMap { exprs =>
        parseExpression(tokens).map(exprs :+ _)
      }
    }

  def parseExpressions(tokens: PeekingIterator[Token]): Either[String, Seq[ExpressionNode]] =
    var expressions = List.empty[ExpressionNode]
    var peek = tokens.peek(1)
    while (peek.nonEmpty && peek(0).tokenType != RightParenthesis)
      parseExpression(tokens) match
        case Left(value) => return Left(value)
        case Right(expression) =>
          expressions = expressions :+ expression
      tokens.next()
      peek = tokens.peek(1)
    Right(expressions)

trait Parser[ResultType <: Node, TParserContext <: ParserContext]:
  protected def checkTokensForPresence(tokens: PeekingIterator[Token], expected: TokenType*) : Either[String, Seq[Token]] =
    var peeks = tokens.peek(expected.length)
    if peeks.length < expected.length then
      return invalidEndOfProgram
    expected.zip(peeks).foldLeft[Either[String, List[Token]]](Right(Nil)) {
      case (Right(acc), (expectedType, peek)) =>
        if expectedType == peek.tokenType then Right(acc :+ peek)
        else invalidToken(peek)
      case (left, _) => left // Early termination if error found
    }

  def parse(tokens: PeekingIterator[Token])(using context: TParserContext): Either[String, ResultType] =
    if tokens.hasNext then
      invalidToken(tokens.next())
    else
      invalidEndOfProgram

  protected def invalidToken(token: Token): Left[String, Nothing] =
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


trait IntegerValueExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.Integer) match
      case Right(Seq(value)) =>
        tokens.consumeTokens(1)
        Right(ASTValueExpressionNode(ASTIntegerValueNode(value.literal.toInt)))
      case _ => super.parse(tokens)


trait VariableExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.Name) match
      case Right(Seq(value)) =>
        tokens.consumeTokens(1)
        Right(ASTVariableExpressionNode(ASTVariableNode(value.literal)))
      case _ => super.parse(tokens)

trait IfExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.If) match
      case Left(_) => super.parse(tokens)
      case Right(_) =>
        tokens.consumeTokens(2)
        context.parseFixedNumberOfExpressions(tokens, 3) match
          case Left(value) => Left(value)
          case Right(Seq(testExpression, consequenceExpression, alternativeExpression)) =>
            checkTokensForPresence(tokens, TokenType.RightParenthesis) match
              case Left(value) => Left(value)
              case Right(_) =>
                tokens.consumeTokens(1)
                Right(kamin.ASTIfExpressionNode(testExpression, consequenceExpression, alternativeExpression))

trait WhileExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.While) match
      case Left(_) => super.parse(tokens)
      case Right(_) =>
        tokens.consumeTokens(2)
        context.parseFixedNumberOfExpressions(tokens, 2) match
          case Left(value) => Left(value)
          case Right(Seq(testExpression, bodyExpression)) =>
            checkTokensForPresence(tokens, TokenType.RightParenthesis) match
              case Left(value) => Left(value)
              case Right(_) =>
                tokens.consumeTokens(1)
                Right(kamin.ASTWhileExpressionNode(testExpression, bodyExpression))

trait SetExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.Set) match
      case Left(_) => super.parse(tokens)
      case Right(_) =>
        tokens.consumeTokens(2)
        checkTokensForPresence(tokens, TokenType.Name) match
          case Right(Seq(Token(TokenType.Name, variable))) =>
            tokens.consumeTokens(1)
            context.parseFixedNumberOfExpressions(tokens, 1) match
              case Left(value) =>
                Left(value)
              case Right(Seq(valueExpression)) =>
                checkTokensForPresence(tokens, TokenType.RightParenthesis) match
                  case Left(value) => Left(value)
                  case Right(_) => Right(kamin.ASTSetExpressionNode(ASTVariableNode(variable), valueExpression))
          case Left(value) => Left(value)

trait BeginExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.Begin) match
      case Left(_) => super.parse(tokens)
      case Right(_) =>
        tokens.consumeTokens(2)
        context.parseExpressions(tokens) match
          case Left(value) => Left(value)
          case Right(expressions) if expressions.length > 0 =>
            checkTokensForPresence(tokens, TokenType.RightParenthesis) match
              case Left(value) => Left(value)
              case Right(_) => Right(ASTBeginExpressionNode(expressions))
          case Right(_) =>
            tokens.peek(1) match
              case List(token) => invalidToken(token)
              case _ => invalidEndOfProgram

trait OptrExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  protected def parse(tokens: PeekingIterator[Token], expectedOptrTokenType: TokenType, optrNodeProducer: String => OptrNode, context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, expectedOptrTokenType) match
      case Left(_) => super.parse(tokens)(using context)
      case Right(Seq(Token(LeftParenthesis, _), Token(expectedOptrTokenType, literal))) =>
        tokens.consumeTokens(2)
        context.parseExpressions(tokens) match
          case Left(value) => Left(value)
          case Right(expressions)  =>
            checkTokensForPresence(tokens, TokenType.RightParenthesis) match
              case Left(value) => Left(value)
              case Right(_) => Right(ASTOptrExpressionNode(optrNodeProducer(literal), expressions))

trait PlusExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parse(tokens, TokenType.Plus, _ => ASTPlusValueOperationNode(), context)

trait MinusExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parse(tokens, TokenType.Minus, _ => ASTMinusValueOperationNode(), context)

trait MultiplicationExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parse(tokens, TokenType.Asteriks, _ =>ASTMultiplicationValueOperationNode(), context)

trait DivisionExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parse(tokens, TokenType.Slash, _ => ASTDivisionValueOperationNode(), context)

trait EqualExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parse(tokens, TokenType.Equal, _ => ASTEqualValueOperationNode(), context)

trait LessThanExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parse(tokens, TokenType.LessThan, _ => ASTLessThanValueOperationNode(), context)

trait GreaterThanExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parse(tokens, TokenType.GreaterThan, _ => ASTGreaterThanValueOperationNode(), context)

trait PrintExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parse(tokens, TokenType.Print, _ => ASTPrintValueOperationNode(), context)

trait FunctionCallExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    parse(tokens, TokenType.Name, literal => ASTFunctionOperationNode(ASTFunctionNode(literal)), context)
