package kamin

import kamin.InputNode
import kamin.TokenType.{If, LeftParenthesis, Name, RightParenthesis, While}

import scala.util.Right

trait ParserContext

trait BasicLanguageFamilyParserContext extends ParserContext:
  def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode]

trait Parser[ResultType <: Node, ParserContextType <: ParserContext]:

  protected def parseFixedNumberOfElements[ElementType <: Node](tokens: PeekingIterator[Token], count: Int,
                                                                elementParser: PeekingIterator[Token] => Either[String, ElementType]):
  Either[String, Seq[ElementType]] =
    (1 to count).foldLeft(Right(Nil): Either[String, List[ElementType]]) { (acc, _) =>
      acc.flatMap { elements =>
        elementParser(tokens).map(elements :+ _)
      }
    }

  protected def parseListOfElements[ElementType <: Node](tokens: PeekingIterator[Token], elementParser: PeekingIterator[Token] => Either[String, ElementType]): Either[String, Seq[ElementType]] =
    var list = List.empty[ElementType]
    var peek = tokens.peek(1)
    while (peek.nonEmpty && peek(0).tokenType != RightParenthesis)
      elementParser(tokens) match
        case Left(value) => return Left(value)
        case Right(element) =>
          list = list :+ element
      tokens.next()
      peek = tokens.peek(1)
    Right(list)

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

  def parse(tokens: PeekingIterator[Token])(using context: ParserContextType): Either[String, ResultType] =
    println(getClass.getSimpleName)
    if tokens.hasNext then
      invalidToken(tokens.next())
    else
      invalidEndOfProgram

  protected def invalidToken(token: Token): Left[String, Nothing] =
    Left(s"${token.literal} is an unexpected token")

  protected def invalidEndOfProgram: Either[String, Nothing] =
    Left("Invalid end of program")

trait FunDefNodeParser extends Parser[FunDefNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, FunDefNode] =
    println(getClass.getSimpleName)
    checkTokensForPresence(tokens, LeftParenthesis, TokenType.Define) match
      case Left(_) => super.parse(tokens) // Handle fallback case directly
      case Right(_) =>
        tokens.consumeTokens(2) // Skip '(' and 'define'

        for
          // Parse function name
          name <- checkTokensForPresence(tokens, TokenType.Name).flatMap {
            case Seq(Token(_, name)) => Right(name)
            case Seq(token) => invalidToken(token)
          }
          _ = tokens.consumeTokens(1)

          // Parse arguments
          _ <- checkTokensForPresence(tokens, LeftParenthesis)
          _ = tokens.consumeTokens(1)
          args <- parseListOfElements[ArgumentNode](tokens, t =>
            t.peek(1) match
              case Seq(Token(TokenType.Name, literal)) => Right(ASTArgumentNode(literal))
              case Seq(token) => invalidToken(token)
              case _ => invalidEndOfProgram
          )

          // Check closing ')' after arguments
          _ <- checkTokensForPresence(tokens, TokenType.RightParenthesis)
          _ = tokens.consumeTokens(1)

          // Parse function body
          expression <- context.parseExpression(tokens)

          // Check final closing ')'
          _ <- checkTokensForPresence(tokens, TokenType.RightParenthesis)
          _ = tokens.consumeTokens(1)
        yield ASTFunDefNode(ASTFunctionNode(name), args, expression)


trait IntegerValueExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    checkTokensForPresence(tokens, TokenType.Integer) match
      case Right(Seq(value)) =>
        tokens.consumeTokens(1)
        Right(ASTValueExpressionNode(ASTIntegerValueNode(value.literal.toInt)))
      case _ => super.parse(tokens)


trait VariableExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    checkTokensForPresence(tokens, TokenType.Name) match
      case Right(Seq(value)) =>
        tokens.consumeTokens(1)
        Right(ASTVariableExpressionNode(ASTVariableNode(value.literal)))
      case _ => super.parse(tokens)

trait IfExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    for
      _ <- checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.If)
      _ = tokens.consumeTokens(2) // Skip '(' and 'if'
      expressions <- parseFixedNumberOfElements(tokens, 3, context.parseExpression)
      _ <- checkTokensForPresence(tokens, TokenType.RightParenthesis)
      _ = tokens.consumeTokens(1)
      ifNode <- expressions match
        case Seq(testExpr, consequenceExpr, alternativeExpr) =>
          Right(kamin.ASTIfExpressionNode(testExpr, consequenceExpr, alternativeExpr))
        case _ => Left("Unexpected number of expressions parsed")
    yield ifNode


trait WhileExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    for
      _ <- checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.While)
      _ = tokens.consumeTokens(2) // Skip '(' and 'while'
      expressions <- parseFixedNumberOfElements(tokens, 2, context.parseExpression)
      _ <- checkTokensForPresence(tokens, TokenType.RightParenthesis)
      _ = tokens.consumeTokens(1)
      whileNode <- expressions match
        case Seq(testExpr, bodyExpr) => Right(kamin.ASTWhileExpressionNode(testExpr, bodyExpr))
        case _ => Left("Unexpected number of expressions parsed")
    yield whileNode

trait SetExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    for
      _ <- checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.Set)
      _ = tokens.consumeTokens(2)
      nameTokens <- checkTokensForPresence(tokens, TokenType.Name)
      variable = nameTokens.headOption.collect { case Token(TokenType.Name, varName) => varName }
        .toRight("Expected a variable name")
      _ = tokens.consumeTokens(1)
      valueExprSeq <- parseFixedNumberOfElements(tokens, 1, context.parseExpression)
      valueExpr = valueExprSeq.headOption.toRight("Expected an expression")
      _ <- checkTokensForPresence(tokens, TokenType.RightParenthesis)
    yield kamin.ASTSetExpressionNode(ASTVariableNode(variable.getOrElse("")), valueExpr.getOrElse(null))

trait BeginExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    for
      _           <- checkTokensForPresence(tokens, TokenType.LeftParenthesis, TokenType.Begin)
      _           = tokens.consumeTokens(2)
      expressions <- parseListOfElements(tokens, context.parseExpression)
      _           <- if (expressions.nonEmpty) Right(()) else Left("No expressions found")
      _           <- checkTokensForPresence(tokens, TokenType.RightParenthesis)
    yield ASTBeginExpressionNode(expressions)

trait OptrExpressionNodeParser extends Parser[ExpressionNode, BasicLanguageFamilyParserContext]:
  protected def parse(tokens: PeekingIterator[Token], expectedOptrTokenType: TokenType, optrNodeProducer: String => OptrNode, context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    checkTokensForPresence(tokens, TokenType.LeftParenthesis, expectedOptrTokenType) match
      case Left(_) => super.parse(tokens)(using context)
      case Right(Seq(Token(LeftParenthesis, _), Token(expectedOptrTokenType, literal))) =>
        tokens.consumeTokens(2)
        parseListOfElements(tokens, t => context.parseExpression(t)) match
          case Left(value) => Left(value)
          case Right(expressions)  =>
            checkTokensForPresence(tokens, TokenType.RightParenthesis) match
              case Left(value) => Left(value)
              case Right(_) => Right(ASTOptrExpressionNode(optrNodeProducer(literal), expressions))

trait PlusExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    parse(tokens, TokenType.Plus, _ => ASTPlusValueOperationNode(), context)

trait MinusExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    parse(tokens, TokenType.Minus, _ => ASTMinusValueOperationNode(), context)

trait MultiplicationExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    parse(tokens, TokenType.Asteriks, _ =>ASTMultiplicationValueOperationNode(), context)

trait DivisionExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    parse(tokens, TokenType.Slash, _ => ASTDivisionValueOperationNode(), context)

trait EqualExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    parse(tokens, TokenType.Equal, _ => ASTEqualValueOperationNode(), context)

trait LessThanExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    parse(tokens, TokenType.LessThan, _ => ASTLessThanValueOperationNode(), context)

trait GreaterThanExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    parse(tokens, TokenType.GreaterThan, _ => ASTGreaterThanValueOperationNode(), context)

trait PrintExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    parse(tokens, TokenType.Print, _ => ASTPrintValueOperationNode(), context)

trait FunctionCallExpressionNodeParser extends OptrExpressionNodeParser:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, ExpressionNode] =
    println(getClass.getSimpleName)
    parse(tokens, TokenType.Name, literal => ASTFunctionOperationNode(ASTFunctionNode(literal)), context)
