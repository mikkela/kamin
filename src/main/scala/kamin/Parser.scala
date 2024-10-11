package kamin

import kamin.InputNode
import kamin.TokenType.{If, LeftParenthesis, RightParenthesis, While}

trait InvalidToken:
  protected def invalidToken(literal: String) =
    Left(s"${literal} is an unexpected token")

trait InvalidEndOfProgram
  def invalidEndOfProgram: Either[String, Nothing] =
    Left("Invalid end of program")

trait ExtractFromOption:
  def extract[T](maybeExpr: Option[T], errorMsg: String): Either[String, T] =
    maybeExpr.toRight(errorMsg)

trait ExpectToken extends InvalidToken:
  def expectToken(peekingIterator: PeekingIterator[Token], expectedType: TokenType): Either[String, Token] =
    if !peekingIterator.hasNext then Left("Unexpected end of tokens")
    else
      val token = peekingIterator.next()
      if token.tokenType == expectedType then Right(token)
      else invalidToken(token.literal)

abstract class Parser[InputType <: InputNode] (private val inputParsers: MultiKeyContainer[TokenType, InputParser[InputType]])
  extends InvalidToken
  with InvalidEndOfProgram:
  def parse(tokens: Iterator[Token]) : Either[String, Option[InputType]] =
    val peekingIterator = PeekingIterator[Token](tokens)

    val peek = peekingIterator.peek(1)
    if peek.isEmpty then
      return invalidEndOfProgram

    var potentialParsers = inputParsers.getAllWithPrefix(peek.map(_.tokenType): _*)
    potentialParsers.length match
      case 0 =>
        invalidToken(peek.head.literal)
      case 1 =>
        potentialParsers.head.parseInput(peekingIterator)
      case 2 =>
        val peekTwo = peekingIterator.peek(2)
        if peekTwo.length == 1 then
          return invalidToken(peekTwo.head.literal)

        potentialParsers = inputParsers.getAllWithPrefix(peekTwo.map(_.tokenType): _*)

        if potentialParsers.isEmpty then
          // Does any fit just with the first key?
          val potentialParser = inputParsers.get(peek.map(_.tokenType): _*)
          if potentialParser.isEmpty then
            return invalidToken(peekTwo(1).literal)
          return potentialParser.get.parseInput(peekingIterator)

        potentialParsers.head.parseInput(peekingIterator)

trait InputParser[InputType <: InputNode]:
  def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputType]]


trait FundefParser:
  def parseFundef(peekingIterator: PeekingIterator[Token]): Either[String, Option[FunDefNode]]

trait ExpressionParser:
  def parseExpression(peekingIterator: PeekingIterator[Token]): Either[String, Option[ExpressionNode]]

class IfExpressionParser(using expressionParser: ExpressionParser) extends ExpressionParser
  with ExpectToken
  with ExtractFromOption
  with InvalidToken:
  override def parseExpression(peekingIterator: PeekingIterator[Token]): Either[String, Option[ExpressionNode]] =
    if !peekingIterator.hasNext then return Right(None)

    def parseIfExpression(): Either[String, ASTIfExpressionNode] =
      for
        testExprOpt <- expressionParser.parseExpression(peekingIterator)
        testExpr <- extract(testExprOpt, "Expected test expression")
        consequenceExprOpt <- expressionParser.parseExpression(peekingIterator)
        consequenceExpr <- extract(consequenceExprOpt, "Expected consequence expression")
        alternativeExprOpt <- expressionParser.parseExpression(peekingIterator)
        alternativeExpr <- extract(alternativeExprOpt, "Expected alternative expression")
      yield ASTIfExpressionNode(testExpr, consequenceExpr, alternativeExpr)

    for
      _ <- expectToken(peekingIterator, LeftParenthesis)
      _ <- expectToken(peekingIterator, If)
      astIfNode <- parseIfExpression()
      _ <- expectToken(peekingIterator, RightParenthesis)
    yield Some(astIfNode)

class WhileExpressionParser(using expressionParser: ExpressionParser) extends ExpressionParser
  with ExpectToken
  with ExtractFromOption
  with InvalidToken:
  override def parseExpression(peekingIterator: PeekingIterator[Token]): Either[String, Option[ExpressionNode]] =
    if !peekingIterator.hasNext then return Right(None)

    def parseWhileExpression(): Either[String, ASTWhileExpressionNode] =
      for
        testExprOpt <- expressionParser.parseExpression(peekingIterator)
        testExpr <- extract(testExprOpt, "Expected test expression")
        bodyExprOpt <- expressionParser.parseExpression(peekingIterator)
        bodyExpr <- extract(bodyExprOpt, "Expected body expression")
      yield ASTWhileExpressionNode(testExpr, bodyExpr)

    for
      _ <- expectToken(peekingIterator, LeftParenthesis)
      _ <- expectToken(peekingIterator, While)
      astIfNode <- parseWhileExpression()
      _ <- expectToken(peekingIterator, RightParenthesis)
    yield Some(astIfNode)

