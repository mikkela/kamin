package kamin

import kamin.InputNode
import kamin.TokenType.{If, LeftParenthesis, RightParenthesis, While}

trait InvalidToken:
  protected def invalidToken(literal: String) =
    Left(s"${literal} is an unexpected token")

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

trait Parser[InputType <: InputNode] protected ():
  def parse(tokens: Iterator[Token]) : Either[String, InputType]

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

