package kamin

import kamin.InputNode
import kamin.TokenType.{If, LeftParenthesis, RightParenthesis}

trait InvalidToken:
  protected def invalidToken(literal: String) =
    Left(s"${literal} is an unexpected token")

trait ExtractFromOption:
  def extract[T](maybeExpr: Option[T], errorMsg: String): Either[String, T] =
    maybeExpr.toRight(errorMsg)
  
trait Parser[InputType <: InputNode] protected ():
  def parse(tokens: Iterator[Token]) : Either[String, InputType]

trait FundefParser:
  def parseFundef(peekingIterator: PeekingIterator[Token]): Either[String, Option[FunDefNode]]

trait ExpressionParser:
  def parseExpression(peekingIterator: PeekingIterator[Token]): Either[String, Option[ExpressionNode]]

class IfExpressionParser(using expressionParser: ExpressionParser) extends ExpressionParser 
  with ExtractFromOption
  with InvalidToken:
  override def parseExpression(peekingIterator: PeekingIterator[Token]): Either[String, Option[ExpressionNode]] =
    if !peekingIterator.hasNext then return Right(None)

    def expectToken(expectedType: TokenType): Either[String, Token] =
      if !peekingIterator.hasNext then Left("Unexpected end of tokens")
      else
        val token = peekingIterator.next()
        if token.tokenType == expectedType then Right(token)
        else invalidToken(token.literal)

    def parseThreeExpressions(): Either[String, ASTIfExpressionNode] =
      for
        testExprOpt <- expressionParser.parseExpression(peekingIterator)
        testExpr <- extract(testExprOpt, "Expected test expression")
        consequenceExprOpt <- expressionParser.parseExpression(peekingIterator)
        consequenceExpr <- extract(consequenceExprOpt, "Expected consequence expression")
        alternativeExprOpt <- expressionParser.parseExpression(peekingIterator)
        alternativeExpr <- extract(alternativeExprOpt, "Expected alternative expression")
      yield ASTIfExpressionNode(testExpr, consequenceExpr, alternativeExpr)

    for
      _ <- expectToken(LeftParenthesis)
      _ <- expectToken(If)
      astIfNode <- parseThreeExpressions()
      _ <- expectToken(RightParenthesis)
    yield Some(astIfNode)
