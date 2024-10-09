package kamin

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BasicExpressionParserSpec extends AnyFunSpec with Matchers{
  private val lexer = Lexer(using BasicTokenizer)
  private val parser = BasicExpressionParser()
  describe("parseExpression method") {
    it("should return a value node when parsing an integer") {
      val result = parser.parseExpression(PeekingIterator[Token](lexer.tokens("3542")))

      result shouldBe Right(Some(ASTValueExpressionNode(ASTValueNode(3542))))
    }

    it("should return a value node when parsing a name") {
      val result = parser.parseExpression(PeekingIterator[Token](lexer.tokens("foo")))
      result shouldBe Right(Some(ASTVariableExpressionNode(ASTVariableNode("foo"))))
    }

    it("should return an if node when parsing a valid if expression") {
      val result = parser.parseExpression(PeekingIterator[Token](lexer.tokens("(if foo 123 poo)")))

      result shouldBe Right(Some(
        ASTIfExpressionNode(
          ASTVariableExpressionNode(ASTVariableNode("foo")),
          ASTValueExpressionNode(ASTValueNode(123)),
          ASTVariableExpressionNode(ASTVariableNode("poo"))
        )))
    }

    it("should return an error when parsing an invalid if expression") {
      val result = parser.parseExpression(PeekingIterator[Token](lexer.tokens("(if foo 123)")))

      result shouldBe Left(") is an unexpected token")
    }

    it("should return an error when parsing an incomplete if expression") {
      val result = parser.parseExpression(PeekingIterator[Token](lexer.tokens("(if foo 123")))

      result shouldBe Left("Expected alternative expression")
    }

    it("should return an while node when parsing a valid while expression") {
      val result = parser.parseExpression(PeekingIterator[Token](lexer.tokens("(while foo -123)")))

      result shouldBe Right(Some(
        ASTWhileExpressionNode(
          ASTVariableExpressionNode(ASTVariableNode("foo")),
          ASTValueExpressionNode(ASTValueNode(-123))
        )))
    }

    it("should return an error when parsing an invalid while expression") {
      val result = parser.parseExpression(PeekingIterator[Token](lexer.tokens("(while foo )")))

      result shouldBe Left(") is an unexpected token")
    }

    it("should return an error when parsing an incomplete while expression") {
      val result = parser.parseExpression(PeekingIterator[Token](lexer.tokens("(while foo")))

      result shouldBe Left("Expected body expression")
    }
  }
}
