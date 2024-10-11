package kamin

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BasicParserSpec extends AnyFunSpec with Matchers {
  /*private val lexer = Lexer(using BasicTokenizer)
  private val parser = BasicParser(using BasicFundefParser())(using BasicExpressionParser())
  describe("parse method") {
    it("should return an error message when presented with an empty string") {
      val result = parser.parse(lexer.tokens(""))

      result shouldBe Left("Token stream is empty")
    }

    it("should return an error message when parsing an integer and then some") {
      val result = parser.parse(PeekingIterator[Token](lexer.tokens("3542 !")))

      result shouldBe Left("3542 is an unexpected token")
    }

    it("should return an error message when parsing a name and then some") {
      val result = parser.parse(lexer.tokens("foo 345"))

      result shouldBe Left("foo is an unexpected token")
    }
  }*/
}
