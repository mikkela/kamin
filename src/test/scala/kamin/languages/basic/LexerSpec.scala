package kamin.languages.basic

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class LexerSpec extends AnyFunSpec
  with Matchers
  with TableDrivenPropertyChecks {
  private val singleTokenTable = Table(
    ("Text", "Is Token"),
    ("(", Token.LeftParenthesis),
    (")", Token.RightParenthesis),
    ("=", Token.Equal),
    ("+", Token.Plus),
    ("-", Token.Minus),
    ("*", Token.Asteriks),
    ("/", Token.Slash),
    ("<", Token.LessThan),
    (">", Token.GreaterThan),
    ("define", Token.Define),
    ("print", Token.Print),
    ("if", Token.If),
    ("while", Token.While),
    ("set", Token.Set),
    ("begin", Token.Begin),
    ("abe", Token.Identifier("abe")),
    ("-345", Token.Integer("-345")),
    ("56", Token.Integer("56"))
  )

  describe("token method") {
    it("should lex a single token correctly") {
      val lexer = Lexer()
      forAll(singleTokenTable) {
        (str, expectedToken) =>
          val it = lexer.tokens(str)
          it.hasNext shouldBe true
          it.next() shouldBe expectedToken
          it.hasNext shouldBe false
      }
    }

    it("should lex a string with multple tokens correctly") {
      val lexer = Lexer()
      val it = lexer.tokens("( ) = + - * / < > define print if while set begin abe -345 56")
      it.next() shouldBe Token.LeftParenthesis
      it.next() shouldBe Token.RightParenthesis
      it.next() shouldBe Token.Equal
      it.next() shouldBe Token.Plus
      it.next() shouldBe Token.Minus
      it.next() shouldBe Token.Asteriks
      it.next() shouldBe Token.Slash
      it.next() shouldBe Token.LessThan
      it.next() shouldBe Token.GreaterThan
      it.next() shouldBe Token.Define
      it.next() shouldBe Token.Print
      it.next() shouldBe Token.If
      it.next() shouldBe Token.While
      it.next() shouldBe Token.Set
      it.next() shouldBe Token.Begin
      it.next() shouldBe Token.Identifier("abe")
      it.next() shouldBe Token.Integer("-345")
      it.next() shouldBe Token.Integer("56")
      it.hasNext shouldBe false
    }
  }
}
