package kamin.languages.basic

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.mutable.ListBuffer

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
    ("abe", Token.Identifier("abe") ),
    ("-345", Token.Integer("-345") ),
    ("56", Token.Integer("56") )
  )

  describe("nextToken method") {
    it("should lex a single token correctly") {
      forAll(singleTokenTable) {
        (str, expectedToken) =>
          Lexer(str).nextToken() shouldBe expectedToken
      }
    }

    it("should lex a string with multple tokens correctly") {
      val lexer = Lexer("( ) = + - * / < > define print if while set begin abe -345 56")
      lexer.nextToken() shouldBe Token.LeftParenthesis
      lexer.nextToken() shouldBe Token.RightParenthesis
      lexer.nextToken() shouldBe Token.Equal
      lexer.nextToken() shouldBe Token.Plus
      lexer.nextToken() shouldBe Token.Minus
      lexer.nextToken() shouldBe Token.Asteriks
      lexer.nextToken() shouldBe Token.Slash
      lexer.nextToken() shouldBe Token.LessThan
      lexer.nextToken() shouldBe Token.GreaterThan
      lexer.nextToken() shouldBe Token.Define
      lexer.nextToken() shouldBe Token.Print
      lexer.nextToken() shouldBe Token.If
      lexer.nextToken() shouldBe Token.While
      lexer.nextToken() shouldBe Token.Set
      lexer.nextToken() shouldBe Token.Begin
      lexer.nextToken() shouldBe Token.Identifier("abe")
      lexer.nextToken() shouldBe Token.Integer("-345")
      lexer.nextToken() shouldBe Token.Integer("56")
      lexer.nextToken() shouldBe Token.EoF
    }

    it("should lex using left parenthesis as separator") {
      val lexer = Lexer("++(")
      lexer.nextToken() shouldBe Token.Identifier("++")
      lexer.nextToken() shouldBe Token.LeftParenthesis
      lexer.nextToken() shouldBe Token.EoF
    }

    it("should lex using right parenthesis as separator") {
      val lexer = Lexer(")--")
      lexer.nextToken() shouldBe Token.RightParenthesis
      lexer.nextToken() shouldBe Token.Identifier("--")
      lexer.nextToken() shouldBe Token.EoF
    }

    it("should lex using space as separator") {
      val lexer = Lexer(" <> ")
      lexer.nextToken() shouldBe Token.Identifier("<>")
      lexer.nextToken() shouldBe Token.EoF
    }

    it("should lex not using any other as separator") {
      val lexer = Lexer("!=")
      lexer.nextToken() shouldBe Token.Identifier("!=")
      lexer.nextToken() shouldBe Token.EoF
    }

    it("should lex ignoring comments") {
      val lexer = Lexer("be#gin")
      lexer.nextToken() shouldBe Token.Identifier("be")
      lexer.nextToken() shouldBe Token.EoF
    }

    it("should lex ignoring comments until end of line") {
      val lexer = Lexer("be#ignore\ngin")
      lexer.nextToken() shouldBe Token.Identifier("be")
      lexer.nextToken() shouldBe Token.Identifier("gin")
      lexer.nextToken() shouldBe Token.EoF
    }
  }
}
