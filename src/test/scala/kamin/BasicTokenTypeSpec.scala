package kamin

import kamin.{TokenType, BasicTokenizer, Token}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class BasicTokenTypeSpec extends AnyFunSpec
  with Matchers
  with TableDrivenPropertyChecks {

  private val singleTokenTable = Table(
    ("Text", "Is Token"),
    ("(", Token(TokenType.LeftParenthesis, "(")),
    (")", Token(TokenType.RightParenthesis, ")")),
    ("=", Token(TokenType.Equal, "=")),
    ("+", Token(TokenType.Plus, "+")),
    ("-", Token(TokenType.Minus, "-")),
    ("*", Token(TokenType.Asteriks, "*")),
    ("/", Token(TokenType.Slash, "/")),
    ("<", Token(TokenType.LessThan, "<")),
    (">", Token(TokenType.GreaterThan, ">")),
    ("define", Token(TokenType.Define, "DEFINE")),
    ("print", Token(TokenType.Print, "PRINT")),
    ("if", Token(TokenType.If, "IF")),
    ("while", Token(TokenType.While, "WHILE")),
    ("set", Token(TokenType.Set, "SET")),
    ("begin", Token(TokenType.Begin, "BEGIN")),
    ("abe", Token(TokenType.Name, "abe")),
    ("-345", Token(TokenType.Integer, "-345")),
    ("56", Token(TokenType.Integer, "56"))
  )

  describe("toToken method") {
      it("should convert the provided text correctly") {
        forAll(singleTokenTable) {
          (str, expectedToken) =>
            BasicTokenizer.toToken(str) shouldBe expectedToken
        }
      }
  }
}
