package kamin.lexer

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class BasicTokenTypeSpec extends AnyFunSpec
  with Matchers
  with TableDrivenPropertyChecks {

  private val singleTokenTable = Table(
    ("Text", "Is Token"),
    ("(", Token(BasicTokenType.LeftParenthesis, "(")),
    (")", Token(BasicTokenType.RightParenthesis, ")")),
    ("=", Token(BasicTokenType.Equal, "=")),
    ("+", Token(BasicTokenType.Plus, "+")),
    ("-", Token(BasicTokenType.Minus, "-")),
    ("*", Token(BasicTokenType.Asteriks, "*")),
    ("/", Token(BasicTokenType.Slash, "/")),
    ("<", Token(BasicTokenType.LessThan, "<")),
    (">", Token(BasicTokenType.GreaterThan, ">")),
    ("define", Token(BasicTokenType.Define, "DEFINE")),
    ("print", Token(BasicTokenType.Print, "PRINT")),
    ("if", Token(BasicTokenType.If, "IF")),
    ("while", Token(BasicTokenType.While, "WHILE")),
    ("set", Token(BasicTokenType.Set, "SET")),
    ("begin", Token(BasicTokenType.Begin, "BEGIN")),
    ("abe", Token(BasicTokenType.Identifier, "abe")),
    ("-345", Token(BasicTokenType.Integer, "-345")),
    ("56", Token(BasicTokenType.Integer, "56"))
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
