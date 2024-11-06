package kamin

import kamin.TokenType.{If, LeftParenthesis, Name}
import kamin.{Lexer, Token}
import org.scalatest.funspec.AnyFunSpec

class LexerSpec extends AnyFunSpec {
  
  describe("tokens method") {
    it("should ignore whitespaces so '     ' should be empty") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("     ")
      assert(!it.hasNext)
    }

    it("should treat single parts of text as a token") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("token")
      assert(it.hasNext)
      assert(it.next().literal == "token")
      assert(!it.hasNext)
    }

    it("should treat given separator char as a separator between tokens") {
      val lexer = Lexer(Seq(Token(LeftParenthesis, "(")), Seq.empty)

      val it = lexer.tokens("token1(token2")
      assert(it.hasNext)
      assert(it.next().literal == "token1")

      assert(it.hasNext)
      assert(it.next().literal == "(")

      assert(it.hasNext)
      assert(it.next().literal == "token2")
      assert(!it.hasNext)
    }
    
    it("should treat space as a separator between tokens") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("token5 token6")
      assert(it.hasNext)
      assert(it.next().literal == "token5")

      assert(it.hasNext)
      assert(it.next().literal == "token6")
      assert(!it.hasNext)
    }

    it("should treat tab as a separator between tokens") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("443\t-5676")
      assert(it.hasNext)
      assert(it.next().literal == "443")

      assert(it.hasNext)
      assert(it.next().literal == "-5676")
      assert(!it.hasNext)
    }

    it("should treat return as a separator between tokens") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("token9\rtoken0")
      assert(it.hasNext)
      assert(it.next().literal == "token9")

      assert(it.hasNext)
      assert(it.next().literal == "token0")
      assert(!it.hasNext)
    }

    it("should treat newline as a separator between tokens") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens("TokenA\nTokenB")
      assert(it.hasNext)
      assert(it.next().literal == "TokenA")

      assert(it.hasNext)
      assert(it.next().literal == "TokenB")
      assert(!it.hasNext)
    }

    it("should ignore comments until the end of line ';this is a comment' should be empty") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens(";this is a comment")
      assert(!it.hasNext)
    }

    it("should ignore comments so ';this is a comment\nx' should be the text x") {
      val lexer = Lexer(Seq.empty, Seq.empty)

      val it = lexer.tokens(";this is a comment\nx")
      assert(it.hasNext)
      assert(it.next().literal == "x")

      assert(!it.hasNext)
    }
    
    it("should match strings as given keywords with keywords") {
      val lexer1 = Lexer(Seq.empty, Seq.empty)
      val lexer2 = Lexer(Seq.empty, Seq(Token(If, "if")))

      val token1 = lexer1.tokens("if").next()
      val token2 = lexer2.tokens("if").next()
      
      assert(token1.tokenType == Name)
      assert(token1.literal == "if")
      assert(token2.tokenType == If)
      assert(token2.literal == "if")
    }
  }
}
