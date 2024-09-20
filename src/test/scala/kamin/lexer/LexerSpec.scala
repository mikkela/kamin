package kamin.lexer

import org.scalatest.funspec.AnyFunSpec

class LexerSpec extends AnyFunSpec {
  enum Token(val text: String):
    case LPAR extends Token("(")
    case RPAR extends Token(")")
    case NUMBER(n: String) extends Token(n)
    case TEXT(t: String) extends Token(t)

  describe("tokens method") {
    it("should ignore whitespaces so '     ' should be empty") {
      val lexer = Lexer[Token](
        new Tokenizer[Token]:
          override def to_token(s: String): Token = ???

          override def left_parenthesis: Token = ???

          override def right_parenthesis: Token = ???

          override def number(s: String): Token = ???
      )

      val it = lexer.tokens("     ")
      assert(!it.hasNext)
    }

    it("should return left parenthesis when tokens is called with '('") {
      val lexer = Lexer[Token](
        new Tokenizer[Token]:
          override def left_parenthesis: Token = Token.LPAR

          override def right_parenthesis: Token = ???
          override def number(s: String): Token = ???
          override def to_token(s: String): Token = ???
        )

      val it = lexer.tokens("(")
      assert(it.hasNext)
      assert(it.next() == Token.LPAR)
      assert(!it.hasNext)
    }

    it("should return right parenthesis when tokens is called with ')'") {
      val lexer = Lexer[Token](
        new Tokenizer[Token]:
          override def right_parenthesis: Token = Token.RPAR

          override def left_parenthesis: Token = ???
          override def number(s: String): Token = ???
          override def to_token(s: String): Token = ???
      )

      val it = lexer.tokens(")")
      assert(it.hasNext)
      assert(it.next() == Token.RPAR)
      assert(!it.hasNext)
    }

    it("should return number when tokens is called with 9999") {
      val lexer = Lexer[Token](
        new Tokenizer[Token]:
          override def number(s: String): Token = Token.NUMBER(s)

          override def left_parenthesis: Token = ???
          override def right_parenthesis: Token = ???
          override def to_token(s: String): Token = ???
      )

      val it = lexer.tokens("9999")
      assert(it.hasNext)
      assert(it.next() == Token.NUMBER("9999"))
      assert(!it.hasNext)
    }

    it("should return number when tokens is called with -6432") {
      val lexer = Lexer[Token](
        new Tokenizer[Token]:
          override def number(s: String): Token = Token.NUMBER(s)

          override def left_parenthesis: Token = ???

          override def right_parenthesis: Token = ???

          override def to_token(s: String): Token = ???
      )

      val it = lexer.tokens("-6432")
      assert(it.hasNext)
      assert(it.next() == Token.NUMBER("-6432"))
      assert(!it.hasNext)
    }

    it("should return text when tokens is called with '-ttffde'") {
      val lexer = Lexer[Token](
        new Tokenizer[Token]:
          override def to_token(s: String): Token = Token.TEXT(s)

          override def left_parenthesis: Token = ???

          override def right_parenthesis: Token = ???

          override def number(s: String): Token = ???
      )

      val it = lexer.tokens("-ttffde")
      assert(it.hasNext)
      assert(it.next() == Token.TEXT("-ttffde"))
      assert(!it.hasNext)
    }

    it("should return several tokens when each part is separated '(+(* 455 -556) x)'") {
      val lexer = Lexer[Token](
        new Tokenizer[Token]:
          override def to_token(s: String): Token = Token.TEXT(s)

          override def left_parenthesis: Token = Token.LPAR

          override def right_parenthesis: Token = Token.RPAR

          override def number(s: String): Token = Token.NUMBER(s)
      )

      val it = lexer.tokens("(+( ** 455 -556) x)")
      assert(it.hasNext)
      assert(it.next() == Token.LPAR)

      assert(it.hasNext)
      assert(it.next() == Token.TEXT("+"))

      assert(it.hasNext)
      assert(it.next() == Token.LPAR)

      assert(it.hasNext)
      assert(it.next() == Token.TEXT("**"))

      assert(it.hasNext)
      assert(it.next() == Token.NUMBER("455"))

      assert(it.hasNext)
      assert(it.next() == Token.NUMBER("-556"))

      assert(it.hasNext)
      assert(it.next() == Token.RPAR)

      assert(it.hasNext)
      assert(it.next() == Token.TEXT("x"))

      assert(it.hasNext)
      assert(it.next() == Token.RPAR)

      assert(!it.hasNext)
    }

    it("should ignore comments until the end of line '#this is a comment' should be empty") {
      val lexer = Lexer[Token](
        new Tokenizer[Token]:
          override def to_token(s: String): Token = ???

          override def left_parenthesis: Token = ???

          override def right_parenthesis: Token = ???

          override def number(s: String): Token = ???
      )

      val it = lexer.tokens("#this is a comment")
      assert(!it.hasNext)
    }
    
    it("should ignore comments so '#this is a comment\nx' should be the text x") {
      val lexer = Lexer[Token](
        new Tokenizer[Token]:
          override def to_token(s: String): Token = Token.TEXT(s)
          override def left_parenthesis: Token = ???
          override def right_parenthesis: Token = ???
          override def number(s: String): Token = ???
      )

      val it = lexer.tokens("#this is a comment\nx")
      assert(it.hasNext)
      assert(it.next() == Token.TEXT("x"))

      assert(!it.hasNext)
    }
  }
}
