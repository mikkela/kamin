package kamin

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFunSpec with Matchers{
  describe("A Parser") {
    it("should return an error when presented with an empty token stream") {
      val sut = new Parser[InputNode](MultiKeyContainer()) {}

      sut.parse(Seq.empty.iterator) shouldBe Left("Invalid end of program")
    }

    it("should return the parse result when presented with token stream starting with the correct token") {
      object inputNode extends InputNode {}
      val result = Right(Some(inputNode))
      val parsers = MultiKeyContainer[TokenType, InputParser[InputNode]]()
      val parser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] =
          result
      parsers.put(TokenType.Name)(parser)
      val sut = new Parser[InputNode](parsers) {}

      sut.parse(Seq(Token(TokenType.Name, "foo")).iterator) shouldBe result
    }

    it("should return invalid token when presented with token stream starting with an invalid token") {
      object inputNode extends InputNode {}
      val result = Right(Some(inputNode))
      val parsers = MultiKeyContainer[TokenType, InputParser[InputNode]]()
      val parser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] =
          result
      parsers.put(TokenType.Name)(parser)
      val sut = new Parser[InputNode](parsers) {}

      sut.parse(Seq(Token(TokenType.Integer, "345")).iterator) shouldBe Left("345 is an unexpected token")
    }

    it("should return the correct parse result when presented with token stream starting with a valid token") {
      object inputNode extends InputNode {}
      val result = Right(Some(inputNode))
      val parsers = MultiKeyContainer[TokenType, InputParser[InputNode]]()
      val correctParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] =
          result
      val invalidParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] = ???
      parsers.put(TokenType.Name)(correctParser)
      parsers.put(TokenType.LeftParenthesis, TokenType.If)(invalidParser)
      val sut = new Parser[InputNode](parsers) {}

      sut.parse(Seq(Token(TokenType.Name, "foo")).iterator) shouldBe result
    }

    it("should return the correct parse result when presented with token stream starting with two valid token forming the key") {
      object inputNode extends InputNode {}
      val result = Right(Some(inputNode))
      val parsers = MultiKeyContainer[TokenType, InputParser[InputNode]]()
      val correctParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] =
          result
      val invalidParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] = ???
      parsers.put(TokenType.Name)(invalidParser)
      parsers.put(TokenType.LeftParenthesis, TokenType.If)(correctParser)
      val sut = new Parser[InputNode](parsers) {}

      sut.parse(Seq(Token(TokenType.LeftParenthesis, "("), Token(TokenType.If, "IF")).iterator) shouldBe result
    }

    it("should return the correct parse result when presented with token stream starting with two valid token forming the key even when keys share the first token") {
      object inputNode extends InputNode {}
      val result = Right(Some(inputNode))
      val parsers = MultiKeyContainer[TokenType, InputParser[InputNode]]()
      val correctParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] =
          result
      val invalidParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] = ???
      parsers.put(TokenType.LeftParenthesis, TokenType.While)(invalidParser)
      parsers.put(TokenType.LeftParenthesis, TokenType.If)(correctParser)
      val sut = new Parser[InputNode](parsers) {}

      sut.parse(Seq(Token(TokenType.LeftParenthesis, "("), Token(TokenType.If, "IF")).iterator) shouldBe result
    }

    it("should return the correct parse result of the parser with the longest matching key sequence") {
      object inputNode extends InputNode {}
      val result = Right(Some(inputNode))
      val parsers = MultiKeyContainer[TokenType, InputParser[InputNode]]()
      val correctParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] =
          result
      val invalidParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] = ???
      parsers.put(TokenType.LeftParenthesis)(invalidParser)
      parsers.put(TokenType.LeftParenthesis, TokenType.If)(correctParser)
      val sut = new Parser[InputNode](parsers) {}

      sut.parse(Seq(Token(TokenType.LeftParenthesis, "("), Token(TokenType.If, "IF")).iterator) shouldBe result
    }

    it("should return the correct parse result of the parser with shortest matching key sequence if no longer fits") {
      object inputNode extends InputNode {}
      val result = Right(Some(inputNode))
      val parsers = MultiKeyContainer[TokenType, InputParser[InputNode]]()
      val correctParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] =
          result
      val invalidParser = new InputParser[InputNode]:
        override def parseInput(peekingIterator: PeekingIterator[Token]): Either[String, Option[InputNode]] = ???
      parsers.put(TokenType.LeftParenthesis)(correctParser)
      parsers.put(TokenType.LeftParenthesis, TokenType.If)(invalidParser)
      val sut = new Parser[InputNode](parsers) {}

      sut.parse(Seq(Token(TokenType.LeftParenthesis, "("), Token(TokenType.While, "WHILE")).iterator) shouldBe result
    }
  }
}
