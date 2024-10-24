package kamin

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

class ParserSpec extends AnyFunSpec
  with Matchers
  with MockitoSugar
  with TableDrivenPropertyChecks {
  describe("A Parser") {
    it("should return an error when presented with an empty token stream") {
      val sut = new Parser[Node, ParserContext] {}

      sut.parse(PeekingIterator[Token](Seq.empty.iterator))(using context = null) shouldBe Left("Invalid end of program")
    }

    it("should return an error when presented with an invalid token as the first part") {
      val sut = new Parser[Node, ParserContext] {}

      sut.parse(PeekingIterator[Token](Seq(Token(TokenType.RightParenthesis, ")")).iterator))(using context = null) shouldBe Left(") is an unexpected token")
    }
  }

  describe("An optr node parser") {
    it("should return a plus value node when + is the next token") {
      val sut = new OptrNodeParser {}

      sut.parse(PeekingIterator(Seq(Token(TokenType.Plus, "+")).iterator))(using context = null) shouldBe Right(ASTPlusValueOperationNode())
    }

    it("should return a minus value node when - is the next token") {
      val sut = new OptrNodeParser {}

      sut.parse(PeekingIterator(Seq(Token(TokenType.Minus, "-")).iterator))(using context = null) shouldBe Right(ASTMinusValueOperationNode())
    }

    it("should return a multiplication value node when * is the next token") {
      val sut = new OptrNodeParser {}

      sut.parse(PeekingIterator(Seq(Token(TokenType.Asteriks, "*")).iterator))(using context = null) shouldBe Right(ASTMultiplicationValueOperationNode())
    }

    it("should return a division value node when / is the next token") {
      val sut = new OptrNodeParser {}

      sut.parse(PeekingIterator(Seq(Token(TokenType.Slash, "/")).iterator))(using context = null) shouldBe Right(ASTDivisionValueOperationNode())
    }

    it("should return a equality value node when / is the next token") {
      val sut = new OptrNodeParser {}

      sut.parse(PeekingIterator(Seq(Token(TokenType.Equal, "=")).iterator))(using context = null) shouldBe Right(ASTEqualValueOperationNode())
    }

    it("should return a less than value node when < is the next token") {
      val sut = new OptrNodeParser {}

      sut.parse(PeekingIterator(Seq(Token(TokenType.LessThan, "<")).iterator))(using context = null) shouldBe Right(ASTLessThanValueOperationNode())
    }

    it("should return a greater than value node when > is the next token") {
      val sut = new OptrNodeParser {}

      sut.parse(PeekingIterator(Seq(Token(TokenType.GreaterThan, ">")).iterator))(using context = null) shouldBe Right(ASTGreaterThanValueOperationNode())
    }

    it("should return a print value node when print is the next token") {
      val sut = new OptrNodeParser {}

      sut.parse(PeekingIterator(Seq(Token(TokenType.Print, "PRINT")).iterator))(using context = null) shouldBe Right(ASTPrintValueOperationNode())
    }

    it("should return a function node when name is the next token") {
      val sut = new OptrNodeParser {}

      sut.parse(PeekingIterator(Seq(Token(TokenType.Name, "myFoo")).iterator))(using context = null) shouldBe Right(ASTFunctionOperationNode(ASTFunctionNode("myFoo")))
    }
  }

  describe("An integer value expression node parser") {
    it("should return a value node expression when presented with a valid integer") {
      val peekingIterator = PeekingIterator(Seq(Token(TokenType.Integer, "96575")).iterator)
      val sut = new IntegerValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Right(ASTValueExpressionNode(ASTIntegerValueNode(96575)))
    }

    it("should return an error when presented with a non-integer") {
      val peekingIterator = PeekingIterator(Seq(Token(TokenType.Name, "HH96575")).iterator)
      val sut = new IntegerValueExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Left("HH96575 is an unexpected token")
    }
  }

  describe("An variable expression node parser") {
    it("should return a variable node expression when presented with a valid name") {
      val peekingIterator = PeekingIterator(Seq(Token(TokenType.Name, "myFoo")).iterator)

      val sut = new VariableExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Right(ASTVariableExpressionNode(ASTVariableNode("myFoo")))
    }

    it("should return an error when presented with an invalid name") {
      val peekingIterator = PeekingIterator(Seq(Token(TokenType.Integer, "96575")).iterator)
      val sut = new VariableExpressionNodeParser {}

      sut.parse(peekingIterator)(using context = null) shouldBe Left("96575 is an unexpected token")
    }
  }

  describe("An if expression node parser") {
    it("should return a if expression node expression when presented with a valid if construction") {
      val testExpression = mock[ExpressionNode]
      val consequenceExpression = mock[ExpressionNode]
      val alternativeExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(consequenceExpression), Right(alternativeExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.If, "IF"),
        Token(TokenType.RightParenthesis, ")")
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(ASTIfExpressionNode(testExpression, consequenceExpression, alternativeExpression))
    }

    it("should return an error when presented with an if construction not closed") {
      val testExpression = mock[ExpressionNode]
      val consequenceExpression = mock[ExpressionNode]
      val alternativeExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(consequenceExpression), Right(alternativeExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.If, "IF")
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Invalid end of program")
    }

    it("should return an error when presented with an if construction not properly closed with correct token") {
      val testExpression = mock[ExpressionNode]
      val consequenceExpression = mock[ExpressionNode]
      val alternativeExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(consequenceExpression), Right(alternativeExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.If, "IF"), Token(TokenType.Plus, "+")
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("+ is an unexpected token")
    }

    it("should return an error when the alternative expression is not parsed correctly") {
      val testExpression = mock[ExpressionNode]
      val consequenceExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(consequenceExpression), Left("Failed due to problem in alternative expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.If, "IF")
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in alternative expression")
    }

    it("should return an error when the consequence expression is not parsed correctly") {
      val testExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Left("Failed due to problem in consequence expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.If, "IF")
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in consequence expression")
    }

    it("should return an error when the test expression is not parsed correctly") {
      val results = Seq(Left("Failed due to problem in test expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.If, "IF")
      ).iterator)
      val sut = new IfExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in test expression")
    }
  }

  describe("A while expression node parser") {
    it("should return a while expression node expression when presented with a valid while construction") {
      val testExpression = mock[ExpressionNode]
      val bodyExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(bodyExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.While, "WHILE"),
        Token(TokenType.RightParenthesis, ")")
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(ASTWhileExpressionNode(testExpression, bodyExpression))
    }

    it("should return an error when presented with an while construction not closed") {
      val testExpression = mock[ExpressionNode]
      val bodyExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(bodyExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.While, "WHILE")
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Invalid end of program")
    }

    it("should return an error when presented with an while construction not properly closed with correct token") {
      val testExpression = mock[ExpressionNode]
      val bodyExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Right(bodyExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.While, "WHILE"), Token(TokenType.Minus, "-")
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("- is an unexpected token")
    }

    it("should return an error when the body expression is not parsed correctly") {
      val testExpression = mock[ExpressionNode]
      val results = Seq(Right(testExpression), Left("Failed due to problem in body expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.While, "WHILE")
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in body expression")
    }

    it("should return an error when the test expression is not parsed correctly") {
      val results = Seq(Left("Failed due to problem in test expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.While, "WHILE")
      ).iterator)
      val sut = new WhileExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in test expression")
    }
  }

  describe("A set expression node parser") {
    it("should return a set expression node expression when presented with a valid set construction") {
      val valueExpression = mock[ExpressionNode]
      val results = Seq(Right(valueExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.Set, "SET"),
        Token(TokenType.Name, "foo"),
        Token(TokenType.RightParenthesis, ")")
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(ASTSetExpressionNode(ASTVariableNode("foo"), valueExpression))
    }

    it("should return an error when presented with a set construction not closed") {
      val valueExpression = mock[ExpressionNode]
      val results = Seq(Right(valueExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.Set, "SET"),
        Token(TokenType.Name, "foo")
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Invalid end of program")
    }

    it("should return an error when presented with an set construction not properly closed with correct token") {
      val valueExpression = mock[ExpressionNode]
      val results = Seq(Right(valueExpression), Left("Failed due to many calls")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.Set, "SET"),
        Token(TokenType.Name, "foo"),
        Token(TokenType.Asteriks, "*")
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("* is an unexpected token")
    }

    it("should return an error when the value expression is not parsed correctly") {
      val results = Seq(Left("Failed due to problem in value expression")).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.Set, "SET"),
        Token(TokenType.Name, "foo")
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Failed due to problem in value expression")
    }

    it("should return an error when the variable part is not parsed correctly") {
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.Set, "SET"),
        Token(TokenType.Slash, "/")
      ).iterator)
      val sut = new SetExpressionNodeParser {}

      sut.parse(peekingIterator)(using null) shouldBe Left("/ is an unexpected token")
    }
  }

  describe("A begin expression node parser") {
    it("should return a begin expression node expression when presented with a valid begin construction and list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]
      val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.Begin, "BEGIN"),
        Token(TokenType.Name, "eaten"), Token(TokenType.Name, "eaten"), Token(TokenType.Name, "eaten"),
        Token(TokenType.RightParenthesis, ")")
      ).iterator)
      val sut = new BeginExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(ASTBeginExpressionNode(List(expression1, expression2, expression3)))
    }

    it("should return a begin expression node expression when presented with a valid begin construction and single expression") {
      val expression = mock[ExpressionNode]
      val results = Seq(Right(expression)).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.Begin, "BEGIN"),
        Token(TokenType.Name, "eaten"),
        Token(TokenType.RightParenthesis, ")")
      ).iterator)
      val sut = new BeginExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Right(ASTBeginExpressionNode(List(expression)))
    }

    it("should return an unexpected token with a begin construction and no expressions") {
      val results = Seq().iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.Begin, "BEGIN"),
        Token(TokenType.RightParenthesis, ")")
      ).iterator)
      val sut = new BeginExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left(") is an unexpected token")
    }

    it("should return the error when an expression fails") {
      val results = Seq(Right(mock[ExpressionNode]), Left("Something went wrong in parsing"), Right(mock[ExpressionNode])).iterator
      val context = new BasicLanguageFamilyParserContext {
        override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
          results.next()
      }
      val peekingIterator = PeekingIterator(Seq(
        Token(TokenType.LeftParenthesis, "("), Token(TokenType.Begin, "BEGIN"),
        Token(TokenType.Name, "eaten"), Token(TokenType.Name, "eaten"),
        Token(TokenType.RightParenthesis, ")")
      ).iterator)
      val sut = new BeginExpressionNodeParser {}

      sut.parse(peekingIterator)(using context) shouldBe Left("Something went wrong in parsing")
    }
  }

  private val optrTable = Table(
    ("Parser", "Token", "Expected Optr Value Node"),
    (new PlusExpressionNodeParser{}, Token(TokenType.Plus, "OPERAND"), ASTPlusValueOperationNode()),
    (new MinusExpressionNodeParser{}, Token(TokenType.Minus, "OPERAND"), ASTMinusValueOperationNode()),
    (new MultiplicationExpressionNodeParser{}, Token(TokenType.Asteriks, "OPERAND"), ASTMultiplicationValueOperationNode()),
    (new DivisionExpressionNodeParser {}, Token(TokenType.Slash, "OPERAND"), ASTDivisionValueOperationNode()),
    (new EqualExpressionNodeParser{}, Token(TokenType.Equal, "OPERAND"), ASTEqualValueOperationNode()),
    (new LessThanExpressionNodeParser{}, Token(TokenType.LessThan, "OPERAND"), ASTLessThanValueOperationNode()),
    (new GreaterThanExpressionNodeParser{}, Token(TokenType.GreaterThan, "OPERAND"), ASTGreaterThanValueOperationNode()),
    (new PrintExpressionNodeParser{}, Token(TokenType.Print, "OPERAND"), ASTPrintValueOperationNode()),
    (new FunctionCallExpressionNodeParser{}, Token(TokenType.Name, "foo"), ASTFunctionOperationNode(kamin.ASTFunctionNode("foo")))
  )

  describe("Optr expression node parsers") {
    it("should return a optr expression node expression when presented with a valid optr construction and list of expressions") {
      val expression1 = mock[ExpressionNode]
      val expression2 = mock[ExpressionNode]
      val expression3 = mock[ExpressionNode]

      forAll(optrTable) {
        (parser, token, optrValueNode) =>
          val results = Seq(Right(expression1), Right(expression2), Right(expression3)).iterator
          val context = new BasicLanguageFamilyParserContext {
            override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
              results.next()
          }

          val peekingIterator = PeekingIterator(Seq(
            Token(TokenType.LeftParenthesis, "("), token,
            Token(TokenType.Name, "eaten"), Token(TokenType.Name, "eaten"), Token(TokenType.Name, "eaten"),
            Token(TokenType.RightParenthesis, ")")
          ).iterator)

          val sut = parser

          sut.parse(peekingIterator)(using context) shouldBe Right(ASTOptrExpressionNode(optrValueNode, Seq(expression1, expression2, expression3)))
      }
    }

    it("should return a optr expression node expression when presented with a valid optr construction and single expression") {
      val expression = mock[ExpressionNode]

      forAll(optrTable) {
        (parser, token, optrValueNode) =>
          val results = Seq(Right(expression)).iterator
          val context = new BasicLanguageFamilyParserContext {
            override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
              results.next()
          }

          val peekingIterator = PeekingIterator(Seq(
            Token(TokenType.LeftParenthesis, "("), token,
            Token(TokenType.Name, "eaten"),
            Token(TokenType.RightParenthesis, ")")
          ).iterator)

          val sut = parser

          sut.parse(peekingIterator)(using context) shouldBe Right(ASTOptrExpressionNode(optrValueNode, Seq(expression)))
      }
    }

    it("should return a optr expression node expression when presented with a valid optr construction and no expressions") {
      forAll(optrTable) {
        (parser, token, optrValueNode) =>
          val results = Seq.empty.iterator
          val context = new BasicLanguageFamilyParserContext {
            override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
              results.next()
          }

          val peekingIterator = PeekingIterator(Seq(
            Token(TokenType.LeftParenthesis, "("), token,
            Token(TokenType.RightParenthesis, ")")
          ).iterator)

          val sut = parser

          sut.parse(peekingIterator)(using context) shouldBe Right(ASTOptrExpressionNode(optrValueNode, Seq.empty))
      }
    }

    it("should return the error when an expression parsing fails") {
      forAll(optrTable) {
        (parser, token, optrValueNode) =>
          val results = Seq(Right(mock[ExpressionNode]), Left("Something went wrong in parsing"), Right(mock[ExpressionNode])).iterator
          val context = new BasicLanguageFamilyParserContext {
            override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
              results.next()
          }

          val peekingIterator = PeekingIterator(Seq(
            Token(TokenType.LeftParenthesis, "("), token,
            Token(TokenType.Name, "eaten"), Token(TokenType.Name, "eaten"),
            Token(TokenType.RightParenthesis, ")")
          ).iterator)

          val sut = parser

          sut.parse(peekingIterator)(using context) shouldBe Left("Something went wrong in parsing")
      }
    }
  }
}
