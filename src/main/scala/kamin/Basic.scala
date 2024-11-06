package kamin

import kamin.BasicLexer.{LeftParenthesisToken, RightParenthesisToken, EqualToken, LessThanToken, GreaterThanToken, 
  PlusToken, MinusToken, AsteriksToken, SlashToken, PrintToken, DefineToken, IfToken, WhileToken, SetToken, BeginToken}

import kamin.TokenType.{Define, If, LeftParenthesis, Name, RightParenthesis, While}

object BasicLexer extends Lexer(
  Seq(LeftParenthesisToken, RightParenthesisToken),
  Seq(EqualToken, LessThanToken, GreaterThanToken, PlusToken, MinusToken, AsteriksToken, SlashToken, PrintToken,
    DefineToken, IfToken, WhileToken, SetToken, BeginToken)
)

object BasicFunDefNodeParser extends FunDefNodeParser

object BasicExpressionNodeParser extends IntegerValueExpressionNodeParser
  with VariableExpressionNodeParser
  with IfExpressionNodeParser
  with WhileExpressionNodeParser
  with SetExpressionNodeParser
  with BeginExpressionNodeParser
  with PlusExpressionNodeParser
  with MinusExpressionNodeParser
  with MultiplicationExpressionNodeParser
  with DivisionExpressionNodeParser
  with EqualExpressionNodeParser
  with LessThanExpressionNodeParser
  with GreaterThanExpressionNodeParser
  with PrintExpressionNodeParser
  with FunctionCallExpressionNodeParser

object BasicParser extends Parser[FunDefNode | ExpressionNode, BasicLanguageFamilyParserContext]:
  override def parse(tokens: PeekingIterator[Token])(using context: BasicLanguageFamilyParserContext): Either[String, FunDefNode | ExpressionNode] =
    BasicFunDefNodeParser.parse(tokens) match
      case Right(value) => Right(value)
      case Left(_) => BasicExpressionNodeParser.parse(tokens)

object BasicParserContext extends BasicLanguageFamilyParserContext:
  override def parseExpression(tokens: PeekingIterator[Token]): Either[String, ExpressionNode] =
    BasicExpressionNodeParser.parse(tokens)(using this)
