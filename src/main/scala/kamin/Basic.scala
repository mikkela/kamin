package kamin

import kamin.TokenType.{Define, If, LeftParenthesis, Name, RightParenthesis, While}

object BasicTokenizer extends Tokenizer
  with IntegerTokenizer with NameTokenizer:
  def toToken(s: String): Token =
    s match
      case "(" => Token(TokenType.LeftParenthesis, "(")
      case ")" => Token(TokenType.RightParenthesis, ")")
      case "=" => Token(TokenType.Equal, s)
      case "<" => Token(TokenType.LessThan, s)
      case ">" => Token(TokenType.GreaterThan, s)
      case "+" => Token(TokenType.Plus, s)
      case "-" => Token(TokenType.Minus, s)
      case "*" => Token(TokenType.Asteriks, s)
      case "/" => Token(TokenType.Slash, s)
      case "define" => Token(TokenType.Define, s.toUpperCase)
      case "print" => Token(TokenType.Print, s.toUpperCase)
      case "if" => Token(TokenType.If, s.toUpperCase)
      case "while" => Token(TokenType.While, s.toUpperCase)
      case "set" => Token(TokenType.Set, s.toUpperCase)
      case "begin" => Token(TokenType.Begin, s.toUpperCase)
      case _ if isInteger(s) => Token(TokenType.Integer, s) // If all characters are digits, it's a number
      case _ if isName(s) => Token(TokenType.Name, s) // If all characters are letters, it's an identifier
      case _ => Token(TokenType.Illegal, s) // Anything else is considered illegal

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
