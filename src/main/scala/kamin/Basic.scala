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

/*class BasicParser(using fundefParser: FundefParser)(using expressionParser: ExpressionParser)
  extends Parser[FunDefNode | ExpressionNode]
    with ExtractFromOption
    with InvalidToken:
  override def parse(tokens: Iterator[Token]): Either[String, FunDefNode | ExpressionNode] =
    val rest = PeekingIterator[Token](tokens)
    val peeked = rest.peek(2)
    peeked.length match
      case 0 =>
        Left("Token stream is empty")
      case 1 =>
        for
          exprOpt <- expressionParser.parseExpression(rest)
          expr <- extract(exprOpt, "Expected expression")
        yield expr
      case 2 =>
        if peeked.apply(0).tokenType != LeftParenthesis then
          invalidToken(peeked.apply(0).literal)
        else if peeked.apply(1).tokenType == Define then
          for
            fundefOpt <- fundefParser.parseFundef(rest)
            fundef <- extract(fundefOpt, "Expected function definition")
          yield fundef
        else
          for
            exprOpt <- expressionParser.parseExpression(rest)
            expr <- extract(exprOpt, "Expected expression")
          yield expr
*/
class BasicFundefParser extends
  FundefParser
  with InvalidToken:
  override def parseFundef(peekingIterator: PeekingIterator[Token]): Either[String, Option[FunDefNode]] = ???

class BasicExpressionParser extends
  ExpressionParser
  with InvalidToken:
  override def parseExpression(peekingIterator: PeekingIterator[Token]): Either[String, Option[ExpressionNode]] =
    var tokens = peekingIterator.peek(1)
    tokens.length match
      case 0 => Right(None)
      case 1 =>
        tokens(0).tokenType match
          case TokenType.Integer =>
            peekingIterator.next()
            Right(Some(ASTValueExpressionNode(ASTValueNode(tokens(0).literal.toInt))))
          case TokenType.Name =>
            peekingIterator.next()
            Right(Some(ASTVariableExpressionNode(ASTVariableNode(tokens(0).literal))))
          case TokenType.LeftParenthesis =>
            tokens = peekingIterator.peek(2)
            if tokens.length == 2 then
              tokens(1).tokenType match
                case If => IfExpressionParser(using this).parseExpression(peekingIterator)
                case While => WhileExpressionParser(using this).parseExpression(peekingIterator)
                case _ => invalidToken(tokens(1).literal)
            else
              Right(None)
          case _ =>
            invalidToken(tokens(0).literal)
