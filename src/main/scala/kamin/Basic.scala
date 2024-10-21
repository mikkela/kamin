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

