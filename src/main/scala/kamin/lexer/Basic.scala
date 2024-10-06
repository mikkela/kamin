package kamin.lexer

enum BasicTokenType:
  case Illegal, Identifier, Integer, LeftParenthesis, RightParenthesis, Equal, LessThan, GreaterThan, Plus, Minus, Asteriks, Slash, Define, Print, If, While, Set, Begin

object BasicTokenizer extends Tokenizer[BasicTokenType]
with IntegerTokenizer with IdentifierTokenizer:
  def toToken(s: String): Token[BasicTokenType] =
    s match
      case "(" => Token(BasicTokenType.LeftParenthesis, "(")
      case ")" => Token(BasicTokenType.RightParenthesis, ")")
      case "=" => Token(BasicTokenType.Equal, s)
      case "<" => Token(BasicTokenType.LessThan, s)
      case ">" => Token(BasicTokenType.GreaterThan, s)
      case "+" => Token(BasicTokenType.Plus, s)
      case "-" => Token(BasicTokenType.Minus, s)
      case "*" => Token(BasicTokenType.Asteriks, s)
      case "/" => Token(BasicTokenType.Slash, s)
      case "define" => Token(BasicTokenType.Define, s.toUpperCase)
      case "print" => Token(BasicTokenType.Print, s.toUpperCase)
      case "if" => Token(BasicTokenType.If, s.toUpperCase)
      case "while" => Token(BasicTokenType.While, s.toUpperCase)
      case "set" => Token(BasicTokenType.Set, s.toUpperCase)
      case "begin" => Token(BasicTokenType.Begin, s.toUpperCase)
      case _ if isInteger(s) => Token(BasicTokenType.Integer, s) // If all characters are digits, it's a number
      case _ if isIdentifier(s) => Token(BasicTokenType.Identifier, s) // If all characters are letters, it's an identifier
      case _ => Token(BasicTokenType.Illegal, s) // Anything else is considered illegal

