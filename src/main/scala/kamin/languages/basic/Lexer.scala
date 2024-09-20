package kamin.languages.basic

enum Token(val literal: String):
  case Illegal extends Token("Illegal")
  case Identifier(l: String) extends Token(l)
  case Integer(l: String) extends Token(l)
  case LeftParenthesis extends Token("(")
  case RightParenthesis extends Token(")")
  case Equal extends Token("=")
  case LessThan extends Token("<")
  case GreaterThan extends Token(">")
  case Plus extends Token("+")
  case Minus extends Token("-")
  case Asteriks extends Token("*")
  case Slash extends Token("/")
  case Define extends Token("DEFINE")
  case Print extends Token("PRINT")
  case If extends Token("IF")
  case While extends Token("WHILE")
  case Set extends Token("SET")
  case Begin extends Token("BEGIN")

class Lexer extends kamin.lexer.Lexer[Token](
    new kamin.lexer.Tokenizer[Token]:
      override def left_parenthesis: Token = Token.LeftParenthesis
      override def right_parenthesis: Token = Token.RightParenthesis
      override def number(value: String): Token = Token.Integer(value)
      override def to_token(value: String): Token =
        value.toLowerCase() match
        case "=" => kamin.languages.basic.Token.Equal
        case "+" => kamin.languages.basic.Token.Plus
        case "-" => kamin.languages.basic.Token.Minus
        case "*" => kamin.languages.basic.Token.Asteriks
        case "/" => kamin.languages.basic.Token.Slash
        case "<" => kamin.languages.basic.Token.LessThan
        case ">" => kamin.languages.basic.Token.GreaterThan
        case "define" => kamin.languages.basic.Token.Define
        case "print" => kamin.languages.basic.Token.Print
        case "if" => kamin.languages.basic.Token.If
        case "while" => kamin.languages.basic.Token.While
        case "set" => kamin.languages.basic.Token.Set
        case "begin" => kamin.languages.basic.Token.Begin
        case _ => kamin.languages.basic.Token.Identifier(value)
    )

