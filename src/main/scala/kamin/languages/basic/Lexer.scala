package kamin.languages.basic

enum Token(val literal: String):
  case Illegal extends Token("Illegal")
  case EoF extends Token("EoF")
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

class Lexer(i: String) extends kamin.lexer.Lexer[Token](i) {
  override protected def illegal(): Token = kamin.languages.basic.Token.Illegal
  override protected def eof(): Token = kamin.languages.basic.Token.EoF
  override protected def leftParenthesis(): Token = kamin.languages.basic.Token.LeftParenthesis
  override protected def rightParenthesis(): Token = kamin.languages.basic.Token.RightParenthesis
  override protected def integer(value: String): Token = kamin.languages.basic.Token.Integer(value)

  override protected def lookup(value: String): Token =
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

}
