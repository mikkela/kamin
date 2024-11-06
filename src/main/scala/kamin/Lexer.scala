package kamin

import kamin.TokenType.If

enum TokenType:
  case Illegal, Name, Integer, LeftParenthesis, RightParenthesis, Equal, LessThan, GreaterThan, Plus, Minus, Asteriks, Slash, Define, Print, If, While, Set, Begin


case class Token(tokenType: TokenType, literal: String)

class Lexer(separators: => Seq[Token], keywords: => Seq[Token]):
  private def isInteger(s: String): Boolean = s.head == '-' && s.tail.forall(_.isDigit) || s.forall(_.isDigit)

  private def isName(s: String): Boolean = s.forall(c => c.isLetter || isAllowedSpecialChar(c))

  private def isAllowedSpecialChar(c: Char): Boolean =
    !Set(';', '(', ')').contains(c) && !c.isWhitespace

  def tokens(input: String):Iterator[Token] =
    var position = 0

    def currentChar: Char =
      if position < input.length then input(position) else '\u0000'

    def nextChar: Char =
      if position + 1 < input.length then input(position + 1) else '\u0000'

    def isSeparator(c: Char): Boolean =
      c == '\u0000' || c.isWhitespace || separators.exists(_.literal == c.toString)

    def advance(): Unit = position += 1

    def isEndOfLine: Boolean =
      position >= input.length

    def skipComments(): Unit =
      if currentChar == ';' then
        while !isEndOfLine && currentChar != '\n' && currentChar != '\r' do advance()
        skipWhitespaces()

    def skipWhitespaces(): Unit =
      while currentChar.isWhitespace do advance()

    def toToken(text: String): Token =
      text match
        case _ if separators.exists(_.literal == text) => separators.find(_.literal == text).get
        case _ if keywords.exists(_.literal == text) => keywords.find(_.literal == text).get
        case _ if isInteger(text) => Token(TokenType.Integer, text)
        case _ if isName(text) => Token(TokenType.Name, text)
        case _ => Token(TokenType.Illegal, text)

    def lexToken(): Token =
      val start = position
      while !isSeparator(currentChar) do advance()
      val text = input.substring(start, position)
      toToken(text)

    new Iterator[Token]:
      override def hasNext: Boolean =
        skipWhitespaces()
        skipComments()
        !isEndOfLine

      override def next(): Token =
        skipWhitespaces()
        skipComments()

        currentChar match
          case c if separators.exists(_.literal == c.toString) =>
            advance()
            toToken(c.toString)
          case _ => lexToken()


  object LeftParenthesisToken extends Token(TokenType.LeftParenthesis, "(")
  object RightParenthesisToken extends Token(TokenType.RightParenthesis, ")")
  object EqualToken extends Token(TokenType.Equal, "=")
  object LessThanToken extends Token(TokenType.LessThan, "<")
  object GreaterThanToken extends Token(TokenType.GreaterThan, ">")
  object PlusToken extends Token(TokenType.Plus, "+")
  object MinusToken extends Token(TokenType.Minus, "-")
  object AsteriksToken extends Token(TokenType.Asteriks, "*")
  object SlashToken extends Token(TokenType.Slash, "/")
  object DefineToken extends Token(TokenType.Define, "define")
  object PrintToken extends Token(TokenType.Print, "print")
  object IfToken extends Token(If, "if")
  object WhileToken extends Token(TokenType.While, "while")
  object SetToken extends Token(TokenType.Set, "set")
  object BeginToken extends Token(TokenType.Begin, "begin")


