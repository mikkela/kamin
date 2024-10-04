package kamin.lexer

case class Token[TokenType](tokenType: TokenType, literal: String)

trait Tokenizer[TokenType]:
  def toToken(s: String): Token[TokenType]

class Lexer[TokenType](using tokenizer: Tokenizer[TokenType]):
  def tokens(input: String):Iterator[Token[TokenType]] =
    var position = 0

    def currentChar: Char =
      if position < input.length then input(position) else '\u0000'

    def nextChar: Char =
      if position + 1 < input.length then input(position + 1) else '\u0000'

    def isSeparator(c: Char): Boolean =
      c == '\u0000' || c.isWhitespace || c == '(' || c == ')'

    def advance(): Unit = position += 1

    def isEndOfLine() : Boolean =
      position >= input.length

    def skipComments(): Unit =
      if currentChar == '#' then
        while !isEndOfLine() && currentChar != '\n' && currentChar != '\r' do advance()
        skipWhitespaces()

    def skipWhitespaces(): Unit =
      while currentChar.isWhitespace do advance()

    def lexToken(): Token[TokenType] =
      val start = position
      while !isSeparator(currentChar) do advance()
      val text = input.substring(start, position)
      tokenizer.toToken(text)

    new Iterator[Token[TokenType]]:
      override def hasNext: Boolean =
        skipWhitespaces()
        skipComments()
        !isEndOfLine()

      override def next(): Token[TokenType] =
        skipWhitespaces()
        skipComments()

        currentChar match
          case '(' =>
            advance()
            tokenizer.toToken("(")
          case ')' =>
            advance()
            tokenizer.toToken(")")
          case _ => lexToken()
