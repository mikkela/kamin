package kamin.lexer

trait Tokenizer[Token]:
  def left_parenthesis:Token
  def right_parenthesis: Token
  def number(s: String): Token
  def to_token(s: String): Token

class Lexer[Token](val tokenizer: Tokenizer[Token]):
  def tokens(input: String):Iterator[Token] =
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

    def lexNumber(): Token =
      val start = position
      if currentChar == '-' then advance()
      while currentChar.isDigit do advance()
      val number = input.substring(start, position)
      tokenizer.number(number)

    def lexToken(): Token =
      val start = position
      while !isSeparator(currentChar) do advance()
      val text = input.substring(start, position)
      tokenizer.to_token(text)

    new Iterator[Token]:
      override def hasNext: Boolean =
        skipWhitespaces()
        skipComments()
        !isEndOfLine()

      override def next(): Token =
        skipWhitespaces()
        skipComments()

        currentChar match
          case c if c == '-' && nextChar.isDigit => lexNumber()
          case c if c.isDigit => lexNumber()
          case '(' =>
            advance()
            tokenizer.left_parenthesis
          case ')' =>
            advance()
            tokenizer.right_parenthesis
          case _ => lexToken()
