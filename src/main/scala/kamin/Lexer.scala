package kamin

enum TokenType:
  case Illegal, Name, Integer, LeftParenthesis, RightParenthesis, Equal, LessThan, GreaterThan, Plus, Minus, Asteriks, Slash, Define, Print, If, While, Set, Begin


case class Token(tokenType: TokenType, literal: String)

trait IntegerTokenizer {
  def isInteger(s: String): Boolean = s.head == '-' && s.tail.forall(_.isDigit) || s.forall(_.isDigit)
}

trait NameTokenizer {
  def isName(s: String): Boolean = s.forall(c => c.isLetter || isAllowedSpecialChar(c))

  private def isAllowedSpecialChar(c: Char): Boolean =
    !Set(';', '(', ')').contains(c) && !c.isWhitespace
}

trait Tokenizer:
  def toToken(s: String): Token
  def isSeparator(c: Char): Boolean

class Lexer(using tokenizer: Tokenizer):
  def tokens(input: String):Iterator[Token] =
    var position = 0

    def currentChar: Char =
      if position < input.length then input(position) else '\u0000'

    def nextChar: Char =
      if position + 1 < input.length then input(position + 1) else '\u0000'

    def isSeparator(c: Char): Boolean =
      c == '\u0000' || c.isWhitespace || tokenizer.isSeparator(c)

    def advance(): Unit = position += 1

    def isEndOfLine: Boolean =
      position >= input.length

    def skipComments(): Unit =
      if currentChar == ';' then
        while !isEndOfLine && currentChar != '\n' && currentChar != '\r' do advance()
        skipWhitespaces()

    def skipWhitespaces(): Unit =
      while currentChar.isWhitespace do advance()

    def lexToken(): Token =
      val start = position
      while !isSeparator(currentChar) do advance()
      val text = input.substring(start, position)
      tokenizer.toToken(text)

    new Iterator[Token]:
      override def hasNext: Boolean =
        skipWhitespaces()
        skipComments()
        !isEndOfLine

      override def next(): Token =
        skipWhitespaces()
        skipComments()

        currentChar match
          case c if tokenizer.isSeparator(c) =>
            advance()
            tokenizer.toToken(c.toString)
          case _ => lexToken()
