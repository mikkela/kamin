package kamin.lexer

import kamin.lexer.Lexer.{isDelimiter, isNumberStart, isSpace}

abstract class Lexer[Token](val input: String):
  private var position: Int = 0
  private var readPosition: Int = 0
  private var character: Char = EndOfInput
  private val inputWithoutComments = Lexer.removeComments(input)

  readCharacter()

  def nextToken() : Token =
    skipWhiteSpaces()

    character match
      case '(' =>
        val token = leftParenthesis()
        readCharacter()
        token
      case ')' =>
        val token = rightParenthesis()
        readCharacter()
        token
      case EndOfInput =>
        eof()
      case _ =>
        if isNumberStart(character, peekCharacter()) then
          integer(readNumber())
        else
          if !isDelimiter(character) then
            lookup(readIdentifier())
          else
            illegal()

  private def readCharacter(): Unit =
    character = peekCharacter()
    position = readPosition
    readPosition = readPosition + 1

  private def readCharacters(clause: Char => Boolean): Unit =
    while clause(character) do
      readCharacter()

  private def readString(filter: () => Unit): String =
    val p = position
    filter.apply()
    inputWithoutComments.substring(p, position)

  private def readNumber() : String =
    readString(
      () =>
        if (isNumberStart(character, peekCharacter()))
          readCharacter()
          readCharacters(Lexer.isDigit)
    )

  private def readIdentifier() : String =
    readString(
      () => readCharacters(c => !isDelimiter(c))
    )

  private def skipWhiteSpaces(): Unit =
    readCharacters(isSpace)

  private def peekCharacter(): Char =
    if readPosition >= inputWithoutComments.length() then EndOfInput else inputWithoutComments.charAt(readPosition)

  protected def leftParenthesis() : Token
  protected def rightParenthesis() : Token
  protected def eof() : Token
  protected def integer(value: String): Token
  protected def lookup(value: String): Token
  protected def illegal(): Token

object Lexer:
  private val commentExpression = "#.*?(\r?\n|$)".r
  def removeComments(input: String): String =
    commentExpression.replaceAllIn(input, "\r\n")

  private def isNumberStart(current: Char, next: Char) : Boolean =
    isDigit(current) || (current == '-' && isDigit(next))

  private def isDigit(current: Char): Boolean =
    Character.isDigit(current)

  private def isSpace(current: Char): Boolean =
    current == ' ' || current == '\t' || current == '\n' || current == '\r'

  private def isBlockStart(current: Char) : Boolean =
    current == '('

  private def isBlockEnd(current: Char): Boolean =
    current == ')'

  private def isEndOfInput(current: Char) : Boolean =
    current == EndOfInput

  private def isDelimiter(current: Char) : Boolean =
    isSpace(current) || isBlockStart(current) || isBlockEnd(current) || isEndOfInput(current)
private val EndOfInput: Char = 0
