package kamin.lexer

import org.scalatest.funspec.AnyFunSpec

class LexerSpec extends AnyFunSpec{

  describe("removeComments method") {
    it("should return the same string when the string does not have a comment marker") {
      assert(Lexer.removeComments("This has no comment marker") == "This has no comment marker")
    }
    it("should return a string where the rest of the string is removed from the comment marker") {
      assert(Lexer.removeComments("This has a# comment marker") == "This has a\r\n")
    }
    it("should return a string where the part string until carriage return and rewline is removed from the comment marker") {
      assert(Lexer.removeComments("This has a# comment marker \r\n that will be removed") == "This has a\r\n that will be removed")
    }
    it("should return a string where the part string until rewline is removed from the comment marker") {
      assert(Lexer.removeComments("This has a# comment marker \n that is ended by newline") == "This has a\r\n that is ended by newline")
    }
  }
}
