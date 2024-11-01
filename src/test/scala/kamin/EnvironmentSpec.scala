package kamin

import org.scalatest.funspec.AnyFunSpec

class EnvironmentSpec extends AnyFunSpec:

  describe("An Environment") {

    it("should set and get a variable in a single scope") {
      val env = new Environment[String]
      env.setVariable("x", "10")
      assert(env.getVariable("x").contains("10"))
    }

    it("should get the variable from the closest scope") {
      val env = new Environment[String]
      env.setVariable("x", "10")
      env.openScope()
      env.setVariable("x", "20")
      assert(env.getVariable("x").contains("20"))
    }

    it("should retrieve a variable from an outer scope if it is not found in the current scope") {
      val env = new Environment[String]
      env.setVariable("x", "10")
      env.openScope()
      assert(env.getVariable("x").contains("10"))
    }

    it("should return None if a variable is not found in any scope") {
      val env = new Environment[String]
      assert(env.getVariable("y").isEmpty)
    }

    it("should revert to an outer scope variable after closing the inner scope") {
      val env = new Environment[String]
      env.setVariable("x", "10")
      env.openScope()
      env.setVariable("x", "20")
      env.closeScope()
      assert(env.getVariable("x").contains("10"))  // Should revert to outer scope value
    }

    it("should throw an exception when attempting to close the global scope") {
      val env = new Environment[String]
      assertThrows[IllegalStateException] {
        env.closeScope()
      }
    }

    it("should manage nested scopes with different variables") {
      val env = new Environment[String]
      env.setVariable("a", "global")
      env.openScope()
      env.setVariable("b", "scope1")
      env.openScope()
      env.setVariable("c", "scope2")

      assert(env.getVariable("a").contains("global"))
      assert(env.getVariable("b").contains("scope1"))
      assert(env.getVariable("c").contains("scope2"))

      env.closeScope()
      assert(env.getVariable("c").isEmpty)
      assert(env.getVariable("b").contains("scope1"))
    }
  }
