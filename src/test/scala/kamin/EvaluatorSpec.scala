package kamin

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar

class EvaluatorSpec extends AnyFunSpec
  with Matchers
  with MockitoSugar
  with TableDrivenPropertyChecks {

  describe("evaluate for IntegerValueNode") {
    it("should return the integer value even without an environment") {
      val sut = ValueExpressionNode(IntegerValueNode(100))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunDefTable()) shouldBe Right(100)
    }
  }

  describe("evaluate for VariableNode") {
    it("should return the evaluated expression registered at the variable") {
      val environment = GlobalAndLocalScopeEnvironment()
      environment.set("x", 20)
      val sut = VariableExpressionNode("x")

      sut.evaluate(using environment)(using FunDefTable()) shouldBe Right(20)
    }

    it("should return an error if the variable does not exists") {
      val sut = VariableExpressionNode("x")

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunDefTable()) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluate for IfExpressionNode") {
    it("should return the second expression if the evaluation of the first is non zero") {
      val sut = IfExpressionNode(ValueExpressionNode(IntegerValueNode(1)), ValueExpressionNode(IntegerValueNode(2)), ValueExpressionNode(IntegerValueNode(3)))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunDefTable()) shouldBe Right(2)
    }

    it("should return the third expression if the evaluation of the first is zero") {
      val sut = IfExpressionNode(ValueExpressionNode(IntegerValueNode(0)), ValueExpressionNode(IntegerValueNode(2)), ValueExpressionNode(IntegerValueNode(3)))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunDefTable()) shouldBe Right(3)
    }

    it("should return the the error if the evaluation of the first an error") {
      val sut = IfExpressionNode(VariableExpressionNode("x"), ValueExpressionNode(IntegerValueNode(2)), ValueExpressionNode(IntegerValueNode(3)))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunDefTable()) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluate for SetExpressionNode") {
    it("should register the evaluation result in the environment and return it as well") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = SetExpressionNode("foo", ValueExpressionNode(IntegerValueNode(265)))

      sut.evaluate(using env)(using FunDefTable()) shouldBe Right(265)
      env.get("foo") shouldBe Some(265)
    }

    it("should return the the error if the evaluation of the value expression returns an error") {
      val sut = SetExpressionNode("wrong", VariableExpressionNode("x"))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunDefTable()) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluate for WhileExpressionNode") {
    it("should only evaluate the test and not the body if the test returns 0") {
      val sut = WhileExpressionNode(ValueExpressionNode(IntegerValueNode(0)), VariableExpressionNode("x"))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunDefTable()) shouldBe Right(0)
    }

    it("should only evaluate the body if the test returns non-zero") {
      val env = GlobalAndLocalScopeEnvironment()
      env.set("x", 1)
      val sut = WhileExpressionNode(VariableExpressionNode("x"), SetExpressionNode("x", ValueExpressionNode(IntegerValueNode(0))))

      sut.evaluate(using env)(using FunDefTable()) shouldBe Right(0)
      env.get("x") shouldBe Some(0)
    }

    it("should return the the error if the evaluation of the test expression returns an error") {
      val sut = WhileExpressionNode(VariableExpressionNode("x"), ValueExpressionNode(IntegerValueNode(100)))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunDefTable()) shouldBe Left("x is not recognized")
    }

    it("should return the the error if the evaluation of the body expression returns an error") {
      val sut = WhileExpressionNode(ValueExpressionNode(IntegerValueNode(100)), VariableExpressionNode("x"))

      sut.evaluate(using GlobalAndLocalScopeEnvironment())(using FunDefTable()) shouldBe Left("x is not recognized")
    }
  }

  describe("evaluate for BeginExpressionNode") {
    it("should return the result of the last expression after evaluating them all") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = BeginExpressionNode(Seq(
        SetExpressionNode("x", ValueExpressionNode(IntegerValueNode(2))),
        SetExpressionNode("y", ValueExpressionNode(IntegerValueNode(25))),
        ValueExpressionNode(IntegerValueNode(123))))

      sut.evaluate(using env)(using FunDefTable()) shouldBe Right(123)
      env.get("x") shouldBe Some(2)
      env.get("y") shouldBe Some(25)
    }

    it("should return the the error if one of the expression returns an error") {
      val env = GlobalAndLocalScopeEnvironment()
      val sut = BeginExpressionNode(Seq(
        SetExpressionNode("x", ValueExpressionNode(IntegerValueNode(2))),
        VariableExpressionNode("y"),
        ValueExpressionNode(IntegerValueNode(123))))

      sut.evaluate(using env)(using FunDefTable()) shouldBe Left("y is not recognized")
      env.get("x") shouldBe Some(2)
    }
  }
}
