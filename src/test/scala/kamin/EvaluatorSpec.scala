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
}
