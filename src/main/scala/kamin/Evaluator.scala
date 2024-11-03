package kamin

import kamin.Environment

trait Evaluator[T <: ExpressionNode]:
  extension (t: T) def evaluate(using environment: Environment)(using funDefTable: FunDefTable): Either[String, Int]

private def unrecognizedName(name: String) : String = s"${name} is not recognized"

given Evaluator[ExpressionNode] with
  extension (t: ExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunDefTable): Either[String, Int]=
    t match
      case n:ValueExpressionNode =>
        summon[Evaluator[ValueExpressionNode]].evaluate(n)(using environment)(using funDefTable)
      case n: VariableExpressionNode =>
        summon[Evaluator[VariableExpressionNode]].evaluate(n)(using environment)(using funDefTable)
      case n: IfExpressionNode =>
        summon[Evaluator[IfExpressionNode]].evaluate(n)(using environment)(using funDefTable)
      case _ =>
        Left("Not implemented")

given Evaluator[ValueExpressionNode] with
  extension (t: ValueExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunDefTable): Either[String, Int] =
    t.valueExpression match
      case IntegerValueNode(value) => Right(value)

given Evaluator[VariableExpressionNode] with
  extension (t: VariableExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunDefTable): Either[String, Int] =
    environment.get(t.variableExpression) match
      case Some(value) => Right(value)
      case None => Left(unrecognizedName(t.variableExpression))

given Evaluator[IfExpressionNode] with
  extension (t: IfExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunDefTable): Either[String, Int] =
    t.testExpression.evaluate.flatMap {
      case 0 => t.alternativeExpression.evaluate
      case _ => t.consequenceExpression.evaluate
    }

/*given Evaluator[SetExpressionNode] with
  extension (t: SetExpressionNode) override def evaluate(environment: Environment[ExpressionNode]): Either[String, Int] =
    t.testExpression.evaluate(environment).flatMap {
      case 0 => t.alternativeExpression.evaluate(environment)
      case _ => t.consequenceExpression.evaluate(environment)
    }

given Evaluator[WhileExpressionNode] with
  extension (t: WhileExpressionNode) override def evaluate(environment: Environment[ExpressionNode]): Either[String, Int] =
    var loop = true
    while (loop) do
      t.testExpression.evaluate(environment) match
        case Left(value) => return Left(value)
        case Right(test) =>
          loop = test != 0
          if (loop) then
            t.bodyExpression.evaluate(environment) match
              case Left(value) => return Left(value)
    Right(0)*/

