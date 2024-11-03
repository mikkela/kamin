package kamin

import kamin.Environment

import scala.annotation.tailrec

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
      case n: SetExpressionNode =>
        summon[Evaluator[SetExpressionNode]].evaluate(n)(using environment)(using funDefTable)
      case n: WhileExpressionNode =>
        summon[Evaluator[WhileExpressionNode]].evaluate(n)(using environment)(using funDefTable)
      case n: BeginExpressionNode =>
        summon[Evaluator[BeginExpressionNode]].evaluate(n)(using environment)(using funDefTable)
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

given Evaluator[SetExpressionNode] with
  extension (t: SetExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunDefTable): Either[String, Int] =
    t.value.evaluate match
      case Left(value) => Left(value)
      case Right(value) =>
        environment.set(t.variable, value)
        Right(value)

given Evaluator[WhileExpressionNode] with
  extension (t: WhileExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunDefTable): Either[String, Int] =
    @tailrec
    def evaluateLoop(): Either[String, Int] =
      t.testExpression.evaluate match
        case Left(error) => Left(error)
        case Right(test) if test == 0 => Right(0)
        case Right(_) =>
          t.bodyExpression.evaluate match
            case Left(error) => Left(error)
            case Right(_) => evaluateLoop() // Recur to continue the loop

    evaluateLoop()

given Evaluator[BeginExpressionNode] with
  extension (t: BeginExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunDefTable): Either[String, Int] =
    t.expressions.foldLeft[Either[String, Int]](Right(0)) { (acc, expr) =>
      acc match
        case Left(error) => Left(error)
        case Right(_) => expr.evaluate
    }

