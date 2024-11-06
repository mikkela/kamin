package kamin

import kamin.Environment

import scala.annotation.tailrec

trait Evaluator[T <: ExpressionNode]:
  extension (t: T) def evaluate(using environment: Environment)(using funDefTable: FunctionDefinitionTable): Either[String, Int]

private def unrecognizedName(name: String) : String = s"$name is not recognized"

given Evaluator[ExpressionNode] with
  extension (t: ExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunctionDefinitionTable): Either[String, Int]=
    t match
      case n:IntegerExpressionNode =>
        summon[Evaluator[IntegerExpressionNode]].evaluate(n)(using environment)(using funDefTable)
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

given Evaluator[IntegerExpressionNode] with
  extension (t: IntegerExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunctionDefinitionTable): Either[String, Int] =
    Right(t.integerValue)

given Evaluator[VariableExpressionNode] with
  extension (t: VariableExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunctionDefinitionTable): Either[String, Int] =
    environment.get(t.variableExpression) match
      case Some(value) => Right(value)
      case None => Left(unrecognizedName(t.variableExpression))

given Evaluator[IfExpressionNode] with
  extension (t: IfExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunctionDefinitionTable): Either[String, Int] =
    t.testExpression.evaluate.flatMap {
      case 0 => t.alternativeExpression.evaluate
      case _ => t.consequenceExpression.evaluate
    }

given Evaluator[SetExpressionNode] with
  extension (t: SetExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunctionDefinitionTable): Either[String, Int] =
    t.value.evaluate match
      case Left(value) => Left(value)
      case Right(value) =>
        environment.set(t.variable, value)
        Right(value)

given Evaluator[WhileExpressionNode] with
  extension (t: WhileExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunctionDefinitionTable): Either[String, Int] =
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
  extension (t: BeginExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunctionDefinitionTable): Either[String, Int] =
    t.expressions.foldLeft[Either[String, Int]](Right(0)) { (acc, expr) =>
      acc match
        case Left(error) => Left(error)
        case Right(_) => expr.evaluate
    }

given Evaluator[FunctionCallExpressionNode] with
  extension (t: FunctionCallExpressionNode) override def evaluate(using environment: Environment)(using funDefTable: FunctionDefinitionTable): Either[String, Int] =

    val parameters =
      t.expressions.foldLeft(Right(List.empty[Int]): Either[String, List[Int]]) { (acc, p) =>
        acc match
          case Left(error) => Left(error) // If there's already an error, keep it
          case Right(parameters) =>
            p.evaluate(using environment)(using funDefTable) match
              case Left(error) => Left(error) // Stop and return the error if evaluation fails
              case Right(result) => Right(parameters :+ result) // Append result to the list if successful
      }

    val arguments = funDefTable.lookupFunctionArguments(t.function)

    (parameters, arguments) match
      case (Right(params), Right(args)) if params.length == args.length =>
        environment.openScope(args)
        args.zip(params).foreach((a, p)=>environment.set(a, p))

        val result = funDefTable.lookupFunctionCall(t.function).flatMap { body =>
          body(environment, funDefTable)
        }
        environment.closeScope()
        result
      case (Right(params), Right(args)) => Left(s"Invalid number of arguments. Expected: ${args.length}, got: ${params.length}")
      case (Left(error), _) => Left(error)
      case (_, Left(error)) => Left(error)
