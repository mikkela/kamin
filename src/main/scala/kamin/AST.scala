package kamin

trait Node

sealed trait ValueNode extends Node

case class IntegerValueNode(value: Int) extends ValueNode

trait InputNode extends Node

case class FunDefNode(function: String, arguments: Seq[String], expression: ExpressionNode)
  extends InputNode

sealed trait ExpressionNode extends InputNode

case class ValueExpressionNode(valueExpression: ValueNode) extends ExpressionNode

case class VariableExpressionNode(variableExpression: String) extends ExpressionNode

case class IfExpressionNode( testExpression: ExpressionNode,
                                consequenceExpression: ExpressionNode,
                                alternativeExpression: ExpressionNode) extends ExpressionNode

case class WhileExpressionNode(testExpression: ExpressionNode,
                              bodyExpression: ExpressionNode) extends ExpressionNode

case class SetExpressionNode(variable: String,
                                value: ExpressionNode) extends ExpressionNode

case class BeginExpressionNode(expressions: Seq[ExpressionNode]) extends ExpressionNode

case class OptrExpressionNode(optr: OptrNode, expressions: Seq[ExpressionNode]) extends ExpressionNode

sealed trait OptrNode extends Node

sealed trait ValueOperationNode extends OptrNode

case class PlusValueOperationNode() extends ValueOperationNode

case class MinusValueOperationNode() extends ValueOperationNode

case class MultiplicationValueOperationNode() extends ValueOperationNode

case class DivisionValueOperationNode() extends ValueOperationNode

case class EqualValueOperationNode() extends ValueOperationNode

case class LessThanValueOperationNode() extends ValueOperationNode

case class GreaterThanValueOperationNode() extends ValueOperationNode

case class PrintValueOperationNode() extends ValueOperationNode

case class FunctionOperationNode(functionNode: String) extends OptrNode


