package kamin

trait Node

sealed trait FunctionNode extends Node

case class ASTFunctionNode(name: String) extends FunctionNode

sealed trait ValueNode extends Node

case class ASTIntegerValueNode(value: Int) extends ValueNode

sealed trait VariableNode extends Node

case class ASTVariableNode(variable: String) extends VariableNode

trait InputNode extends Node

sealed trait FunDefNode extends InputNode:
  def function: FunctionNode

case class ASTFunDefNode(function: FunctionNode, arglist: Seq[String], expression: ExpressionNode)
  extends FunDefNode

sealed trait ExpressionNode extends InputNode

sealed trait ValueExpressionNode extends ExpressionNode

case class ASTValueExpressionNode(valueExpression: ValueNode) extends ValueExpressionNode

sealed trait VariableExpressionNode extends ExpressionNode

case class ASTVariableExpressionNode(variableExpression: VariableNode) extends VariableExpressionNode

sealed trait IfExpressionNode extends ExpressionNode

case class ASTIfExpressionNode( testExpression: ExpressionNode,
                                consequenceExpression: ExpressionNode,
                                alternativeExpression: ExpressionNode) extends IfExpressionNode

sealed trait WhileExpressionNode extends ExpressionNode

case class ASTWhileExpressionNode(testExpression: ExpressionNode,
                              bodyExpression: ExpressionNode) extends WhileExpressionNode

sealed trait SetExpressionNode extends ExpressionNode

case class ASTSetExpressionNode(variable: VariableNode,
                                value: ExpressionNode) extends SetExpressionNode

sealed trait BeginExpressionNode extends ExpressionNode

case class ASTBeginExpressionNode(expressions: Seq[ExpressionNode]) extends BeginExpressionNode

sealed trait OptrExpressionNode extends ExpressionNode

case class ASTOptrExpressionNode(optr: OptrNode, expressions: Seq[ExpressionNode]) extends OptrExpressionNode

sealed trait OptrNode extends Node

sealed trait ValueOperationNode extends OptrNode

case class ASTPlusValueOperationNode() extends ValueOperationNode

case class ASTMinusValueOperationNode() extends ValueOperationNode

case class ASTMultiplicationValueOperationNode() extends ValueOperationNode

case class ASTDivisionValueOperationNode() extends ValueOperationNode

case class ASTEqualValueOperationNode() extends ValueOperationNode

case class ASTLessThanValueOperationNode() extends ValueOperationNode

case class ASTGreaterThanValueOperationNode() extends ValueOperationNode

case class ASTPrintValueOperationNode() extends ValueOperationNode

case class ASTFunctionOperationNode(functionNode: FunctionNode) extends OptrNode


