package kamin

sealed trait Node

sealed trait InputNode extends Node

sealed trait FunDefNode extends InputNode:
  def function: FunctionNode

case class ASTFunDefNode(function: FunctionNode, arglist: Seq[VariableNode], expression: ExpressionNode)
  extends FunDefNode

sealed trait ExpressionNode extends InputNode

sealed trait ValueExpressionNode extends ExpressionNode

case class ASTValueExpressionNode(value: ValueNode) extends ValueExpressionNode

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
                            valueExpression: ValueExpressionNode) extends SetExpressionNode

sealed trait BeginExpressionNode extends ExpressionNode

case class ASTBeginExpressionNode(expressions: Seq[ExpressionNode]) extends BeginExpressionNode

sealed trait OptrExpressionNode extends ExpressionNode

case class ASTOptrExpressionNode(optr: OptrNode, expressions: Seq[ExpressionNode]) extends OptrExpressionNode

trait OptrNode extends Node

sealed trait FunctionOptrNode extends ExpressionNode

case class ASTFunctionOptrNode(function: FunctionNode) extends FunctionOptrNode

trait ValueOperationNode extends OptrNode

trait ValueNode extends Node:
  def value: Int

case class ASTValueNode(value: Int) extends ValueNode

case class ASTPlusValueOperationNode() extends ValueOperationNode

case class ASTMinusValueOperationNode() extends ValueOperationNode

case class ASTMultiplicationValueOperationNode() extends ValueOperationNode

case class ASTDivisionValueOperationNode() extends ValueOperationNode

case class ASTEqualValueOperationNode() extends ValueOperationNode

case class ASTLessThanValueOperationNode() extends ValueOperationNode

case class ASTGreaterThanValueOperationNode() extends ValueOperationNode

case class ASTPrintValueOperationNode() extends ValueOperationNode

trait FunctionNode extends Node

case class ASTFunctionNode(name: String) extends FunctionNode

trait VariableNode extends Node

case class ASTVariableNode(name: String) extends VariableNode
