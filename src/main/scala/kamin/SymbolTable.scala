package kamin

import scala.collection.mutable

class FunctionDefinitionTable:
  protected val table = mutable.HashMap[String, FunctionDefinitionNode]()

  def register(functionDefinition: FunctionDefinitionNode) =
    table.put(functionDefinition.function, functionDefinition)

  def lookupFunctionDefinition(name: String): Option[FunctionDefinitionNode] =
    table.get(name)

