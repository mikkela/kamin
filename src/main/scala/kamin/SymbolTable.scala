package kamin

import scala.collection.mutable

class FunDefTable:
  private val table = mutable.HashMap[String, FunDefNode]()

  def register(funDef: FunDefNode) =
    table.put(funDef.function, funDef)

  def lookup(name: String) =
    table.get(name)
