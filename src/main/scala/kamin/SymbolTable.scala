package kamin

import scala.collection.mutable

private def undefinedFunctionName(name: String): Left[String, Nothing] =
  Left(s"$name is not recognized as a function")

abstract class FunctionDefinitionTable:
  protected case class FunDefSymbol(name: String, arguments: Seq[String], body: (Environment, FunctionDefinitionTable) => Either[String, Int])
  protected val table = mutable.HashMap[String, FunDefSymbol]()

  initializeOperators()

  protected def initializeOperators(): Unit = return

  def register(funDef: FunctionDefinitionNode) =
    table.put(funDef.function, FunDefSymbol(funDef.function, funDef.arguments, (e, t) => funDef.expression.evaluate(using e)(using t)))

  def lookupFunctionArguments(name: String): Either[String, Seq[String]] =
    table.get(name) match
      case None => undefinedFunctionName(name)
      case Some(symbol) => Right(symbol.arguments)

  def lookupFunctionCall(name: String): Either[String, (Environment, FunctionDefinitionTable) => Either[String, Int]]=
    table.get(name) match
      case None => undefinedFunctionName(name)
      case Some(symbol) => Right(symbol.body)
