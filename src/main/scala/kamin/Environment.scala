package kamin

import scala.collection.mutable

// Environment class to store variables and manage scopes
class Environment[EnvironmentType]:
  // Stack of scopes, each scope is a mutable map of variable names to expressions
  private val scopes: mutable.Stack[mutable.Map[String, EnvironmentType]] = mutable.Stack(mutable.Map())

  // Open a new scope
  def openScope(): Unit =
    scopes.push(mutable.Map())

  // Close the current scope
  def closeScope(): Unit =
    if scopes.size > 1 then scopes.pop()
    else throw new IllegalStateException("Cannot close the global scope")

  // Store a variable with an associated expression in the current (top) scope
  def setVariable(name: String, value: EnvironmentType): Unit =
    scopes.top(name) = value

  // Look up a variable by name, starting from the newest scope to the oldest
  def getVariable(name: String): Option[EnvironmentType] =
    scopes.iterator.flatMap(_.get(name)).nextOption()
