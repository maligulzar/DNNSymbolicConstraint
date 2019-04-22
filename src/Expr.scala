
object ArithmeticOp extends Enumeration {
  type ArithmeticOp = Value
  val Addition = Value("+")
  val Subtraction = Value("-")
  val Multiplication = Value("*")
  val Division = Value("/")
}

import ArithmeticOp._
import com.microsoft.z3.{ArithExpr, Context }

abstract class Expr {
  var actualType: VType
  var cachedExpr:Option[com.microsoft.z3.Expr]
  def toString: String
  def applyEffect(x: SymVar, effect: Expr): Expr
  def toZ3Query(initials: Z3QueryState): String
  def deepCopy: Expr
  def replace(thisVar: SymVar, other: SymVar): Expr
  def addSuffix(sfx: String): Expr
  def solveUsingZ3(context: Context , simplify:Boolean = true ): ArithExpr
}



case class SymOp(atype: VType, op: ArithmeticOp) /*extends Terminal*/ {
  val actualType = atype
  override def toString: String = {
    op match {
      case Division =>
        "/"
      case Multiplication =>
        "*"
      case Addition =>
        "+"
      case Subtraction =>
        "-"
     // case _ =>
        //throw new NotSupportedRightNow("String Operator not supported")
    }
  }
}

case class ConcreteValue(atype: VType, var value: String) extends Expr {
  override var cachedExpr: Option[com.microsoft.z3.Expr] = None
  var actualType = atype
  //check validity of passed ConcreteValue
  assert(atype match {
    case t: Numeric =>
      try {
        if (value.startsWith("CONST_")) {
          value = value.substring(6)
        }
        val v = value.toDouble
        true
      } catch {
        case _: java.lang.NumberFormatException => false
      }
  })

  override def toString: String = { value.toString /*+" of type "+actualType*/ }

  override def applyEffect(x: SymVar, effect: Expr): Expr = { this }

  override def toZ3Query(initials: Z3QueryState): String = {
    return value.toString
  }

  override def solveUsingZ3(context:Context, simplify: Boolean = true): ArithExpr = {
    atype match {
      case t: Numeric =>
        t.underlyingType match {
          case NumericUnderlyingType._Float =>
            return  context.mkReal(value.toString)
          case _  => throw new Exception("Not supported Type")
        }
      case _  => throw new Exception("Not supported Type")        }
  }

  override def deepCopy: ConcreteValue = {
    new ConcreteValue(actualType, value)
  }

  override def replace(thisVar: SymVar, other: SymVar): ConcreteValue = { this }
  override def addSuffix(sfx: String) = { this }
}

// case class UnaryExpr(op: SymOp, right: Expr) extends Expr{}

case class ArithmeticExpr(left: Expr, middle: SymOp, right: Expr) extends Expr {
  override var cachedExpr: Option[com.microsoft.z3.Expr] = None
  val op: SymOp = middle

  val leftExpr: Expr = left
  val rightExpr: Expr = right

  //check validity of this partial expression before proceeding
  assert(left != null && right != null)
  assert(op.actualType == leftExpr.actualType && op.actualType == rightExpr.actualType)
  var actualType = op.actualType

  override def toString(): String = {
    "("+left.toString + " " + op.toString + " " + right.toString+")"
  }

  override def applyEffect(x: SymVar, effect: Expr): Expr = {
    new ArithmeticExpr(left.applyEffect(x, effect), op, right.applyEffect(x, effect))
  }


  override def toZ3Query(initials: Z3QueryState): String = {
    // left.toString + " " + op.toString + " " + right.toString
    s"""(${op.toString}  ${leftExpr.toZ3Query(initials)} ${rightExpr
      .toZ3Query(initials)} )"""
    //"FIX NON TERMINAL Z3 QUERY"
  }

  override def solveUsingZ3(context: Context, simplify: Boolean): ArithExpr = {
    op.op match {
      case ArithmeticOp.Multiplication  =>
        context.mkMul(rightExpr.solveUsingZ3(context , simplify) , leftExpr.solveUsingZ3(context, simplify))

      case ArithmeticOp.Division  =>
        context.mkDiv(rightExpr.solveUsingZ3(context , simplify) , leftExpr.solveUsingZ3(context, simplify))

      case ArithmeticOp.Addition  =>
        context.mkAdd(rightExpr.solveUsingZ3(context , simplify) , leftExpr.solveUsingZ3(context, simplify))

      case ArithmeticOp.Subtraction  =>
        context.mkSub(rightExpr.solveUsingZ3(context , simplify) , leftExpr.solveUsingZ3(context, simplify))

      case _  => throw new Exception("Not supported Type")

    }

  }

  override def deepCopy(): ArithmeticExpr = {
    new ArithmeticExpr(left.deepCopy, middle, right.deepCopy)
  }
  override def replace(thisVar: SymVar, other: SymVar): ArithmeticExpr = {
    new ArithmeticExpr(left.replace(thisVar, other), middle, right.replace(thisVar, other))
  }
  override def addSuffix(sfx: String) = {
    new ArithmeticExpr(left.addSuffix(sfx), middle, right.addSuffix(sfx))
  }
}
