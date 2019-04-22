object ComparisonOp extends Enumeration {
  type ComparisonOp = Value
  val Equality = Value("=")
  val Inequality = Value("!=")
  val LessThan = Value("<")
  val LessThanOrEq = Value("<=")
  val GreaterThan = Value(">")
  val GreaterThanOrEq = Value(">=")
  val Equals = Value("equals")
  val Notequals = Value("notequals")
  val isIn = Value("isIn")
  val isNotIn = Value("isNotIn")

  //def isComparisonOp(s: String): Boolean = values.exists(_.toString == s)
}

import com.microsoft.z3.ApplyResult
import com.microsoft.z3.BoolExpr
import com.microsoft.z3.Goal

import ComparisonOp._
import com.microsoft.z3.{ArithExpr, BoolExpr, Context}
class Constraint(c: Array[Clause]) {
  var clauses: Array[Clause] = c //there are (implicit) conjunctions among elements of array (clauses)

  def this() {
    this(new Array[Clause](0))
  }

  def replace(thisVar: SymVar, other: SymVar): Constraint = {
    val replacedArray = clauses.map(_.replace(thisVar, other))
    new Constraint(replacedArray)
  }

  def addSuffix(sfx: String): Constraint = {
    val replacedArray = clauses.map(_.addSuffix(sfx))
    new Constraint(replacedArray)
  }

  override def toString: String = {
    if (clauses.length == 0)
      return ""
    var result = clauses(0).toString
    for (i <- 1 to clauses.length - 1) {
      result += " && " + clauses(i)
    }
    result
  }
  def toZ3Query(initials: Z3QueryState): String = {
    if (clauses.length == 0)
      return ""
    if (clauses.length == 1) {
      return s"""(assert ${andClauses(0, initials)} )"""
    }
    val idx = 0
    //s"""(assert (${andClauses(idx , initials)}) )"""
    s"""(assert ${andClauses(idx, initials)} )"""
  }

  def solveWithZ3(context: Context, simplify: Boolean = true): BoolExpr = {
    if (clauses.length == 0)
      return null
    if (clauses.length == 1) {
      return clauses(0).solveWithZ3(context, simplify)
    }
    clauses.map(s=>s.solveWithZ3(context, simplify)).reduce(
      (c1,c2) =>
        context.mkAnd(
          c1,
          c2
        )
    )

  }

  def andClauses(idx: Int, initials: Z3QueryState): String = {
    if (idx == clauses.length - 1) {
      clauses(idx).toZ3Query(initials)
    } else {
      s""" (and ${clauses(idx).toZ3Query(initials)} ${andClauses(idx + 1, initials)} )"""
    }
  }

  //  def andClauses(idx: Int, initials: Z3QueryState): String = {
  //    if (idx == clauses.length - 1) {
  //      clauses(idx).toZ3Query(initials)
  //    } else {
  //      s""" (and ${clauses(idx).toZ3Query(initials)} ${andClauses(idx + 1, initials)} )"""
  //    }
  //  }

  override def equals(other: Any): Boolean = {
    if (other != null && other.isInstanceOf[Constraint]) {
      this.clauses.deep == other.asInstanceOf[Constraint].clauses.deep
    } else false
  }

  def conjunctWith(other: Constraint):Constraint = {
    //TODO: might want to simplify before merging, in case there are inconsistent clauses or repetitive ones
    clauses = clauses ++ other.clauses
    return this
  }

  def conjunctWithSideEffectFree(other: Constraint):Constraint = {
    //TODO: might want to simplify before merging, in case there are inconsistent clauses or repetitive ones
    return  new Constraint(clauses ++ other.clauses)
  }
  def conjunctWithSideEffectFree(other: Clause):Constraint = {
    //TODO: might want to simplify before merging, in case there are inconsistent clauses or repetitive ones
    return new Constraint(clauses ++ Array(other))
  }
  def applyEffect(x: SymVar, effect: Expr): Constraint = {
    /*
            map builds a new collection(Array)
     */
    val updated = clauses.map(_.applyEffect(x, effect))
    // for(c <- clauses) {
    //     // if(c.contains(x))
    //     c.applyEffect(x, effect)
    // }
    new Constraint(updated)
  }


  def deepCopy: Constraint = {
    val newArray = new Array[Clause](this.clauses.size)
    this.clauses
      .copyToArray(newArray) //TODO TEST: might shallow copying the clauses
    new Constraint(newArray)
  }
}


class Clause(left: Expr, op: ComparisonOp = null, right: Expr = null) {
  val compOp: ComparisonOp = op
  var leftExpr: Expr = left
  var rightExpr: Expr = right

  var cachedExpr: Option[BoolExpr] = None
  assert(left != null)


  def simplify(context:Context): Unit ={
    val g = context.mkGoal(true, false, false)
    val constraint = this.solveWithZ3(context, true)
    g.add(constraint)

    val ar = context.mkTactic("propagate-values").apply(g)
    val subgoals = ar.getSubgoals
    cachedExpr = Some(subgoals(0).getFormulas()(0))
  }
  def toZ3Query(initials: Z3QueryState): String = {

    var leftstr = leftExpr.toZ3Query(initials)
    var rightstr = rightExpr.toZ3Query(initials)

    if (compOp == null || rightExpr == null)
      leftExpr.toString
    else {

      if (compOp == Notequals || compOp == Inequality) {
        return s""" (not (=  ${leftstr} ${rightstr} ))"""
      } else {
        //Z3 -- > Assertion (assert (> x 2))
        //  if(leftExpr.isInstanceOf[Terminal] && rightExpr.isInstanceOf[Terminal])
        return s"""(${if (compOp == Notequals || compOp == Equals) {
          "="
        } else {
          compOp.toString()
        }}  ${leftstr} ${rightstr} )"""
      }
    }
  }

  def solveWithZ3(context:Context, simplify:Boolean = true): BoolExpr ={
//    if(leftCachedExpr.isDefined) {
//      op match {
//        case Equality => context.mkEq(leftCachedExpr.get, rightCachedExpr.get)
//        case Inequality => context.mkNot(context.mkEq(leftCachedExpr.get, rightCachedExpr.get))
//        case LessThan => context.mkLt(leftCachedExpr.get, rightCachedExpr.get)
//        case LessThanOrEq => context.mkLe(leftCachedExpr.get, rightCachedExpr.get)
//        case GreaterThan => context.mkGt(leftCachedExpr.get, rightCachedExpr.get)
//        case GreaterThanOrEq => context.mkGe(leftCachedExpr.get, rightCachedExpr.get)
//        case _ => throw new Exception("Not supported Type")
//      }
//    }else{
      op match {
        case   Equality => context.mkEq(leftExpr.solveUsingZ3(context, simplify),rightExpr.solveUsingZ3(context, simplify))
        case Inequality => context.mkNot(context.mkEq(leftExpr.solveUsingZ3(context, simplify),rightExpr.solveUsingZ3(context, simplify)))
        case LessThan => context.mkLt(leftExpr.solveUsingZ3(context, simplify),rightExpr.solveUsingZ3(context, simplify))
        case LessThanOrEq => context.mkLe(leftExpr.solveUsingZ3(context, simplify),rightExpr.solveUsingZ3(context, simplify))
        case GreaterThan => context.mkGt(leftExpr.solveUsingZ3(context, simplify),rightExpr.solveUsingZ3(context, simplify))
        case GreaterThanOrEq =>context.mkGe(leftExpr.solveUsingZ3(context, simplify),rightExpr.solveUsingZ3(context, simplify))
        case _=> throw new Exception("Not supported Type")
      }
 //   }
  }

  override def equals(other: Any): Boolean = {
    if (other != null && other.isInstanceOf[Clause]) {
      this.toString == other.asInstanceOf[Clause].toString
    } else false
  }

  override def toString: String = {
    if (compOp == null || rightExpr == null) leftExpr.toString
    else leftExpr.toString + " " + compOp.toString + " " + rightExpr.toString
  }

  def applyEffect(x: SymVar, effect: Expr): Clause = {
    val newLeftExpr = leftExpr.applyEffect(x, effect)

    val newRightExpr =
      if (rightExpr != null) {
        rightExpr.applyEffect(x, effect)
      } else null

    new Clause(newLeftExpr, this.compOp, newRightExpr)
  }

  def replace(thisVar: SymVar, other: SymVar): Clause = {
    if (rightExpr != null)
      new Clause(leftExpr.replace(thisVar, other), compOp, rightExpr.replace(thisVar, other))
    else new Clause(leftExpr.replace(thisVar, other))
  }

  def addSuffix(sfx: String): Clause = {
    if (rightExpr != null)
      new Clause(leftExpr.addSuffix(sfx), compOp, rightExpr.addSuffix(sfx))
    else new Clause(leftExpr.addSuffix(sfx))
  }
}

