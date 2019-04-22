import com.microsoft.z3.{ArithExpr, Context}
class SymVar(atype: VType, var name: String) extends Expr {
  override var cachedExpr: Option[com.microsoft.z3.Expr] = None
   var actualType = atype

  /**
    * Setting types of the newly introduced return variable in the effect
    * */
   def setType(_type: VType) {
    actualType = _type
  }
  def getName: String = { name }

  override def toString: String = { name /*+": "+actualType*/ }

  override def applyEffect(x: SymVar, effect: Expr): Expr = {
    if (this.equals(x)) effect
    else
      this //TODO TEST: may need to do a deep-copy instead of returning the same instance, in case of further effects
  }


  override def toZ3Query(initials: Z3QueryState): String = {
    var temp_name = name.replaceAll("[^A-Za-z0-9_]", "")
    initials.addtoInit((temp_name, actualType))
    temp_name
  }


  override def solveUsingZ3(context: Context, simplify: Boolean): ArithExpr = {
    atype match {
      case t: Numeric =>
        t.underlyingType match {
          case NumericUnderlyingType._Float =>
          //  print(name)
            return  context.mkRealConst(name)
          case _  => throw new Exception("Not supported Type")
        }
      case _  => throw new Exception("Not supported Type")        }
  }


   override def deepCopy: SymVar = {
    new SymVar(actualType, name)
  }
   override def replace(thisVar: SymVar, other: SymVar): SymVar = other
   override def addSuffix(sfx: String) = {
    new SymVar(actualType, name + "_" + sfx)
  }
}