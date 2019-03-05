/**
  * Created by malig on 3/4/19.
  */
class SymbolicNeuron(p: Array[PathEffect]) {

  var paths: Array[PathEffect] = p


  def addNeuron(neuron : SymbolicNeuron) : SymbolicNeuron = {
    val array_paths = new Array[PathEffect](this.paths.length * neuron.paths.length)
    var i = 0
    for(p1 <- this.paths){
      for ( p2 <- neuron.paths){
        array_paths(i) = new PathEffect(p1.pathConstraint.conjunctWith(p2.pathConstraint), add(p1.effect, p2.effect))
        i = i + 1
      }
    }
    new SymbolicNeuron(array_paths)
  }

  def applyActivation(af : PathEffect => Array[PathEffect]) = {
    var array_paths = new Array[PathEffect](0)
    for(p <- paths){
      array_paths = array_paths ++ af(p)
    }
    paths = array_paths
  }

  def computeNeuronValue(w : Float) : SymbolicNeuron = {
    for (p <- paths){
      p.effect = multiple(p.effect, w)
    }
    return this
  }

  def add(left: Expr, right: Float): ArithmeticExpr = {
    new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Addition), new ConcreteValue(
      new Numeric(
        NumericUnderlyingType._Float
      ), right.toString
    )
    )
  }

  def add(left: Expr, right: Expr): ArithmeticExpr = {
    new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Addition), right)
  }

  def multiple(left: Expr, right: Float): ArithmeticExpr = {
    new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Multiplication), new ConcreteValue(
      new Numeric(
        NumericUnderlyingType._Float
      ), right.toString
    )
    )
  }

  def multiple(left: Expr, right: Expr): ArithmeticExpr = {
    new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Multiplication), right)
  }

  override def toString: String = {
    var  i = 0
    var string = ""
    for(p <- paths){
      string = string + s"""Path:$i \n Constraints: ${p.pathConstraint.toString}\n Effect: ${p.effect.toString}\n"""
      i = i+1
    }
    return string
  }
}
