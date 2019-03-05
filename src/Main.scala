/**
  * Created by malig on 2/28/19.
  */
object Main {
  def main(args: Array[String]): Unit = {

    val layers = new Layers(2,"/Users/malig/workspace/git/SymbolicDNN/layers")

    val symArr = new Array[SymbolicNeuron](2)
    for( i <- 0 to 1){
      symArr(i) = new SymbolicNeuron(Array(new PathEffect(new Constraint(Array()) , new SymVar(new Numeric(NumericUnderlyingType._Float), "x"+i))))
    }
    layers.printLayers
    val symDnn = new SymbolicDNN()

    def activation(input: PathEffect) : Array[PathEffect] = {
      // Representing ReLU
      val if_stmt = new Clause(input.effect , ComparisonOp.GreaterThan , new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0"))
      val effect_if = input.effect

      val else_stmt = new Clause(input.effect , ComparisonOp.LessThanOrEq , new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0"))
      val effect_else  = new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0")

      val p1 = new PathEffect(input.pathConstraint.conjunctWithSideEffectFree(if_stmt),effect_if)
      val p2 = new PathEffect(input.pathConstraint.conjunctWithSideEffectFree(else_stmt),effect_else)

      return Array(p1, p2)

    }

    symDnn.sym_Exec_DNN(symArr,layers , activation)

  }


}
