import java.util

/**
  * Created by malig on 2/28/19.
  */
object Main_Neuron {

  var unreachable = 0
  var activateBoth = true
  def main(args: Array[String]): Unit = {
    var activations = new Activation()
    activations.load("/Users/malig/workspace/git/SymbolicDNN/ActivationPattern/default")
    val layers = new Layers(2,"/Users/malig/workspace/git/SymbolicDNN/layers/default" , activations)
    val inputsize = 3

    val symArr = new Array[SymbolicNeuron](inputsize)
    for( i <- 0 to inputsize-1){
      symArr(i) = new SymbolicNeuron(Array(new PathEffect(new Constraint(Array()) , new SymVar(new Numeric(NumericUnderlyingType._Float), "x"+i))))
    }
    layers.printLayers

    def activation(input: PathEffect , activate : Boolean) : Array[PathEffect] = {
      // Representing ReLU

      val if_stmt = new Clause(input.effect , ComparisonOp.GreaterThan , new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0"))
      val effect_if = input.effect
      val p1 = new PathEffect(input.pathConstraint.conjunctWithSideEffectFree(if_stmt),effect_if)
      val else_stmt = new Clause(input.effect , ComparisonOp.LessThanOrEq , new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0"))
      val effect_else  = new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0")
      val p2 = new PathEffect(input.pathConstraint.conjunctWithSideEffectFree(else_stmt),effect_else)
      return Array(p1, p2)


    }


    val symDnn = new SymbolicDNN(symArr,layers , activation)
    val startTime = System.nanoTime
    val symneuron = symDnn.symExecForANeuron(1,2)
    symDnn.solveConstraints(inputsize , symneuron)
    val endTime = System.nanoTime
    val duration = endTime - startTime
    printf("Time taken "+duration/1000000);
   // printf("\nNumber of Unreachable "+unreachable);
  }


}
