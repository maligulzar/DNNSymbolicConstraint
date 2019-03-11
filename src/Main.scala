import java.util

import udfExtractor.Configuration

import scala.collection.mutable

/**
  * Created by malig on 2/28/19.
  */
object Main {


val map:util.HashMap[String,String] = new util.HashMap[String, String]();
  map.put("Z3_lib","/Users/Downloads/z3/build/");
  map.put("PYTHON_PATH","/Users/Downloads/z3/build/python");
  var activations = new Activation("/Users/Aish/Downloads/SymbolicDNN/ActivationPattern/6")
  var unreachable = 0
  var activateBoth = true
  def main(args: Array[String]): Unit = {

    val layers = new Layers(3,"/Users/Aish/Downloads/SymbolicDNN/layers/6")
    val inputsize = 5

    val symArr = new Array[SymbolicNeuron](inputsize)
    for( i <- 0 to inputsize-1){
      symArr(i) = new SymbolicNeuron(Array(new PathEffect(new Constraint(Array()) , new SymVar(new Numeric(NumericUnderlyingType._Float), "x"+i))))
    }
    layers.printLayers
    activations.print_activations()

    def activation(input: PathEffect) : Array[PathEffect] = {
      // Representing ReLU
      val if_stmt = new Clause(input.effect , ComparisonOp.GreaterThan , new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0"))
      val effect_if = input.effect
      val p1 = new PathEffect(input.pathConstraint.conjunctWithSideEffectFree(if_stmt),effect_if)
      if(activateBoth) {
              val else_stmt = new Clause(input.effect , ComparisonOp.LessThanOrEq , new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0"))
              val effect_else  = new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0")
              val p2 = new PathEffect(input.pathConstraint.conjunctWithSideEffectFree(else_stmt),effect_else)
              return Array(p1, p2)
      }

      return Array(p1)

    }


    val symDnn = new SymbolicDNN(symArr,layers , activation)

//    val symneuron = symDnn.symExecForANeuron(2,1)
//    symDnn.solveConstraints(inputsize , symneuron)

    symDnn.sym_Exec_DNN()
    print("hello here");
    symDnn.print_SymbolicNeuron()
    val startTime = System.nanoTime
    symDnn.solveConstraints(inputsize)
    val endTime = System.nanoTime
    val duration = endTime - startTime
    printf("Time taken "+duration/1000000);
    printf("Number of Unreachable "+unreachable);
  }


}
