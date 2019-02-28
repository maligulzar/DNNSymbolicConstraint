/**
  * Created by malig on 2/28/19.
  */
object Main {
  def main(args: Array[String]): Unit = {

    val layers = new Layers(2,"/Users/malig/workspace/git/SymbolicDNN/layers")

    val symArr = new Array[Expr](3)
    for( i <- 0 to 2){
      symArr(i) = new SymVar(new Numeric(NumericUnderlyingType._Float), "x"+i)
    }
    layers.printLayers
    val symDnn = new SymbolicDNN()
    symDnn.sym_Exec_DNN(symArr,layers)

  }


}
