

/**
  * Created by malig on 2/28/19.
  */
object MainConcrete {

  var unreachable = 0
  var activateBoth = true
  def main(args: Array[String]): Unit = {
    var activations = new Activation()
    //activations.load("/Users/malig/workspace/git/SymbolicDNN/ActivationPattern/default")
    val layers = new Layers(5,"/Users/malig/workspace/git/SymbolicDNN/layers/regression" , activations)
    val arr =  Array[Float](2.3f,4.0f,5.1f, 0.1f,0.0002f,0.11f, 0.22f, 9.2f, 2.4f, 5.5f)
    layers.printLayers
    activations.print_activations()

    def activation(input: Float) : Boolean = {
          input > 0
    }

    val symDnn = new SymbolicDNN(Array(),layers , null)
    symDnn.concreteExecDNN(arr ,activation).foreach(println)
    activations.print_activations()
  }


}
