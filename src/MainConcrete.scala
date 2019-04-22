import java.io.File

/**
  * Created by malig on 2/28/19.
  */
object MainConcrete {

  var unreachable = 0
  var activateBoth = true
  def main(args: Array[String]): Unit = {
    var activations = new Activation()
//    activations.load("/Users/malig/workspace/git/SymbolicDNN/ActivationPattern/default")
    val layers = new Layers(7,"/Users/malig/workspace/git/SymbolicDNN/layers/AcasXu/" , activations)

    val inputs = readInputValues("/Users/malig/workspace/git/SymbolicDNN/ActivationPattern/AcasXu/input",1)
    layers.printLayers
    activations.print_activations()

    def activation(input: Float) : Boolean = {
          input > 0
    }
    for(input <- inputs) {
      val symDnn = new SymbolicDNN(Array(), layers, null)
      symDnn.concreteExecDNN(input, activation).foreach(println)
      activations.print_activations()

    }
  }

  def readInputValues(filePath: String, inputCount: Int): Array[Array[Float]] ={
    val inputs = new Array[Array[Float]](inputCount)
    val files = getListOfFiles(filePath)
    for (file <- files) {
      val source = scala.io.Source.fromFile(file)
      try {
        val txt = source.getLines().mkString("\n")
        val lines = txt.split("\n")
        var i =0
        for (line <- lines) {
          inputs(i) = line.split(",").map(_.toFloat)
          i=i+1
        }
      }
      finally source.close()
    }
    return inputs
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
}
