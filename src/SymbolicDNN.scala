import java.io.File


/**
  * Created by malig on 2/28/19.
  */
class SymbolicDNN {
  def sym_Exec_DNN(input: Array[SymbolicNeuron], layers: Layers , af : PathEffect => Array[PathEffect]): Unit = {
    layers.se_compute(input , af).map(s => println(s.toString))
  }
}

/**
  *
  * Class representing a weight matrix for each layer
  *
  * */
class WeightMatrix(layer: Int, neuron_count: Int, prev_layer_neuron_count: Int) {

  private val matrix = Array.ofDim[Float](prev_layer_neuron_count, neuron_count)

  def insert(r: Int, c: Int, value: Float) = matrix(r - 1)(c - 1) = value

  def get(r: Int, c: Int): Float = matrix(r - 1)(c - 1)

  def print(): Unit = {
    println(s"""Layer $layer """)
    for (arr <- matrix) {
      println(arr.map(_.toString).reduce(_ + "," + _))
    }
    println("\n")
  }

  def compute(input: Array[SymbolicNeuron] , af : PathEffect => Array[PathEffect]): Array[SymbolicNeuron] = {
    val return_array = new Array[SymbolicNeuron](neuron_count)
    for (i <- 0 to neuron_count - 1) {
      var sym_neuron = input(0).computeNeuronValue(matrix(0)(i))
      for (j <- 1 to input.length - 1) {
        val sym_neuron_temp = input(j).computeNeuronValue(matrix(j)(i))
        sym_neuron = sym_neuron.addNeuron(sym_neuron_temp)
      }
      sym_neuron.applyActivation(af)
      return_array(i) = sym_neuron
    }
    return_array
  }



}
/**
  *
  * layers encode the weight matrices for all the layers in the DNN
  *
  * */
class Layers(layers: Int, path_to_dir: String) {

  private val w_matrices = new Array[WeightMatrix](layers)

  load(path_to_dir)


  def se_compute(input :Array[SymbolicNeuron] , af : PathEffect => Array[PathEffect]): Array[SymbolicNeuron] ={
    var current  = input
    for(matrix <- w_matrices){
      current  = matrix.compute(current, af)
    }
    return current
  }
  def getLayerWeight(layer: Int): WeightMatrix = {
    return w_matrices(layer - 1)
  }

  def printLayers: Unit = {
    for (matrix <- w_matrices) {
      matrix.print()
    }

  }

  def load(path: String): Unit = {
    val files = getListOfFiles(path)
    for (file <- files) {
      val source = scala.io.Source.fromFile(file)
      try {
        val layer = file.getName.split("\\.")(0).toInt
        val txt = source.getLines().mkString("\n")
        val lines = txt.split("\n")
        val prev_layer_neuron = lines.length
        val r1 = lines(0).split(",")
        val neuron_count = r1.length
        val matrix = new WeightMatrix(layer, neuron_count, prev_layer_neuron)
        var r = 0
        var c = 0
        for (line <- lines) {
          val row = line.split(",")
          c = 0
          for (v <- row) {
            matrix.insert(r + 1, c + 1, v.toFloat)
            c = c + 1
          }
          r = r + 1
        }
        addLayerWeight(matrix, layer)
      }
      finally source.close()
    }
  }

  def addLayerWeight(matrix: WeightMatrix, layer: Int) = {
    w_matrices(layer - 1) = matrix
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