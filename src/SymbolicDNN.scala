import java.io.File


/**
  * Created by malig on 2/28/19.
  */
class SymbolicDNN {

  def sym_Exec_DNN(input: Array[Expr], layers: Layers): Unit = {
    layers.se_compute(input).map(s => println(s.toString))
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

  def compute(input: Array[Expr]): Array[Expr] = {
    val return_array = new Array[Expr](neuron_count)
    for (i <- 0 to neuron_count - 1) {
      var expr = multiple(input(0),
        new ConcreteValue(
          new Numeric(
            NumericUnderlyingType._Float
          ), matrix(0)(i).toString
        )
      )

      for (j <- 1 to input.length - 1) {
        expr = add(expr,
          multiple(input(j),
            new ConcreteValue(
              new Numeric(
                NumericUnderlyingType._Float
              ), matrix(j)(i).toString
            )
          )
        )
      }
      return_array(i) = expr
    }
    return_array
  }

  def add(left: Expr, right: Expr): ArithmeticExpr = {
    new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Addition), right)
  }

  def multiple(left: Expr, right: Expr): ArithmeticExpr = {
    new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Multiplication), right)
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


  def se_compute(input :Array[Expr]): Array[Expr] ={
    var current  = input
    for(matrix <- w_matrices){
      current  = matrix.compute(current)
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