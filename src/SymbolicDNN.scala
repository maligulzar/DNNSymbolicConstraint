import java.io.{BufferedWriter, File, FileWriter}
import java.util
import java.util.HashSet

import udfExtractor.SystemCommandExecutor

import scala.collection.mutable.ArrayBuffer
/**
  * Created by malig on 2/28/19.
  */
class SymbolicDNN(input: Array[SymbolicNeuron], layers: Layers , af : PathEffect => Array[PathEffect]) {


  var Z3DIR: String = "/Users/Aish/Downloads/"
  var SOLVER: String = "Z3"

  def setZ3Dir(path: String) {
    Z3DIR = path
  }

  def setSolver(path: String) {
    SOLVER = path
  }

  var current_symbolic_neurons : Array[SymbolicNeuron] = new Array[SymbolicNeuron](0)


  def symExecForANeuron(neuron: Int, layer: Int): SymbolicNeuron = {
    layers.se_compute(input , af ,neuron, layer)
  }

  def sym_Exec_DNN(): Unit = {
    current_symbolic_neurons = layers.se_compute(input , af)
  }

  def print_SymbolicNeuron(): Unit ={
    current_symbolic_neurons.map(s => println(s.toString))
  }

  def solveConstraints(neurons:Int): Unit ={
    current_symbolic_neurons.map(s => s.solveWithZ3(log=false, SOLVER, Z3DIR , neurons))
  }

  def solveConstraints(neurons:Int , symNeuron:SymbolicNeuron): Unit ={
    symNeuron.solveWithZ3(log=false, SOLVER, Z3DIR , neurons)
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

//      if((I,J) == (NEURON, LAYER))
      println(Main.activations.checkForPattern(this.layer+":"+(i+1)))
      if(Main.activations.checkForPattern(this.layer+":"+(i+1))==true){
        Main.activateBoth = false
      } else{
        Main.activateBoth = true
      }
      sym_neuron.applyActivation(af)
      return_array(i) = sym_neuron
      printf("For layer "+this.layer);
      printf(sym_neuron.toString);
    }
    return_array
  }



}

class Activation( path_to_dir: String){
  private var activationPattern = new ArrayBuffer[String]()
  load(path_to_dir)

  def checkForPattern(pattern:String): Boolean ={
    for(patt <- activationPattern){
      if(patt.contains(pattern)){
        return true
      }
    }
    false
  }
  def print_activations(): Unit ={
    println(activationPattern)
  }
  def load(path: String): Unit = {
    val files = getListOfFiles(path)
    for (file <- files) {
      val source = scala.io.Source.fromFile(file)
      try {
        val txt = source.getLines().mkString("\n")
        val lines = txt.split("\n")
        var r = 0
        var c = 0
        for (line <- lines) {
          activationPattern.+=(line)
        }
      }
      finally source.close()
    }
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


  def se_compute(input :Array[SymbolicNeuron] , af : PathEffect => Array[PathEffect] , neuron:Int , layer:Int): SymbolicNeuron ={
    var current  = input
    if(layer > w_matrices.length){
      println("Given layer exceed the total number of layers")
      System.exit(1)
    }
    for(a <- 0 to layer-1){
      current  = w_matrices(a).compute(current, af)
    }
    if(neuron > current.length){
      println("Given neuron location greater than the total number of neuron at this layer")
      System.exit(1)
    }
    return current(neuron-1)
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