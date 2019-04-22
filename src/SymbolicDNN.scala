import java.io.{BufferedWriter, File, FileWriter}

import com.microsoft.z3.Context

import scala.collection.mutable.ArrayBuffer
/**
  * Created by malig on 2/28/19.
  */

object SymbolicDNN{
  val c = new Context()
}
class SymbolicDNN(input: Array[SymbolicNeuron], layers: Layers , af : (PathEffect , Boolean) => Array[PathEffect]) {


  var Z3DIR: String = "/Users/malig/workspace/up_jpf/"
    //"/Users/Aish/Downloads/"
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

  def symExecDNN(): Unit = {
    current_symbolic_neurons = layers.se_compute(input , af)
  }

  def concreteExecDNN(arr : Array[Float] , act: Float=> Boolean): Array[Float] = {
    return layers.concreteCompute(arr , act)
  }
  def print_SymbolicNeuron(): Unit ={
    current_symbolic_neurons.map(s => println(s.toString))
  }

  def solveConstraints(neurons:Int): Unit ={
    //current_symbolic_neurons.map(s => s.solveWithZ3(log=false, SOLVER, Z3DIR , neurons))
    current_symbolic_neurons.map(s => s.solverOnline(neurons))

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
class WeightMatrix(layer: Int, neuron_count: Int, prev_layer_neuron_count: Int , activation: Activation) {

  private val matrix = Array.ofDim[Float](prev_layer_neuron_count, neuron_count)
  private var transposedMatrix = Array.ofDim[Float](neuron_count,prev_layer_neuron_count)

  def transpose() :Unit={
    transposedMatrix = matrix.transpose
  }

  def insert(r: Int, c: Int, value: Float) = matrix(r - 1)(c - 1) = value

  def get(r: Int, c: Int): Float = matrix(r - 1)(c - 1)

  def print(): Unit = {
    println(s"""Layer $layer """)
    for (arr <- matrix) {
      println(arr.map(_.toString).reduce(_ + "," + _))
    }
    println("\n")
  }


  def concreteCompute(input: Array[Float] , af : Float => Boolean): Array[Float] = {
    val return_array = new Array[Float](neuron_count)
    try{

    for (i <- 0 to neuron_count - 1) {
      var sym_neuron = input(0)*transposedMatrix(0)(i)
      for (j <- 1 to input.length - 1) {
        val sym_neuron_temp = input(j)*transposedMatrix(j)(i)
        sym_neuron = sym_neuron + sym_neuron_temp
      }

      if(af(sym_neuron)){
        activation.addPattern(this.layer+":"+(i+1))
      }else{

      }
      return_array(i) = sym_neuron
    }
    } catch{
      case e: Exception => println(e.getMessage)
    }
    return_array
  }

  def compute(input: Array[SymbolicNeuron] , af : (PathEffect , Boolean) => Array[PathEffect]): Array[SymbolicNeuron] = {
    val return_array = new Array[SymbolicNeuron](neuron_count)
    for (i <- 0 to neuron_count - 1) {
      var sym_neuron = input(0).computeNeuronValue(matrix(0)(i))
      for (j <- 1 to input.length - 1) {
        val sym_neuron_temp = input(j).computeNeuronValue(matrix(j)(i))
        sym_neuron = sym_neuron.addNeuron(sym_neuron_temp)
      }

      println(activation.checkForPattern(this.layer+":"+(i+1)))
      if(activation.checkForPattern(this.layer+":"+(i+1))==true){
        Main.activateBoth = false
      } else{
        Main.activateBoth = true
      }
      sym_neuron.applyActivation(af ,Main.activateBoth)
      return_array(i) = sym_neuron
      println("For layer " + this.layer + " activated : " + Main.activateBoth);
     // printf(sym_neuron.toString);
    }
    return_array
  }



}

class Activation(){


  private var activationPattern = new ArrayBuffer[String]()



  def addPattern(pattern:String) ={
   activationPattern.append(pattern)
  }

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
        val lines = txt.split(",")
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
class Layers(layers: Int, path_to_dir: String , activations :Activation) {

  private val w_matrices = new Array[WeightMatrix](layers)

  load(path_to_dir)


  def se_compute(input :Array[SymbolicNeuron] , af : (PathEffect , Boolean) => Array[PathEffect]): Array[SymbolicNeuron] ={
    var current  = input
    for(matrix <- w_matrices){
      current  = matrix.compute(current, af)
      current.map( s => s.simplify(SymbolicDNN.c))
    }
    return current
  }

  def concreteCompute(input :Array[Float] , af :Float => Boolean): Array[Float] ={
    var current  = input
    for(matrix <- w_matrices){
      current  = matrix.concreteCompute(current, af)
    }
    return current
  }


  def se_compute(input :Array[SymbolicNeuron] , af : (PathEffect , Boolean) => Array[PathEffect] , neuron:Int , layer:Int): SymbolicNeuron ={
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
        val matrix = new WeightMatrix(layer, neuron_count, prev_layer_neuron , activations)
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
        matrix.transpose()
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