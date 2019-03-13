import java.io.{BufferedWriter, File, FileWriter}
import java.util

import udfExtractor.SystemCommandExecutor

/**
  * Created by malig on 3/4/19.
  */
class SymbolicNeuron(p: Array[PathEffect]) {

  var paths: Array[PathEffect] = p


  def addNeuron(neuron : SymbolicNeuron) : SymbolicNeuron = {
    val array_paths = new Array[PathEffect](this.paths.length * neuron.paths.length)
    var i = 0
    for(p1 <- this.paths){
      for ( p2 <- neuron.paths){
        array_paths(i) = new PathEffect(p1.pathConstraint.conjunctWithSideEffectFree(p2.pathConstraint), add(p1.effect, p2.effect))
        i = i + 1
      }
    }
    new SymbolicNeuron(array_paths)
  }

  def applyActivation(af : (PathEffect,Boolean) => Array[PathEffect] , activate : Boolean) = {
    var array_paths = new Array[PathEffect](0)
    for(p <- paths){
      array_paths = array_paths ++ af(p ,activate)
    }
    paths = array_paths
  }

  def computeNeuronValue(w : Float) : SymbolicNeuron = {
    val duplicate_pe  = new Array[PathEffect](paths.length)
    var i = 0;
    for (p <- paths){
      duplicate_pe(i) = new PathEffect(p.pathConstraint, multiple(p.effect, w))
      i = i+1
    }
    return new SymbolicNeuron(duplicate_pe)
  }

  def add(left: Expr, right: Float): ArithmeticExpr = {
    new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Addition), new ConcreteValue(
      new Numeric(
        NumericUnderlyingType._Float
      ), right.toString //f"${1}%1f
    )
    )
  }

  def add(left: Expr, right: Expr): ArithmeticExpr = {
    new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Addition), right)
  }

  def multiple(left: Expr, right: Float): Expr = {
    if(right == 0.0){
      return new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0")
    }

    if(left.isInstanceOf[ConcreteValue]){
      if(left.asInstanceOf[ConcreteValue].actualType.asInstanceOf[Numeric].underlyingType == NumericUnderlyingType._Float && left.asInstanceOf[ConcreteValue].value.equals("0")){
        return new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0")
      }
    }


    return    new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Multiplication), new ConcreteValue(
      new Numeric(
        NumericUnderlyingType._Float
      ), right.toString // f"${1}%1f
    )
    )
  }

  def multiple(left: Expr, right: Expr): Expr = {

    if(left.isInstanceOf[ConcreteValue]){
      if(left.asInstanceOf[ConcreteValue].actualType.asInstanceOf[Numeric].underlyingType == NumericUnderlyingType._Float && left.asInstanceOf[ConcreteValue].value.equals("0")){
        return new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0")
      }
    }

    if(right.isInstanceOf[ConcreteValue]){
      if(right.asInstanceOf[ConcreteValue].actualType.asInstanceOf[Numeric].underlyingType == NumericUnderlyingType._Float && right.asInstanceOf[ConcreteValue].value.equals("0")){
        return new ConcreteValue(new Numeric(NumericUnderlyingType._Float) , "0")
      }
    }

    return new ArithmeticExpr(left, new SymOp(Numeric(NumericUnderlyingType._Float), ArithmeticOp.Multiplication), right)

  }




  def writeTempSMTFile(filename: String, z3: String): Unit = {
    try {
      val file: File = new File(filename)
      if (!file.exists) {
        file.createNewFile
      }
      val fw: FileWriter = new FileWriter(file)
      val bw = new BufferedWriter(fw)
      bw.write(z3);
      bw.close();
    } catch {
      case ex: Exception =>
        ex.printStackTrace();
    }
  }

  def runZ3Command(filename: String, Z3dir: String, args: Array[String] = Array() , log :Boolean = false , solver: String, neurons:Int): String = {
    // build the system command we want to run
    var s = ""
    if (solver.equals("CVC4")) {
      s = "/Users/malig/Downloads/cvc4-1.5/builds/x86_64-apple-darwin16.7.0/production/bin/cvc4 --strings-exp --lang smt2 < " + filename

    } else {
      s = "python " + Z3dir + "runZ3.py " + filename + " " + neurons
    }

    for (a <- args) {
      s = s + "  " + a
    }
    if(log) println("run z3 for file " + s)
    try {
      val commands: util.List[String] = new util.ArrayList[String]
      commands.add("/bin/sh")
      commands.add("-c")
      commands.add(s)
      val commandExecutor: SystemCommandExecutor =
        new SystemCommandExecutor(commands, Z3dir)
      val result: Int = commandExecutor.executeCommand();
      val stdout: java.lang.StringBuilder =
        commandExecutor.getStandardOutputFromCommand
      val stderr: java.lang.StringBuilder =
        commandExecutor.getStandardErrorFromCommand
    //  println("********** Satisfying Assigments **********************************************")
      val str_lines = stdout.toString.split("\n")//.filter(p => p.contains("x"))
      if(str_lines.size > 0 )
        println(str_lines.reduce(_+"\n"+_))

      //println("*******************************************************************************")
      if(!stderr.toString.isEmpty()){
        Main.unreachable +=1
      }
      println("\n" + stderr.toString)
      return stdout.toString()
    } catch {
      case e: Exception => {
        e.printStackTrace
      }
    }
    return "";
  }


  def solveWithZ3(log: Boolean = false , solver:String , z3dir:String , neurons :Int): Unit = {
    var i = 0
    Main.unreachable = 0
    for (path <- paths) {
      var str = path.toZ3Query();
      if (solver.equals("CVC4")) {
        str = str + "\n(check-sat)\n(get-model)"
      }
      var filename = "/tmp/" + path.hashCode()+".bt";
      writeTempSMTFile(filename, str);
      if(log) {
        println(path.toString)
        println("Z3Query:\n" + str)
      }
        println("------------------------")
        println("Paths :  " + i)
        println(path)
      i = i+1
      runZ3Command(filename, z3dir , log=log, solver=solver , neurons = neurons);
      println("------------------------")
      println("Number of Unreachable "+Main.unreachable);

    }
  }



  override def toString: String = {
    var  i = 0
    var string = ""
    for(p <- paths){
      string = string + s"""Path:$i \n Constraints: ${p.pathConstraint.toString}\n Effect: ${p.effect.toString}\n"""
      i = i+1
    }
    return string
  }



}
