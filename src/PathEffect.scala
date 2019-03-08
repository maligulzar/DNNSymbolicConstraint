
import scala.collection.mutable.ArrayBuffer
import java.util.HashSet
import scala.collection.mutable.HashMap

class PathEffect(pc: Constraint, udfEffect: Expr) {
  var pathConstraint: Constraint = pc


  var effect: Expr = udfEffect


  override def toString: String = {
    var eString: String = effect.toString
    "path constraint: {" + pathConstraint.toString + "}\t effect: {" + eString + "} ---------"
  }


//  def getEffectZ3Query(initial: Z3QueryState): String = {
//    var eString: String = ""
//    var rName: String = ""
//    // val clauses: util.ArrayList[Clause] = new util.ArrayList[Clause]()
//    val clauses: Array[Clause] = new Array[Clause](effects.size)
//    var i = 0;
//    for (e <- effects) {
//      clauses(i) = new Clause(e._1, ComparisonOp.Equality, e._2)
//      i = i + 1
//    }
//    val pathCond = new Constraint(clauses.toArray)
//    return pathCond.toZ3Query(initial)
//  }


  //    def generateSplitConstraints(state: Z3QueryState ): String = {
  //      var s = ""
  //      //var buff = new ArrayBuffer[String]()
  //      for( (k,v) <- state.split ){
  //          val del = v.del
  //          val arr = v.str_arr
  //          val query  = arr.reverse.map(s=> if(s==null) "\" \"" else s).reduce((a,b) => "(str.++ " + "(str.++ " + b +del+" )  " + a +")")
  //        //     arr.filter(s => s!=null).map( a => buff.append(a))
  //      }
  //
  //
  //      //    return buff.filter(s=>s!=null).reduce(_+"\n"+_)
  //    }

  def toZ3Query(): String = {

    val list: HashSet[(String, VType)] = new HashSet[(String, VType)]();

    val replace = new HashMap[String, String]();

    val state: Z3QueryState = Z3QueryState(list, replace)
    var pc = pathConstraint.toZ3Query(state)
    //fix the references

    // for((k,v) <- state.replacements){
    //  pc =  pc.replaceAll(v, k)
    // }

    var decls = s"""
                   |(set-logic AUFLIRA)
                   |(set-option :produce-models true)
                   |
                   |
          |""".stripMargin
    val itr = state.init.iterator()
    while (itr.hasNext) {
      val i = itr.next()
      decls +=
        s"""(declare-fun ${i._1} () ${i._2.toZ3Query()})
           |""".stripMargin
    }
    s"""$decls
       |$pc
       |

     """.stripMargin //,generateSplitConstraints(state))
  }

  def processOutput(str: String) {
    val arr = str.split("\n")
    val var_map = HashMap[String, String]()
    //      arr.map(s => s.split(":")).filter(s => s.length>0).map{
    //        s =>s
    //          var_map(s(0)) = s(1)
    //      ""}
  }

  //    def generateFinalData(map: HashMap ): String = {
  //      var s = ""
  //      var buff = new ArrayBuffer[String]()
  //      for( (k,v) <- state.split ){
  //          val del = v.del
  //          val arr = v.str_arr
  //         // val query  = arr.reverse.map(s=> if(s==null) "\" \"" else s).reduce((a,b) => "(str.++ " + "(str.++ " + b +del+" )  " + a +")")
  //             arr.filter(s => s!=null).map( a => buff.append(a))
  //      }
  //      return buff.filter(s=>s!=null).reduce(_+"\n"+_)
  //    }
  //
}


case class Z3QueryState(init: HashSet[(String, VType)], replacements: HashMap[String, String]) {

  def addtoInit(a: (String, VType)) {
    val itr = init.iterator()
    while (itr.hasNext) {
      val de = itr.next()
      if (a._1.equals(de._1)) {
        return
      }
    }
    init.add(a);

  }
}
