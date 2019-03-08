
object NumericUnderlyingType extends Enumeration {
  type NumericUnderlyingType = Value
  val _Float = Value("Real")
}

import NumericUnderlyingType._

abstract class VType {
  def toZ3Query(): String
}

case class Numeric(ut: NumericUnderlyingType) extends VType {
  val underlyingType: NumericUnderlyingType = ut
  def toZ3Query(): String = {
    ut.toString
  }
}