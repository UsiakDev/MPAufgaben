object UnitsAndConversion {
  abstract class ConversionMethod
  case class ProportionalConversion(factor:Int)
  case class LinearConversion(m:Int,b:Int)
}