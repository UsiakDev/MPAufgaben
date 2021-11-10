import org.scalatest.FunSuite

class UnitsAndConversionTests extends FunSuite{
  abstract class ConversionMethod
  case class ProportionalConversion(factor:Int)
  case class LinearConversion(m:Int,b:Int)

}
