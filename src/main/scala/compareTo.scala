import FractionObject._
import StringAufgaben._

object compareTo {

  val compareDifferentType:Exception = new Exception("You tried to compare 2 different types")

  /**
   * Less function for any type
   * @param x first input
   * @param y 2nd input
   * @return Outputs Boolean depending if first input is smaller than 2nd
   */
  def compareTo(x:Any,y:Any):Boolean = (x,y) match{
    case (x:Boolean,y:Boolean) => !x && y
    case (x:Char,y:Char) => toUnicode(x)<toUnicode(y)
    case (x:Int,y:Int) => x<y
    case (x:BigInt,y:BigInt) => x<y
    case (x:String,y:String) => StringAufgaben.less(x,y)
    case (x:Fraction,y:Fraction) => FractionObject.less(x,y)

    case _ => throw new Exception("Data type not found or not equal")
  }
}
