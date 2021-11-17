import StringAufgaben._

import scala.annotation.tailrec

object FractionObject {
  def main(args: Array[String]): Unit = {
    println(Fraction(3,5))
  }

  val divider:Char = '/'
  val div0:Exception = new Exception("Dividing by 0")

  /**
   * private Fraction class
   * @param enumerator given enumerator
   * @param denominator given denominator ; => enumerator / denominator
   */
  case class Fraction(enumerator:BigInt,denominator:BigInt){
    if(denominator==0) throw div0
  }

  /**
   * Mod Function for BigInt
   * @param x first int
   * @param y second int
   * @return rest after division
   */
  def %(x:BigInt,y:BigInt):BigInt = {
    if(y==0) throw div0
    else x - y*(x/y)
  }

  /**
   * Returns absolute value of a BigInt
   * @param x given BigInt
   * @return absolute value of BigInt
   */
  def bigIntAbs(x:BigInt):BigInt = if(x<0) -x else x

  /**
   * ggT Function
   * @param x first int
   * @param y second int
   * @return outputs greatest common divisor
   */
  @tailrec def ggT(x:BigInt, y:BigInt):BigInt = {
    if(y==0) bigIntAbs(x)
    else ggT(y,%(x,y))
  }

  val One:BigInt = BigInt(1)
  /**
   * returns Fraction as String
   * @param x given Fraction
   * @return Outputs Fraction as String
   */
  def toString(x:Fraction):String = x.denominator match{
    case One => toString2(x.enumerator)
    case _ => toString2(x.enumerator) + divider + toString2(x.denominator)
  }

  /**
   * Rationalising a Fraction | Puts minus in the right place
   * @param x given Fraction
   * @return Outputs rationalised Fraction
   */
  @tailrec def toRational(x:Fraction):Fraction = {
    if(x.denominator<0) toRational(Fraction(x.enumerator*(-1),x.denominator*(-1)))
    else if(ggT(x.enumerator,x.denominator)==1) x
    else{
      val ggT:BigInt = FractionObject.ggT(x.enumerator,x.denominator)
      Fraction(x.enumerator/ggT,x.denominator/ggT)
    }
  }

  /**
   * Checks if 2 Fractions have the same Value
   * @param x first Fraction
   * @param y second Fraction
   * @return Outputs true if they are the same, otherwise false
   */
  def sameValue(x:Fraction,y:Fraction):Boolean = toRational(x)==toRational(y)

  /**
   * Checks if first Fraction is smaller than 2nd
   * @param x first Fraction
   * @param y second Fraction
   * @return Outputs true if first Fraction is smaller; otherwise false
   */
  def less(x:Fraction,y:Fraction):Boolean = x.enumerator*y.denominator<y.enumerator*x.denominator

  /**
   * add Function for Fractions
   * @param x first Fraction
   * @param y second Fraction
   * @return Outputs addition of first and second Fraction
   */
  def add(x:Fraction,y:Fraction):Fraction =
    toRational(Fraction(x.enumerator*y.denominator+y.enumerator*x.denominator, x.denominator*y.denominator))

  /**
   * subtract Function for Fractions
   * @param x first Fraction
   * @param y second Fraction
   * @return Outputs subtraction of first and second Fraction
   */
  def subtract(x:Fraction,y:Fraction):Fraction =
    toRational(Fraction(x.enumerator*y.denominator-y.enumerator*x.denominator, x.denominator*y.denominator))

  /**
   * * function for Fractions
   * @param x first Fraction
   * @param y second Fraction
   * @return Outputs multiplication of first and second Fraction
   */
  def *(x:Fraction,y:Fraction):Fraction =
    toRational(Fraction(x.enumerator*y.enumerator,x.denominator*y.denominator))

  /**
   * / function for Fractions
   * @param x first Fraction
   * @param y second Fraction
   * @return Outputs division of first and second Fraction
   */
  def /(x:Fraction,y:Fraction):Fraction =
    toRational(Fraction(x.enumerator*y.denominator,x.denominator*y.enumerator))
}
