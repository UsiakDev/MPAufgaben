import FractionObject._

object UnitsAndConversion {

  def main(args: Array[String]): Unit = {
    //println(toString2(BaseUnit("Meter")))
    //println(toString2(convertToBase(Quantity(Fraction(2,1),DerivedUnit("Kilometer",BaseUnit("Meter"),ProportionalConversion(Fraction(1000,1)))))))
    //println(toString2(Quantity(400,BaseUnit("Meter"))))
    //println(toString2(LinearConversion(Fraction(40,1),Fraction(3,1))))
    //println(toString2(ProportionalConversion(Fraction(1000,1))))
    //println(convertFromBase(Quantity(Fraction(30,1),BaseUnit("Grad Celsius")),
    // DerivedUnit("Kelvin",BaseUnit("Grad Celsius"),LinearConversion(Fraction(1,1),Fraction(-273,1)))))
  }

  /**
   * Conversion Method Abstract Class :
   * ProportionalConversion Example Meter -> Kilometer Factor 1/1000 cause Meter*(1/1000) -> Kilometer
   *  x*factor
   * LinearConversion Example Celsius -> Fahrenheit with m=18/10 and b=32/1 cause (18/10)*celsius+32 -> Fahrenheit
   *  m*x+b
   */
  abstract class ConversionMethod
  case class ProportionalConversion(factor:Fraction) extends ConversionMethod
  case class LinearConversion(m:Fraction,b:Fraction)      extends ConversionMethod

  /**
   * Abstract Unit Abstract Class :
   * BaseUnit is self explanatory
   * DerivedUnit is a extended BaseUnit, therefore : DerivedUnit,BaseUnit,ConversionMethod
   */
  abstract class AbstractUnit
  case class BaseUnit(baseUnit:String) extends AbstractUnit
  case class DerivedUnit(derivedUnit:String,baseUnit: BaseUnit,conMethod: ConversionMethod) extends AbstractUnit

  /**
   * Quantity of a Unit
   * @param amount How much of a Unit
   * @param unit Given Unit
   */
  case class Quantity(amount:Fraction,unit:AbstractUnit)

  /**
   * AbstractUnit to String
   * @param x given Abstract Unit
   * @return return String of Abstract Unit
   */
  def toString2(x:AbstractUnit):String = x match{
    case x:BaseUnit => x.baseUnit
    case x:DerivedUnit => x.derivedUnit
  }

  /**
   * Quantity to String
   * @param x given Quantity
   * @return return String of Quantity
   */
  def toString2(x:Quantity):String = {
    (if(x.amount.denominator!=1) FractionObject.toString(x.amount) else x.amount.enumerator) + " " + toString2(x.unit)
  }

  /**
   * ConversionMethod to String
   * @param x given ConversionMethod
   * @return String of ConversionMethod : Shows how it's calculating
   */
  def toString2(x:ConversionMethod):String = x match{
    case x:ProportionalConversion => "x * " + FractionObject.toString(x.factor)
    case x:LinearConversion => FractionObject.toString(x.m) + " * x + " + FractionObject.toString(x.b)
  }

  /**
   * Converts Amount via ConversionMethod
   * @param amount given Amount
   * @param convm given ConversionMethod
   * @return Calculated Amount with ConversionMethod as Fracion
   */
  def convertAmount(amount:Fraction, convm:ConversionMethod):Fraction = convm match{
    case convm:ProportionalConversion => *(amount,convm.factor)
    case convm:LinearConversion => add(*(convm.m,amount),convm.b)
  }

  /**
   * Converts Quantity to BaseUnit
   * @param x given Quantity
   * @return Returns Quantity in BaseUnit Form
   */
  def convertToBase(x:Quantity):Quantity = x.unit match{
    case _:BaseUnit => x
    case d:DerivedUnit => Quantity(convertAmount(x.amount,d.conMethod),d.baseUnit)
  }

  /**
   * Converts BaseUnit Quantity into TargetUnit if transferable!
   * @param q given Quantity
   * @param targetUnit self explanatory
   * @return Quantity of transformed Quantity into TargetUnit
   */
  def convertFromBase(q:Quantity,targetUnit:AbstractUnit):Quantity = q.unit match{
    case b:BaseUnit => targetUnit match{
      case d:DerivedUnit =>
        //Doppelte .baseUnit da beim ersten mal nur auf die Klasse BaseUnit zugegriffen wird
        if(d.baseUnit.baseUnit==b.baseUnit) Quantity(convertAmount(q.amount,getInverseConversion(d.conMethod)),d)
        else throw new Exception("Quantity and targetUnit dont have same BaseUnit!")
      case d:BaseUnit => if(d.baseUnit==b.baseUnit) q else throw new Exception("Quantity and targetUnit dont have same BaseUnit!")
    }
    case _:DerivedUnit => throw new Exception("Given Quantity is not in BaseUnit Form")
  }

  /**
   * Gets Inverse Conversion
   * @param convm given Conversion
   * @return ConversionMethod as Inverse to given Conversion
   */
  def getInverseConversion(convm:ConversionMethod):ConversionMethod = convm match {
    case f:ProportionalConversion => ProportionalConversion(/(Fraction(1,1),f.factor))
    case l:LinearConversion => LinearConversion(/(Fraction(1,1),l.m),*(Fraction(-1,1),l.b))
  }

  /**
   * Converts any Quantity into different Unit if transferable!
   * @param q given Quantity
   * @param into transfer to this TargetUnit
   * @return transfered Quantity
   */
  def convert(q:Quantity,into:AbstractUnit):Quantity = q.unit match{
    case _:BaseUnit => convertFromBase(q,into)
    case d:DerivedUnit =>
      into match{
        case b:BaseUnit =>
          if(d.baseUnit.baseUnit==b.baseUnit) convertToBase(q)
          else throw new Exception("Quantity and targetUnit dont have same BaseUnit!")
        case _:DerivedUnit => convertFromBase(convertToBase(q),into)
      }
  }

}