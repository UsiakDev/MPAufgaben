import FractionObject._

object UnitsAndConversion {

  def main(args: Array[String]): Unit = {
    //println(toString2(BaseUnit("Meter")))
    //println(toString2(Quantity(400,BaseUnit("Meter"))))
    //println(toString2(LinearConversion(40,3)))
    //println(convertFromBase(Quantity(Fraction(30,1),BaseUnit("Grad Celsius")),DerivedUnit("Kelvin",BaseUnit("Grad Celsius"),LinearConversion(Fraction(1,1),Fraction(-273,1)))))
  }

  abstract class ConversionMethod
  case class ProportionalConversion(factor:Fraction) extends ConversionMethod
  case class LinearConversion(m:Fraction,b:Fraction)      extends ConversionMethod

  abstract class AbstractUnit
  case class BaseUnit(baseUnit:String) extends AbstractUnit
  case class DerivedUnit(derivedUnit:String,baseUnit: BaseUnit,conMethod: ConversionMethod) extends AbstractUnit

  case class Quantity(amount:Fraction,unit:AbstractUnit)

  def toString2(x:AbstractUnit):String = x match{
    case x:BaseUnit => x.baseUnit
    case x:DerivedUnit => x.derivedUnit
  }

  def toString2(x:Quantity):String = {
    (if(x.amount.denominator!=1) FractionObject.toString(x.amount) else x.amount.enumerator) + " " + toString2(x.unit)
  }

  def toString2(x:ConversionMethod):String = x match{
    case x:ProportionalConversion => FractionObject.toString(x.factor)
    case x:LinearConversion => FractionObject.toString(x.m) + " * x + " + FractionObject.toString(x.b)
  }

  def convertAmount(amount:Fraction, convm:ConversionMethod):Fraction = convm match{
    case convm:ProportionalConversion => *(amount,convm.factor)
    case convm:LinearConversion => add(*(convm.m,amount),convm.b)
  }

  def convertToBase(x:Quantity):Quantity = x.unit match{
    case _:BaseUnit => Quantity(x.amount,x.unit)
    case d:DerivedUnit => Quantity(convertAmount(x.amount,d.conMethod),d.baseUnit)
  }

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

  def getInverseConversion(convm:ConversionMethod):ConversionMethod = convm match {
    case f:ProportionalConversion => ProportionalConversion(/(Fraction(1,1),f.factor))
    case l:LinearConversion => LinearConversion(/(Fraction(1,1),l.m),*(Fraction(-1,1),l.b))
  }

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