import org.scalatest.FunSuite
import UnitsAndConversion._
import FractionObject._

class UnitsAndConversionTests extends FunSuite{
  test("toString Tests"){
    assert(toString2(BaseUnit("Meter"))=="Meter")
    assert(toString2(DerivedUnit("Kilometer",BaseUnit("Meter"),ProportionalConversion(Fraction(1000,1))))=="Kilometer")
    assert(toString2(ProportionalConversion(Fraction(1000,1)))=="1000")
    assert(toString2(LinearConversion(Fraction(40,1),Fraction(5,1)))=="40 * x + 5")
    assert(toString2(Quantity(Fraction(400,1),BaseUnit("Meter")))=="400 Meter")
    assert(toString2(Quantity(Fraction(400,2),BaseUnit("Meter")))=="400/2 Meter")
  }

  test("convertAmount Test"){
    assert(convertAmount(Fraction(2,5),ProportionalConversion(Fraction(1000,1)))==Fraction(400,1))
    assert(convertAmount(Fraction(2,5),LinearConversion(Fraction(-10,1),Fraction(5,1)))==Fraction(1,1))
    assert(convertAmount(Fraction(2,5),LinearConversion(Fraction(10,1),Fraction(5,1)))==Fraction(9,1))
    assert(convertAmount(Fraction(2,5),LinearConversion(Fraction(-10,1),Fraction(-5,1)))==Fraction(-9,1))
    assert(convertAmount(Fraction(2,5),LinearConversion(Fraction(10,1),Fraction(-4,1)))==Fraction(0,1))
  }

  test("converToBase Test"){
    assert(convertToBase(Quantity(Fraction(30,1),DerivedUnit("Centimeter",BaseUnit("Meter"),ProportionalConversion(Fraction(1,100)))))
      ==Quantity(Fraction(3,10),BaseUnit("Meter")))

    assert(convertToBase(Quantity(Fraction(86,1),DerivedUnit("Grad Fahrenheit",BaseUnit("Grad Celsius"),LinearConversion(Fraction(5,9),Fraction(-160,9)))))
      ==Quantity(Fraction(30,1),BaseUnit("Grad Celsius")))
  }

  test("getInversionConversion Test"){
    //Inverse Conversion Meter -> Kilometer is *1000 and therefore Kilometer -> Meter is 1/1000
    assert(getInverseConversion(ProportionalConversion(Fraction(1000,1)))==ProportionalConversion(Fraction(1,1000)))
    //Linear Conversion Inverse : 3*x+2 <-> 1/3*x-2
    assert(getInverseConversion(LinearConversion(Fraction(3,1),Fraction(2,1)))==LinearConversion(Fraction(1,3),Fraction(-2,1)))
  }

  test("convertFromBase Test"){
    // 2 Meter -> 1/500 Kilometer
    assert(convertFromBase(Quantity(Fraction(2,1),BaseUnit("Meter")),DerivedUnit("Kilometer",BaseUnit("Meter"),ProportionalConversion(Fraction(1000,1))))
    ==Quantity(Fraction(1,500),DerivedUnit("Kilometer",BaseUnit("Meter"),ProportionalConversion(Fraction(1000,1)))))

    //Different Base Units Error
    intercept[Exception]{convertFromBase(Quantity(Fraction(2,1),BaseUnit("Meter")),DerivedUnit("Fahrenheit",BaseUnit("Grad Celsius"),LinearConversion(Fraction(18,10),Fraction(32,1))))}
  }

  test("convert Test"){
    //Derived -> Derived
    assert(convert(Quantity(Fraction(86,1),DerivedUnit("Grad Fahrenheit",BaseUnit("Grad Celsius"),LinearConversion(Fraction(5,9),Fraction(-160,9)))),
      DerivedUnit("Kelvin",BaseUnit("Grad Celsius"),LinearConversion(Fraction(1,1),Fraction(-273,1))))
      ==Quantity(Fraction(303,1),DerivedUnit("Kelvin",BaseUnit("Grad Celsius"),LinearConversion(Fraction(1,1),Fraction(-273,1)))))

    //Derived -> Base
    assert(convert(Quantity(Fraction(86,1),DerivedUnit("Grad Fahrenheit",BaseUnit("Grad Celsius"),LinearConversion(Fraction(5,9),Fraction(-160,9)))),
      BaseUnit("Grad Celsius"))
      ==Quantity(Fraction(30,1),BaseUnit("Grad Celsius")))

    //Base -> Derived
    assert(convert(Quantity(Fraction(100,1),BaseUnit("Meter")),DerivedUnit("Kilometer",BaseUnit("Meter"),ProportionalConversion(Fraction(1000,1))))
      ==Quantity(Fraction(1,10),DerivedUnit("Kilometer",BaseUnit("Meter"),ProportionalConversion(Fraction(1000,1)))))

    //Base -> Base
    assert(convert(Quantity(Fraction(100,1),BaseUnit("Meter")),BaseUnit("Meter"))
      ==Quantity(Fraction(100,1),BaseUnit("Meter")))

    //Derived -> Derived
    intercept[Exception]{convert(Quantity(Fraction(86,1),DerivedUnit("Grad Fahrenheit",BaseUnit("Grad Celsius"),LinearConversion(Fraction(5,9),Fraction(-160,9)))),
      DerivedUnit("Kilometer",BaseUnit("Meter"),LinearConversion(Fraction(1,1),Fraction(1000,1))))}

    //Derived -> Base
    intercept[Exception]{convert(Quantity(Fraction(86,1),DerivedUnit("Grad Fahrenheit",BaseUnit("Grad Celsius"),LinearConversion(Fraction(5,9),Fraction(-160,9)))),
      BaseUnit("Meter"))}

    //Base -> Derived
    intercept[Exception]{convert(Quantity(Fraction(30,1),BaseUnit("Grad Celsius")),
      DerivedUnit("Kilometer",BaseUnit("Meter"),LinearConversion(Fraction(1,1),Fraction(1000,1))))}

    //Base -> Base
    intercept[Exception]{convert(Quantity(Fraction(30,1),BaseUnit("Grad Celsius")),
      BaseUnit("Meter"))}
  }

}
