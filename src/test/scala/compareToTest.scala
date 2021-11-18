import org.scalatest.FunSuite
import FractionObject._
import compareTo._

class compareToTest extends FunSuite{
  test("compare Tests"){
    //Boolean Type
    assert(compareTo(false,true))
    assert(!compareTo(false,false))
    assert(!compareTo(true,true))
    assert(!compareTo(true,false))
    intercept[Exception]{compareTo(true,2)}

    //Int Type
    assert(compareTo(1,2))
    assert(compareTo(-2,-1))
    assert(!compareTo(2,2))
    intercept[Exception]{compareTo(2,true)}

    //BigInt Type
    assert(compareTo(BigInt(1),BigInt(2)))
    assert(compareTo(BigInt(-2),BigInt(1)))
    assert(!compareTo(BigInt(2),BigInt(2)))
    intercept[Exception]{compareTo(BigInt(2),3)}

    //String Type
    assert(compareTo("abc","abcd"))
    assert(compareTo("ABC","abc"))
    assert(!compareTo("ABC","ABC"))
    intercept[Exception]{compareTo("abc",2)}

    //Fraction Type
    assert(compareTo(Fraction(1,2),Fraction(2,3)))
    assert(!compareTo(Fraction(1,2),Fraction(1,3)))
    intercept[Exception]{compareTo(Fraction(3,5),true)}

    //undefined
    intercept[Exception]{compareTo(0.2f,0.4f)}
  }
}
