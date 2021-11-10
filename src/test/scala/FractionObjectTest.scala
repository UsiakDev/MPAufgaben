import org.scalatest.FunSuite
import FractionObject._

class FractionObjectTest extends FunSuite{
  test("Modulo Test"){
    assert(%(7,10)==7)
    assert(%(13,10)==3)
    assert(%(7,7)==0)
    assert(%(0,1)==0)
    assert(%(10,-7)==3)
    assert(%(-10,7)==(-3))
    assert(%(-10,-7)==(-3))
  }

  test("ggT Test"){
    assert(ggT(5,20)==5)
    assert(ggT(5,21)==1)
    assert(ggT(120,144)==24)
    assert(ggT(35,35)==35)
    assert(ggT(4,0)==4)
    assert(ggT(0,4)==4)
  }

  test("toRational Test"){
    assert(toRational(Fraction(3,6))==Fraction(1,2))
    assert(toRational(Fraction(-3,-6))==Fraction(1,2))
    assert(toRational(Fraction(3,-6))==Fraction(-1,2))
    assert(toRational(Fraction(-3,6))==Fraction(-1,2))
    assert(toRational(Fraction(3,9))==Fraction(1,3))
    assert(toRational(Fraction(12,120))==Fraction(1,10))
  }

  test("Fraction less Test"){
    assert(!less(Fraction(1,2),Fraction(1,3)))
    assert(less(Fraction(1,2),Fraction(2,3)))
    assert(less(Fraction(1,2),Fraction(3,5)))
    assert(less(Fraction(6,10),Fraction(4,5)))
  }

  test("add Test"){
    assert(add(Fraction(1,2),Fraction(1,2))==Fraction(1,1))
    assert(add(Fraction(1,2),Fraction(2,2))==Fraction(3,2))
    assert(add(Fraction(3,5),Fraction(5,7))==Fraction(46,35))
  }

  test("subtract Test"){
    //assert(add(createFraction(1,2),createFraction(1,2))==createFraction(0,0))
    assert(subtract(Fraction(2,1),Fraction(1,2))==Fraction(3,2))
    assert(subtract(Fraction(7,5),Fraction(2,5))==Fraction(1,1))
  }

  test("* Test"){
    assert(*(Fraction(1,2),Fraction(1,2))==Fraction(1,4))
    assert(*(Fraction(2,1),Fraction(1,2))==Fraction(1,1))
    assert(*(Fraction(7,5),Fraction(2,5))==Fraction(14,25))
  }

  test("/ Test"){
    assert(/(Fraction(1,2),Fraction(1,2))==Fraction(1,1))
    assert(/(Fraction(2,1),Fraction(1,2))==Fraction(4,1))
    assert(/(Fraction(7,5),Fraction(2,5))==Fraction(7,2))
  }


}
