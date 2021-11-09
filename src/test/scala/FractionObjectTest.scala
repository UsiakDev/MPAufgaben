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
    assert(toRational(createFraction(3,6))==createFraction(1,2))
    assert(toRational(createFraction(3,9))==createFraction(1,3))
    assert(toRational(createFraction(12,120))==createFraction(1,10))
  }

  test("Fraction less Test"){
    assert(!less(createFraction(1,2),createFraction(1,3)))
    assert(less(createFraction(1,2),createFraction(2,3)))
    assert(less(createFraction(1,2),createFraction(3,5)))
    assert(less(createFraction(6,10),createFraction(4,5)))
  }

  test("add Test"){
    assert(add(createFraction(1,2),createFraction(1,2))==createFraction(1,1))
    assert(add(createFraction(1,2),createFraction(2,2))==createFraction(3,2))
    assert(add(createFraction(3,5),createFraction(5,7))==createFraction(46,35))
  }

  test("subtract Test"){
    //assert(add(createFraction(1,2),createFraction(1,2))==createFraction(0,0))
    assert(subtract(createFraction(2,1),createFraction(1,2))==createFraction(3,2))
    assert(subtract(createFraction(7,5),createFraction(2,5))==createFraction(1,1))
  }

  test("* Test"){
    assert(*(createFraction(1,2),createFraction(1,2))==createFraction(1,4))
    assert(*(createFraction(2,1),createFraction(1,2))==createFraction(1,1))
    assert(*(createFraction(7,5),createFraction(2,5))==createFraction(14,25))
  }

  test("/ Test"){
    assert(/(createFraction(1,2),createFraction(1,2))==createFraction(1,1))
    assert(/(createFraction(2,1),createFraction(1,2))==createFraction(4,1))
    assert(/(createFraction(7,5),createFraction(2,5))==createFraction(7,2))
  }


}
