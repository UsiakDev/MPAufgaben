import org.scalatest.FunSuite
import Lists._

class ListsTest extends FunSuite{
  val randomList:List = nonEmptyList(2,nonEmptyList("Hallo",nonEmptyList(true,emptyList())))
  val primList:List
  = nonEmptyList(1,nonEmptyList(2,nonEmptyList(3,nonEmptyList(5,nonEmptyList(8,emptyList())))))
  val notFlattendList:List
  = nonEmptyList(nonEmptyList(2,nonEmptyList(true,emptyList())),nonEmptyList("Hallo",emptyList()))
  val flattendList:List = nonEmptyList(2,nonEmptyList(true,nonEmptyList("Hallo",emptyList())))

  test("Length Test"){
    assert(lengthOfList(randomList)==3)
    assert(lengthOfList(emptyList())==0)
  }

  test("toString Test"){
    assert(toString2(randomList)=="2,Hallo,true")
    assert(toString2(emptyList())=="")
  }

  test("getPrimesList Test"){
    assert(getPrimes(primList)==nonEmptyList(2,nonEmptyList(3,nonEmptyList(5,emptyList()))))
    assert(getPrimes(emptyList())==emptyList())
  }

  test("reverse List Test"){
    assert(reverseList(randomList)==nonEmptyList(true,nonEmptyList("Hallo",nonEmptyList(2,emptyList()))))
    assert(reverseList(emptyList())==emptyList())
  }

  test("append Test"){
    assert(append(nonEmptyList(2,emptyList()),nonEmptyList(true,emptyList()))
      ==nonEmptyList(2,nonEmptyList(true,emptyList())))

    assert(append(emptyList(),nonEmptyList(true,emptyList()))
      ==nonEmptyList(true,emptyList()))

    assert(append(nonEmptyList(2,emptyList()),emptyList())
      ==nonEmptyList(2,emptyList()))
  }

  test("flatten Test"){
    assert(flatten(randomList)==randomList)
    assert(flatten(notFlattendList)==flattendList)
  }

  /*
  test("flattenTest") {

    assert(flatten(emptyList()) == emptyList())

    assert(flatten(nonEmptyList(1, emptyList())) == nonEmptyList(1, emptyList()))

    assert(flatten(nonEmptyList(
      nonEmptyList(
        nonEmptyList(1,
          nonEmptyList(2,
            nonEmptyList(3, emptyList()))),
        nonEmptyList(4,
          nonEmptyList(5, emptyList()))),
      nonEmptyList(6,
        nonEmptyList(7,
          nonEmptyList(8,
            nonEmptyList(9, emptyList()))))))
      == nonEmptyList(1,
      nonEmptyList(2,
        nonEmptyList(3,
          nonEmptyList(4,
            nonEmptyList(5,
              nonEmptyList(6,
                nonEmptyList(7,
                  nonEmptyList(8,
                    nonEmptyList(9, emptyList()))))))))))

    assert(flatten(nonEmptyList(
      nonEmptyList(1, emptyList()),
      nonEmptyList(3, emptyList()))) == nonEmptyList(1, nonEmptyList(3, emptyList())))

    assert(flatten(
      nonEmptyList(
        nonEmptyList(
          nonEmptyList(1, emptyList()),
          nonEmptyList(4, emptyList())),
        nonEmptyList(1, emptyList()))) == nonEmptyList(1, nonEmptyList(4, nonEmptyList(1, emptyList()))))
  }
   */
}
