import org.scalatest.FunSuite
import Lists._

class ListsTest extends FunSuite{
  val randomList:List = nonEmptyList(2,nonEmptyList("Hallo",nonEmptyList(true,emptyList())))

  test("Length Test"){
    assert(lengthOfList(randomList)==3)
    assert(lengthOfList(emptyList())==0)
  }

  test("toString Test"){
    assert(toString2(randomList)=="2,Hallo,true")
    assert(toString2(emptyList())=="")
  }


}
