import org.scalatest.FunSuite
import StringAufgaben._

class StringAufgabenTest extends FunSuite{

  test("toUnicode Test"){
    assert(toUnicode('a')==97)
    assert(toUnicode('z')==122)
  }

  test("toChar Test"){
    assert(toChar(97)=='a')
    assert(toChar(122)=='z')
  }

  test("concat Tests"){
    assert(concat("Hallo ","Welt")=="Hallo Welt")
    assert(concat("","")=="")
  }

  test("head Test"){
    assert(head("Hallo")=='H')
    assert(head("H")=='H')
  }

  test("tail Test"){
    assert(tail("Hallo Welt")=="allo Welt")
    assert(tail("Ha")=="a")
    assert(tail("H")=="")
    assert(tail("")=="")
  }

  test("length Test"){
    assert(lengthOfString("")==0)
    assert(lengthOfString("H")==1)
    assert(lengthOfString("Hallo")==5)
  }

  test("reverse"){
    assert(reverse("Hallo")=="ollaH")
    assert(reverse("allo")=="olla")
    assert(reverse("")=="")
    assert(reverse("d")=="d")
  }

  test("containsHowOften Test"){
    assert(containsHowOften("aaa",'a')==3)
    assert(containsHowOften("",'z')==0)
    assert(containsHowOften("ZZzHalloz5",'z')==2)
  }

  test("swapAt Test"){
    assert(swapAt("Hallo Welt",' ')=="Welt Hallo")
    assert(swapAt("Hallo Welt",'z')=="Hallo Welt")
    assert(swapAt("Hallo Welt",'W')=="eltWHallo ")
    assert(swapAt("",' ')=="")
    assert(swapAt("ABCDEFG",'D')=="EFGDABC")
  }

  test("shift Test"){
    assert(shift("abc")=="cab")
    assert(shift("Hallo")=="oHall")
    assert(shift("")=="")
    assert(shift("d")=="d")
    assert(shift("da")=="ad")
    assert(shift(shift(shift("abc")))=="abc")
  }

  test("less Test"){
    assert(!less("abc","abc"))
    assert(less("ab","abc"))
    assert(less("","abc"))
    assert(!less("abc",""))
  }

  test("charToString Test"){
    assert(charToString('a')=="a")
    assert(charToString(' ')==" ")
  }

  test("boolToString Test"){
    assert(boolToString(false)=="false")
    assert(boolToString(true)=="true")
  }

  test("intToString Test"){
    assert(intToString(23)=="23")
    assert(intToString(0)=="0")
  }

  test("delChar Test"){
    assert(delChar("a",'c')=="a")
    assert(delChar("abc",'c')=="ab")
    assert(delChar("abcc",'c')=="abc")
    assert(delChar("",'c')=="")
    assert(delChar("c",'c')=="")
  }

  test("sort Test"){
    assert(sort("cba")=="abc")
    assert(sort("")=="")
    assert(sort("abbbc012ABC")=="012ABCabbbc")
    assert(sort("c")=="c")
  }

}
