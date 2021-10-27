import MPAufgaben._
import org.scalatest.FunSuite

class MPAufgabenTest extends FunSuite {
  test(testName = "BetragInUSDTest"){
    assert(betragInUS == 120)
  }

  test(testName = "Division durch 2"){
    assert(divRestZwei(13)==1)
  }

  test(testName = "Ueber Constante Zahl"){
    assert(overConstant(13))
    assert(!overConstant(12))
    assert(!overConstant(11))
  }

  test(testName = "Quadrieren einer Zahl"){
    assert(square(0)==0)
    assert(square(1)==1)
    assert(square(2)==4)
    assert(square(9)==81)
    assert(square(-9)==81)
  }

  test(testName = "Gibt die größte/max Zahl aus"){
    assert(max(4,5)==5)
    assert(max(26,87)==87)
    assert(max(8,8)==8)
  }

  test("min Test"){
    assert(min(10,10)==10)
    assert(min(5,10)==5)
    assert(min(10,6)==6)
  }

  test(testName = "Gibt die Zahl absolut zurück"){
    assert(abs(10)==10)
    assert(abs(-10)==10)
    assert(abs(0)==0)
  }

  test(testName = "Rest bei Division ganzer Zahlen"){
    assert(%(7,10)==7)
    assert(%(13,10)==3)
    assert(%(7,7)==0)
    assert(%(0,1)==0)
    assert(%(10,-7)==3)
    assert(%(-10,7)==(-3))
    assert(%(-10,-7)==(-3))
  }

  test(testName = "Fakultät einer Zahl"){
    val fac20:BigInt = 2432902008176640000L
    assert(factorial(0)==1)
    assert(factorial(1)==1)
    assert(factorial(3)==6)
    assert(factorial(5)==120)
    assert(factorial(20)==fac20)
  }

  test(testName = "Power Funktion Test"){
    assert(power(0,0)==1)
    assert(power(0,1)==0)
    assert(power(6,0)==1)
    assert(power(1,1)==1)
    assert(power(2,3)==8)
    assert(power(3,3)==27)
  }

  test(testName = "Teste sumPower2"){
    assert(sumPower2(139)==91)
    assert(sumPower2(0)==0)
    assert(sumPower2(1)==1)
    assert(sumPower2(2)==4)
    assert(sumPower2(12)==5)
  }

  test("Invers Tests"){
    assert(invers(3,11)==4)
    assert(invers(6,11)==2)
    assert(invers(5,126)==101)
    assert(invers(1,2)==1)
    assert(invers(8,79)==10)
    assert(invers(8,80)==(-1))
  }

  test("Mirror Test"){
    assert(mirror(12)==21)
    assert(mirror(0)==0)
    assert(mirror(3)==3)
    assert(mirror(331)==133)
    assert(mirror(43210)==1234)
    assert(mirror(2300189)==9810032)
  }

  test("Fib Test"){
    assert(fib(0)==0)
    assert(fib(1)==1)
    assert(fib(5)==5)
    assert(fib(6)==8)
  }

  test("isPrim"){
    assert(!isPrim(0))
    assert(!isPrim(1))
    assert(isPrim(2))
    assert(isPrim(7))
    assert(!isPrim(8))
    assert(isPrim(47))
  }

  test("numberOfPrimesUnder Test"){
    assert(numberOfPrimesUnder(100)==25)
    assert(numberOfPrimesUnder(1000)==168)
    assert(numberOfPrimesUnder(10000000)==664579)
  }

  test("sumDividers Test"){
    assert(sumDividers(6)==6)
    assert(sumDividers(10)==8)
    assert(sumDividers(0)==0)
    assert(sumDividers(1)==0)
    assert(sumDividers(2)==1)
  }

  test("PerfectNumberTest"){
    assert(isPerfectNumber(6))
    assert(!isPerfectNumber(10))
  }

  test("nextPerfectNumber"){
    assert(findNextPerfectNumber(3)==6)
    assert(findNextPerfectNumber(6)==6)
    assert(findNextPerfectNumber(7)==28)
    assert(findNextPerfectNumber(29)==496)
  }

  test("FindBiggestExponentFitting Test"){
    assert(findBiggestExponentFitting(33,2)==5)
    assert(findBiggestExponentFitting(27,3)==3)
    assert(findBiggestExponentFitting(0,10)==0)
    assert(findBiggestExponentFitting(12,10)==1)
  }

  test("toOctal"){
    assert(toOctal(0)==0)
    assert(toOctal(8)==10)
    assert(toOctal(7562)==16612)
  }

  test("convertDecimalTo"){
    assert(convertDecimalTo(8,8)==10)
    assert(convertDecimalTo(15,2)==1111)
    assert(convertDecimalTo(15,7)==21)
    assert(convertDecimalTo(15,4)==33)
    assert(convertDecimalTo(15,12)==13)
  }

  test("ggT Test"){
    assert(ggT(5,20)==5)
    assert(ggT(5,21)==1)
    assert(ggT(120,144)==24)
    assert(ggT(35,35)==35)
  }

  test("kgV Test"){
    assert(kgV(15,10)==30)
    assert(kgV(3,3)==3)
    assert(kgV(15,100)==300)
    assert(kgV(-4,-5)==20)
  }

  test("teilerExperiment"){
    assert(teilerExperiment(8)==4)
    assert(teilerExperiment(1)==1)
    assert(teilerExperiment(12)==6)
    assert(teilerExperiment(0)==0)
  }

  test("product Test"){
    assert(product(0,1)==0)
    assert(product(1,2)==1)
    assert(product(1,5)==factorial(4))
    assert(product(1,10)==factorial(9))
    assert(product(5,7)==30)
    assert(product(5,8)==210)
    assert(product(10,30) ==
      BigInt("24365525776399090483200000")
    )
  }

  test("collatzFunction Test"){
    assert(collatzFunction(0))
    assert(collatzFunction(27))
    assert(collatzFunction(101))
  }

  test("fastFib Test"){
    assert(fastFib(0)==0)
    assert(fastFib(1)==1)
    assert(fastFib(5)==5)
    assert(fastFib(6)==8)
    assert(fastFib(30)==832040)
    assert(fastFib(50)==12586269025L)
  }

  test("tailFib Test"){
    assert(tailFib(0)==0)
    assert(fastFib(1)==1)
    assert(fastFib(5)==5)
    assert(fastFib(6)==8)
    assert(fastFib(30)==832040)
    assert(fastFib(50)==12586269025L)
  }

  test("DaC Factorial Test"){
    assert(dacFactorial(0)==factorial(1))
    assert(dacFactorial(1)==factorial(1))
    assert(dacFactorial(5)==factorial(5))
    assert(dacFactorial(10)==factorial(10))
  }

  test("testFib Test"){
    assert(testFib(0))
    assert(testFib(1))
    assert(testFib(2))
    assert(testFib(5))
  }

  test("xor Test"){
    assert(!xor(x = true,y = true))
    assert(xor(x = true,y = false))
    assert(xor(x = false,y = true))
    assert(!xor(x = false,y = false))
  }

  test("not Test"){
    assert(!not(true))
    assert(not(false))
  }

  test("implies Test"){
    assert(implies(x = true,y = true))
    assert(!implies(x = true,y = false))
    assert(implies(x = false,y = true))
    assert(implies(x = false,y = false))
  }
}
