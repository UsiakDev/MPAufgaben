import scala.annotation.tailrec

object MPAufgaben {
  def main(args: Array[String]): Unit = {
    println(reverse("Hallo"))
    //println("Z : " + numberOfPrimesUnder(10000000))
    //collatzFunction(27)
    // Bei || und && wird nach Betrachtung des ersten Wertes abgebrochen falls
    // dieser den Operator bereits Eindeutig festlegt
    // | und & betrachten immer beide Werte
    /*
    println(theValueTrue()||theValueFalse())
    println()
    println(theValueTrue()|theValueFalse())
    println()
    println(theValueFalse()&&theValueTrue())
    println()
    println(theValueFalse()&theValueTrue())
     */
    //testFibUp()
    //testCollatzFunctionUp()
  }

  val betragEuro: Int = 100
  val betragInUS: Int = (betragEuro * 1.2).toInt

  val constant = 12

  def and(x:Boolean,y:Boolean):Boolean = if(x) y else false

  /**
   * Berechnet den Rest bei Division von x durch 2
   * @param x Eingabe
   * @return Rest
   */
  def divRestZwei(x: Int): Int = x - (x / 2) * 2

  /**
   * Berechnet den Mittelwert von x und y und rundet das Ergebnis auf
   * Erwartet x,y Int Eingabe - Int Ausgabe
   */
  def mittelWert(x: Int, y: Int): Int =
    if (divRestZwei(x) == 0) (x + y) / 2
    else (x + y) / 2 + 1

  /**
   * Überprüft ob x über der Konstante liegt
   * @param x Int Eingabe die überprüft wird
   * @return Boolean
   */
  def overConstant(x: Int): Boolean = x > constant

  /**
   * Quadriert die Eingabe
   * @param x Int Eingabe- die zu quadrierende Zahl
   * @return Übergibt das Ergebnis als Int
   */
  def square(x: Int): Int = x * x

  /**
   * Gibt die Größte Zahl beider Eingaben als Int aus
   * @param x ,y Int - Beide Eingaben
   */
  def max(x: Int, y: Int): Int =
    if (x > y) x
    else y

  def min(x:Int,y:Int):Int =
    if(x<y) x
    else y

  /**
   * Gibt eine Zahl als absolute Zahl als Int wieder mit
   * @param x Int Eingabe
   */
  def abs(x: Int): Int =
    if (x < 0) -x
    else x

  /**
   * Eingabe zweier Zahlen;
   * Berechnet wird Division ganzer Zahlen mit Rest
   * @param x ,y mit x>=0 und y>0
   * @return den Rest
   */
  def %(x: Int, y: Int): Int = {
    var b = abs(x)

    while (b >= abs(y))
      b = b - abs(y)
    if (x < 0) b = b * (-1)
    b
  }

  /**
   * Gibt die Fakultät der Eingabe aus; Definiert nur für n<=0
   * n BigInt Eingabe; Ausgabe als BigInt; n>=1
   */
  def factorial(n: BigInt): BigInt = {
    if(n>0) factorial(n-1)*n
    else 1
  }

  /**
   * Power x,y => x hoch y; wobei wenn y=0 -> 1 als Ausgabe
   * oder für x=0 und y>0 -> 0 als Ausgabe
   * Keine Negativ Exponenten ! ! !
   * @param x basis
   * @param y exponent
   * @return result Int Ausgabe
   */
  def power(x: Int, y: Int): Int = {
    if(y>0) power(x,y-1)*x
    else 1
  }

  /**
   * Summiert die Quadrate der einzelnen Ziffern
   * @param n Int Eingabe nur natürliche Zahlen
   * @param result given through the recursion as result
   * @return result Int Output
   */
  @tailrec def sumPower2(n: Int, result:Int = 0): Int = {
    if(n!=0) sumPower2(n/10,result+square(%(n,10)))
    else result
  }

  /**
   * Ermittelt Ausgabe erstes y als Int, welches %(x*y,m)=1 ergibt
   * Erhöht i solange bis dies erfüllt ist. Kein abbruch vorhanden!
   * Falls x < m nicht stimmt wird -1 ausgegeben
   * @param x und m als Int Eingaben
   * @return y Int Output
   */
  def invers(x: Int, m: Int): Int = {
    if(ggT(x,m)==1 && x<m){
      var i: Int = 1
      while (%(x * i, m) != 1) i += 1
      i
    }
    else -1
  }

  /**
   * Invers Function but Recursive
   * Ermittelt Ausgabe erstes y als Int, welches %(x*y,m)=1 ergibt
   * Falls x < m nicht stimmt oder kein Ergebnis möglich ist wird -1 ausgegeben
   * @param x erste Zahl
   * @param m zweite Zahl
   * @param y mit x die Inverse zu y
   * @return Output Int die Inverse y
   */
  @tailrec def inversRec(x:Int, m:Int, y:Int=0):Int = {
    if(ggT(x,m)!=1){
      if(%(x*y,m)==0) y
      else inversRec(x,m,y+1)
    }
    else -1
  }

  /**
   * mirror spiegelt seinen Input und gibt ihn wieder aus | 123 -> 321
   * Außerdem werden vorrangestellt nullen gestrichen | 123000 -> 321
   * Basierend auf den natürlichen Zahlen
   * @param x Int Input, welcher gespiegelt werden soll
   * @param mirrorInt default value = 0; saves the mirrored Number
   * @return Int als Output || Also Input gespiegelt
   */
  @tailrec def mirror(x:Int, mirrorInt:Int = 0):Int =
    if(x > 0) mirror(x/10, mirrorInt * 10 + %(x,10))
    else mirrorInt

  /**
   * Gibt die n-te Fibonacci Zahl aus. Negativ eingaben: undefiniert als 0 ausgegeben
   * @param n Int Input, welche Fibonacci Zahl ausgegeben werden soll
   * @return Int Output, die berechnete Fibonacci Zahl
   */
  def fib(n:Int):BigInt = {
    if(n<=0) 0
    else{
      if(n==1) 1
      else fib(n-2)+fib(n-1)
    }
  }

  /**
   * Input one Integer; Checks if its a Prime; Output true if so, otherwise false
   * @param x Integer Input which is checked
   * @return Outputs a boolean depending if its Prime or not
   */
  @tailrec def isPrim(x: Int, i:Int=3):Boolean = {
    if (x%2 == 0) x == 2
    else if (x<=1) false
    else if ((i*i) >x) true
    else if (x%i == 0) false
    else isPrim(x,i+2)
  }

  /**
   * Checks how many prime numbers there are under given value
   * @param num given value / upper border
   * @param count default value 0; No prime numbers found yet
   * @return Output Int; Amount of found prime numbers
   */
  @tailrec def numberOfPrimesUnder(num:Int, count:Int=0):Int = {
    if (num <= 1) return count
    if (isPrim(num)) numberOfPrimesUnder(num-1, count+1)
    else numberOfPrimesUnder(num-1, count)
  }

  /**
   * Finds next Perfect Number
   * @param x Checks if its a Perfect Number or looks for one above
   * @return Output Int the next found Perfect Number
   */
  @tailrec def findNextPerfectNumber(x:Int):Int = {
    if(!isPerfectNumber(x)) findNextPerfectNumber(x+1)
    else x
  }

  /**
   * Checks if given Number is a perfect number.
   * @param x Input Int; Number to be checked
   * @return Boolean output; true if given number is a perfect number
   */
  def isPerfectNumber(x:Int):Boolean = {
    if(x<=0) false
    else x==sumDividers(x)
  }

  /**
   * Sums up the Dividers; Divider has to be < Num itself
   * @param x is the Number of which the Dividers have to be summed up
   * @param y is the testing Divider
   * @param save Saves the summed up Dividers found
   * @return
   */
  @tailrec def sumDividers(x:Int, y:Int=1, save:Int=0):Int =
    if(x>=2)
      if (y > (x / 2))
        save
      else
        sumDividers(x, y + 1, save + (if(%(x, y) == 0) y else 0) )
    else 0

  /**
   * Finds biggest Fitting Numbers Exponent
   * @param x Number you are trying to fit in
   *          power(y,e) <=> e is the exponent to y
   * @param y has to be bigger than zero : y>0 !
   * @return Output Int the biggest Exponent to y thats fitting in x
   */
  @tailrec def findBiggestExponentFitting(x:Int, y:Int, e:Int=1):Int = {
    if(x>=power(y,e)) findBiggestExponentFitting(x,y,e+1)
    else e-1
  }

  /**
   * Converts Decimal Input into Octal Output
   * @param num Int Decimal Input and also remainder given through recursion
   * @return Int Octal Output
   */
  def toOctal(num:Int):Int = if(num/8 == 0) %(num,8) else toOctal(num/8)*10 + %(num,8)

  /**
   * Converts Decimal Input with given Converter to other Base number
   * @param num Int Input; Number thats converted
   * @param convert Int Input; converting factor
   *                num has to be bigger than convert; convert cannot be 0 ! ! !
   * @return converted Number to base_convert
   */
  def convertDecimalTo(num:Int, convert:Int):Int = {
    if(num/convert == 0) %(num,convert)
    else convertDecimalTo(num/convert,convert)*10 + %(num,convert)
  }

  /**
   * Calculates the greatest common divisor!
   * Undefined for x or y == 0 ! ! !
   * @param x first Number Input
   * @param y second Number Input
   * @return
   */
  def ggT(x:Int,y:Int):Int = {
    if(x==0 || y==0) 0
    else abs(x*y)/kgV(x,y)
  }

  /**
   * Calculates the least common multiple
   * No negative result ! ! Everything is absolute & 0 is undefined ! ! !
   * @param x first Number Input
   * @param y second Number Input
   * @param xMultiple multiplier of X
   * @param yMultiple multiplier of Y
   * @return the least common multiple; first found number when incremeting the multipliers
   */
  @tailrec def kgV(x:Int,y:Int, xMultiple:Int = 1, yMultiple:Int=1):Int = {
    if(x*xMultiple == y*yMultiple) abs(x*xMultiple)
    else{
      if(min(abs(x*xMultiple),abs(y*yMultiple))==abs(x*xMultiple)) kgV(x,y,xMultiple+1,yMultiple)
      else kgV(x,y,xMultiple,yMultiple+1)
    }
  }

  /**
   * Counts up the amount of dividers a number has; including itself and 1
   * @param n Int Input Number of which the dividers are counted; n>0 ! ! !
   * @return Int Output as the amount of dividers found
   */
  def teilerExperiment(n:Int):Int = {
    var counter:Int = 0
    var index:Int = 1
    while(index<=n){
      if(%(n,index)==0) counter += 1
      index += 1
    }
    counter
  }

  /**
   * "Divide and Conquer"; lowerBorder * (lwB+1) * (lwB+2) * ... * (upperBorder-1)
   * Upper Border/Y has to be > lower border/X ! ! !
   * Undefined for negative Numbers ! ! !
   * @param x including lower Border
   * @param y expluding upper Border
   * @return Outputs the calculated number
   */
  def product(x:BigInt , y:BigInt):BigInt = {
    if(x+1==y){
      println(x)
      x
    }
    else product(x,(x+y)/2) * product((x+y)/2,y)
  }

  /**
   * Factorial Function with Divide and Conquer Algo
   * Calculates the Factorial of given Number
   * @param n Input Int ; Number to calculate the factorial of
   * @return BigInt Outputs the calculated number
   */
  def dacFactorial(n:Int): BigInt ={
    if(n<=0) 1
    else product(1,n+1)
  }

  /**
   * Never Ending Function ! ! !
   * Checks all Numbers above 0 or Input and continues if the Collatz Function reaches 1
   * @param x number from where we start checking
   */
  @tailrec def testCollatzFunctionUp(x:Int=0):Unit = {
    if(collatzFunction(x)){
      println("Für " + x + " erreicht die Collatz Funktion 1")
      testCollatzFunctionUp(x+1)
    }
  }

  /**
   * A Function for representation as a number interates through the Collatz Problem
   * @param n Number to be Checked regarding the Collatz Problem
   * @return Outputs true if it reached 1
   */
  @tailrec def collatzFunction(n:Int):Boolean = {
    if(n<=1){
      //println("1 wurde erreicht!")
      true
    }
    else{
      if(n%2==0){
        //println("Derzeitige Zahl : " + n)
        collatzFunction(n/2)
      }
      else{
        //println("Derzeitige Zahl : " + n)
        collatzFunction(3*n+1)
      }
    }
  }


  /**
   * Main fastFib Function with helper function that's Tail Recursive
   * @param n : The Fib Number you're looking for
   * @return Outputs the n-th Fib Number
   */
  def fastFib(n:BigInt):BigInt = {
    fastFibHelper(n)
  }

  // Helper Function for fastFib
  @tailrec private def fastFibHelper(num:BigInt,i:BigInt=0, a:BigInt=0, b:BigInt=1):BigInt = {
    if(i==num) a
    else fastFibHelper(num,i+1,b,a+b)
  }

  /**
   * TailRecursion Fibonacci : calculates the n fibonacci number
   * @param n : n fibonacci number you're looking for
   * @param i : counts up till we've calculated n-times
   * @param a : safes the latest/newest fib number
   * @param b : calculated next fib number
   * @return Outputs the Fib number n
   */
  @tailrec def tailFib(n:Int,i:BigInt=0,a:BigInt=0,b:BigInt=1):BigInt = {
    if(i==n) a
    else tailFib(n,i+1,b,a+b)
  }

  /**
   * Never Ending Function ! ! !
   * Checks all Numbers above 0 or Input and continues if the Fib Theory of
   * fib(0) + fib(1) + ... + fib(n) == fib(n+2) - 1 is true
   * @param x number from where we start checking
   */
  @tailrec def testFibUp(x:Int=0):Unit = {
    if(testFib(x)){
      println("Für " + x + " gilt die Fibonacci aussage")
      testFibUp(x+1)
    }
  }
  /**
   * Figures out if fib(0) + fib(1) + ... + fib(n) == fib(n+2) - 1 is true
   * Not defined for negative numbers ! ! !
   * @param n upper border of fib numbs
   * @return Outputs Boolean depending if Theory is true
   */
  def testFib(n:Int):Boolean = fastFib(n+2)-1 == sumFibs(n)

  /**
   * Sums up all fibonacci numbers till and including given number
   * Not defined for negative numbers ! ! !
   * @param n upper border of summed up numbers
   * @return Outputs BigInt of Summed up Fibonacci Numbers
   */
  def sumFibs(n:Int):BigInt = {
    if(n<=1) n
    else fastFib(n) + sumFibs(n-1)
  }


  /**
   * @return Always False Output
   */
  def theValueFalse():Boolean = {
    println("I am false")
    false
  }

  /**
   * @return Always True Output
   */
  def theValueTrue():Boolean = {
    println("I am true")
    true
  }

  /**
   * Exclusive OR :
   * @param x first boolean statement
   * @param y second boolean statement
   * @return Outputs Boolean : True if only 1 Statement is true; Otherwise false
   */
  def xor(x:Boolean,y:Boolean):Boolean = x!=y

  /**
   * Negates boolean value
   * @param x boolean statement
   * @return Outputs negated statement of x; true -> false; false -> true
   */
  def not(x:Boolean):Boolean =
    if(x) false
    else true

  /**
   * Implies Operator
   * @param x first boolean statement
   * @param y second boolean statement
   * @return Outputs Boolean Value ;
   *         False if first statement true and second false; Otherwise True
   */
  def implies(x:Boolean,y: => Boolean):Boolean =
    if(x && not(y)) false
    else true

  /**
   * Equivalent Operator
   * @param x first boolean statement
   * @param y second boolean statement
   * @return Output Boolean statement: If x==y True; otherwise false
   */
  def equivalent (x:Boolean,y:Boolean):Boolean = x==y

  /**
   * Converts Char to Unicode; CANNOT BE EMPTY ! ! !
   * @param c given Char that's converted
   * @return Outputs the Integer of the Char
   */
  def toUnicode(c:Char):Int = c

  /**
   * Converts Unicode to Char; CANNOT BE EMPTY ! ! !
   * @param code given code that's converted
   * @return Outputs the Char of the Unicode
   */
  def toChar(code:Int):Char = code.toChar

  /**
   * Puts together 2 Strings : concat("abc","def") = "abcdef"
   * DOES NOT WORK FOR CHARS ! ! ! So there is another Function right after this one..
   * @param firstString first given String
   * @param secondString second given String
   * @return Outputs the combined String
   */
  def concat(firstString:String,secondString:String):String = firstString + secondString

  /**
   * Puts together a Char before a String. Char cannot be empty ! ! !
   * @param char given Char
   * @param rest given String
   * @return Outputs a String of Char+String
   */
  def concatWithCharBefore(char:Char, rest:String):String = {
    char + rest
  }

  /**
   * Puts together a Char after a String. Char cannot be empty ! ! !
   * @param char given Char
   * @param rest given String
   * @return Outputs a String of String+Char
   */
  def concatWithCharAfter(char:Char, rest:String):String = {
    rest + char
  }

  /**
   * Takes the first Char of a String; CANNOT BE EMPTY ! ! !
   * @param word given String
   * @return Outputs the first Char of the given String
   */
  def head(word:String):Char = word.head

  /**
   * Removes the first Char of a String; Can be Empty : Would return "".
   * @param word given String
   * @return Outputs the given String without the first Char
   */
  def tail(word:String):String = word.tail

  /**
   * Counts up the length of a String
   * @param word given String
   * @return Outputs Integer Length of String
   */
  def lengthOfString(word:String):Int = {
    lengthOfStringHelper(word)
  }

  /**
   * Private Helper Function of lengthOfString;
   * @param word given String
   * @param counter Int with default Value 0; No length in the beginning of the counting
   * @return Outputs Int Counter/Length of word
   */
  @tailrec private def lengthOfStringHelper(word:String,counter:Int=0):Int = {
    if(word=="") counter
    else lengthOfStringHelper(tail(word),counter+1)
  }

  /**
   * Reverses a String / Mirrors it
   * @param word given String
   * @return Outputs String of reversed word
   */
  def reverse(word:String):String = reverseHelper(word)

  /**
   * Helper Function Of reverse
   * @param word given word
   * @param save saved up reversed word
   * @return Outputs the reversed word as String
   */
  @tailrec private def reverseHelper(word:String, save:String=""):String = {
    if(word=="") save
    else reverseHelper(tail(word),concatWithCharBefore(head(word),save))
  }

  /**
   * Counts up how often given char is found in word. Char may not be empty ! ! !
   * @param word given String/Word
   * @param char given char
   * @return Outputs Integer of how often char was found in word
   */
  def containsHowOften(word:String,char:Char):Int = containsHowOftenHelper(word,char)

  /**
   * Helper Function of containsHowOften; Counts up how often given char is found in word. Char not empty !
   * @param word given String
   * @param char given Char
   * @param counter default Value 0; starting at 0 found chars in word
   * @return Outputs Integer of how often char was found in word
   */
  @tailrec private def containsHowOftenHelper(word:String, char:Char, counter:Int = 0):Int = {
    if(word=="") counter
    else containsHowOftenHelper(tail(word),char, counter + (if(head(word)==char) 1 else 0))
  }


}
