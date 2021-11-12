import scala.annotation.tailrec
import MPAufgaben._

object StringAufgaben {
  def main(args: Array[String]): Unit = {

  }

  /**
   * Converts Char to String Type
   * @param x given Char
   * @return return String of converted Char
   */
  def toString2(x:Char):String = "" + x

  /**
   * Converts a String to String
   * @param s given String
   * @return returns String
   */
  def toString2(s:String):String = s

  /**
   * Converts Bool to String : true -> "true" and false -> "false"
   * @param x given Boolean
   * @return return String of converted Bool
   */
  def toString2(x:Boolean):String =
    if(x) "true"
    else "false"

  /**
   * Converts Int To String
   * @param x given Int
   * @return Outputs String of Int 23->"23"
   */
  def toString2(x:Int):String = intToStringHelper(x)

  /**
   * intToString Helper Function
   * Converts Int to String
   * @param x given Int
   * @param save default Value = "" ; saves converted nums
   * @return Outputs String of Int
   */
  private def intToStringHelper(x:Int,save:String=""):String = {
    if(x<0) concat("-",intToStringHelper(abs(x)))
    else if(x<=9) concat(toString2(toChar(48+x)),save)
    else intToStringHelper(x/10, concat(toString2(toChar(48 + %(x,10))),save))
  }

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
   * DOES NOT WORK FOR CHARS ! ! ! Gotta convert a char to string
   * @param firstString first given String
   * @param secondString second given String
   * @return Outputs the combined String
   */
  def concat(firstString:String,secondString:String):String = firstString + secondString


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
    else reverseHelper(tail(word),concat(toString2(head(word)),save))
  }

  /**
   * Reverses / Mirrors a String
   * @param s given String
   * @return Outputs mirrored string
   */
  def reverseRecursive(s:String):String = {
    if(s=="") s
    else concat(reverseRecursive(tail(s)),toString2(head(s)))
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

  /**
   * Swaps a String on first occurence of given Char. String1 + Char + String2 -> String2 + Char + String 1
   * @param s given String
   * @param c given Char
   * @return Outputs swapped String
   */
  def swapAt(s:String,c:Char):String = {
    val index:Int = findCharIndex(s,c)
    if(index>=0){
      val firstPart = s.substring(0,index)
      val secondPart = concat(s.substring(index+1,lengthOfString(s)),toString2(c))
      concat(secondPart,firstPart)
    }
    else s
  }

  /**
   * Deletes a given char from a string only once if found. Otherwise returns string itself
   * @param s given String
   * @param c given Char
   * @return Outputs String with a deleted char or not depending if it found one
   */
  def delChar(s:String,c:Char):String = {
    val index:Int = findCharIndex(s,c)
    if(index>0){
      val firstPart = s.substring(0,index)
      val secondPart = s.substring(index+1,lengthOfString(s))
      concat(firstPart,secondPart)
    }
    else if(index==0) tail(s)
    else s
  }

  /**
   * Helper function of Swap at; Finds first occurence of given Char in String
   *                              OR returns 0 if not found at all.
   * @param s given String
   * @param c given Char
   * @param index default Value = 0 , index at 0 where we start checking
   * @return Outputs the Index where we found char or -1
   */
  @tailrec private def findCharIndex(s:String,c:Char,index:Int=0):Int = {
    if(s=="") -1
    else if(head(s)==c) index
    else findCharIndex(tail(s),c,index+1)
  }

  /**
   * Shifts last Char of a String in the front so : String+LastChar -> LastChar+String(without last Char)
   * @param s given String
   * @return Outputs shifted String
   */
  def shift(s:String):String = {
    shiftHelper(s)
  }

  /**
   * Private Helper Function for Shift
   * Figures out the Last Char of given String; Puts it in front of rest of the String
   * @param s given String
   * @param save default Value = "" ; Saves the rest of the String
   * @return Outputs shifted String
   */
  @tailrec private def shiftHelper(s:String,save:String=""):String = {
    if(lengthOfString(s)<=1) concat(s,save)
    else shiftHelper(tail(s),concat(save,toString2(head(s))))
  }

  /**
   * Shifts with substrings; Puts last char infront of rest
   * @param s given string
   * @return Outputs shifted String
   */
  def shiftWithSubString(s:String):String = {
    if(lengthOfString(s)<=1) s
    else concat(s.substring(lengthOfString(s)-1,lengthOfString(s)),s.substring(0,lengthOfString(s)-1))
  }

  /**
   * Checks if Unicode of first String is < Unicode of second String
   * First head of the Strings in Unicode, if they are equal second char and so on..
   * if equal its false; if first string end before 2nd does it's smaller and true : "Hel" < "Hello"
   * @param x first String
   * @param y second String
   * @return true only if first String < second String
   */
  @tailrec def less(x:String,y:String):Boolean = {
    if(x==y) false
    else if (x=="") true else if (y=="") false
    else if(toUnicode(head(x))<toUnicode(head(y))) true
    else less(tail(x),tail(y))
  }

  /**
   * Sorts a String based on it's Unicode
   * @param s given String
   * @return Outputs sorted String
   */
  def sort(s:String):String = {
    sortThat(s)
  }

  /**
   * Helper Function of Sort. Does the actual sorting stuff
   * @param s given String
   * @param save default Value = ""; Saves up the sorted String
   * @return Outputs sorted String
   */
  @tailrec private def sortThat(s:String,save:String=""):String = {
    if(s=="") save
    else{
      val smallestChar:Char = findSmallestChar(s,head(s))
      sortThat(delChar(s,smallestChar),concat(save,toString2(smallestChar)))
    }
  }

  /**
   * Helper function of sortThat - findsSmallestChar according to it's Unicode Value
   * @param s given String
   * @param c given Char that's supposed to be the Smallest Char
   * @return Outputs smallest Char found in String
   */
  @tailrec private def findSmallestChar(s:String,c:Char):Char = {
    if(s=="") c
    else findSmallestChar(tail(s),if(toUnicode(head(s))<toUnicode(c)) head(s) else c)
  }
}