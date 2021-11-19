import MPAufgaben._

import scala.annotation.tailrec

object Lists {

  def main(args: Array[String]): Unit = {

  }

  /**
   * Abstract Class List
   *  nonEmptyList contains something and another list
   *  emptyList to end a nonEmptyList
   */
  abstract class List
  case class nonEmptyList(content:Any, furtherList:List) extends List
  case class emptyList() extends List

  /**
   * Calculates Length of given List
   * @param x given List
   * @return Outputs int of list length
   */
  def lengthOfList(x:List):Int = x match{
    case l:nonEmptyList => 1+lengthOfList(l.furtherList)
    case _:emptyList => 0
  }

  /**
   * String of a whole List with any content
   * @param x given List
   * @return Outputs String of the List
   */
  def toString2(x:List):String = x match{
    case l:nonEmptyList => l.furtherList match{
      case _:nonEmptyList => l.content + "," + toString2(l.furtherList)
      case _:emptyList => l.content + ""
    }
    case _:emptyList => ""
  }

  /**
   * Makes a new list with all found Primes in given Int List
   * @param x given List full of integers
   * @return Outputs List with all found Prims
   */
  def getPrimes(x:List):List = x match{
    case l:nonEmptyList => l.content match{
      case content:Int =>
        if(isPrim(content)) nonEmptyList(content, getPrimes(l.furtherList))
        else getPrimes(l.furtherList)
      case _ => throw new Exception("List is not made out of integers only")
    }
    case _:emptyList => emptyList()
  }

  /**
   * Reverses a given List
   * @param x given List
   * @return Outputs reversed List
   */
  def reverseList(x:List):List = {
    reverseListHelper(x)
  }

  //Helper Function of ReverseList
  @tailrec private def reverseListHelper(l:List, save:List=emptyList()):List = l match{
    case x:nonEmptyList => reverseListHelper(x.furtherList,nonEmptyList(x.content,save))
    case _:emptyList => save
  }

  /**
   * Appends 2 lists together
   * @param x first given list
   * @param y second given list
   * @return Outputs a list with first list + second list appends
   */
  def append(x:List,y:List):List = x match{
    case l:nonEmptyList => nonEmptyList(l.content,append(l.furtherList,y))
    case _:emptyList => y
  }

  def flatten(x:List):List = x match{
    case l:nonEmptyList => l.content match{
      case list:nonEmptyList => ??? //nonEmptyList(list.content,append(flatten(list.furtherList),flatten(l.furtherList)))
      case content:Any => ??? //nonEmptyList(content,flatten(l.furtherList))
    }
    case _:emptyList => emptyList()
  }
}
