import MPAufgaben._
import compareTo._

import scala.annotation.tailrec
import scala.util.Random

object Lists {

  def main(args: Array[String]): Unit = {
    println(toString2(createRandomList(0)))
    println(toString2(createRandomList(1)))
    println(toString2(createRandomList(2)))
    println(toString2(createRandomList(16)))
    //println(toString2(bubbleSort(createRandomList(20))))
  }

  val intListOnly:Exception = new Exception("List is not made out of integers only")

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
      case _ => throw intListOnly
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

  /**
   * Flattens a multi dimensional List down to only 1 Dimension
   * @param l given List
   * @return Outputs flattened List
   */
  def flatten(l:List):List = flattenHelper(l)

  //Helper function of flatten
  @tailrec private def flattenHelper(x:List,save:List=emptyList()):List = x match{
    case mainList:nonEmptyList => mainList.content match{
      case list:nonEmptyList => flattenHelper(append(append(save,list),mainList.furtherList))
      case content:Any => flattenHelper(mainList.furtherList,add(save,content))
    }
    case _:emptyList => save
  }

  /**
   * Adds some content at the end of a List
   * @param x given List
   * @param content given Content
   * @return Outputs a List with added content
   */
  def add(x:List,content:Any):List = x match{
    case l:nonEmptyList => nonEmptyList(l.content,add(l.furtherList,content))
    case _:emptyList => nonEmptyList(content,emptyList())
  }

  /**
   * Calculates last number of a integer sequence list according to elsnerSpecial Game
   * @param l given List
   * @return Outputs calculated last number
   */
  def elsnerSpecial(l:List):Int = elsnerSpecialHelper(l)

  //Helper function of elsnerSpecial
  @tailrec private def elsnerSpecialHelper(list:List,counter:Int=0):Int = list match{
    case l:nonEmptyList => l.content match {
      case n:Int =>
        if(lengthOfList(list)==1) n
        else if(counter%2==0) elsnerSpecialHelper(add(l.furtherList,l.content),counter+1)
        else elsnerSpecialHelper(l.furtherList,counter+1)
      case _ => throw intListOnly
    }
    case _:emptyList => throw new Exception("Cant compute ElsnerSpecial on emptyList")
  }

  /**
   * Creates a random List with given length out of integer only. Ints have values from 0 to 100
   * @param n given length
   * @return Outputs randomized List
   */
  def createRandomList(n:Int):List = {
    if(n==0) emptyList()
    else {
      var list:List = emptyList()
      for(_ <-1 to n){
        list = add(list,Random.nextInt(100))
      }
      list
    }
  }

  /*
  def bubbleSort(list:List):List = list match {
    case _: nonEmptyList =>
      var liste = list
      for (_ <- 1 to lengthOfList(list)) {
        liste = bubbleSorting(liste)
      }
      liste
    case _:emptyList => emptyList()
  }

  @tailrec private def bubbleSorting(list:List,save:List=emptyList()):List = list match {
    case list:nonEmptyList => list.furtherList match {
      case n:nonEmptyList =>
        if(compareTo(list.content,n.content)) bubbleSorting(n,add(save,list.content))
        else bubbleSorting(nonEmptyList(list.content,n.furtherList),add(save,n.content))
      case _:emptyList => save
    }
    case _:emptyList => save
  }
   */
}
