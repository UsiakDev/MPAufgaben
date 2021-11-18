import MPAufgaben._

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

}
