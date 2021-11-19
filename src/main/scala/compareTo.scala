import FractionObject._

object compareTo {

  val compareDifferentType:Exception = new Exception("You tried to compare 2 different types")

  /**
   * Less function for any type
   * @param x first input
   * @param y 2nd input
   * @return Outputs Boolean depending if first input is smaller than 2nd
   */
  def compareTo(x:Any,y:Any):Boolean = x match{
    case b1:Boolean => y match{
      case b2:Boolean => !b1 && b1!=b2
      case _ => throw compareDifferentType
    }
    case i1:Int => y match{
      case i2:Int => i1<i2
      case _ => throw compareDifferentType
    }
    case bigI1:BigInt => y match{
      case bigI2:BigInt => bigI1<bigI2
      case _ => throw compareDifferentType
    }

    case s1:String => y match{
      case s2:String => StringAufgaben.less(s1,s2)
      case _ => throw compareDifferentType
    }

    case f1:Fraction => y match{
      case f2:Fraction => FractionObject.less(f1,f2)
      case _ => throw compareDifferentType
    }

    case _ => throw new Exception("Data type not found")
  }
}
