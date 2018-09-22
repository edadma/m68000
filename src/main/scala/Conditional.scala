package xyz.hyperreal.m68k


object Conditional {

  def apply( cond: Int ) =
    cond match {
      case True => "RA"
      case False => "F"
      case High => "HI"
      case LowSame => "LS"
      case CarryClear => "CC"
      case CarrySet => "CS"
      case NotEqual => "NE"
      case Equal => "EQ"
      case OverflowClear => "VC"
      case OverflowSet => "VS"
      case Plus => "PL"
      case Minus => "MI"
      case GreaterEqual => "GE"
      case LessThan => "LT"
      case GreaterThan => "GT"
      case LessEqual => "LE"
    }

  val True = 0
  val False = 1
  val High = 2
  val LowSame = 3
  val CarryClear = 4
  val CarrySet = 5
  val NotEqual = 6
  val Equal = 7
  val OverflowClear = 8
  val OverflowSet = 9
  val Plus = 10
  val Minus = 11
  val GreaterEqual = 12
  val LessThan = 13
  val GreaterThan = 14
  val LessEqual = 15

}