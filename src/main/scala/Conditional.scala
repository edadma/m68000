package xyz.hyperreal.m68k


object Conditional extends Enumeration {

  val True = Value( "T" )
  val False = Value( "F" )
  val High = Value( "HI" )
  val LowSame = Value( "LS" )
  val CarryClear = Value( "CC(HS)" )
  val CarrySet = Value( "CS(LO)" )
  val NotEqual = Value( "NE" )
  val Equal = Value( "EQ" )
  val OverflowClear = Value( "VC" )
  val OverflowSet = Value( "VS" )
  val Plus = Value( "PL" )
  val Minus = Value( "MI" )
  val GreaterEqual = Value( "GE" )
  val LessThan = Value( "LT" )
  val GreaterThan = Value( "GT" )
  val LessEqual = Value( "LE" )

}