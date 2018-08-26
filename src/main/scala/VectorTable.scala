package xyz.hyperreal.m68k


object VectorTable {

  val stack = 0
  val start = 4
  val accessFault = 8
  val addressError = 12
  val illegalInstruction = 0x10
  val integerDivideByZero = 0x14
  val CHKInstruction = 0x18

}