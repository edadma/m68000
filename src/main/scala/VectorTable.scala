package xyz.hyperreal.m68k


object VectorTable {

  val SSP =                 0x000
  val PC =                  0x004
  val accessFault =         0x008
  val addressError =        0x00C
  val illegalInstruction =  0x010
  val integerDivideByZero = 0x014
  val CHKInstruction =      0x018
  val TRAPVInstruction =    0x01C
  val privilegeViolation =  0x020
  val trace =               0x024
  val lineA =               0x028
  val lineF =               0x02C
  val TRAPInstruction =     0x080

}
