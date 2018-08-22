package xyz.hyperreal.m68k


trait Addressing {

  val DataRegisterDirect = 0
  val AddressRegisterDirect = 1
  val AddressRegisterIndirect = 2
  val OtherModes = 7

  val ImmediateData = 4

}

trait Size
object ByteSize extends Size
object ShortSize extends Size
object IntSize extends Size