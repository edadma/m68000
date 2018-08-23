package xyz.hyperreal.m68k


object SRBit {

  val T = 0xC0
  val NO_TRACE = 0x00
  val TRACE_ON_ANY_INSTRUCTION = 0x80
  val TRACE_ON_CHANGE_OF_FLOW = 0x40
  val S = 0x20
  val M = 0x10
  val I = 0x70
  val I_shift = 8

}