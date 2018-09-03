package xyz.hyperreal.m68k


object SRBit {

  val NO_TRACE_MASK = 0x3FFF
  val TRACE_ON_ANY_INSTRUCTION = 0x8000
  val TRACE_ON_CHANGE_OF_FLOW = 0x4000
  val S = 0x2000
  val M = 0x1000
  val I_shift = 8
  val I = 0x0700

}

object CCR {

  val X = 4
  val N = 3
  val Z = 2
  val V = 1
  val C = 0

}